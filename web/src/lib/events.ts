import { getJWT, getApiBase } from './api';
import type { LibraryStats, Task } from '../types/api';

export type ServerEventType =
  | 'stats'
  | 'tasks'
  | 'TaskCreated'
  | 'TaskStarted'
  | 'TaskProgressUpdated'
  | 'TaskCompleted'
  | 'TaskFailed'
  | 'LibraryScanRequested'
  | 'MetadataReadStarted'
  | 'MetadataReadProgress'
  | 'MetadataReadComplete'
  | 'MetadataWriteStarted'
  | 'MetadataWriteProgress'
  | 'MetadataWriteCompleted'
  | 'MetadataWriteFailed'
  | 'StatsUpdated'
  | 'ConfigUpdated'
  | 'TrackedArtistAdded'
  | 'Heartbeat';

export interface ServerEvent {
  type: ServerEventType;
  data: unknown;
}

export type EventHandler = (event: ServerEvent) => void;

/**
 * Creates and manages a Server-Sent Events connection to the backend.
 * Automatically reconnects on disconnect.
 */
export class EventSource {
  private eventSource: globalThis.EventSource | null = null;
  private handlers: Map<ServerEventType, Set<EventHandler>> = new Map();
  private url: string;
  private reconnectTimeout: number | null = null;
  private reconnectDelay = 3000; // 3 seconds
  private maxReconnectDelay = 30000; // 30 seconds

  constructor(url: string) {
    this.url = url;
  }

  /**
   * Connect to the event stream.
   * If a JWT is available, it will be included in the connection.
   */
  connect(): void {
    const jwt = getJWT();

    // Include JWT in URL if available (EventSource doesn't support custom headers)
    const url = jwt
      ? `${this.url}?token=${encodeURIComponent(jwt)}`
      : this.url;

    try {
      console.log('[SSE] Connecting to event stream...');
      this.eventSource = new globalThis.EventSource(url);

      this.eventSource.onopen = () => {
        console.log('[SSE] Connected');
        this.reconnectDelay = 3000; // Reset reconnect delay on successful connection
      };

      this.eventSource.onerror = (error) => {
        console.error('[SSE] Error:', error);
        this.handleDisconnect();
      };

      // Helper to add event listener
      const addListener = (eventType: ServerEventType) => {
        this.eventSource!.addEventListener(eventType, (event: MessageEvent) => {
          try {
            const data = JSON.parse(event.data);
            // Only log non-heartbeat events to reduce noise
            if (eventType !== 'Heartbeat') {
              console.log(`[SSE] Received ${eventType} event:`, data);
            }
            this.emit({ type: eventType, data });
          } catch (err) {
            console.error(`[SSE] Failed to parse ${eventType} event:`, err);
          }
        });
      };

      // Listen for all event types
      addListener('stats');
      addListener('TaskCreated');
      addListener('TaskStarted');
      addListener('TaskProgressUpdated');
      addListener('TaskCompleted');
      addListener('TaskFailed');
      addListener('StatsUpdated');
      addListener('ConfigUpdated');
      addListener('Heartbeat');
    } catch (err) {
      console.error('[SSE] Failed to create EventSource:', err);
      this.handleDisconnect();
    }
  }

  /**
   * Disconnect from the event stream.
   */
  disconnect(): void {
    if (this.reconnectTimeout) {
      clearTimeout(this.reconnectTimeout);
      this.reconnectTimeout = null;
    }

    if (this.eventSource) {
      this.eventSource.close();
      this.eventSource = null;
      console.log('[SSE] Disconnected');
    }
  }

  /**
   * Handle disconnection and schedule reconnect.
   */
  private handleDisconnect(): void {
    this.disconnect();

    // Schedule reconnect with exponential backoff
    console.log(`[SSE] Reconnecting in ${this.reconnectDelay}ms...`);
    this.reconnectTimeout = window.setTimeout(() => {
      this.connect();
      // Increase delay for next time (up to max)
      this.reconnectDelay = Math.min(this.reconnectDelay * 2, this.maxReconnectDelay);
    }, this.reconnectDelay);
  }

  /**
   * Subscribe to events of a specific type.
   */
  on(eventType: ServerEventType, handler: EventHandler): () => void {
    if (!this.handlers.has(eventType)) {
      this.handlers.set(eventType, new Set());
    }
    this.handlers.get(eventType)!.add(handler);

    // Return unsubscribe function
    return () => {
      const handlers = this.handlers.get(eventType);
      if (handlers) {
        handlers.delete(handler);
      }
    };
  }

  /**
   * Emit an event to all registered handlers.
   */
  private emit(event: ServerEvent): void {
    const handlers = this.handlers.get(event.type);
    if (handlers) {
      handlers.forEach(handler => {
        try {
          handler(event);
        } catch (err) {
          console.error(`[SSE] Error in ${event.type} handler:`, err);
        }
      });
    }
  }
}

// Create a singleton instance
let eventSourceInstance: EventSource | null = null;

/**
 * Get or create the global EventSource instance.
 */
export function getEventSource(): EventSource {
  if (!eventSourceInstance) {
    const apiBase = getApiBase();
    eventSourceInstance = new EventSource(`${apiBase}/events`);
  }
  return eventSourceInstance;
}
