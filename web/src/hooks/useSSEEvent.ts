import { useEffect, useRef } from 'react';

const sseEventBus = new EventTarget();

export function emitSSEEvent(name: string, data: unknown) {
  sseEventBus.dispatchEvent(new CustomEvent(name, { detail: data }));
}

export function useSSEEvent<T = unknown>(name: string, handler: (data: T) => void) {
  const handlerRef = useRef(handler);
  handlerRef.current = handler;

  useEffect(() => {
    const listener = (e: Event) => handlerRef.current((e as CustomEvent).detail);
    sseEventBus.addEventListener(name, listener);
    return () => sseEventBus.removeEventListener(name, listener);
  }, [name]);
}

/**
 * Subscribe to several SSE events and run `reload` when any of them fires,
 * debounced so a burst of events (e.g. during a library scan) triggers a
 * single reload rather than one per event.
 */
export function useSSERefresh(names: string[], reload: () => void, debounceMs = 500) {
  const reloadRef = useRef(reload);
  reloadRef.current = reload;
  const timeoutRef = useRef<ReturnType<typeof setTimeout> | null>(null);
  const key = names.join(',');

  useEffect(() => {
    const listener = () => {
      if (timeoutRef.current) clearTimeout(timeoutRef.current);
      timeoutRef.current = setTimeout(() => reloadRef.current(), debounceMs);
    };
    names.forEach((name) => sseEventBus.addEventListener(name, listener));
    return () => {
      names.forEach((name) => sseEventBus.removeEventListener(name, listener));
      if (timeoutRef.current) clearTimeout(timeoutRef.current);
    };
    // `names` is reconstructed each render; `key` captures its identity.
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [key, debounceMs]);
}
