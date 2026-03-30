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
