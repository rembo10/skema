import toast from 'react-hot-toast';

/**
 * Standard API error handler. Suppresses 401 auth errors (the global
 * handler in App.tsx redirects to login), logs and toasts everything else.
 *
 * @returns true if the error was an auth error (caller can skip further handling)
 */
export function handleApiError(error: unknown, message: string): boolean {
  if (
    error !== null &&
    typeof error === 'object' &&
    'isAuthError' in error &&
    (error as { isAuthError: boolean }).isAuthError
  ) {
    return true;
  }
  console.error(message, error);
  toast.error(message);
  return false;
}
