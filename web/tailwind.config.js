/** @type {import('tailwindcss').Config} */
export default {
  content: [
    './src/**/*.{js,ts,jsx,tsx}',
    './index.html',
  ],
  theme: {
    extend: {
      colors: {
        dark: {
          bg: '#0d0f12',
          'bg-elevated': '#15181d',
          'bg-subtle': '#1a1e24',
          'bg-hover': '#20252c',
          border: '#2a2f38',
          'border-bright': '#3a4048',
          text: '#e8e8e8',
          'text-secondary': '#a8b0ba',
          'text-tertiary': '#6b7280',
          accent: '#14daf0',
          'accent-hover': '#3de9ff',
          'accent-muted': '#0a5a66',
          success: '#10b981',
          'success-muted': '#064e3b',
          warning: '#f59e0b',
          'warning-muted': '#78350f',
          error: '#ef4444',
          'error-muted': '#7f1d1d',
          info: '#3b82f6',
          'info-muted': '#1e3a8a',
        },
      },
      fontFamily: {
        mono: ['JetBrains Mono', 'Fira Code', 'Consolas', 'Monaco', 'monospace'],
      },
      animation: {
        'pulse-slow': 'pulse 3s cubic-bezier(0.4, 0, 0.6, 1) infinite',
        'slide-up': 'slideUp 0.3s ease-out',
        'fade-in': 'fadeIn 0.2s ease-out',
        'glow': 'glow 2s ease-in-out infinite',
      },
      keyframes: {
        slideUp: {
          '0%': { transform: 'translateY(10px)', opacity: '0' },
          '100%': { transform: 'translateY(0)', opacity: '1' },
        },
        fadeIn: {
          '0%': { opacity: '0' },
          '100%': { opacity: '1' },
        },
        glow: {
          '0%, 100%': { opacity: '1' },
          '50%': { opacity: '0.5' },
        },
      },
    },
  },
  plugins: [],
}
