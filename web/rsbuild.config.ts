import { defineConfig } from '@rsbuild/core';
import { pluginReact } from '@rsbuild/plugin-react';

// Read ports from environment or use defaults
const backendPort = process.env.SKEMA_PORT || '8181';
const frontendPort = parseInt(process.env.SKEMA_FRONTEND_PORT || '3000', 10);
const backendHost = process.env.SKEMA_HOST || 'localhost';

export default defineConfig({
  plugins: [pluginReact()],
  html: {
    template: './index.html',
  },
  server: {
    port: frontendPort,
    proxy: {
      '/api': {
        target: `http://${backendHost}:${backendPort}`,
        changeOrigin: true,
      },
      '/images': {
        target: `http://${backendHost}:${backendPort}`,
        changeOrigin: true,
      },
    },
  },
  output: {
    distPath: {
      root: 'dist',
    },
    // Use relative paths for assets to support deployment at any subpath
    assetPrefix: './',
  },
});
