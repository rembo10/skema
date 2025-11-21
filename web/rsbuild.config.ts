import { defineConfig } from '@rsbuild/core';
import { pluginReact } from '@rsbuild/plugin-react';

// Read ports from environment or use defaults
const backendPort = process.env.SKEMA_PORT || '8182';
const frontendPort = parseInt(process.env.SKEMA_FRONTEND_PORT || '3000', 10);

export default defineConfig({
  plugins: [pluginReact()],
  html: {
    template: './index.html',
  },
  server: {
    port: frontendPort,
    proxy: {
      '/api': {
        target: `http://localhost:${backendPort}`,
        changeOrigin: true,
      },
      '/images': {
        target: `http://localhost:${backendPort}`,
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
