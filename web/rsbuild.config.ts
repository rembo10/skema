import { defineConfig } from '@rsbuild/core';
import { pluginReact } from '@rsbuild/plugin-react';

// Read backend port from environment or default to 8181
const backendPort = process.env.SKEMA_PORT || '8181';

export default defineConfig({
  plugins: [pluginReact()],
  html: {
    template: './index.html',
  },
  server: {
    port: 3000,
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
