# Multi-stage Dockerfile for Skema
# Stage 1: Build frontend
FROM node:lts-alpine AS frontend-builder

WORKDIR /build/web

# Copy frontend package files
COPY web/package*.json ./

# Install dependencies (including devDependencies needed for build)
RUN npm ci

# Copy frontend source
COPY web/ ./

# Build frontend
RUN npm run build

# Stage 2: Build backend
FROM haskell:9.10.3 AS backend-builder

# Install system dependencies
RUN apt-get update && apt-get install -y \
    libgmp-dev \
    libsqlite3-dev \
    libffi-dev \
    zlib1g-dev \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /build/server

# Copy cabal files first for better caching
COPY server/skema.cabal server/cabal.project ./

# Update cabal package list
RUN cabal update

# Build dependencies (cached layer)
RUN cabal build --only-dependencies -j$(nproc)

# Copy source code
COPY server/src ./src
COPY server/app ./app
COPY LICENSE ../LICENSE

# Build the application
RUN cabal build exe:skema -j$(nproc)

# Find and copy the built binary
RUN find dist-newstyle -name skema -type f -executable -exec cp {} /build/skema \;

# Stage 3: Runtime
FROM debian:stable-slim

# Install runtime dependencies
RUN apt-get update && apt-get install -y \
    libgmp10 \
    libsqlite3-0 \
    libffi8 \
    zlib1g \
    ca-certificates \
    wget \
    locales \
    && rm -rf /var/lib/apt/lists/*

# Configure UTF-8 locale to handle Unicode filenames properly
RUN sed -i '/en_US.UTF-8/s/^# //g' /etc/locale.gen && \
    locale-gen en_US.UTF-8

ENV LANG=en_US.UTF-8
ENV LANGUAGE=en_US:en
ENV LC_ALL=en_US.UTF-8

# Create non-root user
RUN groupadd -g 1000 skema && \
    useradd -u 1000 -g skema -m -s /bin/bash skema

# Copy binary from builder
COPY --from=backend-builder /build/skema /usr/local/bin/skema

# Copy frontend assets from frontend builder
COPY --from=frontend-builder /build/web/dist /usr/share/skema/web

# Set environment variables for Docker deployment
ENV SKEMA_WEB_ROOT=/usr/share/skema/web
ENV SKEMA_PORT=8182
ENV SKEMA_DATA_DIR=/data
ENV SKEMA_CACHE_DIR=/cache

# Switch to non-root user
USER skema

# Expose port
EXPOSE 8182

# Health check
HEALTHCHECK --interval=30s --timeout=10s --start-period=5s --retries=3 \
    CMD wget --no-verbose --tries=1 --spider http://localhost:8182/ || exit 1

# Set working directory
WORKDIR /data

# Run the application
CMD ["/usr/local/bin/skema", "--config", "/config/config.yaml"]
