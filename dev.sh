#!/usr/bin/env bash
# Development mode - auto-reload server on code changes using watchexec

set -e

# Default config path (relative to root)
CONFIG_PATH="../config.yaml"
BACKEND_HOST=""
BACKEND_PORT=""
FRONTEND_PORT=""
MODE="server"

# Parse command-line arguments
while [[ $# -gt 0 ]]; do
  case $1 in
    -c)
      CONFIG_PATH="$2"
      shift 2
      ;;
    --host)
      BACKEND_HOST="$2"
      shift 2
      ;;
    -p|--backend-port)
      BACKEND_PORT="$2"
      shift 2
      ;;
    --frontend-port)
      FRONTEND_PORT="$2"
      shift 2
      ;;
    -h|--help)
      echo "Usage: $0 [OPTIONS] MODE"
      echo ""
      echo "Modes:"
      echo "  server    Launch the backend server with auto-reload (default)"
      echo "  web       Launch the frontend dev server"
      echo ""
      echo "Run in two separate terminals:"
      echo "  Terminal 1: ./dev.sh server"
      echo "  Terminal 2: ./dev.sh web"
      echo ""
      echo "Options:"
      echo "  -c PATH              Path to config file (default: ../config.yaml)"
      echo "  -p, --backend-port   Backend port (default: read from config, or 8182)"
      echo "  --frontend-port      Frontend port (default: 3000)"
      echo "  -h, --help           Show this help message"
      echo ""
      echo "Examples:"
      echo "  ./dev.sh server -p 8080"
      echo "  ./dev.sh web -p 8080 --frontend-port 4000"
      exit 0
      ;;
    server|web)
      MODE="$1"
      shift
      ;;
    *)
      echo "Unknown argument: $1" >&2
      echo "Use -h for help"
      exit 1
      ;;
  esac
done

# Function to extract port from YAML config
get_port_from_config() {
  local config_file="$1"
  if [[ -f "$config_file" ]]; then
    # Try to extract port from server.port in YAML
    # This is a simple grep/sed approach that works for basic YAML
    local port_line=$(grep -A 10 "^server:" "$config_file" | grep "^\s*port:" | head -n1)
    if [[ -n "$port_line" ]]; then
      echo "$port_line" | sed 's/.*port:\s*//' | tr -d ' '
      return 0
    fi
  fi
  return 1
}

# Set the backend hostname to use, if provided
# Default value is localhost
# See web/rsbuild.config.ts for usage
if [[ -n "$BACKEND_HOST" ]]; then
  # Host explicitly provided via -h flag
  SKEMA_HOST="$BACKEND_HOST"
fi

# Determine the backend port to use
if [[ -n "$BACKEND_PORT" ]]; then
  # Port explicitly provided via -p flag
  SKEMA_PORT="$BACKEND_PORT"
else
  # Try to read from config file
  cd server
  SKEMA_PORT=$(get_port_from_config "$CONFIG_PATH") || SKEMA_PORT="8182"
  cd ..
fi

# Determine frontend port
if [[ -n "$FRONTEND_PORT" ]]; then
  SKEMA_FRONTEND_PORT="$FRONTEND_PORT"
else
  SKEMA_FRONTEND_PORT="3000"
fi

export SKEMA_HOST
export SKEMA_PORT
export SKEMA_FRONTEND_PORT

echo "🔧 Skema Development Environment"
echo "   Backend Host:  $SKEMA_HOST"
echo "   Backend Port:  $SKEMA_PORT"
echo "   Frontend Port: $SKEMA_FRONTEND_PORT"
echo ""

# Function to launch server
launch_server() {
  echo "📦 Starting backend server..."
  cd server

  # Set development data directories (keeps everything in project)
  export SKEMA_DATA_DIR="../data"
  export SKEMA_CACHE_DIR="../cache"
  export SKEMA_STATE_DIR="../state"

  echo "📁 Development directories:"
  echo "   Data:  $SKEMA_DATA_DIR"
  echo "   Cache: $SKEMA_CACHE_DIR"
  echo "   State: $SKEMA_STATE_DIR"
  echo "   Config: $CONFIG_PATH"
  echo ""

  # Build once first
  echo "📦 Initial build..."
  cabal build exe:skema

  # Watch for changes and rebuild/restart
  exec watchexec \
    --restart \
    --clear \
    --watch src \
    --watch skema.cabal \
    --exts hs,cabal \
    -- cabal run exe:skema -- -c "$CONFIG_PATH"
}

# Function to launch web
launch_web() {
  echo "🌐 Starting frontend dev server..."
  echo "   Frontend on port $SKEMA_FRONTEND_PORT"
  echo "   Proxying API requests to backend on port $SKEMA_PORT"
  echo ""
  cd web
  echo "📦 Installing dependencies..."
  npm install
  exec npm run dev
}

# Execute based on mode
case "$MODE" in
  server)
    launch_server
    ;;
  web)
    launch_web
    ;;
  *)
    echo "❌ Please specify a mode: server or web"
    echo "   Terminal 1: ./dev.sh server"
    echo "   Terminal 2: ./dev.sh web"
    exit 1
    ;;
esac
