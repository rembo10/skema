#!/usr/bin/env bash
# Development mode - auto-reload server on code changes using watchexec
# Supports launching backend server, frontend, or both in tmux

set -e

# Default config path (relative to root)
CONFIG_PATH="../config.yaml"
PORT=""
MODE=""

# Parse command-line arguments
while [[ $# -gt 0 ]]; do
  case $1 in
    -c)
      CONFIG_PATH="$2"
      shift 2
      ;;
    -p)
      PORT="$2"
      shift 2
      ;;
    -h|--help)
      echo "Usage: $0 [OPTIONS] [MODE]"
      echo ""
      echo "Modes:"
      echo "  server    Launch only the backend server (default if no mode specified)"
      echo "  web       Launch only the frontend dev server"
      echo "  (empty)   Launch both server and web in a tmux session"
      echo ""
      echo "Options:"
      echo "  -c PATH   Path to config file (default: ../config.yaml from server/)"
      echo "  -p PORT   Override port (default: read from config, or 8182 if not set)"
      echo "  -h, --help    Show this help message"
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

# Determine the port to use
if [[ -n "$PORT" ]]; then
  # Port explicitly provided via -p flag
  SKEMA_PORT="$PORT"
else
  # Try to read from config file
  cd server
  SKEMA_PORT=$(get_port_from_config "$CONFIG_PATH") || SKEMA_PORT="8182"
  cd ..
fi

export SKEMA_PORT

echo "🔧 Skema Development Environment"
echo "   Port: $SKEMA_PORT"
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
  cd web
  echo "📦 Installing dependencies..."
  npm install
  exec npm run dev
}

# Function to launch both in tmux
launch_both() {
  # Check if tmux is available
  if ! command -v tmux &> /dev/null; then
    echo "❌ tmux is not installed. Please install tmux or run server/web separately."
    echo "   To run separately:"
    echo "   Terminal 1: ./dev.sh server"
    echo "   Terminal 2: ./dev.sh web"
    exit 1
  fi

  SESSION_NAME="skema-dev"

  # Check if we're already inside a tmux session
  if [[ -n "$TMUX" ]]; then
    echo "📺 Already in tmux session, creating new window..."
    echo "   Backend on port $SKEMA_PORT, frontend on port 3000"
    echo ""

    # Create a new window for skema
    tmux new-window -n "skema"

    # Set environment variables
    tmux send-keys "export SKEMA_PORT=$SKEMA_PORT" C-m
    tmux send-keys "export SKEMA_DATA_DIR=\"$PWD/data\"" C-m
    tmux send-keys "export SKEMA_CACHE_DIR=\"$PWD/cache\"" C-m
    tmux send-keys "export SKEMA_STATE_DIR=\"$PWD/state\"" C-m

    # Start server
    tmux send-keys "cd server && echo '🚀 Backend Server (port $SKEMA_PORT)' && sleep 1" C-m
    tmux send-keys "cabal build exe:skema && watchexec --restart --clear --watch src --watch skema.cabal --exts hs,cabal -- cabal run exe:skema -- -c $CONFIG_PATH" C-m

    # Split window horizontally
    tmux split-window -h

    # Set environment in second pane
    tmux send-keys "export SKEMA_PORT=$SKEMA_PORT" C-m

    # Start web
    tmux send-keys "cd web && echo '🌐 Frontend Dev Server (port 3000 -> proxies to $SKEMA_PORT)' && sleep 1" C-m
    tmux send-keys "npm install && npm run dev" C-m

    echo "✅ Skema dev environment started in new window"
    echo "   Use Ctrl+b then 'n' to switch to the skema window"
    exit 0
  fi

  # Not in tmux, so create new session or attach to existing

  # Check if session already exists
  if tmux has-session -t "$SESSION_NAME" 2>/dev/null; then
    echo "📺 Attaching to existing tmux session: $SESSION_NAME"
    exec tmux attach-session -t "$SESSION_NAME"
  fi

  echo "📺 Creating new tmux session: $SESSION_NAME"
  echo "   Backend on port $SKEMA_PORT, frontend on port 3000"
  echo ""
  echo "💡 Tmux shortcuts:"
  echo "   Ctrl+b then \"%\"  - Split pane vertically"
  echo "   Ctrl+b then arrow - Navigate between panes"
  echo "   Ctrl+b then d    - Detach from session"
  echo "   Ctrl+b then x    - Kill current pane"
  echo ""
  sleep 2

  # Create new session with server
  tmux new-session -d -s "$SESSION_NAME" -n "skema"

  # Set environment in tmux session
  tmux send-keys -t "$SESSION_NAME:0" "export SKEMA_PORT=$SKEMA_PORT" C-m
  tmux send-keys -t "$SESSION_NAME:0" "export SKEMA_DATA_DIR=\"$PWD/data\"" C-m
  tmux send-keys -t "$SESSION_NAME:0" "export SKEMA_CACHE_DIR=\"$PWD/cache\"" C-m
  tmux send-keys -t "$SESSION_NAME:0" "export SKEMA_STATE_DIR=\"$PWD/state\"" C-m

  # Start server in first pane
  tmux send-keys -t "$SESSION_NAME:0" "cd server && echo '🚀 Backend Server (port $SKEMA_PORT)' && sleep 1" C-m
  tmux send-keys -t "$SESSION_NAME:0" "cabal build exe:skema && watchexec --restart --clear --watch src --watch skema.cabal --exts hs,cabal -- cabal run exe:skema -- -c $CONFIG_PATH" C-m

  # Split window horizontally
  tmux split-window -h -t "$SESSION_NAME:0"

  # Set environment in second pane
  tmux send-keys -t "$SESSION_NAME:0.1" "export SKEMA_PORT=$SKEMA_PORT" C-m

  # Start web in second pane
  tmux send-keys -t "$SESSION_NAME:0.1" "cd web && echo '🌐 Frontend Dev Server (port 3000 -> proxies to $SKEMA_PORT)' && sleep 1" C-m
  tmux send-keys -t "$SESSION_NAME:0.1" "npm install && npm run dev" C-m

  # Attach to session
  exec tmux attach-session -t "$SESSION_NAME"
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
    # Default: launch both in tmux
    launch_both
    ;;
esac
