# Skema

skema is a music library management/acquisition system.

> ⚠️ **Note**: This is a proof-of-concept project. It's functional but has lots of rough edges.

## Quick Start

### Using Nix (Recommended)

```bash
# Enter development shell with all dependencies
nix develop

# Run both backend and frontend with auto-reload (requires tmux)
./dev.sh

# Or run them separately:
./dev.sh server  # Backend only
./dev.sh web     # Frontend only
```

The `dev.sh` script provides auto-reload on code changes and runs both services in a tmux session for convenience.

### Manual Setup

If not using Nix, you'll need to install the following dependencies:

**Prerequisites:**
- GHC 9.10.3
- Cabal 3.4+
- Node.js & npm
- SQLite

**System dependencies:**

```bash
# Ubuntu/Debian
sudo apt-get install zlib1g-dev libsqlite3-dev

# macOS
brew install zlib sqlite3
```

Then run the services:

```bash
# Backend
cd server
cabal run skema

# Frontend (in another terminal)
cd web
npm install
npm run dev
```

The backend will run on `http://localhost:8181` and the frontend on `http://localhost:3000`.

## License

Unlicense - see LICENSE for details.
