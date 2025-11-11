# Skema

skema is a music library management/acquisition system.

> ⚠️ **Note**: This is a proof-of-concept project. It's functional but has lots of rough edges.

## Quick Start

### Using Nix (Recommended)

```bash
# Enter development shell with all dependencies
nix develop

# Run backend and frontend in separate terminals:
# Terminal 1:
./dev.sh server  # Backend with auto-reload

# Terminal 2:
./dev.sh web     # Frontend dev server
```

The `dev.sh` script provides auto-reload on code changes for the backend and hot-reload for the frontend.

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

The backend will run on `http://localhost:8182` and the frontend on `http://localhost:3000`.

## License

Unlicense - see LICENSE for details.
