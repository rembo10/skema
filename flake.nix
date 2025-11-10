{
  description = "skema - music library management and acquisition system";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    monatone = {
      url = "github:rembo10/monatone";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, flake-utils, monatone }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};

        # Build the frontend
        skema-web = pkgs.buildNpmPackage {
          pname = "skema-web";
          version = "0.1.0";

          src = ./web;

          npmDepsHash = "sha256-QUWytH512P0MEGR9B3XNwqMulPAHuY6SJ7bN5gnQo6Q=";

          # Use npm ci for reproducible builds
          npmBuildScript = "build";

          installPhase = ''
            runHook preInstall
            mkdir -p $out
            cp -r dist/* $out/
            runHook postInstall
          '';

          meta = {
            description = "Skema web frontend";
          };
        };

        # Use GHC 9.10 to match monatone
        haskellPackages = pkgs.haskell.packages.ghc910.override {
          overrides = hself: hsuper: {
            # Use monatone from the flake input
            monatone = monatone.packages.${system}.default;

            # Override skema to include all its dependencies
            skema = hself.callCabal2nix "skema" ./server {
              monatone = hself.monatone;
            };
          };
        };

        # Backend only (Haskell)
        skema-backend = haskellPackages.skema;

        # Combined package with backend and frontend
        skema = pkgs.stdenv.mkDerivation {
          pname = "skema";
          version = "0.1.0";

          dontUnpack = true;

          nativeBuildInputs = [ pkgs.makeWrapper ];

          installPhase = ''
            mkdir -p $out/bin
            mkdir -p $out/share/skema/web

            # Copy frontend files
            cp -r ${skema-web}/* $out/share/skema/web/

            # Wrap backend binary with SKEMA_WEB_ROOT environment variable
            makeWrapper ${skema-backend}/bin/skema $out/bin/skema \
              --set SKEMA_WEB_ROOT $out/share/skema/web
          '';

          meta = {
            description = "Skema - declarative music library manager";
            mainProgram = "skema";
          };
        };

      in
      {
        packages = {
          default = skema;
          skema = skema;
          skema-backend = skema-backend;
          skema-web = skema-web;
        };

        devShells.default = haskellPackages.shellFor {
          packages = p: [ skema-backend ];

          buildInputs = with pkgs; [
            # Haskell tooling
            haskellPackages.cabal-install
            haskellPackages.haskell-language-server
            haskellPackages.hlint
            haskellPackages.ormolu
            haskellPackages.cabal2nix
            haskellPackages.hspec-discover

            # Development tools
            watchexec  # Auto-reload on file changes

            # Frontend tools
            nodejs_24  # Node.js for web frontend

            # System tools
            git
            sqlite
            tmux # for the ./dev.sh script
            zlib
          ];

          shellHook = ''
            echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
            echo "🎵 Skema Development Environment"
            echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
            echo ""
            echo "📦 Toolchain:"
            echo "   GHC version:     $(ghc --version | cut -d ' ' -f 8)"
            echo "   Cabal version:   $(cabal --version | cut -d ' ' -f 3)"
            echo "   Node.js version: $(node --version)"
            echo "   npm version:     $(npm --version)"
            echo ""
            echo "🛠️  Available commands:"
            echo "   cabal build              - Build the project"
            echo "   cabal test               - Run all tests"
            echo "   cabal test --test-show-details=direct"
            echo "                            - Run tests with output"
            echo "   cabal repl               - Start GHCi REPL"
            echo "   ghcid                    - Auto-reload on changes"
            echo "   ghcid --test=':main'     - Auto-run tests"
            echo ""
            echo "🔧 Development mode:"
            echo "   ./dev.sh           - Auto-reload server on code changes"
            echo ""
            echo "✨ Formatting:"
            echo "   ormolu -i src/**/*.hs    - Format all source files"
            echo "   hlint src/               - Lint source code"
            echo ""
            echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
          '';
        };

        # Useful for CI or building without entering shell
        apps.default = {
          type = "app";
          program = "${skema}/bin/skema";
        };
      });
}
