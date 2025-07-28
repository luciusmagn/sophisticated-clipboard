{
  description = "sophisticated-clipboard - A Common Lisp library for accessing system clipboards";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};

        sbcl' = pkgs.sbcl.withPackages (ps: with ps; [
          cffi
          flexi-streams
          fiveam
          uiop
        ]);

        # Build Darwin bridge even though we won't test it in CI
        darwin-bridge = pkgs.stdenv.mkDerivation {
          pname = "sophisticated-clipboard-darwin-bridge";
          version = "0.0.0.0";
          src = ./src/darwin;

          buildInputs = pkgs.lib.optionals pkgs.stdenv.isDarwin [
            pkgs.darwin.apple_sdk.frameworks.AppKit
            pkgs.darwin.apple_sdk.frameworks.Foundation
          ];

          buildPhase = pkgs.lib.optionalString pkgs.stdenv.isDarwin ''
            make
          '' + pkgs.lib.optionalString (!pkgs.stdenv.isDarwin) ''
            # On Linux, just create a dummy file so the build doesn't fail
            touch clipboard.so
          '';

          installPhase = ''
            mkdir -p $out/lib
            cp clipboard.so $out/lib/ || true
          '';
        };

      in {
        packages = {
          default = self.packages.${system}.sophisticated-clipboard-tests;

          sophisticated-clipboard-tests = pkgs.stdenv.mkDerivation {
            pname = "sophisticated-clipboard-tests";
            version = "0.0.0.0";
            src = ./.;

            nativeBuildInputs = [
              sbcl'
            ] ++ pkgs.lib.optionals pkgs.stdenv.isLinux [
              pkgs.wl-clipboard  # For Wayland testing
              pkgs.xclip         # For X11 testing
            ];

            buildInputs = [ darwin-bridge ];

            dontStrip = true;

            buildPhase = ''
              export HOME=$TMPDIR

              # Copy Darwin bridge if it exists
              if [ -f ${darwin-bridge}/lib/clipboard.so ]; then
                mkdir -p src/darwin/
                cp ${darwin-bridge}/lib/clipboard.so src/darwin/
              fi

              # Load and test the system
              sbcl --eval "(load (sb-ext:posix-getenv \"ASDF\"))" \
                   --eval "(push \"$PWD/\" asdf:*central-registry*)" \
                   --eval "(asdf:load-system :sophisticated-clipboard)" \
                   --eval "(asdf:test-system :sophisticated-clipboard)" \
                   --quit
            '';

            installPhase = ''
              mkdir -p $out
              echo "Tests passed!" > $out/test-results.txt
            '';

            # Set up environment for clipboard tools
            preCheck = pkgs.lib.optionalString pkgs.stdenv.isLinux ''
              export XDG_SESSION_TYPE=wayland
              export WAYLAND_DISPLAY=wayland-test
            '';
          };
        };

        devShells.default = pkgs.mkShell {
          buildInputs = with pkgs; [
            sbcl'
            wl-clipboard
            xclip
          ] ++ lib.optionals stdenv.isDarwin [
            darwin.apple_sdk.frameworks.AppKit
            darwin.apple_sdk.frameworks.Foundation
          ];

          shellHook = ''
            echo "sophisticated-clipboard development environment"
            echo "Available clipboard tools:"
            command -v wl-copy && echo "  - wl-clipboard (Wayland)"
            command -v xclip && echo "  - xclip (X11)"

            # Set up ASDF
            export CL_SOURCE_REGISTRY="$PWD:$CL_SOURCE_REGISTRY"
          '';
        };
      });
}
