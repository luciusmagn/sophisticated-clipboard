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
        ]);
      in {
        packages = {
          default = self.packages.${system}.sophisticated-clipboard-tests;

          sophisticated-clipboard-tests = pkgs.stdenv.mkDerivation {
            pname = "sophisticated-clipboard-tests";
            version = "0.0.0.0";
            src = ./.;

            nativeBuildInputs = [
              sbcl'
            ];

            dontStrip = true;

            buildPhase = ''
              export HOME=$TMPDIR

              # Load and test the system
              sbcl --eval "(load (sb-ext:posix-getenv \"ASDF\"))" \
                   --eval "(push \"$PWD/\" asdf:*central-registry*)" \
                   --eval "(asdf:load-system :sophisticated-clipboard)" \
                   --eval "(asdf:test-system :sophisticated-clipboard)" \
                   --quit
            '';

            installPhase = ''
              mkdir -p $out
              echo "Guess it's okay" > $out/lol.txt
            '';
          };
        };

        devShells.default = pkgs.mkShell {
          buildInputs = with pkgs; [
            sbcl'
            wl-clipboard
            xclip
          ];

          shellHook = ''
            export CL_SOURCE_REGISTRY="$PWD:$CL_SOURCE_REGISTRY"
          '';
        };
      });
}
