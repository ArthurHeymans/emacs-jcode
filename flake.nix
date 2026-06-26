{
  description = "Emacs frontend plan/package for jcode";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";

    # jcode is not currently a flake, so use it as a source input and build it
    # with nixpkgs' Rust builder.
    jcode-src = {
      url = "github:1jehuang/jcode";
      flake = false;
    };
  };

  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
      jcode-src,
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = import nixpkgs { inherit system; };
        inherit (pkgs) lib stdenv;

        packageSrc = lib.cleanSourceWith {
          src = ./.;
          filter = path: type:
            let
              base = baseNameOf path;
            in
            !lib.hasSuffix ".elc" base
            && base != ".jj"
            && base != ".git"
            && base != "result"
            && base != "test";
        };

        jcode = pkgs.rustPlatform.buildRustPackage {
          pname = "jcode";
          version = "0.31.2-${jcode-src.shortRev or "source"}";
          src = jcode-src;

          # Build only the main CLI binary.  The upstream workspace contains
          # probes/benches/desktop crates that are not needed for the Emacs
          # client development shell.
          cargoBuildFlags = [ "--bin" "jcode" ];
          cargoTestFlags = [ "--bin" "jcode" ];
          doCheck = false;

          cargoLock = {
            lockFile = "${jcode-src}/Cargo.lock";
            # If upstream adds/changes git dependencies, Nix may ask for
            # cargoLock.outputHashes entries here.  Keep this attrset as the
            # single place to pin them.
            outputHashes = {
              "agentgrep-0.1.2" = "sha256-Sf3EmWIZJ29KdaNbYRvM1tFXAPhOGhmpHOyqViEwkRI=";
              "agentgrep-0.1.3" = "sha256-vs8RK85sMa4WVupKU1V2oWxEVs1yHkEy7WNoTCNcMtE=";
              "mermaid-rs-renderer-0.2.0" = "sha256-lQCloOhTqqEU8MNrkUmmJFdoOTEE3j5nvZJo21GJlMU=";
            };
          };

          nativeBuildInputs = with pkgs; [
            cmake
            pkg-config
            perl
          ];

          buildInputs =
            lib.optionals stdenv.isLinux (with pkgs; [
              fontconfig
              libxkbcommon
              wayland
              libxcb
            ])
            ++ lib.optionals stdenv.isDarwin (with pkgs.darwin.apple_sdk.frameworks; [
              AppKit
              CoreFoundation
              CoreGraphics
              Foundation
              Security
              SystemConfiguration
            ]);

          meta = {
            description = "jcode coding agent harness";
            homepage = "https://github.com/1jehuang/jcode";
            license = lib.licenses.mit;
            mainProgram = "jcode";
          };
        };
      in
      {
        packages.jcode-emacs = pkgs.emacsPackages.trivialBuild {
          pname = "jcode-emacs";
          version = "0.1.0";
          src = packageSrc;
          packageRequires = [ pkgs.emacsPackages.md-ts-mode ];
          meta = {
            description = "Emacs frontend for jcode";
            license = lib.licenses.gpl3Plus;
          };
        };

        packages.jcode = jcode;
        packages.default = self.packages.${system}.jcode-emacs;

        apps.jcode = flake-utils.lib.mkApp { drv = jcode; };

        checks.default = pkgs.runCommand "jcode-emacs-check" {
          nativeBuildInputs = [ (pkgs.emacs.pkgs.withPackages (epkgs: [ epkgs.md-ts-mode ])) ];
        } ''
          cp ${packageSrc}/*.el .
          mkdir test
          cp ${./test}/*.el test/
          emacs --batch -Q -L . -f batch-byte-compile *.el
          emacs --batch -Q -L . -L test -l test/jcode-test.el -f ert-run-tests-batch-and-exit
          touch $out
        '';

        devShells.default = pkgs.mkShell {
          packages = [
            pkgs.emacs
            jcode
          ];
        };
      }
    );
}
