{
  description = "Haskell WASM development shell for Formatta";
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  outputs = { self, nixpkgs }:
    let
      system = "aarch64-darwin";
      pkgs = import nixpkgs { inherit system; }; 
    in {
      devShells = {
        aarch64-darwin = {
          default = pkgs.mkShell {
            buildInputs = [
              pkgs.haskell.compiler.ghc96
              pkgs.wasmtime
            ];
            shellHook = ''
              echo "GHC 9.6 and Wasmtime ready. For WASM, use GHC with --target=wasm32-wasi if available."
            '';
          };
        };
      };
    };
}
