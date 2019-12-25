let
  hsPkgs = (import ./default.nix);
  src = builtins.fetchTarball https://github.com/input-output-hk/haskell.nix/archive/master.tar.gz;
  pkgs = import (src + "/nixpkgs") (import src);
in


hsPkgs.shellFor {
  # Shell will provide the dependencies, but not packages themselves.
  packages = ps: with ps; [ generate-all-tests ];
  # This adds cabal-install to the shell, which helps tests because
  # they use a nix-shell --pure. Normally you would BYO cabal-install.
  buildInputs = with pkgs; [ cabal-install stack nix zlib ];
  exactDeps = true;
}
