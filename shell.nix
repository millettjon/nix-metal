let
  unstable = import <unstable> {};
in
with import <nixpkgs> {};
stdenv.mkDerivation {
  name = "nix-metal";

  buildInputs = with pkgs; [
    # unstable.babashka
    # unstable.clj-kondo

    tmux
    git
    emacs
    htop

  ];

  shellHook = ''
    export PATH="$PWD/.cache:$PATH"
  '';
}
