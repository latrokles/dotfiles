{ config, pkgs, ... }: {
  home.stateVersion = "23.05";
  programs.home-manager.enable = true;

  let
     home = builtins.getEnv "HOME";
     dotfilesPath = "${home}/dev/src/latrokles/dotfiles";
     symlink = config.lib.file.mkOutOfStoreSymlink;
  in {
    ".zshenv".source = symlink ${dotfilesPath}/config/zshenv";
    ".emacs.d".source = symlink ${dotfilesPath/config/emacs.d";

    home.packages = with pkgs; [];
    home.sessionVariables = {
      DOTFILES = dotfiles;
      LD_LIBRARY_PATH="$(nix eval nixpkgs#SDL2.outPath --raw)/lib:$LD_LIBRARY_PATH";
      PYSDL2_DLL_PATH="$(nix eval nixpkgs#SDL2.outPath --raw)/lib";
    };
  }
}