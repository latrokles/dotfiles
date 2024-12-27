# List packages installed in system profile. To search by name, run:
# $ nix-env -qaP | grep wget
{ pkgs, ... }: {

  environment.systemPackages = with pkgs; [
    # tools
    pkgs.tree
    pkgs.watch
    pkgs.git
    pkgs.fd
    pkgs.ripgrep
    pkgs.neovim
    pkgs.jujutsu
    pkgs.jq
    pkgs.rlwrap
    pkgs.hexyl
    pkgs.exiftool
    pkgs.yt-dlp
    pkgs.ffmpeg-full
    pkgs.mpv

    # libraries
    pkgs.SDL2
    pkgs.SDL2.dev
    pkgs.SDL2_image
    pkgs.mpg123

    # build
    pkgs.cmake
    pkgs.pkg-config

    # programming languages
    pkgs.python312Full
    pkgs.python312Packages.pip
    pkgs.poetry
    pkgs.nodejs_18
    pkgs.zig
    pkgs.sbcl
    pkgs.clojure
    pkgs.babashka
  ];
}
