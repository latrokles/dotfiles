# List packages installed in system profile. To search by name, run:
# $ nix-env -qaP | grep wget
{ pkgs, ... }: {

  environment.systemPackages = with pkgs; [
    # tools
    pkgs.neovim
    pkgs.git
    pkgs.jujutsu
    pkgs.tree
    pkgs.jq
    pkgs.rlwrap
    pkgs.exiftool
    pkgs.yt-dlp
    pkgs.ffmpeg-full
    pkgs.mpv
  
    # libraries
    pkgs.SDL2
    pkgs.mpg123
  
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
