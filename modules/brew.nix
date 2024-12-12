{ pkgs, ...}: {
  homebrew {
    enable = true;
    enableRosetta = true;

    onActivation = {
      cleanup = "zap";  # uninstall anything not defined here
      autoUpdate = false;
      upgrade = false;
    };
    taps = [
      "nikitabobko/tap"
      "railwaycat/emacsmacport"
    ];

    casks = [
      "emacs-mac"
      "firefox@developer-edition"
      "google-chrome"
      "iterm2"
      "nikitabobko/tap/aerospace"
      "xquartz"
    ];
  };
}
