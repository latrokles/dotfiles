{ pkgs, ...}: {
  homebrew = {
    enable = true;

    onActivation = {
      cleanup = "zap";  # uninstall anything not defined here
      autoUpdate = false;
      upgrade = false;
    };
    taps = [
      "railwaycat/emacsmacport"
    ];

    casks = [
      "emacs-mac"
      "firefox@developer-edition"
      "google-chrome"
      "iterm2"
      "xquartz"
      "phoenix"
    ];
  };
}
