{ config, pkgs, ... }:
  let
     home = config.home.homeDirectory;
     dotfilesPath = "${home}/dev/src/latrokles/dotfiles";
     symlink = config.lib.file.mkOutOfStoreSymlink;
  in {
    home.stateVersion = "23.05";
    programs.home-manager.enable = true;

    programs.zsh = {
      enable = true;
      shellAliases = {
        update = "darwin-rebuild switch --flake";
	refresh = "source ~/.zshenv && source ~/.zshrc";

        # navigation
        pd ="pushd";
        pop = "popd";
        zz = "suspend";
	dotfiles = "cd $DOTFILES";
	dev = "cd $DEVENV";

        # git stuff
        g-stat = "git status";
	g-log = "git log";
	g-blame = "git blame";
        g-branch = "git branch -vv";
        g-add = "git add";
        g-commit = "git commit";
        g-ammend = "git commit --amend";
        g-diff = "git diff";
	g-diff-files = "git diff --name-only";
        g-merge = "git merge";
        g-push = "git push";
        g-pull = "git pull";
        g-pull-r = "git pull --rebase";
        g-rmbr = "git branch -D";

        # some python stuff
        p-run = "poetry run";
        p-python = "poetry run python";
        p-test = "poetry run pytest";
        p-see-cov = "open dev/reports/cov/index.html";
        p-see-tst = "open dev/reports/tst/index.html";

        # general utilities
        vi = "nvim";
        vim ="nvim";
        tree ="tree -I 'target|__pycache__|node_modules'";
        screensaver = "open -a ScreenSaverEngine";
        scfull = "screencapture";
        sccrop = "screencapture -is";
        scclip = "screencapture -cis";
        scwin = "screencapture -W";
        scvid = "screencapture -v";
      };
    };

    home.file = {
      ".emacs.d" = {
        enable = true;
	source = symlink "${dotfilesPath}/config/emacs.d";
      };
    };

    home.packages = with pkgs; [];

    home.sessionVariables = {
      DOTFILES="${dotfilesPath}";
      DEVENV="${home}/dev/src/latrokles";
      PYSDL2_DLL_PATH="${pkgs.SDL2.outPath}/lib";
    };
  }