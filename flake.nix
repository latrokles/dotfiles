{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    nix-darwin.url = "github:LnL7/nix-darwin";
    nix-darwin.inputs.nixpkgs.follows = "nixpkgs";
    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
    nix-homebrew.url = "github:zhaofengli-wip/nix-homebrew";
  };

  outputs = inputs @ {
    self,
    nixpkgs,
    nix-darwin,
    home-manager,
    nix-homebrew
  }: let
    configuration = {...}: {
      nix.settings.experimental-features = "nix-command flakes";

      # Set Git commit hash for darwin-version.
      system.configurationRevision = self.rev or self.dirtyRev or null;

      # Used for backwards compatibility, please read the changelog before changing.
      # $ darwin-rebuild changelog
      system.stateVersion = 5;

      # The platform the configuration will be used on.
      nixpkgs.hostPlatform = "aarch64-darwin";
    };

    homeconfig = {pkgs, ...}: {
      home.stateVersion = "23.05";
      programs.home-manager.enable = true;
      home.file = {
        emacs = {
          enable = true;
	  executable = false;
	  recursive = true;
	  source = ./config/emacs.d;
	  target = ".emacs.d";
        };
      };

      home.packages = with pkgs; [];
      home.sessionVariables = {
      };
    };

    darwinSystem = { user }:
      nix-darwin.lib.darwinSystem {
        modules = [
          configuration
	  home-manager.darwinModules.home-manager {
	    home-manager.useGlobalPkgs = true;
	    home-manager.useUserPackages = true;
	    home-manager.verbose = true;
	    home-manager.users.${user} = import ./modules/home.nix
	    users.users.${user}.home = "/Users/${user}";
	  }
          ./modules/macos.nix
          ./modules/pkgs.nix
          ./modules/brew.nix
          nix-homebrew.darwinModules.nix-homebrew {
             nix-homebrew = {
               enable = true;
               enableRosetta = true;
               user = user;
             };
          }
        ];
      };

  in {
    # Build darwin flake using:
    # $ darwin-rebuild build --flake .#pandebono
    darwinConfigurations."pandebono" = darwinSystem {
      user = "latrokles";
    };
  };
}
