{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    nix-darwin.url = "github:LnL7/nix-darwin";
    nix-darwin.inputs.nixpkgs.follows = "nixpkgs";
    nix-homebrew.url = "github:zhaofengli-wip/nix-homebrew";
  };

  outputs = inputs @ {
    self,
    nixpkgs,
    nix-darwin,
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

      # TODO migrate these to home-manager?
      environment.variables = {
        LD_LIBRARY_PATH="$(nix eval nixpkgs#SDL2.outPath --raw)/lib:$LD_LIBRARY_PATH";
        PYSDL2_DLL_PATH="$(nix eval nixpkgs#SDL2.outPath --raw)/lib";
      };
    };

    darwinSystem = { user }:
      nix-darwin.lib.darwinSystem {
        modules = [
          configuration
          ./modules/macos.nix
          ./modules/pkgs.nix
          ./modules/brew.nix
          nix-homebrew.darwinModules.nix-homebrew
          {
             nix-homebrew = {
               enable = true
               enableRosetta = true;
               user = user;
             }
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
