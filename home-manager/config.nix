{
  # Allow to install proprietary or unfree packages
  # https://bit.ly/3Zmmfm3
  allowUnfree = true;

  packageOverrides = pkgs: {
    nur = import (builtins.fetchTarball "https://github.com/nix-community/NUR/archive/master.tar.gz") {
      inherit pkgs;
    };
  };
}
