{config, pkgs, lib, self, ...}:
{
  nixpkgs.overlays = [
    (import self.inputs.emacs-overlay)
    (self: super: {
      qtile = super.qtile.unwrapped.overrideAttrs (prev: rec {
        # version = "master";
        # src = super.fetchgit {
        #   url = "https://github.com/qtile/qtile.git";
        #   rev = "7d71600aedb37bad0147aa780ad71d0a3634daf2";
        #   sha256 = "POBtkbHAjmF/NowyRrM9YUhtLk8lhosbuaTUk92s/wA=";
        # };
      });
    })
  ];
}
