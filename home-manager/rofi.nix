{pkgs, ...}:

{
  programs.rofi = {
    enable = true;
    package = pkgs.rofi.override {
      plugins = with pkgs; [
        rofi-emoji
      ];
    };
  };
}
