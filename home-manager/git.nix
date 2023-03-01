{pkgs, ...}:
{
  programs.git = {
    enable = true;

    userName = "Shahin Azad";
    userEmail = "hi@shahinism.com";
    signing = {
      signByDefault = true;
      key = null;  # set to null for GPG to decide
    };
    includes = [
      {
        path = "~/.config/git/includes/DataChef";
        condition = "gitdir:~/projects/DataChef/";
      }
    ];
  };

  home.file = {
    ".config/git/includes" = {
      source = ./. + "/git/includes";
      recursive = true;
    };
  };

}
