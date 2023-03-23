{ pkgs, ... }: {
  programs.zoxide = {
    enable = true;
    enableBashIntegration = true;
  };

  programs.direnv = {
    enable = true;
    enableBashIntegration = true;
  };

  programs.readline = {
    enable = true;
    variables = {
      completion-ignore-case = "On";
      expand-tilde = true;
    };
  };

  programs.bash = {
    enable = true;

    initExtra = "source ${./bash/init.sh}";
    bashrcExtra = "source ${./bash/bashrc.sh}";
    profileExtra = "source ${./bash/profile.sh}";
  };
}
