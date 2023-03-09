{pkgs, ...}:
{
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
    extraConfig = "source ${./bash/inputrc.sh}";
  };

  programs.bash = {
    enable = true;

    initExtra = "source ${./bash/init.sh}";
    bashrcExtra = "source ${./bash/bashrc.sh}";
    profileExtra = "source ${./bash/profile.sh}";

    shellOptions = [
      "autocd"                 # Enter the name of directory and it'll
                               # cd into it.
      
      "cdable_vars"            # Allow `cd`ing into variables.
      "cdspell"                # Autocorrect typos in cd command.
      
      "checkjobs"              # Show the status of stopped/running
                               # jobs before exit.
      
      "dirspell"               # Try autocorrection on directory name
                               # spell with autocompletion.
      
      "globstar"               # Support recursive globbing like
                               # **/*.sh.

      "histappend"             # Append to the history file on shell
                               # exits.
      # Save multi-line commands as such (do not rearrange in single line)
      "cmdhist"
      "lithist"
    ];
  };
}
