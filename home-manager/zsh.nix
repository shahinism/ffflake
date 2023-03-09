{pkgs, config, ...}:
{
  programs.zoxide = {
    enable = true;
    enableZshIntegration = true;
  };

  programs.direnv = {
    enable = true;
    enableZshIntegration = true;
  };

  programs.zsh = {
    enable = true;

    # https://github.com/chisui/zsh-nix-shell
    plugins = [
      {
        name = "zsh-nix-shell";
        file = "nix-shell.plugin.zsh";
        src = pkgs.fetchFromGitHub {
          owner = "chisui";
          repo = "zsh-nix-shell";
          rev = "v0.5.0";
          sha256 = "0za4aiwwrlawnia4f29msk822rj9bgcygw6a8a6iikiwzjjz0g91";
        };
      }
    ];

    zplug = {
      enable = true;
      plugins = [
        {name = "plugins/fzf"; tags = [ from:oh-my-zsh ];}
        {name = "plugins/command-not-found"; tags = [ from:oh-my-zsh ];}
        {name = "blimmer/zsh-aws-vault"; tags = [ at:main ];}
        {name = "zdharma-continuum/fast-syntax-highlighting";}
      ];
    };

    initExtra = "source ${./zsh/initExtra.zsh}";
  };
  
}
