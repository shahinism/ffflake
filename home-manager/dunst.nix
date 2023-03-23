{ ... }: {
  services.dunst = {
    enable = true;
    settings = {
      global = {
        font = "Hack 9";
        format = ''
          <b>%s</b>
          %b'';
        sort = true;
        indicate_hidden = true;
        alignment = "center";
        show_age_threshold = 60;
        word_wrap = "true";
        ignore_newline = false;
        transparency = 0;
        idle_threshold = 120;
        monitor = 0;
        follow = "mouse";
        sticky_history = true;
        line_height = 0;
        separator_height = 2;
        padding = 8;
        horizontal_padding = 8;
        separator_color = "#595959";
        frame_width = 2;
        gap_size = 3;
        close = "ctrl+space";
        close_all = "ctrl+shift+space";
        history = "ctrl+grave";
        context = "ctrl+shift+period";
      };
      urgency_low = {
        background = "#000000";
        foreground = "#ffffff";
        timeout = 5;
      };
      urgency_normal = {
        background = "#000000";
        foreground = "#ffffff";
        timeout = 10;
      };
      urgency_critical = {
        background = "#ef8b50";
        foreground = "#000000";
        timeout = 0;
      };
    };
  };
}
