{
  home.shellAliases = {
    "." = "cd ..";
    ".2" = "cd ../..";
    ".3" = "cd ../../..";
    ".4" = "cd ../../../..";
    ".5" = "cd ../../../../..";
    c = "xclip -selection clipboard";
    cat = "bat";
    flightoff = "sudo rfkill unblock all";
    flighton = "sudo rfkill block all";
    l = "exa";
    ll = "exa -al";
    ls = "exa -a";
    man = "batman";
    mkdir = "mkdir -pv";
    ports = "sudo netstat -tunapl";
    poweroff = "shutdow -h now";
    ps = "procs";
    reboot = "shutdown -r now";
    rm = "rm -i --preserve-root";
    serve = "python -m http.server";
    sleep = "systemctl suspend";
    snr = "sudo systemctl restart NetworkManager";
    top = "btm";
    watch = "batwatch";
    wget = "wget -c";      # Resume by default
  };
}
