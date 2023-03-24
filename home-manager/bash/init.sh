### Variables ###
# SSH using GPG
gpg-connect-agent /bye
export SSH_AUTH_SOCK=$(gpgconf --list-dirs agent-ssh-socket)

export EDITOR="emacsclient -nw"
export VISUAL="emacsclient -nw"
export PATH="$PATH:/home/shahin/.local/bin"
