# Enable FZF
if command -v fzf-share >/dev/null; then
  source "$(fzf-share)/key-bindings.bash"
  source "$(fzf-share)/completion.bash"
fi

### Sensible ###

## GENERAL OPTIONS ##

# Prevent file overwrite on stdout redirection
# Use `>|` to force redirection to an existing file
set -o noclobber

# Update window size after every command
shopt -s checkwinsize

# Automatically trim long paths in the prompt (requires Bash 4.x)
PROMPT_DIRTRIM=2

# Enable history expansion with space
# E.g. typing !!<space> will replace the !! with your last command
bind Space:magic-space

# Turn on recursive globbing (enables ** to recurse all directories)
shopt -s globstar 2> /dev/null

# Case-insensitive globbing (used in pathname expansion)
shopt -s nocaseglob;

## SMARTER TAB-COMPLETION (Readline bindings) ##

# Perform file completion in a case insensitive fashion
bind "set completion-ignore-case on"

# Treat hyphens and underscores as equivalent
bind "set completion-map-case on"

# Display matches for ambiguous patterns at first tab press
bind "set show-all-if-ambiguous on"

# Immediately add a trailing slash when autocompleting symlinks to directories
bind "set mark-symlinked-directories on"

## SANE HISTORY DEFAULTS ##

# Append to the history file, don't overwrite it
shopt -s histappend

# Save multi-line commands as one command
shopt -s cmdhist

# Record each line as it gets issued
PROMPT_COMMAND='history -a'

# Huge history. Doesn't appear to slow things down, so why not?
HISTSIZE=500000
HISTFILESIZE=100000

# Avoid duplicate entries
HISTCONTROL="erasedups:ignoreboth"

# Don't record some commands
export HISTIGNORE="&:[ ]*:exit:ls:bg:fg:history:clear"

# Use standard ISO 8601 timestamp
# %F equivalent to %Y-%m-%d
# %T equivalent to %H:%M:%S (24-hours format)
HISTTIMEFORMAT='%F %T '

# Enable incremental history search with up/down arrows (also Readline goodness)
# Learn more about this here: http://codeinthehole.com/writing/the-most-important-command-line-tip-incremental-history-searching-with-inputrc/
bind '"\e[A": history-search-backward'
bind '"\e[B": history-search-forward'
bind '"\e[C": forward-char'
bind '"\e[D": backward-char'

## BETTER DIRECTORY NAVIGATION ##

# Prepend cd to directory names automatically
shopt -s autocd 2> /dev/null
# Correct spelling errors during tab-completion
shopt -s dirspell 2> /dev/null
# Correct spelling errors in arguments supplied to cd
shopt -s cdspell 2> /dev/null

# This defines where cd looks for targets
# Add the directories you want to have fast access to, separated by
# colon
#
# Ex: CDPATH=".:~:~/projects" will look for targets in the current
# working directory, in home and in the ~/projects folder
CDPATH="."

# This allows you to bookmark your favorite places across the file
# system Define a variable containing a path and you will be able to
# cd into it regardless of the directory you're in
shopt -s cdable_vars

# Show the status of stopped/running jobs before exit
shopt -s checkjobs

### Functions ###
docker-clean () {
    docker volume rm $(docker volume ls -qf dangling=true) || echo "No volume to clean!"
    docker network rm $(docker network ls | grep "bridge" | awk '/ / { print $1 }') || echo "No network to clean!"
    # see: http://stackoverflow.com/questions/32723111/how-to-remove-old-and-unused-docker-images
    docker rmi $(docker images --filter "dangling=true" -q --no-trunc) || echo "No dangling images to clean!"
    docker rm $(docker ps -qa --no-trunc --filter "status=exited") || echo "No exited container to delete!"
}

nix-clean () {
    # https://discourse.nixos.org/t/what-to-do-with-a-full-boot-partition/2049/2
    nix-env --delete-generations old
    nix-store --gc
    nix-channel --update
    nix-env -u --always
    for link in /nix/var/nix/gcroots/auto/*
    do
        rm $(readlink "$link")
    done
    nix-collect-garbage -d

    # FIXME home-manager gets removed with this operation.
    nix-shell '<home-manager>' -A install
}

# https://docs.gitignore.io/install/command-line
function gi() { curl -sL https://www.toptal.com/developers/gitignore/api/$@; }
