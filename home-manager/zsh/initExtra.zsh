### Environment Variables ###
export EDITOR="emacsclient -nw"
export VISUAL="emacsclient -nw"

### SSH using Yubikey ###
gpg-connect-agent /bye
export SSH_AUTH_SOCK=$(gpgconf --list-dirs agent-ssh-socket)

### Functions ###
docker-clean () {
    docker volume rm $(docuer volume ls -qf dangling=true) || echo "No volume to clean!"
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


### Plugins ###
# https://docs.gitignore.io/install/command-line
function gi() { curl -sLw "\n" https://www.toptal.com/developers/gitignore/api/$@ ;}

