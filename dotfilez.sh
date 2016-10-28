#!/bin/zsh
# My personal key
typeset key_id='0xF6F2D0445DC172F8'
typeset -a protected groups packages npm_packages

###
# Pack/Unpack my encrypted documents and configuration examples
###
local -aU protected
protected=(
    irssi/sasl.auth
    ssh/id_{rsa,ed25519}
    ssh/config
    org/*
    certificates/{nxvr.crt,gpgsm-nxvr.crt,gpsm-nxvr.csr,startssl-nxvr.crt,README}
)

function protect   { gpg -a -r "$key_id" --encrypt-files ${(@)protected} }
function unprotect { gpg --decrypt-files **/*.asc }

case $1 in
    protect)
        protect
        ;;
    unprotect)
        unprotect
        ;;
    keyring)
        keyring_utils
        ;;
    js)
        javascript
        ;;
    *)
        echo "protect"
        echo "unprotect"
        echo "keyring"
esac

function configure_env {
    # Set my shell to zsh
    sudo chsh -s =zsh zv

    # Setting up environment
    echo "Making Personal Directories"
    mkdir ~/{Development,Books,bin}

    echo "Linking Personal Directories"
    ln -s $HOME/Development/ $HOME/z

    # Install my custom /etc files
    echo "Installing custom /etc files"
    for f in etc/**/*; do
        cp -i "/$f" "$f"
    done

    # Link a few user systemd unit files
    for uf in systemd/user/default.target.wants/*; do
        systemctl --user link "$uf"
    done

    # Load bitlbee on startup
    if [[ $+commands[bitlbee] ]]; then
        systemctl enable bitlbee
    fi

    # Import certificates into gpg
    # gpg --import certificates/*
}

enable_trim () {
    sudo systemctl enable fstrim.timer
}

# install packages

# configure env (setup keys, shell, yubikey)

# cryptographic setup
## unpack gpg
### configure yubikey
### disable gnome ssh-agent && enable gnupg agent
### udev rules
## setup ssh keys
## install certificates

# configure gnome stuff
## gnome-terminal (dconf)

# restore thunderbird
## restore feeds

### gpg-agent

### systemd unit files

# configure emacs

# configure {javascript,haskell,ocaml,rust,erlang/elixir}

# setup dropbox

# pcsc-tools pcsc-lite-ccid:q

# auditing  ausearch -c bitlbee --raw | audit2allow -M bitlbeepol
