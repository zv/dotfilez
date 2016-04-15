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

# Includes two-factor authentication development libraries
function two_factor_auth {
    sudo dnf install ykpers-devel libyubikey-devel libusb-devel autoconf gnupg gnupg2-smime pcsc-lite
}

function keyring_utils {
    local destination=~/Dropbox/gnome-keyring.tar.gpg
    local keyring_path=~/.local/share/keyrings
    if [[ $2 = "backup" ]]; then
        echo 'Backing up gnome keyring';
        tar --create -C $keyring_path(:h) --wildcards -O $keyring_path(:t) | gpg --encrypt -r $key_id -o $destination - && echo "Wrote file to $destination"
    elif [[ $2 = "restore" ]]; then
        # Use gpg not gnome ssh-agent
        if [[ $(gconftool-2 --get /apps/gnome-keyring/daemon-components/ssh) != "false" ]]; then
            gconftool-2 --type bool --set /apps/gnome-keyring/daemon-components/ssh false
        fi

        if [[ -e $destination ]]; then
            local temp_dir=$(mktemp -d)
            gpg2 -d $destination > $temp_dir/gnome-keyring.tar
            tar xvf $temp_dir/gnome-keyring.tar
            if mv $keyring_path $temp_dir; then
                echo "backed up keyring to $temp_dir"
            else
                echo 'failed to backup keyring: EXITING'
                exit
            fi
            mv keyrings ~/.local/share/keyrings
            mv $temp_dir/keyrings $keyring_path/old_keyrings
            echo "Restored keyring"
            echo "old keyrings are still in /tmp -- be careful if no tmpfs"
        else
            echo "No file"
        fi
    else
        echo "keyring backup # backup the keyring"
        echo "keyring restore # restore the keyring"
    fi
}

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
#
# auditing  ausearch -c bitlbee --raw | audit2allow -M bitlbeepol
