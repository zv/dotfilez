#!/bin/zsh
KEYRING_DEST=~/Dropbox/gnome-keyring.tar.gpg
KEYRING_PATH=~/.local/share/keyrings

function keyring_restore {
    # Use gpg not gnome ssh-agent
    if [[ $(gconftool-2 --get /apps/gnome-keyring/daemon-components/ssh) != "false" ]]; then
        gconftool-2 --type bool --set /apps/gnome-keyring/daemon-components/ssh false
    fi

    if [[ -e $KEYRING_DEST ]]; then
        local temp_dir=$(mktemp -d)
        gpg2 -d $KEYRING_DEST > $temp_dir/gnome-keyring.tar
        tar xvf $temp_dir/gnome-keyring.tar
        if mv $KEYRING_PATH $temp_dir; then
            zv_debug "backed up keyring to $temp_dir"
        else
            zv_error 'failed to backup keyring: EXITING'
            exit
        fi
        mv keyrings ~/.local/share/keyrings
        mv $temp_dir/keyrings $keyring_path/old_keyrings
        zv_info "Restored keyring"
        zv_warn "old keyrings are still in /tmp -- be careful if no tmpfs"
    else
        zv_error "No keyring at KEYRING_DEST"
    fi
}

function keyring_backup {
    zv_info 'Backing up gnome keyring';
    tar --create -C $KEYRING_PATH(:h) --wildcards -O $KEYRING_PATH(:t) \
        | gpg --encrypt -r $key_id -o $KEYRING_DEST - \
        && zv_info "Wrote file to $KEYRING_DEST"
}

function keyring_utils {
    if [[ $2 = "backup" ]]; then
        keyring_backup
    elif [[ $2 = "restore" ]]; then
        keyring_restore
    else
        zv_info "keyring backup # backup the keyring"
        zv_info "keyring restore # restore the keyring"
    fi
}

function copy_templates {
    for f (templates/*) do
        expand_template "$f" > "~/.${f:h}"
    done
}

function expand_template {
    local template="$(cat $1)"
    eval "echo \"${template}\""
}

function fetch-secret {
    local attributes="${(s/=/ /g)@}"
    secret-tool lookup "$keys"
}
