#!/usr/bin/env zsh
function backup_profile {
    local tb_conf_path=~/.thunderbird
    local profile="zv.default"
    local profile_path=$tb_conf_path/$profile
    if [[ -e $profile_path ]]; then
        echo "Creating profile..."
        tar --create \
            --exclude-from=thunderbird_exclude \
            -C $tb_conf_path $profile \
            -f temp.tar && \
            tar -r \
                -f temp.tar \
                -C $tb_conf_path \
                -aJ \
                $profile/Mail/Feeds/{feeds,feeditems}.rdf \
                -O \
            | gpg -a --encrypt -o profile.tar.xz.gpg - && rm temp.tar
        echo "Appending critical files..."
        echo "Encrypting"
        echo "Saved profile"
    else
        echo "No thunderbird profile found at $profile_path"
    fi
}

backup_profile
