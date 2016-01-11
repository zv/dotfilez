#!/usr/bin/env zsh
function backup_profile {
  local profile_path=~/.thunderbird/zv.default
  if [[ -e $profile_path ]]; then
      echo "Creating profile..."
      tar --create \
          --exclude-from=thunderbird_exclude \
          -C ~/.thunderbird \
          -aJ zv.default \
          -O \
          | gpg -a --encrypt -r zv@nxvr.org -o profile.tar.xz.gpg -
      echo "Saved profile"
  else
      echo "No thunderbird profile found at $profile_path"
  fi
}

function restore_profile {
    echo "Extracting profile..."
    gpg --decrypt profile.tar.xz.gpg > profile.tar.xz
    tar xf profile.tar.xz -C ~/.thunderbird
    
    if [[ -e ~/.thunderbird/profiles.ini ]]; then
        echo "Backing up our manifest"
        mv ~/.thunderbird/profiles.ini ~/.thunderbird/profile.ini.backup
    fi

    echo "Updating thunderbird profile manifest"
    cp profiles.ini ~/.thunderbird/profiles.ini

    echo '----------------------------------------------'
    echo '----------------------------------------------'
    echo '----------------------------------------------'
    echo 'Remember to restore the OPML'
    echo '----------------------------------------------'
    echo '----------------------------------------------'
    echo '----------------------------------------------'
}

if [[ $@[1] = "--extract" ]]; then
    restore_profile
else
    backup_profile
fi
