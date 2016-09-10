#!/usr/bin/env zsh
function backup_profile {
  local profile_path=~/.thunderbird/zv.default
  if [[ -e $profile_path ]]; then
      echo "Creating profile..."
      XZ_OPT=-9 tar --create \
          --exclude-from=thunderbird_exclude \
          -C ~/.thunderbird \
          -J zv.default \
          -O \
          | gpg --encrypt -r zv@nxvr.org -o profile.tar.xz.gpg -
      echo "Saved profile"
  else
      echo "No thunderbird profile found at $profile_path"
  fi
}

function restore_profile {
    echo "Extracting profile..."
    # ugly hack to avoid sigpipe on mac osx where no <(gpg) or =(gpg) is allowed
    gpg --decrypt profile.tar.xz.gpg > profile.tar.xz
    tar xf profile.tar.xz -C ~/.thunderbird
    rm profile.tar.xz
    
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
    echo 'You can run rsync -av ~/.thunderbird/zv.default ~/.thunderbird/OLD_PROFILE if you want'
}

if [[ $@[1] = "--extract" ]]; then
    restore_profile
else
    backup_profile
fi
