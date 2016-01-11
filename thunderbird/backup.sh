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
          | gpg -a --encrypt -o profile.tar.xz.gpg -
      echo "Saved profile"
  else
      echo "No thunderbird profile found at $profile_path"
  fi
}

backup_profile
