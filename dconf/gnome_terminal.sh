backup_term () {
  dconf dump /org/gnome/terminal/ > gterm_profile
}

restore_term () {
    dconf load gterm_profile
}

$1
