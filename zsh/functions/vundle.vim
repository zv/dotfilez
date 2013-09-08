#############################################
# Vim vundle helpers 
#############################################

function vundle () {
  vim -c "execute \"BundleInstall\" | q | q"
}

function vundle-update () {
  vim -c "execute \"BundleInstall!\" | q | q"
}

function vundle-clean () {
  vim -c "execute \"BundleClean!\" | q | q"
}

