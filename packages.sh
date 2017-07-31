#!/bin/bash
###################################
## A bootstrapping package    #####
###################################

# TODO: Write code to pull in rust, setup rust path (RUST_SRC_PATH), etc.
BASEDIR=$(dirname "$0")

red=$(tput setaf 1)
green=$(tput setaf 2)
yellow=$(tput setaf 3)
blue=$(tput setaf 4)
magenta=$(tput setaf 5)
cyan=$(tput setaf 6)
bold=$(tput bold)
reset=$(tput sgr0)


zv_debug() {
    echo "$1"
}

zv_info()  {
    echo "$bold$*$reset"
}

zv_warn()  {
    echo "$yellow$*$reset"
}

zv_error() {
    echo "$bold$red$*$reset"
}

###
# e.x read_configuration data/packages base ocaml
# This will fetch all the files with the ocaml and base fields
###
find_tagged () {
    local file=$1
    local tags=$(echo "${@:2}" | sed -e "s/ /|/g")
    cat "$file" | awk "/$tags/ { FS=\"#\"; print \$1 }"
}

install_by_tag () {
    local package_location="$BASEDIR/data/packages"
    local tags="$1"
    local packages=$(find_tagged $package_location "$tags")
    command sudo dnf install -y ${packages[@]}
}

install_base_packages () {
    zv_info "Installing base packages, grab some coffee"
    local tags="$*"
    local group_location="data/groups"
    local groups=$(find_tagged "$group_location" "$tags")
    command sudo dnf check-update
    command sudo dnf update
    install_by_tag $tags
    command sudo dnf group install -y ${groups[@]}
}

# Link all relevant RC files.
link_dotfiles () {
    for filen in rc/*; do
	sourcefile=$(realpath $filen)
	basefile=$(basename $filen)
	destination="$HOME/.$basefile"
        read -p "Link $sourcefile to $destination (y/n/q) :" CONDITION
	if [ "$CONDITION" = "q" ]; then
		return 1
	fi
        if [ "$CONDITION" != "n" ]; then
            echo "linking $sourcefile to $destination"
            ln -sf $sourcefile $destination
        fi
    done
}

###
# This downloads () and installs all the fonts I use.
###
install_fonts () {
    # mkdir -p ~/.local/share/fonts && mv *.ttf ~/.local/share/fonts && fc-cache
    zv_info "Installing Distribution Fonts"
    install_by_tag "font"
    zv_debug "Making temporary font directory..."
    if hash git 2>/dev/null; then
        local tmp_font_dir=$(mktemp -d)
        pushd
        cd "$tmp_font_dir"
        zv_info "Installing Powerline Fonts..."
        git clone git@github.com:powerline/fonts.git && install.sh
        zv_debug "Popping directory stack"
        popd
    else
        zv_error "No git binary detected, unable to install Powerline"
    fi
}

post_install () {
	echo "POST_INSTALL"
}

if [ "$#" -lt 1 ]; then
    cat $BASEDIR/data/usage
fi

case "$1" in
    newenv) asdfadfs ;;
    install) install_base_packages ${@:2} ;;
    link) link_dotfiles 
;;
    *) $BASEDIR/script/main.zsh "$*" ;;
esac
