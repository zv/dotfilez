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

# Find all packages that match the 'tag' supplied by the user
find_matching_packages() {
    local tag=$1
    awk -v tag="$tag" -f "$BASEDIR/script/search_packages.awk" "$BASEDIR/data/packages"
}

install_packages() {
    local tag
    # Update DNF
    command sudo dnf check-update
    command sudo dnf update
    # Find all packages / groups matching our tags
    local package_specs=$(find_matching_packages $tag)
    # .. and install them
    command sudo dnf install -y ${packages[@]}
}

# Returns the intended destination of a RC/Dotfile Reads a special destination
# database (rc/rc_destination) which contains a `filename' / `destination' pair,
# separated by a pipe (|). In addition to ordinary "source/dest" strings, this
# file may also contain regular expressions to match source filenames & special
# optional modifiers:
#
#   %filename   Replace the path in the destination with the filename that is
#               currectly being used (used for globbing)
#   %skip       Return an empty string (we shouldn't link this)
rc_file_destination() {
    local file=$1
    local RC_SPECIAL_DEST="$BASEDIR/data/rc_destination"
    if [[ ! -e $RC_SPECIAL_DEST ]]; then
           echo "Could not find special destination file @ $RC_SPECIAL_DEST"
           return -1;
    fi
    awk -v file="$file" -f $BASEDIR/script/read_rc_path.awk $RC_SPECIAL_DEST
}

# Link all relevant RC files.
link_dotfiles () {
    for filen in rc/*; do
        local destination=$(eval echo $(rc_file_destination $(basename $filen)))
        local sourcefile=$(realpath $filen)
        if [[ -z $destination ]]; then
            continue;
        fi
        read -p "Link $(zv_info $sourcefile) to $(zv_info $destination) (y/n/q) : " CONDITION
        if [ "$CONDITION" = "q" ]; then
            return 1
        fi
        if [ "$CONDITION" = "y" ]; then
            echo "linking $sourcefile to $destination"
            ln -s $sourcefile $destination
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
