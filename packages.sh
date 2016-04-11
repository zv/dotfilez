#!/bin/bash
###################################
## A bootstrapping package    #####
###################################

function install_base_packages {
    echo "--------------------------------------------------------------------------------"
    echo "--------------------------------------------------------------------------------"
    echo "Installing base packages, grab some coffee"
    echo "--------------------------------------------------------------------------------"
    echo "--------------------------------------------------------------------------------"

    sudo dnf check-update
    sudo dnf update

    # Development tools
    ## List of yum groups to be installed
    groups=(
        "'C Development Tools and Libraries'"
        "'Development Tools'"
    )

    if [[ "$TYPE" -ge 2 ]]; then
        groups+=" 'System Tools'"
    fi

    # List of packages to be installed
    packages=(clang
              curl
              emacs
              global
              git
              gnupg2
              nmap
              rlwrap
              the_silver_searcher
              tmux
              vim-enhanced
              zsh)

    if [[ "$TYPE" -ge 2 ]]; then
        packages+=(checksec
                   hping3
                   mtr
                   socat)
    fi

    if [[ "$TYPE" -ge 3 ]]; then
        packages+=(acpitool
                   avahi-tools
                   cmake
                   dmenu
                   dunst
                   ettercap
                   graphviz
                   i3
                   i3lock
                   i3status
                   java-1.8.0-openjdk
                   libvirt
                   openssl
                   openssl-devel
                   pcsc-lite
                   scrot
                   thunderbird
                   virt-manager
                   wireshark
                   ykclient
                   ykpers
                   yubikey-personalization-gui)
    fi

    sudo dnf group install -y ${groups[@]}
    sudo dnf install -y ${packages[@]}
}

# Link all relevant RC files.
function link_dotfiles() {
    # basic install list
    local install_list=(
        zsh
        zshrc
        zshenv
        vim
        vimrc
        gitconfig
        gitignore
        psqlrc
        lesskey
        ssh
    )

    # List of files & directories *not* to link
    local nolink=("^\."
                  .asc
                  .pub
                  .sh
                  .gpg
                  .txt
                  Makefile
                  README.md
                  certificates
                  dconf
                  ebin
                  etc
                  ida.cfg
                  layers
                  lesskey
                  /lib
                  newsrc
                  org
                  ssh_config
                  systemd
                  xmobarrc
                  xmonadrc)

    if [[ $TYPE -le 2 ]]; then
        for file in ${install_list[@]}; do
            ln -s $file "~/.$file"
        done
    else
        local dontlink
        for p in ${nolink[@]}; do dontlink+="${p}\|"; done
        dontlink+=".sh"
        git ls-tree --name-only HEAD |\
            grep -v "${dontlink}" |\
            xargs -I % sh -c "ln -s $(realpath %) $HOME/.%"
    fi
}


###
# This function downloads and installs all the fonts I use.
###
function install_fonts {
    local tmp_font_dir
    pushd
    tmp_font_dir=`mktemp -d`
    cd "$tmp_font_dir"
    echo "Installing Source Code Pro Fonts..."
    sudo dnf install adobe-source-code-pro-fonts
    echo "Installing Powerline Fonts..."
    (git clone git@github.com:powerline/fonts.git && install.sh)&
    popd
}

################################################################################
# Javascript
################################################################################
function install_javascript {
    pushd
    ## Build nodejs
    if [[ -x =node ]]; then
        mkdir -p ~/Development/node/local
        cd ~/Development/node
        curl https://nodejs.org/dist/node-latest.tar.gz | tar xz --strip-components=1
        ./configure --prefix=~/Development/node/local
        make install
        rehash node
    fi

    # Build / install npm
    if [[ -x =npm ]]; then
        curl https://www.npmjs.org/install.sh | sh
        rehash npm
    fi

    ## Install our NPM packages
    npm_packages=(
        tern
        js-beautify
        yo
        ember-cli
        gulp
        lodash
        bower
        babel-core
        babel-cli
        webpack
        webpack-dev-server
    )

    npm install -g ${(@)npm_packages}

    popd
}

################################################################################
# Rust
################################################################################
function install_rust {
    curl -sf https://raw.githubusercontent.com/brson/multirust/master/blastoff.sh | sh
}

################################################################################
# Chrome
################################################################################
function install_chrome {
    https://dl.google.com/linux/direct/google-chrome-stable_current_x86_64.rpm
}

################################################################################
# OCaml
################################################################################
function install_ocaml {
    if [[ -x =ocaml ]]; then
        # Install ocaml-proper
        _ dnf install ocaml-camlp4-devel ocaml-ocamldoc ocaml-findlib-devel ocaml-extlib-devel ocaml-calendar-devel
        # Install opam
        wget https://raw.github.com/ocaml/opam/master/shell/opam_installer.sh -O - | sh -s /usr/local/bin/
        # Now install utop
    fi
}

################################################################################
# Emacs
################################################################################
function install_emacs {
    echo "Created build-directory"
    local destination=~/extern
    pushd
    mkdir $destination
    cd $destination
    echo "Installing dependencies"
    sudo dnf builddep emacs
    ## Emacs Developers Keyring
    echo "Install our toolchain"
    # gpg --import =(wget -q "http://savannah.gnu.org/project/memberlist-gpgkeys.php?group=emacs&download=1" -O -)
    git clone -b emacs25 git://git.savannah.gnu.org/emacs.git
    ./configure
    make
    sudo make install
    make clean
    popd
}

function print_help {
    echo "--------------------------------------------------------------------------------"
    echo "--------------------------------------------------------------------------------"
    echo "$0"
    echo "Options "
    echo "$0 system  # Just the basics"
    echo "$0 server  # Administration Tools"
    echo "$0 full    # Everything on my environment"
    echo "$0 osx     # OSX Environment (incomplete)"
    echo "--------------------------------------------------------------------------------"
    echo "--------------------------------------------------------------------------------"
}


#TYPE=""
# Items are assigned numbers because each 'packageset' is a strict subset of
# the item below it.
if [[ $(pwd):a:h != ~/dotfilezx ]]; then
    echo "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
    echo "It's recommended that this directory be located in ~/dotfilez"
    echo "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
    exit
fi


case "$1" in
    system)
        TYPE=1 ;;
    server)
        TYPE=2 ;;
    full)
        TYPE=3 ;;
    *)
        print_help
        exit
esac

install_base_packages
link_dotfiles

if [[ $TYPE -eq 3 ]]; then
    install_fonts
    install_chrome
    install_emacs
    # install_javascript
    # install_rust
fi
