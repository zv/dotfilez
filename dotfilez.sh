#!/bin/zsh
# My personal key
typeset key_id='0xF6F2D0445DC172F8'
# Set to `true` to install yubikey dependencies
typeset twofa_dependencies="false"
typeset -a protected groups packages npm_packages

###
# Pack/Unpack my encrypted documents and configuration examples
###
local -aU protected
protected=(
    irssi/sasl.auth
    ssh/id_{rsa,ed25519}
    ssh/config
    org/*
    certificates/{nxvr.crt,gpgsm-nxvr.crt,gpsm-nxvr.csr,startssl-nxvr.crt,README}
)

function protect   { gpg -a -r "$key_id" --encrypt-files ${(@)protected} }
function unprotect { gpg --decrypt-files **/*.asc }

###
# This function downloads and installs all the fonts I use.
###
function install_fonts {
    local tmp_font_dir
    pushd
    tmp_font_dir=mktemp -d
    cd "$tmp_font_dir"
    print -P "%F{green}Installing Source Code Pro Fonts...%f"
    sudo dnf install adobe-source-code-pro-fonts
    print -P "%F{green}Installing Powerline Fonts...%f"
    (git clone git@github.com:powerline/fonts.git && install.sh)&
    popd
}

function install_pkgs {
    dnf check-update

    dnf update

    print -P "%F{green}Installing local packages, grab some coffee%f"

    # Development tools
    ## List of yum groups to be installed
    groups=(
        'C Development Tools and Libraries'
        'Development Tools'
        'System Tools'
    )

    # List of packages to be installed
    packages=(ack
        aircrack-ng
        clang
        clang-devel
        clang-libs
        curl
        dnsenum
        emacs
        # git-extras
        global # gtags
        gnupg2
        graphviz
        hping3
        i3
        i3lock
        i3status
        libvirt
        mtr
        nmap
        rlwrap
        rpmdevtools
        scapy
        socat
        the_silver_searcher
        thunderbird
        tmux
        vim-enhanced
        vlc
        wireshark
        weechat
        xbacklight
        zsh
    )

    if [[ "$twofa_dependencies" == 'true' ]]; then
      sudo dnf install ykpers-devel libyubikey-devel libusb-devel autoconf gnupg gnupg2-smime pcsc-lite
    fi

    dnf group install -y ${(@)groups}
    dnf install -y ${(@)packages}
}


function configure_env {
    # Set my shell to zsh
    chsh -s =zsh zv

    # Setting up environment
    # wow how fucked up is this
    # I have to put this in an eval sublist
    # in order to not escape
    print "%F{cyan}Making Personal Directories%f"
    mkdir ~/{Development,Books,bin}

    print "%F{cyan}Linking Personal Directories%f"
    ln -s $HOME/Development/ $HOME/z

    ## Install NVM
    #print -P "%F{green}Installing NVM...%f"
    #(curl https://raw.githubusercontent.com/creationix/nvm/master/install.sh | bash)&

    #print -P "%F{green}Installing Lein...%f"
    #(curl https://raw.githubusercontent.com/technomancy/leiningen/stable/bin/lein > ~/bin )&

    # Install my custom /etc files
    for f in etc/**/*; do
        cp -i "/$f" "$f"
    done

    # Link a few user systemd unit files
    for uf in systemd/user/default.target.wants/*; do
        systemctl --user link "$uf"
    done

    # Import certificates into gpg
    gpg --import certificates/*
}

function ocaml {
    # Install ocaml-proper
    _ dnf install ocaml-camlp4-devel ocaml-ocamldoc ocaml-findlib-devel ocaml-extlib-devel ocaml-calendar-devel
    # Install opam
    wget https://raw.github.com/ocaml/opam/master/shell/opam_installer.sh -O - | sh -s /usr/local/bin/
    # Now install utop
}

function javascript {
    ## Build nodejs
    pushd
    mkdir -p ~/Development/node/local
    cd ~/Development/node
    curl https://nodejs.org/dist/node-latest.tar.gz | tar xz --strip-components=1
    ./configure --prefix=~/Development/node/local
    make install
    curl https://www.npmjs.org/install.sh | sh
    rehash npm

    ## Install our NPM packages
    npm_packages=(ternjs js-beautify yo ember-cli bower babel-core babel-cli)

    npm install -g ${(@)npm_packages}
    popd
}

case $1 in
    # install dotfiles one by one
    # I make absolutely zero promises on how this will work with OSX coreutils
    # or any other OS toolchain currently experiencing a time warp to late '80s
    install)
        # List of files & directories not to link
        typeset -aU nolink
        nolink=("^\." certificates Makefile org lib ida.cfg ssh_config
                ebin zv.gpg.pub README.md .gpg .sh)
        # Now link our files
        git ls-tree --name-only HEAD | grep -v "${(j:\|:)nolink}" |\
            xargs -p -I % sh -c "ln -s $(realpath %) $HOME/.%"
        ;;
    protect)
        protect
        ;;
    unprotect)
        unprotect
        ;;
esac
