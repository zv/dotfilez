#!/bin/zsh
# My personal key
typeset key_id='0xF6F2D0445DC172F8'
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
              zsh)

    dnf group install -y ${(@)groups}
    dnf install -y ${(@)packages}
}


# Includes two-factor authentication development libraries
function two_factor_auth {
    sudo dnf install ykpers-devel libyubikey-devel libusb-devel autoconf gnupg gnupg2-smime pcsc-lite
}

function configure_env {
    if [[ $0:a:h != ~/dotfilez ]]; then
        echo "It's recommended that this directory be located in ~/dotfilez"
        echo "Are you sure you want to continue? (Y/N)"
        if ! read -q; then
            exit
        fi
    fi
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

    # Install my custom /etc files
    echo "Installing custom /etc files"
    for f in etc/**/*; do
        cp -i "/$f" "$f"
    done

    # Link a few user systemd unit files
    for uf in systemd/user/default.target.wants/*; do
        systemctl --user link "$uf"
    done

    # Import certificates into gpg
    # gpg --import certificates/*
}

function ocaml {
    if [[ -x =ocaml ]]; then
        # Install ocaml-proper
        _ dnf install ocaml-camlp4-devel ocaml-ocamldoc ocaml-findlib-devel ocaml-extlib-devel ocaml-calendar-devel
        # Install opam
        wget https://raw.github.com/ocaml/opam/master/shell/opam_installer.sh -O - | sh -s /usr/local/bin/
        # Now install utop
    fi
}

function javascript {
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

# Link all relevant RC files.
function link_dotfiles {
    # List of files & directories not to link
    local -aU nolink
    nolink=("^\."
            .gpg
            .sh
            Makefile
            README.md
            certificates
            dconf
            ebin
            etc
            ida.cfg
            lib
            org
            ssh_config
            systemd
            .pub)
    # Now link our files
    git ls-tree --name-only HEAD | grep -v "${(j:\|:)nolink}" |\
        xargs -p -I % sh -c "ln -s $(realpath %) $HOME/.%"
}

case $1 in
    # install dotfiles one by one
    # I make absolutely zero promises on how this will work with OSX coreutils
    # or any other OS toolchain currently experiencing a time warp to late '80s
    test)
        local -a test_opts
        zparseopts -D -E -a test_opts -- l js ocaml base a h
        print -l $test_opts
        case $test_opts in
            -l) echo 'l';&
            -b) echo 'b';&
            --js) echo '-js';&
            --ocaml) echo '-caml';&
        esac
        ;;
    install)
        local -a install_opts
        zparseopts -D -E -a install_opts -- l js ocaml base a h
        case $install_opts in
            -l) link_dotfiles;&
            -b) install_pkgs;&
            --js) javascript;&
            --ocaml) ocaml;&
        esac
        ;;
    protect)
        protect
        ;;
    unprotect)
        unprotect
        ;;
    keyring)
        local destination=~/Dropbox/gnome-keyring.tar.gpg
        local keyring_path=~/.local/share/keyrings
        if [[ $2 = "backup" ]]; then
            echo 'Backing up gnome keyring';
            tar --create -C $keyring_path(:h) --wildcards -O $keyring_path(:t) | gpg --encrypt -r $key_id -o $destination - && echo "Wrote file to $destination"
        elif [[ $2 = "restore" ]]; then
            # Use gpg not gnome ssh-agent
            if [[ $(gconftool-2 --get /apps/gnome-keyring/daemon-components/ssh) != "false" ]]; then
                gconftool-2 --type bool --set /apps/gnome-keyring/daemon-components/ssh false
            fi
            if [[ -e $destination ]]; then
                local temp_dir=$(mktemp -d)
                gpg2 -d $destination > $temp_dir/gnome-keyring.tar
                tar xvf $temp_dir/gnome-keyring.tar
                if mv $keyring_path $temp_dir; then
                    echo "backed up keyring to $temp_dir"
                else
                    echo 'failed to backup keyring: EXITING'
                    exit
                fi
                mv keyrings ~/.local/share/keyrings
                mv $temp_dir/keyrings $keyring_path/old_keyrings
                echo "Restored keyring"
                echo "old keyrings are still in /tmp -- be careful if no tmpfs"
            else
                echo "No file"
            fi
        else
            echo "keyring backup # backup the keyring"
            echo "keyring restore # restore the keyring"
        fi
        ;;
    js)
        javascript
        ;;
    *)
        echo "install"
        echo "protect"
        echo "unprotect"
        echo "keyring"
        echo "js"
esac


# install packages

# configure env (setup keys, shell, yubikey)

# cryptographic setup
## unpack gpg
### configure yubikey
### disable gnome ssh-agent && enable gnupg agent
### udev rules
## setup ssh keys
## install certificates

# configure gnome stuff
## gnome-terminal (dconf)

# restore thunderbird
## restore feeds

### gpg-agent

### systemd unit files

# configure emacs

# configure {javascript,haskell,ocaml,rust,erlang/elixir}

# setup dropbox
