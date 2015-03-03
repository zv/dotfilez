#!/bin/zsh

function build_emacs {
    # Dependencies
    sudo dnf -y install atk-devel cairo-devel freetype-devel \
         fontconfig-devel dbus-devel giflib-devel glibc-devel gtk2-devel \
         libpng-devel libjpeg-devel libtiff-devel libX11-devel libXau-devel \
         libXdmcp-devel libXrender-devel libXt-devel libXpm-devel \
         ncurses-devel xorg-x11-proto-devel zlib-devel librsvg2-devel \
         m17n-lib-devel libotf-devel autoconf automake bzip2 cairo texinfo \
         gzip GConf2-devel alsa-lib-devel desktop-file-utils python2-devel \
         python3-devel util-linux gnutls-devel && \
        ## Build emacs
        git clone -b emacs-24.4 git://git.sv.gnu.org/emacs.git ~/Development/emacs && \
        ./autogen && \
        ./configure --prefix=/usr/local/emacs24 --with-x-toolkit=gtk --with-gnutls --with-file-notification=yes && \
        make bootstrap && \
        make -j4 &&
        (sudo alternatives --install /usr/bin/emacs emacs /usr/local/emacs24/bin/emacs 20000;
         sudo alternatives --install /usr/bin/emacsclient emacsclient /usr/local/emacs24/bin/emacsclient 20000)
    ## Build org mode
    # cd ~/emacs.d/contrib/zv/org-mode
    # make && sudo make install-info
    # rust && cargo && multirust
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

        # compile erlang our prompt here...
        if [[ -x erlc ]]; then
            erlc -o ebin ebin/zv_shell.erl
        fi

        # Link Xresources to Xdefaults
        ln -s $HOME/.Xresources $HOME/.Xdefaults
        ;;
    backup_secrets)
        gpg --export-ownertrust > zv-ownertrust-gpg.txt
        # Tar our setup
        tar -cf backup.tar ssh/{id_rsa,id_ecdsa,other_keys.seahorse} gnupg/pubring.gpg gnupg/secring.gpg zv-ownertrust-gpg.txt
        gpg --output backup.tar.gpg --symmetric backup.tar
        rm -f zv-ownertrust-gpg.txt
        rm -f backup.tar
        ;;
    extract_secrets)
        gpg --decrypt backup.tar.gpg > backup.tar # && tar -xvf backup.tar && rm -f backup.tar
        ;;
esac

function print_zv_greetz {
    print -P "z  %F{green}███████%F{red}╗%F{green}██%F{red}╗   %F{green}██%F{red}╗%f  z"
    print -P "e  %F{red}╚══%F{green}███%F{red}╔╝%F{green}%F{green}██%F{red}║   %F{green}██%f%F{red}║%f  e"
    print -P "t    %F{green}███%F{red}╔╝ %F{green}██%F{red}║   %F{green}██%F{red}║%f  t"
    print -P "a   %F{green}███%F{red}╔╝  %F{red}╚%f%F{green}██%F{red}╗ %F{green}██%f%F{red}╔╝%f  a"
    print -P "v  %F{green}███████%F{red}╗ %F{red}╚%f%F{green}████%f%F{red}╔╝%f   v"
    print -P "o  %F{red}╚══════╝  %F{red}╚═══╝%f    o"
    print -P "l  too hot to handle  l"
    print -P "t  too cold to hold.  t"
}

###
# This function downloads and installs all the fonts I use.
###
function get_fonts {
    wget https://github.com/adobe-fonts/source-code-pro/releases/tag/1.017R


    print -P "%F{green}Installing Powerline Fonts...%f"
    (git clone git@github.com:powerline/fonts.git /tmp && /tmp/install.sh)&
}

# What needs to get done for decrypting
# decrypt all ssh/*.asc files
# decrypt my org files
# What needs to get done for package install
# build emacs from source
function setup_env {
    dnf check-update
    # Setting up environment
    # wow how fucked up is this
    # I have to put this in an eval sublist
    # in order to not escape
    print "%F{cyan}Making Personal Directories%f"
    mkdir ~/{Development,Books,bin}

    print "%F{cyan}Linking Personal Directories%f"
    ln -s $HOME/Development/ $HOME/z
    ln -s $HOME/Development/ $HOME/z

    print -P "%F{green}Installing local packages, grab some coffee%f"

    # Development tools
    ## List of yum groups to be installed
    groups=(
        'C Development Tools and Libraries'
        'Development Tools'
        'System Tools'
    )

    # List of packages to be installed
    packages=(
        ack
        aircrack-ng
        bvi
        clang-devel
        dnsenum
        dnsmap
        emacs
        curl
        htop
        libvirt
        mtr
        mutt
        ncrack
        git-extras
        gitflow
        global # gtags
        nmap
        ntop
        hping3
        rlwrap
        rpmdevtools
        rxvt-unicode-256color-ml
        scapy
        gnupg2
        graphviz
        scrot
        socat
        the_silver_searcher
        tmux
        vim-enhanced
        weechat
        wireshark
        xbacklight
        xmonad
        zsh
    )

    dnf group install -y "${(j: :)groups}"
    dnf install -y "${(j: :)packages}"

    ## Install NVM
    #print -P "%F{green}Installing NVM...%f"
    #(curl https://raw.githubusercontent.com/creationix/nvm/master/install.sh | bash)&

    #print -P "%F{green}Installing Lein...%f"
    #(curl https://raw.githubusercontent.com/technomancy/leiningen/stable/bin/lein > ~/bin )&

    # print -P "%F{green}Building Emacs...%f"
    # build_emacs

    # systemctl unmask tmp.mount
}
