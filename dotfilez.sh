#!/bin/zsh

# install dotfiles one by one
# I make absolutely zero promises on how this will work with OSX coreutils
# or any other OS toolchain currently experiencing a time warp to late '80s



function build_emacs {
    # Dependencies
    sudo yum -y install atk-devel cairo-devel freetype-devel \
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
    cd ~/emacs.d/contrib/zv/org-mode
    make && sudo make install-info
}

function encrypt_setup {
    gpg2 -a -r "zv@nxvr.org" --encrypt-files id_rsa config known_hosts *.pem
}

case $1 in
    install)
        # List of files & directories not to link
        typeset -aU nolink
        nolink=("^\." Makefile dotfilez.sh org lib ida.cfg ssh_config
        ebin zv.gpg.pub README.md id_rsa.gpg)
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
esac

# What needs to get done for decrypting
# decrypt all ssh/*.asc files
# decrypt my org files
# What needs to get done for package install
# build emacs from source

print -P "z  %F{green}███████%F{red}╗%F{green}██%F{red}╗   %F{green}██%F{red}╗%f  z"
print -P "e  %F{red}╚══%F{green}███%F{red}╔╝%F{green}%F{green}██%F{red}║   %F{green}██%f%F{red}║%f  e"
print -P "t    %F{green}███%F{red}╔╝ %F{green}██%F{red}║   %F{green}██%F{red}║%f  t"
print -P "a   %F{green}███%F{red}╔╝  %F{red}╚%f%F{green}██%F{red}╗ %F{green}██%f%F{red}╔╝%f  a"
print -P "v  %F{green}███████%F{red}╗ %F{red}╚%f%F{green}████%f%F{red}╔╝%f   v"
print -P "o  %F{red}╚══════╝  %F{red}╚═══╝%f    o"
print -P "l  too hot to handle  l"
print -P "t  too cold to hold.  t"

function setup_env() {
    # Setting up environment
    # wow how fucked up is this
    # I have to put this in an eval sublist
    # in order to not escape 
    mkdir ~/{Development, Books, bin}
    ln -s $HOME/z ~/Development
    ln -s $HOME/Z ~/Development

    print -P "%F{green}Installing local packages%f"
    yum -y install yum-utils
    # Development tools
    ## List of yum groups to be installed
    groups=(
        'C Development Tools and Libraries'
        'Development Tools'
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
        nmap
        ntop
        rlwrap
        rpmdevtools
        rxvt-unicode-256color-ml
        scapy
        gnupg2
        graphviz
        scrot
        socat
        the_silver_searcher
        levien-inconsolata-fonts
        tmux
        vim-enhanced
        weechat
        wireshark
        xbacklight
        xmonad
        zsh
    )
    yum -y groupinstall "${(j: :)groups}"
    yum install -y "${(j: :)packages}"

    print -P "%F{green}Installing NVM...%f"
    ## Install NVM
    curl https://raw.githubusercontent.com/creationix/nvm/v0.23.2/install.sh | bash

    print -P "%F{green}Installing Lein...%f"

    curl https://raw.githubusercontent.com/technomancy/leiningen/stable/bin/lein > ~/bin

    build_emacs

    systemctl unmask tmp.mount
}
