#!/bin/sh

# install dotfiles one by one
# I make absolutely zero promises on how this will work with OSX coreutils
# or any other OS toolchain currently experiencing a time warp to late '80s

install_deps() {
    git submodule update --init

    yum -y install yum-utils
    # Development tools
    yum -y groupinstall 'Development Tools'
    yum -y groupinstall 'C Development Tools and Libraries'

    # Haskell
    yum -y install haskell-platform
    cabal configure
    cabal install hoogle hlint hdevtools ghc-mok
    # ag
    yum -y install pkgconfig automake gcc zlib-devel pcre-devel xz-devel
    cd lib/ag
    ./build.sh
    sudo make install
    cd ../..
    # Misc
    yum -y install aircrack-ng bvi clang-devel dnsenum dnsmap ettercap-gtk
    flawfinder gcc-go hping3 htop john libvirt lua-devel luajit-devel
    mtr ncrack ncrack nmap ntop p0f mutt rats rlwrap rpmdevtools rxvt-unicode
    scapy scrot socat tmux unhide weechat wireshark xbacklight xmonad
    screensaver yersinia zsh emacs ack the_silver_searcher vim-enhanced
}

## Make /tmp temporary

setup_home() {
    ## For my org files
    mkdir ~/org
}
setup_tmpfs()  { cat "tmpfs   /tmp         tmpfs   nodev,nosuid,size=2G          0  0" >> /etc/fstab }
build_erlang() {
    mkdir /tmp/erlang && cd /tmp/erlang
    wget -qO - http://www.erlang.org/download/otp_src_17.3.tar.gz     | tar zxvf -
    wget -qO - http://www.erlang.org/download/otp_doc_man_17.3.tar.gz | tar zxvf -
}
encrypt_setup() {
    gpg2 -a -r "zv@nxvr.org" --encrypt-files id_rsa config known_hosts *.pem
}

case $1 in
    install)
        git ls-tree --name-only HEAD | \
            grep -v '^\.\|Makefile\|README.md\|id_rsa.gpg\|ssh_config' | \
            xargs -p -I % sh -c "ln -s $(realpath %) $HOME/.%"
        # compile erlang our prompt here...
        if [[ -x $(dirname erlc) ]]; then
            erlc -o ebin ebin/zv_shell.erl
        fi
        ln -s $HOME/.Xresources $HOME/.Xdefaults
        ;;
    # pack/extract/link keys etc.
    secrets)
        case $2 in
            ls | grep "aws$\|ssh$\|gnupg$" | \
                xargs -p -I % sh -c "ln -s $(realpath %) $HOME/.%"
        esac
        # End secrets moving block
        ;;
    *) echo "You must provide 'install', 'backup' or 'secrets' as an argument. See the shell script" ;;
esac
