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
    # Erlang
    # wget -qO - http://www.erlang.org/download/otp_src_17.0.tar.gz | tar zxvf -
    # wget -qO - http://www.erlang.org/download/otp_doc_man_17.0.tar.gz | tar zxvf -
    ### Elixir
    #git clone https://github.com/elixir-lang/elixir.git
    #cd elixir
    #make clean test
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
    xscreensaver yersinia zsh yum build-dep vim-X11 emacs ack
    # Add ag
}



case $1 in
    install)
        # git clone https://github.com/gmarik/Vundle.vim.git vim/bundle/vundle
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
            extract)
                gpg --verify keys.enc.sig keys.enc
                gpg --output secrets.tar.gz --decrypt keys.enc
                tar -xvf secrets.tar.gz
                rm secrets.tar.gz
                ;;

            link)
                ls | grep "aws$\|ssh$\|gnupg$" | \
                    xargs -p -I % sh -c "ln -s $(realpath %) $HOME/.%"
                ;;

            repack) # Pack all my secrets
                tar -cvzf keys.tar.gz --exclude="aws-*" --exclude="ec2-*" aws gnupg/*(.) ssh
                gpg --symmetric --cipher-algo AES256 --armor -o keys.enc keys.tar.gz
                gpg --output keys.enc.sig --armor --detach-sig keys.enc
                rm keys.tar.gz
                ;;
        esac
        # End secrets moving block
        ;;
    *) echo "You must provide 'install', 'backup' or 'secrets' as an argument. See the shell script" ;;
esac
