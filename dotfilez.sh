#!/bin/zsh

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
    dictd # Dictionary
    ## Things that should be installed from git and symlinked
    ## emacs
    ## ditaa
    ## graphviz
    ## erlang
    ## elixir 
    ## zsh
}

## Make /tmp temporary
setup_home() {
    ## For my org files
    mkdir ~/org
}
setup_tmpfs()  { cat "tmpfs   /tmp         tmpfs   nodev,nosuid,size=2G          0  0" >> /etc/fstab
}
encrypt_setup() {
    gpg2 -a -r "zv@nxvr.org" --encrypt-files id_rsa config known_hosts *.pem
}

case $1 in
    install)
        # List of files & directories not to link
        typeset -aU nolink
        nolink=("^\." Makefile dotfilez.sh org lib ida.cfg ssh_config ebin zv.gpg.pub README.md id_rsa.gpg)
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
# build ag from source

print -P "                                                                  z  %F{green}███████%F{red}╗%F{green}██%F{red}╗   %F{green}██%F{red}╗%f  z"
print -P "                                                                  e  %F{red}╚══%F{green}███%F{red}╔╝%F{green}%F{green}██%F{red}║   %F{green}██%f%F{red}║%f  e"
print -P "                                                                  t    %F{green}███%F{red}╔╝ %F{green}██%F{red}║   %F{green}██%F{red}║%f  t"
print -P "                                                                  a   %F{green}███%F{red}╔╝  %F{red}╚%f%F{green}██%F{red}╗ %F{green}██%f%F{red}╔╝%f  a"
print -P "                                                                  v  %F{green}███████%F{red}╗ %F{red}╚%f%F{green}████%f%F{red}╔╝%f   v"
print -P "                                                                  o  %F{red}╚══════╝  %F{red}╚═══╝%f    o"
print -P "                                                                  l  too hot to handle  l"
print -P "                                                                  t  too cold to hold.  t"
