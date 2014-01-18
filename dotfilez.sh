#!/usr/bin/env bash

# install dotfiles one by one
# I make absolutely zero promises on how this will work with OSX coreutils
# or any other OS toolchain currently experiencing a time warp to late '80s

if [ "$1" == "install" ]; then
    git ls-tree --name-only HEAD | \
        grep -v '^\.\|Makefile\|README.md\|id_rsa.gpg\|ssh_config' | \
        xargs -p -I % sh -c "ln -s $(realpath %) $HOME/.%"
    ln -s $HOME/.Xresources $HOME/.Xdefaults
fi

if [ "$1" == "backup" ]; then
    mkdir -p ~/.dotfilez_backups/`date %+F`
    ls $(DOTFILES) | xargs -n 1 readlink -f | \
        xargs -n 1 basename | \
        xargs -t -n 1 -I {} cp -f `pwd`/{} ~/.dotfilez_backups/`date %+F`
    cp -r ~/.vim ~/.dotfilez_backups/`date %+F`/.vim
    cp -r ~/.xmonad ~/.dotfilez_backups/`date %+F`/.xmonad
fi

# pack/extract/link keys etc.
if [ "$1" == "secrets" ]; then
    if [ "$2" == "extract" ]; then
        gpg --verify keys.enc.sig keys.enc
        gpg --output secrets.tar.gz --decrypt keys.enc
        tar -xvf secrets.tar.gz
        rm secrets.tar.gz
    fi

    if [ "$2" == "link" ]; then
        ls | grep "aws$\|ssh$\|gnupg$" | \
        xargs -p -I % sh -c "ln -s $(realpath %) $HOME/.%"
    fi

    if [ "$2" == "repack" ]; then
        tar -cvzf keys.tar.gz --exclude="aws-*" --exclude="ec2-*" aws gnupg ssh
        gpg --symmetric --cipher-algo AES256 --armor -o keys.enc keys.tar.gz
        gpg --output keys.enc.sig --armor --detach-sig keys.enc
        rm keys.tar.gz
    fi
fi
