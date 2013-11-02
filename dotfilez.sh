#!/usr/bin/env bash

# install dotfiles one by one
# I make absolutely zero promises on how this will work with OSX coreutils
# or any other OS toolchain currently experiencing a time warp to late '80s

if [ "$1" == "interactive" ]; then
    git ls-tree --name-only HEAD | \
        grep -v '^\.\|Makefile\|README.md\|id_rsa.gpg\|ssh_config' | \
        xargs -p -I % sh -c "ln -s $(realpath %) $HOME/.%"

else if [ "$1" == "backup" ]; then
    mkdir -p ~/.dotfilez_backups/`date %+F`
    ls $(DOTFILES) | xargs -n 1 readlink -f | \
        xargs -n 1 basename | \
        xargs -t -n 1 -I {} cp -f `pwd`/{} ~/.dotfilez_backups/`date %+F`
    cp -r ~/.vim ~/.dotfilez_backups/`date %+F`/.vim
    cp -r ~/.xmonad ~/.dotfilez_backups/`date %+F`/.xmonad

fi

