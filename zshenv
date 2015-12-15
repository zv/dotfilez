#!/usr/bin/env zsh
# zv - 2015

#
# Editors
#

export EDITOR='vim'
export VISUAL='vim'

#
# Man
#
export MANPAGER="/bin/sh -c \"sed -r 's/\x1B\[([0-9]{1,2}(;[0-9]{1,2})?)?[m|K]//g' | vim -c 'set ft=man ts=8 norelativenumber nomod nolist nonu noma showtabline=0' -\""

#
# Build
#
export CXX=g++
export CC=gcc
export AR=ar

#
# Language
#
export LANG='en_US.UTF-8'
export LC_CTYPE=$LANG

#
# Environment
#
export GREP_COLORS="37;45"
# 10 years I've been listenining to this list prompt and today I am fucking done!
export LISTPROMPT=''

# Don't automatically rename my windows in screen/tmux
export DISABLE_AUTO_TITLE="true"

## Golang
export GOROOT=$HOME/Development/go
export GOPATH=$HOME/Development/golang
## Rust
export RUST_SRC_PATH=$HOME/Development/rust/src
## NVM
export NVM_DIR="/home/zv/.nvm"

# Ensure path arrays do not contain duplicates.
typeset -gU fpath mailpath path #cdpath

#
# Paths
#
path=(
    $HOME/.bin
    /usr/local/{bin,sbin}
    /usr/local/lib
    $path
    #{$GOROOT,$GOPATH}/bin
    #/usr/local/{pgsql,heroku,plan9}/bin
    #$HOME/depot_tools
)

# Remove all empty directories from the path
#for np ($path) if [[ ! -d $np ]] path=(${path#$np})

# Load our completion functions
fpath=(
    $HOME/.zsh/zsh-completions/src
    $HOME/.zsh/completion
    $HOME/.zsh/completion/rust-completion
    $HOME/.zsh/functions
    $fpath
)

# Set the the list of directories that cd searches.
# cdpath=(
#   $cdpath
# )

#
# Less
#
export PAGER='less'

# Set the default Less options.
# Mouse-wheel scrolling has been disabled by -X (disable screen clearing).
# Remove -X and -F (exit if the content fits on one screen) to enable it.
export LESS='-F -g -i -M -R -S -w -X -z-4'

# Set the Less input preprocessor.
# Try both `lesspipe` and `lesspipe.sh` as either might exist on a system.
#if (( $#commands[(i)lesspipe(|.sh)] )); then
#  export LESSOPEN="| /usr/bin/env $commands[(i)lesspipe(|.sh)] %s 2>&-"
#fi

#
# Temporary Files
#
if [[ ! -d "$TMPDIR" ]]; then
  export TMPDIR="/tmp/$LOGNAME"
  mkdir -p -m 700 "$TMPDIR"
fi

TMPPREFIX="${TMPDIR%/}/zsh"

#
# Browser
#
if [[ -x google-chrome ]]; then
    export BROWSER=google-chrome
fi
