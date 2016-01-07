# -*- mode: shell-script; fill-column: 75; comment-column: 50; -*-

# Editors
export EDITOR='vim'
export VISUAL='vim'

# Man
export MANPAGER="/bin/sh -c \"sed -r 's/\x1B\[([0-9]{1,2}(;[0-9]{1,2})?)?[m|K]//g' | vim -c 'set ft=man ts=8 norelativenumber nomod nolist nonu noma showtabline=0' -\""

# Build
export CC=clang
export CXX=clang++

export AR=ar

# Language
export LANG='en_US.UTF-8'
export LC_CTYPE=$LANG

# Environment
export GREP_COLORS="37;45"
# 10 years I've been listenining to this list prompt and today I am fucking done!
export LISTPROMPT=''

# Don't automatically rename my windows in screen/tmux
export DISABLE_AUTO_TITLE="true"

# Golang
export GOROOT=$HOME/Development/go
export GOPATH=$HOME/Development/golang
# Rust
export RUST_SRC_PATH=$HOME/Development/rust/src
# NVM
export NVM_DIR="$HOME/.nvm"

# Ensure path arrays do not contain duplicates.
typeset -gU fpath mailpath path #cdpath

# Paths
path=(
    $HOME/.bin
    /usr/local/{bin,sbin}
    /usr/local/lib
    $path
    #{$GOROOT,$GOPATH}/bin
    #/usr/local/{pgsql,heroku,plan9}/bin
    $HOME/depot_tools
)

# Remove all empty directories from the path
#for np ($path) if [[ ! -d $np ]] path=(${path#$np})

# Load our completion functions
fpath=(
    $HOME/.zsh/completion
    $HOME/.zsh/completion/rust-completion
    $HOME/.zsh/functions
    $HOME/.zsh/zsh-completions/src
    $fpath
)

# Set the the list of directories that cd searches. Less
export PAGER='less'

# Set the default Less options.
# Mouse-wheel scrolling has been disabled by -X (disable screen clearing).
# Remove -X and -F (exit if the content fits on one screen) to enable it.
export LESS='-F -g -i -M -R -S -w -X -z-4'

# Temporary Files
if [[ ! -d "$TMPDIR" ]]; then
    export TMPDIR="/tmp/$LOGNAME" 
    mkdir -p -m 700 "$TMPDIR" 
fi
export TMPPREFIX="${TMPDIR%/}/zsh"

# Browser
(( $+commands[google-chrome] )) && export BROWSER=google-chrome

# History
export HISTFILE=$HOME/.zsh_history

# Lines to store
export HISTSIZE=$((2**14 - 1))
export SAVEHIST=$((2**14 - 1))
