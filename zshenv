# -*- mode: shell-script; fill-column: 75; comment-column: 50; -*-

# Editors
export EDITOR='vim'
export VISUAL='vim'

# Build
#export CC=clang
#export CXX=clang++
export AR=ar

# Language
if [[ -z "$LANG" ]]; then
    export LANG='en_US.UTF-8'
fi
export LC_CTYPE=$LANG

# Environment
export GREP_COLORS="37;45"
# 10 years I've been listenining to this list prompt and today I am fucking done!
export LISTPROMPT=''

# Don't automatically rename my windows in screen/tmux
export DISABLE_AUTO_TITLE="true"

# Golang
export GOROOT=/usr/local/go
export GOPATH=$HOME/Development/golang
# Rust
export RUST_SRC_PATH=/usr/local/src/rust/src
# NVM
export NVM_DIR="$HOME/.nvm"

# Ensure path arrays do not contain duplicates.
typeset -gU fpath mailpath path cdpath

# Paths
path=(
    $HOME/.bin
    /usr/local/{bin,sbin}
    /usr/local/lib
    $path
    {$GOROOT,$GOPATH}/bin
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

# Set the Less input preprocessor.
# Try both `lesspipe` and `lesspipe.sh` as either might exist on a system.
if (( $#commands[(i)lesspipe(|.sh)] )); then
  export LESSOPEN="| /usr/bin/env $commands[(i)lesspipe(|.sh)] %s 2>&-"
fi

# Temporary Files
if [[ ! -d "$TMPDIR" ]]; then
    export TMPDIR="/tmp/$LOGNAME"
    mkdir -p -m 700 "$TMPDIR"
fi
TMPPREFIX="${TMPDIR%/}/zsh"

# Browser
if (( $+commands[google-chrome] )); then
    export BROWSER==google-chrome
fi

# Font Config
if [[ "$OSTYPE" == linux* ]]; then
    export FONTCONFIG_PATH=${FONTCONFIG_PATH:=/etc/fonts}
fi
