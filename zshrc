# This is zv's ZSHRC.
# haq the plan8
# meow nyan meow nyan meow
# 2013

#! /bin/zsh
# A script to make using 256 colors in zsh less painful.
# P.C. Shyamshankar <sykora@lucentbeing.com>
# Copied from http://github.com/sykora/etc/blob/master/zsh/functions/spectrum/

# Load our completion functions
fpath=(~/.zsh/completion $fpath)



typeset -Ag FX FG BG

FX=(
    reset     "%{[00m%}"
    bold      "%{[01m%}" no-bold      "%{[22m%}"
    italic    "%{[03m%}" no-italic    "%{[23m%}"
    underline "%{[04m%}" no-underline "%{[24m%}"
    blink     "%{[05m%}" no-blink     "%{[25m%}"
    reverse   "%{[07m%}" no-reverse   "%{[27m%}"
)

for color in {000..255}; do
    FG[$color]="%{[38;5;${color}m%}"
    BG[$color]="%{[48;5;${color}m%}"
done

# Show all 256 colors with color number
function spectrum_ls() {
  for code in {000..255}; do
    print -P -- "$code: %F{$code}Test%f"
  done
}

############################################
#  Theme
#############################################

ZEPHYR_THEME_GIT_PROMPT_PREFIX="%{$reset_color%}%{$fg[white]%}["
ZEPHYR_THEME_GIT_PROMPT_SUFFIX="]%{$reset_color%}"
ZEPHYR_THEME_GIT_PROMPT_DIRTY="%{$fg[red]%}●%{$reset_color%}"
ZEPHYR_THEME_GIT_PROMPT_CLEAN=""

prompt_fix_for_git() {
  local cb=$(current_branch)
  if [ -n "$cb" ]; then
    echo "- $ZEPHYR_THEME_GIT_PROMPT_PREFIX$(current_branch)$(parse_git_dirty)$ZEPHYR_THEME_GIT_PROMPT_SUFFIX"
  fi
}

# shows a slightly different prompt for vicmd vs other ZLE command modes to let
# you know what you are dealing with.
zle_vim_prompt_notifier() {
  if [ "$KEYMAP" = vicmd ]; then
    print "%F{001}>>%f"
  else
    print ">>"
  fi
}

PROMPT='[%n@%m]%2~ $(prompt_fix_for_git) $(zle_vim_prompt_notifier) '

#############################################
#   Vim Mode
#############################################

function zle-line-init zle-keymap-select {
  zle reset-prompt
}

# ZSH line editor stuff needed for bindkey to work

zle -N zle-line-init
zle -N zle-keymap-select

# bindkey -e is emacs mode
bindkey -v

#############################################
# Edit Comman Line
#############################################

autoload -U edit-command-line
zle -N edit-command-line
bindkey '\C-x\C-e' edit-command-line


#############################################
# Aliasing
#############################################

# Push and pop directories on directory stack
alias pu='pushd'
alias po='popd'

# Basic directory operations
alias ...='cd ../..'
alias -- -='cd -'

# Super user
alias _='sudo'
alias _bex='bundle exec'

# Show history
alias history='fc -l 1'

# List direcory contents
alias lsa='ls -lah'
alias l='ls -la'

# we're ubuntu bro!
alias ll='ls -l'

# # mkdir & cd to it
function mcd() {
  mkdir -p "$1" && cd "$1";
}

alias ta="tmux attach-session -t"
alias tl="tmux list-sessions"
# esxape ANSI sequences
alias stresc='sed -r "s/\x1B\[([0-9]{1,2}(;[0-9]{1,2})?)?[m|K]//g"'

# Elixir Mix
alias mxc='mix compile'
alias mxd='mix deps'
alias mxg='mix deps.get'
alias mxdc='mix deps.compile'
alias mxdu='mix deps.update'
alias mxt='mix test'

# Switch Users, maining RTP.
alias sudovim='sudo vim -c "set runtimepath+=/home/zv/.vim" -u /home/zv/.vimrc'

# so fuckin sick of this
alias mongostart='sudo service mongod start'

############################################
#   Completions
############################################

unsetopt menu_complete   # do not autoselect the first completion entry
unsetopt flowcontrol
setopt auto_menu         # show completion menu on succesive tab press
setopt complete_in_word
setopt always_to_end

WORDCHARS=''

zmodload -i zsh/complist

## case-insensitive (all),partial-word and then substring completion
if [ "x$CASE_SENSITIVE" = "xtrue" ]; then
  zstyle ':completion:*' matcher-list 'r:|[._-]=* r:|=*' 'l:|=* r:|=*'
  unset CASE_SENSITIVE
else
  zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z}' 'r:|[._-]=* r:|=*' 'l:|=* r:|=*'
fi

zstyle ':completion:*' list-colors ''

# should this be in keybindings?
bindkey -M menuselect '^o' accept-and-infer-next-history

zstyle ':completion:*:*:*:*:*' menu select
zstyle ':completion:*:*:kill:*:processes' list-colors '=(#b) #([0-9]#) ([0-9a-z-]#)*=01;34=0=01'
zstyle ':completion:*:*:*:*:processes' command "ps -u `whoami` -o pid,user,comm -w -w"

# disable named-directories autocompletion
zstyle ':completion:*:cd:*' tag-order local-directories directory-stack path-directories
cdpath=(.)

# use /etc/hosts and known_hosts for hostname completion
if [[ -a /etc/ssh/ssh_known_hosts ]]; then
  [ -r /etc/ssh/ssh_known_hosts ] && _global_ssh_hosts=(${${${${(f)"$(</etc/ssh/ssh_known_hosts)"}:#[\|]*}%%\ *}%%,*}) || _ssh_hosts=()
fi

if [[ -a ~/.ssh/known_hosts ]]; then
  [ -r ~/.ssh/known_hosts ] && _ssh_hosts=(${${${${(f)"$(<$HOME/.ssh/known_hosts)"}:#[\|]*}%%\ *}%%,*}) || _ssh_hosts=()
fi

[ -r /etc/hosts ] && : ${(A)_etc_hosts:=${(s: :)${(ps:\t:)${${(f)~~"$(</etc/hosts)"}%%\#*}##[:blank:]#[^[:blank:]]#}}} || _etc_hosts=()

hosts=(
  "$_global_ssh_hosts[@]"
  "$_ssh_hosts[@]"
  "$_etc_hosts[@]"
  "$HOST"
  localhost
)
zstyle ':completion:*:hosts' hosts $hosts

# Use caching so that commands like apt and dpkg complete are useable
zstyle ':completion::complete:*' use-cache 1
zstyle ':completion::complete:*' cache-path ~/.oh-my-zsh/cache/

#############################################
#  Corrections
#############################################

setopt correct_all

alias man='nocorrect man'
alias mv='nocorrect mv'
alias mysql='nocorrect mysql'
alias mkdir='nocorrect mkdir'
alias gist='nocorrect gist'
alias heroku='nocorrect heroku'
alias ebuild='nocorrect ebuild'
alias hpodder='nocorrect hpodder'

alias bower='noglob bower'

#############################################
# Directory Shortcuts
#############################################

setopt auto_name_dirs
setopt auto_pushd
setopt pushd_ignore_dups


#############################################
#
# "Those who do not remember history are
#  doomed to repeat it."
#  - Cicero
#
### History #################################

# Where I store my shit
HISTFILE=$HOME/.zsh_history

# Lines to store
HISTSIZE=10000
SAVEHIST=10000

setopt append_history
setopt extended_history
setopt hist_expire_dups_first
setopt hist_ignore_dups # ignore duplication command history list
setopt hist_ignore_space
setopt hist_verify
setopt inc_append_history
setopt share_history # share command history data

#############################################
#
# If I had eight hours to chop down a tree,
#  I would spend 6 hours sharpening an axe.
#  - Anonymous
#
### Bindings ################################

bindkey '\ew' kill-region
bindkey -s '\el' "ls\n"
bindkey -s '\e.' "..\n"
bindkey '^r' history-incremental-search-backward
bindkey "^[[5~" up-line-or-history
bindkey "^[[6~" down-line-or-history

# make search up and down work, so partially type and hit up/down to find relevant stuff
bindkey '^[[A' up-line-or-search
bindkey '^[[B' down-line-or-search
bindkey ' ' magic-space    # also do history expansion on space
bindkey "^[[1;5C" forward-word
bindkey "^[[1;5D" backward-word
bindkey '^[[Z' reverse-menu-complete

# Make the delete key work instead of outputting a ~
bindkey '^?' backward-delete-char
bindkey "^[[3~" delete-char
bindkey "^[3;5~" delete-char
bindkey "\e[3~" delete-char


#############################################
#
# "Where lesser men make excuses, I make a path"
#   - Hannibal Barca
#
### Colors from the beyond! #################

# ls colors
autoload colors; colors;
export LSCOLORS="Gxfxcxdxbxegedabagacad"


# Ain't beepin' dog
setopt no_beep

# Automatic CD on directory specification
setopt auto_cd

# CD into variable from var name
setopt cdablevarS

# git theming default: Variables for theming the git info prompt
ZSH_THEME_GIT_PROMPT_PREFIX="git:("         # Prefix at the very beginning of the prompt, before the branch name
ZSH_THEME_GIT_PROMPT_SUFFIX=")"             # At the very end of the prompt
ZSH_THEME_GIT_PROMPT_DIRTY="*"              # Text to display if the branch is dirty
ZSH_THEME_GIT_PROMPT_CLEAN=""               # Text to display if the branch is clean

# Setup the prompt with pretty colors
setopt prompt_subst


# 10 years I've been listenining to this list prompt and today I am fucking done!
export LISTPROMPT=''

# Syntax highlightin'
source $HOME/.zsh/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh

# The crazier the better!
export GREP_COLORS="38;5;230:sl=38;5;240:cs=38;5;100:mt=38;5;161:fn=38;5;197:ln=38;5;212:bn=38;5;44:se=38;5;166"
eval `dircolors ~/.zsh/LS_COLORS`

#############################################
#
# "Thats all folks"
#   - Bugs Bunny
#
### But wait, theres more ##################

export EDITOR='vim' # didn't see that one coming.

# smart urls
autoload -U url-quote-magic
zle -N self-insert url-quote-magic

# file rename magick
bindkey "^[m" copy-prev-shell-word

# Long list of Jobs
setopt long_list_jobs

# Less 4 lyfe
export PAGER="less -R"
export LC_CTYPE=$LANG

# vim for manpages
export MANPAGER="/bin/sh -c \"col -b | vim -c 'set ft=man ts=8 nomod nolist nonu noma' -\""

# make related items
export CFLAGS="-march=native -mtune=native -O2 -pipe"
export CXXFLAGS="$CFLAGS"
export LDFLAGS="-Wl,--hash-style=gnu -Wl,--as-needed"
export MAKEFLAGS="-j4"



############################################
#
# Face the facts of being what you are, for
#  that is what changes what you are.
#  - Kierkegaard
#
#### Autocompletion and associates #########


autoload -U compinit
compinit -i

zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}


for function in ~/.zsh/functions/*; do
  source $function
done


# don't care much for these, but here they are
# main cursor bracket pattern root
# specified as such (main brackets pattern cursor
ZSH_HIGHLIGHT_HIGHLIGHTERS=()

### Closing words ###########################
#
# Isn't it a pleasure to study and practice
#  what you have learned?
#  - Confucious
#
#############################################

export PATH="$HOME/bin:/usr/local/bin:$PATH"

#export PATH=":$PATH:$HOME/go/bin"
#export GOPATH=$HOME/Development/go
#export GOROOT=$HOME/go
