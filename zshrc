# This is zv's ZSHRC.
# haq the plan8
# meow nyan meow nyan meow
# 2013

#! /bin/zsh
# A script to make using 256 colors in zsh less painful.
# P.C. Shyamshankar <sykora@lucentbeing.com>
# Copied from http://github.com/sykora/etc/blob/master/zsh/functions/spectrum/

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

zv_prompt() {
  prompt_fix_for_git 
  #rvm_prompt_info
  #elixir_prompt_info
}

PROMPT='[%n@%m]%2~ $(zv_prompt)>> '

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
# Vim vundle helpers 
#############################################

function vundle () {
  vim -c "execute \"BundleInstall\" | q | q"
}

function vundle-update () {
  vim -c "execute \"BundleInstall!\" | q | q"
}

function vundle-clean () {
  vim -c "execute \"BundleClean!\" | q | q"
}



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

#############################################
# Directory Shortcuts 
#############################################

setopt auto_name_dirs
setopt auto_pushd
setopt pushd_ignore_dups

#############################################
#   Git 
#############################################

# get the name of the branch we are on
function git_prompt_info() {
  ref=$(git symbolic-ref HEAD 2> /dev/null) || return
  echo "$ZSH_THEME_GIT_PROMPT_PREFIX${ref#refs/heads/}$(parse_git_dirty)$ZSH_THEME_GIT_PROMPT_SUFFIX"
}


# Checks if working tree is dirty
parse_git_dirty() {
  local SUBMODULE_SYNTAX=''
  if [[ $POST_1_7_2_GIT -gt 0 ]]; then
        SUBMODULE_SYNTAX="--ignore-submodules=dirty"
  fi
  if [[ -n $(git status -s ${SUBMODULE_SYNTAX}  2> /dev/null) ]]; then
    echo "$ZSH_THEME_GIT_PROMPT_DIRTY"
  else
    echo "$ZSH_THEME_GIT_PROMPT_CLEAN"
  fi
}


# Checks if there are commits ahead from remote
function git_prompt_ahead() {
  if $(echo "$(git log origin/$(current_branch)..HEAD 2> /dev/null)" | grep '^commit' &> /dev/null); then
    echo "$ZSH_THEME_GIT_PROMPT_AHEAD"
  fi
}

# Formats prompt string for current git commit short SHA
function git_prompt_short_sha() {
  SHA=$(git rev-parse --short HEAD 2> /dev/null) && echo "$ZSH_THEME_GIT_PROMPT_SHA_BEFORE$SHA$ZSH_THEME_GIT_PROMPT_SHA_AFTER"
}

# Formats prompt string for current git commit long SHA
function git_prompt_long_sha() {
  SHA=$(git rev-parse HEAD 2> /dev/null) && echo "$ZSH_THEME_GIT_PROMPT_SHA_BEFORE$SHA$ZSH_THEME_GIT_PROMPT_SHA_AFTER"
}

# Get the status of the working tree
git_prompt_status() {
  INDEX=$(git status --porcelain 2> /dev/null)
  STATUS=""
  if $(echo "$INDEX" | grep '^?? ' &> /dev/null); then
    STATUS="$ZSH_THEME_GIT_PROMPT_UNTRACKED$STATUS"
  fi
  if $(echo "$INDEX" | grep '^A  ' &> /dev/null); then
    STATUS="$ZSH_THEME_GIT_PROMPT_ADDED$STATUS"
  elif $(echo "$INDEX" | grep '^M  ' &> /dev/null); then
    STATUS="$ZSH_THEME_GIT_PROMPT_ADDED$STATUS"
  fi
  if $(echo "$INDEX" | grep '^ M ' &> /dev/null); then
    STATUS="$ZSH_THEME_GIT_PROMPT_MODIFIED$STATUS"
  elif $(echo "$INDEX" | grep '^AM ' &> /dev/null); then
    STATUS="$ZSH_THEME_GIT_PROMPT_MODIFIED$STATUS"
  elif $(echo "$INDEX" | grep '^ T ' &> /dev/null); then
    STATUS="$ZSH_THEME_GIT_PROMPT_MODIFIED$STATUS"
  fi
  if $(echo "$INDEX" | grep '^R  ' &> /dev/null); then
    STATUS="$ZSH_THEME_GIT_PROMPT_RENAMED$STATUS"
  fi
  if $(echo "$INDEX" | grep '^ D ' &> /dev/null); then
    STATUS="$ZSH_THEME_GIT_PROMPT_DELETED$STATUS"
  elif $(echo "$INDEX" | grep '^AD ' &> /dev/null); then
    STATUS="$ZSH_THEME_GIT_PROMPT_DELETED$STATUS"
  fi
  if $(echo "$INDEX" | grep '^UU ' &> /dev/null); then
    STATUS="$ZSH_THEME_GIT_PROMPT_UNMERGED$STATUS"
  fi
  echo $STATUS
}

#compare the provided version of git to the version installed and on path
#prints 1 if input version <= installed version
#prints -1 otherwise
function git_compare_version() {
  local INPUT_GIT_VERSION=$1;
  local INSTALLED_GIT_VERSION
  INPUT_GIT_VERSION=(${(s/./)INPUT_GIT_VERSION});
  INSTALLED_GIT_VERSION=($(git --version));
  INSTALLED_GIT_VERSION=(${(s/./)INSTALLED_GIT_VERSION[3]});

  for i in {1..3}; do
    if [[ $INSTALLED_GIT_VERSION[$i] -lt $INPUT_GIT_VERSION[$i] ]]; then
      echo -1
      return 0
    fi
  done
  echo 1
}

#this is unlikely to change so make it all statically assigned
POST_1_7_2_GIT=$(git_compare_version "1.7.2")
#clean up the namespace slightly by removing the checker function
unset -f git_compare_version

# Git aliases
alias g='git'
alias gst='git status'
alias gl='git pull'
alias gup='git fetch && git rebase'
alias gp='git push'
gdv() { git diff -w "$@" | view - }
alias gc='git commit -v'
alias gca='git commit -v -a'
alias gco='git checkout'
alias gcm='git checkout master'
alias gb='git branch'
alias gba='git branch -a'
alias gcount='git shortlog -sn'
alias gcp='git cherry-pick'
alias glg='git log --stat --max-count=5'
alias glgg='git log --graph --max-count=5'
alias gss='git status -s'
alias ga='git add'
alias gm='git merge'
alias grh='git reset HEAD'
alias grhh='git reset HEAD --hard'

alias git-userlinecount="git ls-files | xargs -n1 -d'\n' -i git blame {} | perl -n -e '/\s\((.*?)\s[0-9]{4}/ && print \"$1\n\"' | sort -f | uniq -c -w3 | sort -r"

# Lists all files that have been ignored with git update-index
alias git-ignorelog='git ls-files -v | grep "^h"'

# Will return the current branch name
# Usage example: git pull origin $(current_branch)
function current_branch() {
  ref=$(git symbolic-ref HEAD 2> /dev/null) || return
  echo ${ref#refs/heads/}
}

# Apply the latest stash made inside this branch
alias gsalatest="git stash list | grep $(current_branch) | cut -d ':' -f 1"


#############################################
#  I prove that if markets are efficient,
#  meaning current prices fully reflect all
#  information available in past prices,
#  then P = NP, meaning every computational
#  problem whose solution can be verified
#  in polynomial time can also be solved
#  in polynomial time. I also prove the
#  converse by showing how we can
#  “program” the market to solve
#  NP-complete problems. Since P probably
#  does not equal NP, markets are
#  probably not efficient. Specifically,
#  markets become increasingly
#  inefficient as the time series
#  lengthens or becomes more frequent. An
#  illustration by way of partitioning
#  the excess returns to momentum
#  strategies based on data availability
#  confirms this prediction.#
#
#  - Social Science Research Network
#
### SSH #################################
local _plugin__ssh_env=$HOME/.ssh/environment-$HOST
ssh_identities=("id_rsa" "id_rsa_old") 
local _plugin__forwarding

function _plugin__start_agent()
{
  local -a identities

  # start ssh-agent and setup environment
  /usr/bin/env ssh-agent | sed 's/^echo/#echo/' > ${_plugin__ssh_env}
  chmod 600 ${_plugin__ssh_env}
  . ${_plugin__ssh_env} > /dev/null

  echo starting...
  for id in "${ssh_identities[@]}"
  do 
    if [ -f $HOME/.ssh/${id} ]; then
      /usr/bin/ssh-add $HOME/.ssh/${id}
    fi
  done
}

if [ -f "${_plugin__ssh_env}" ]; then

  # Source SSH settings, if applicable
  . ${_plugin__ssh_env} > /dev/null

  ps -ef | grep ${SSH_AGENT_PID} | grep ssh-agent$ > /dev/null || {

    _plugin__start_agent;

  }

else
  _plugin__start_agent;
fi

# tidy up after ourselves
unfunction _plugin__start_agent
unset _plugin__forwarding
unset _plugin__ssh_env

# Faster SSH forwarding
alias ssh-x='ssh -c arcfour,blowfish-cbc -XC'

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
export LS_COLORS

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

export GREP_OPTIONS='--color=auto'
export GREP_COLOR='1;32'

#############################################
#
# "Thats all folks"
#   - Bugs Bunny
#
### But wait, theres more ##################


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


############################################
#
# Face the facts of being what you are, for
#  that is what changes what you are.
#  - Kierkegaard
#
#### Autocompletion ########################
 
fpath=(~/.zsh/completion $fpath)

autoload -U compinit
compinit -i


#############################################
#
#  Go is like a better C from the guys that
#  didn't bring you C++
#  - Rob Pike 
#
### Golang #################################

# gc
prefixes=(5 6 8)
for p in $prefixes; do
  compctl -g "*.${p}" ${p}l
  compctl -g "*.go" ${p}g
done

# standard go tools
compctl -g "*.go" gofmt

# gccgo
compctl -g "*.go" gccgo

# go tool
__go_tool_complete() {
  typeset -a commands build_flags
  commands+=(
    'build[compile packages and dependencies]'
    'clean[remove object files]'
    'doc[run godoc on package sources]'
    'fix[run go tool fix on packages]'
    'fmt[run gofmt on package sources]'
    'get[download and install packages and dependencies]'
    'help[display help]'
    'install[compile and install packages and dependencies]'
    'list[list packages]'
    'run[compile and run Go program]'
    'test[test packages]'
    'tool[run specified go tool]'
    'version[print Go version]'
    'vet[run go tool vet on packages]'
  )
  if (( CURRENT == 2 )); then
    # explain go commands
    _values 'go tool commands' ${commands[@]}
    return
  fi
  build_flags=(
    '-a[force reinstallation of packages that are already up-to-date]'
    '-n[print the commands but do not run them]'
    "-p[number of parallel builds]:number"
    '-x[print the commands]'
    "-work[print temporary directory name and keep it]"
    "-gcflags[flags for 5g/6g/8g]:flags"
    "-ldflags[flags for 5l/6l/8l]:flags"
    "-gccgoflags[flags for gccgo]:flags"
  )
  __go_list() {
      local expl importpaths
      declare -a importpaths
      importpaths=($(go list ${words[$CURRENT]}... 2>/dev/null))
      _wanted importpaths expl 'import paths' compadd "$@" - "${importpaths[@]}"
  }
  case ${words[2]} in
  clean|doc)
      _arguments -s -w : '*:importpaths:__go_list'
      ;;
  fix|fmt|list|vet)
      _alternative ':importpaths:__go_list' ':files:_path_files -g "*.go"'
      ;;
  install)
      _arguments -s -w : ${build_flags[@]} \
        "-v[show package names]" \
  '*:importpaths:__go_list'
      ;;
  get)
      _arguments -s -w : \
        ${build_flags[@]}
      ;;
  build)
      _arguments -s -w : \
        ${build_flags[@]} \
        "-v[show package names]" \
        "-o[output file]:file:_files" \
        "*:args:{ _alternative ':importpaths:__go_list' ':files:_path_files -g \"*.go\"' }"
      ;;
  test)
      _arguments -s -w : \
        ${build_flags[@]} \
        "-c[do not run, compile the test binary]" \
        "-i[do not run, install dependencies]" \
        "-v[print test output]" \
        "-x[print the commands]" \
        "-short[use short mode]" \
        "-parallel[number of parallel tests]:number" \
        "-cpu[values of GOMAXPROCS to use]:number list" \
        "-run[run tests and examples matching regexp]:regexp" \
        "-bench[run benchmarks matching regexp]:regexp" \
        "-benchtime[run each benchmark during n seconds]:duration" \
        "-timeout[kill test after that duration]:duration" \
        "-cpuprofile[write CPU profile to file]:file:_files" \
        "-memprofile[write heap profile to file]:file:_files" \
        "-memprofilerate[set heap profiling rate]:number" \
        "*:args:{ _alternative ':importpaths:__go_list' ':files:_path_files -g \"*.go\"' }"
      ;;
  help)
      _values "${commands[@]}" \
        'gopath[GOPATH environment variable]' \
        'importpath[description of import paths]' \
        'remote[remote import path syntax]' \
        'testflag[description of testing flags]' \
        'testfunc[description of testing functions]'
      ;;
  run)
      _arguments -s -w : \
          ${build_flags[@]} \
          '*:file:_path_files -g "*.go"'
      ;;
  tool)
      if (( CURRENT == 3 )); then
          _values "go tool" $(go tool)
          return
      fi
      case ${words[3]} in
      [568]g)
          _arguments -s -w : \
              '-I[search for packages in DIR]:includes:_path_files -/' \
              '-L[show full path in file:line prints]' \
              '-S[print the assembly language]' \
              '-V[print the compiler version]' \
              '-e[no limit on number of errors printed]' \
              '-h[panic on an error]' \
              '-l[disable inlining]' \
              '-m[print optimization decisions]' \
              '-o[file specify output file]:file' \
              '-p[assumed import path for this code]:importpath' \
              '-u[disable package unsafe]' \
              "*:file:_files -g '*.go'"
          ;;
      [568]l)
          local O=${words[3]%l}
          _arguments -s -w : \
              '-o[file specify output file]:file' \
              '-L[search for packages in DIR]:includes:_path_files -/' \
              "*:file:_files -g '*.[ao$O]'"
          ;;
      dist)
          _values "dist tool" banner bootstrap clean env install version
          ;;
      *)
          # use files by default
          _files
          ;;
      esac
      ;;
  esac
}

compdef __go_tool_complete go

#############################################
#
# The chief cause of problems is solutions
#  - Sevareid 
#
#### RVM ####################################

PATH=$PATH:$HOME/.rvm/bin # Add RVM to PATH for scripting

if [ -f $HOME/.rvm/scripts/rvm ]; then
  source $HOME/.rvm/scripts/rvm
fi

# get the name of the ruby version
function rvm_prompt_info() {
  [ -f $HOME/.rvm/bin/rvm-prompt ] || return
  local rvm_prompt
  rvm_prompt=$($HOME/.rvm/bin/rvm-prompt ${ZSH_THEME_RVM_PROMPT_OPTIONS} 2>/dev/null)
  [[ "${rvm_prompt}x" == "x" ]] && return
  echo "${ZSH_THEME_RVM_PROMPT_PREFIX:=(}${rvm_prompt}${ZSH_THEME_RVM_PROMPT_SUFFIX:=)}"
}

### Closing words ###########################
#
# Isn't it a pleasure to study and practice
#  what you have learned?
#  - Confucious
#
#############################################

export PATH="$HOME/bin:/usr/local/bin:$HOME/go/bin:$PATH"
export GOPATH=$HOME/Development/go
export GOROOT=$HOME/go
