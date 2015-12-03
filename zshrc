#!/usr/bin/env zsh
# This is zv's ZSHRC.
# meow nyan meow nyan meow
# 2009

# Environment
# go
export GOROOT=$HOME/Development/go
export GOPATH=$HOME/Development/golang

# Add some items to our path
local -aU path_suffix path_prefix

path_prefix=(
    $HOME/.bin
    /usr/local/{bin,sbin}
    /usr/local/lib)

path_suffix=(
    {$GOROOT,$GOPATH}/bin
    /usr/local/{pgsql,heroku,plan9}/bin
    $HOME/depot_tools
)

for np ($path_suffix) if [[ ! -d $np ]] path_suffix=(${path_suffix#$np})
for np ($path_prefix) if [[ ! -d $np ]] path_prefix=(${path_prefix#$np})

# Tack the `real` path all together now
path=($path_prefix $path $path_suffix)

typeset -gU cdpath fpath path mailpath

# Load our completion functions
fpath=(
    $HOME/.zsh/zsh-completions/src
    $HOME/.zsh/completion
    $HOME/.zsh/completion/rust-completion
    $HOME/.zsh/functions
    $fpath
)

# Run our external modules
for fn (~/.zsh/modules/*.zsh) source $fn

setopt BRACE_CCL          # Allow brace character class list expansion.
setopt COMBINING_CHARS    # Combine zero-length punctuation characters
                          # with the base character.
# setopt RC_QUOTES          # Allow 'Zephyr''s Rad Pad' instead of 'Zephyrs'\''s Rad Pad'.
unsetopt MAIL_WARNING     # Don't print a warning message if a mail file has been accessed.
setopt LONG_LIST_JOBS     # List jobs in the long format by default.
setopt AUTO_RESUME        # Attempt to resume existing job before creating a new process.
setopt NOTIFY             # Report status of background jobs immediately.
unsetopt BG_NICE          # Don't run all background jobs at a lower priority.
unsetopt HUP              # Don't kill jobs on shell exit.
unsetopt CHECK_JOBS       # Don't report on jobs when shell exit.

export DISABLE_AUTO_TITLE="true"
setopt EXTENDED_GLOB
setopt BARE_GLOB_QUAL

setopt correct_all # Correct commands

export EDITOR='vim'
export VISUAL='vim'
export PAGER='less'
export LANG='en_US.UTF-8'
export LESS='-F -g -i -M -R -S -w -X -z-4'
export LC_CTYPE=$LANG

if [[ -x google-chrome ]]; then
    export BROWSER=google-chrome
fi

export MANPAGER="/bin/sh -c \"sed -r 's/\x1B\[([0-9]{1,2}(;[0-9]{1,2})?)?[m|K]//g' | vim -c 'set ft=man ts=8 norelativenumber nomod nolist nonu noma showtabline=0' -\""

# build stuff
export CXX=g++
export CC=gcc
export AR=ar

############################################
#  Theme
#############################################
function spectrum_ls() {
    string=${1-Test}
    if [[ $1 == "block" ]]
    then
        string="███████████████████████████████"
    fi
    for code in {000..255}; do
        print -P -- "$code: %F{$code}${string}%f"
    done
}


## Determines if our path has a `node_modules` directory in it's parent
## directory tree
function is_node_project {
    local checkpath=${PWD:a}
    until [[ $checkpath == '/' ]] {
              if [[ -d $checkpath/node_modules ]] {
                     return 0
                 }
                 checkpath=${checkpath:a:h}
          }
    false
}

# If we're in a dumb terminal then dont play fancy pants with our prompt
local baseprompt='>>'
if [[ $TERM == 'dumb' ]]; then
    PROMPT='%2~ $baseprompt '
    unsetopt zle
    unsetopt prompt_cr
    unsetopt prompt_subst
    # Ensure we don't set preexec && precmd
elif [[ $TERM == 'eterm-color' ]]; then
    unsetopt zle
    unsetopt prompt_cr
    unsetopt prompt_subst
    PROMPT='[%y] %2~ $baseprompt '
else
    export KEYTIMEOUT=1 # Immediately switch to vicmd/viinst
    # shows a slightly different prompt for vicmd vs other ZLE command modes to let
    # you know what you are dealing with.

    autoload -Uz vcs_info
    base_vcs_style='%c%b%u%f'
    zstyle ':vcs_info:*' enable git hg # svn cvs fossil bzr hg-git
    zstyle ':vcs_info:*' check-for-changes true
    zstyle ':vcs_info:*' actionformats "- [$base_vcs_style|%F{green}%a%f] "
    zstyle ':vcs_info:*' stagedstr '%F{028}'
    zstyle ':vcs_info:*' unstagedstr '*'
    zstyle ':vcs_info:*' formats " [$base_vcs_style]"
    zstyle ':vcs_info:git:*' branchformat '%b%F{1}:%F{3}%r'
    precmd () { vcs_info }

    PROMPT='[%n@%m] %B%2~%b${vcs_info_msg_0_} `[ "$KEYMAP" == vicmd ] && print "%F{red}$baseprompt%f" || print "$baseprompt"` '
fi

# ls colors
autoload -U colors && colors;

setopt no_beep      # Stop the beep insanity
setopt auto_cd      # Automatic CD on directory specification
setopt cdablevarS   # CD into variable from var name
setopt prompt_subst # Setup the prompt with pretty colors

# 10 years I've been listenining to this list prompt and today I am fucking done!
export LISTPROMPT=''

# The crazier the better!
if [[ -x =dircolors && -e ~/.zsh/LS_COLORS ]]; then
    eval `dircolors --sh ~/.zsh/LS_COLORS`
fi

#############################################
#  Vim & ZSH Line Editor
############################################
WORDCHARS='*?_-.[]~&;!#$%^(){}<>'

function zle-line-init zle-keymap-select {
  zle reset-prompt
}

# ZSH line editor stuff needed for bindkey to work
zle -N zle-line-init
zle -N zle-keymap-select

bindkey -v


bindkey -M viins "^P" up-line-or-search
bindkey -M viins "^N" down-line-or-search

# Some convienent alt bindings
bindkey -M viins "\eh" vi-backward-blank-word
bindkey -M viins "\el" vi-forward-blank-word
bindkey -M viins "\ed" delete-word
bindkey -M viins "^F" vi-cmd-mode

# Edit command in an external editor.
autoload -U edit-command-line
zle -N edit-command-line
bindkey -M vicmd "v" edit-command-line

# History
bindkey -M vicmd "?" history-incremental-pattern-search-backward
bindkey -M vicmd "/" history-incremental-pattern-search-forward
# *-or-search searches for existing history items currently in the command
# *-string, while '*-or-history' ignores this.
bindkey -M vicmd "k" up-line-or-search # up-line-or-history
bindkey -M vicmd "j" down-line-or-search # down-line-or-history

bindkey '\ew' kill-region
bindkey -s '\eu' "..\n"
bindkey -s '\es' "git status\n"
bindkey '^r' history-incremental-search-backward
bindkey "^[[5~" up-line-or-history
bindkey "^[[6~" down-line-or-history

# make search up and down work, so partially type and hit up/down to find relevant stuff
bindkey '^[[A' up-line-or-search
bindkey '^[[B' down-line-or-search
bindkey ' ' magic-space    # also do history expansion on space
bindkey '^[[Z' reverse-menu-complete

# Make the delete key work instead of outputting a ~
bindkey '^?' backward-delete-char
bindkey "^[[3~" delete-char
bindkey "^[3;5~" delete-char
bindkey "\e[3~" delete-char

bindkey ' ' magic-space # [Space] - do history expansion

# these are supposed to map Ctrl-Left/Right arrow but they do nothing
#bindkey "^[[1;5C" forward-word
#bindkey "^[[1;5D" backward-word
#bindkey -M viins "\ek" vi-cmd-mode # not useful
#bindkey "^[m" copy-prev-shell-word # doesn't work
#useless
# autoload -U url-quote-magic
# zle -N self-insert url-quote-magic

#############################################
# Aliasing
#############################################
# Disable correction.
for cmd (
        ack cd cp ebuild gcc gist grep heroku
        ln man mkdir mv mysql rm nmcli ip ag
        git npm ember dnf jekyll
    ) alias $cmd="nocorrect $cmd"

    # Disable globbing.
    for cmd (
            fc find ftp history locate rake rsync
            scp sftp git
        ) alias $cmd="noglob $cmd"

# Define general aliases.
alias _='sudo'
alias e="emacsclient -t"
alias edit="emacs -nw"
alias info="info --vi-keys"
alias -g pr="print"
alias jkl="jekyll serve -D"

# Tmux
alias ta="tmux attach-session -t"
alias tl="tmux list-sessions"

alias zthree="z3 -in"
alias gpg=gpg2
# Bullshit workaround to unlock my yubikey
alias gpginit="date | gpg -a --encrypt -r zv@nxvr.org | gpg --decrypt"

# # mkdir & cd to it
function mcd() { mkdir -p "$1" && cd "$1"; }

# Displays user owned processes status.
function dut {
  (( $# == 0 )) && set -- *

  if grep -q -i 'GNU' < <(du --version 2>&1); then
    du -khsc "$@" | sort -h -r
  else
    local line size name
    local -a record

    while IFS=$'\n' read line; do
      record=(${(z)line})
      size="$(($record[1] / 1024.0))"
      name="$record[2,-1]"
      printf "%9.1LfM    %s\n" "$size" "$name"
    done < <(du -kcs "$@") | sort -n -r
  fi
}

alias clr='clear;echo "Currently logged in on $(tty), as $USER in directory $PWD."'

function lenovo_oxide {
    # Grep through for the actual ID. Hack.
    local prop_id
    prop_id=$(xinput list --short | \
        grep 'Lenovo.*pointer' | \
        awk 'BEGIN { FS="[\t]+" } ; {print $2}' | \
        sed 's/id\=//')
   xinput --set-prop $prop_id 140 $1 0 0 0 $1 0 0 0 0.8
}

alias ptrsp='lenovo_oxide'

## ls ######################################################
alias ls='ls --group-directories-first --color=auto'
alias l='ls -1A'         # Lists in one column, hidden files.
alias ll='ls -lh'        # Lists human readable sizes.
alias lr='ll -R'         # Lists human readable sizes, recursively.
alias la='ll -A'         # Lists human readable sizes, hidden files.
alias lm='la | "$PAGER"' # Lists human readable sizes, hidden files through pager.
alias lx='ll -XB'        # Lists sorted by extension (GNU only).
alias lk='ll -Sr'        # Lists sorted by size, largest last.
alias lc='lt -c'         # Lists sorted by date, most recent last, shows change time.
alias lu='lt -u'         # Lists sorted by date, most recent last, shows access time.

############################################
#  Package Management (dnf)
############################################
if (( $+commands[dnf] )); then
    alias dnfs="dnf search"                       # search package
    alias dnfp="dnf info"                         # show package info
    alias dnfl="dnf list"                         # list packages
    alias dnfgl="dnf grouplist"                   # list package groups
    alias dnfli="dnf list installed"              # print all installed packages
    alias dnfmc="dnf makecache"                   # rebuilds the dnf package list

    alias dnfu="sudo dnf upgrade"                 # upgrade packages
    alias dnfi="sudo dnf install"                 # install package
    alias dnfgi="sudo dnf groupinstall"           # install package group
    alias dnfr="sudo dnf remove"                  # remove package
    alias dnfgr="sudo dnf groupremove"            # remove pagage group
    alias dnfrl="sudo dnf remove --remove-leaves" # remove package and leaves
    alias dnfc="sudo dnf clean all"               # clean cachefi
fi

############################################
#  Grep
############################################
alias grep="$aliases[grep] --color=auto"
export GREP_COLORS="37;45"

############################################
#  Git
############################################

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
alias grb='git reset HEAD'
alias grh='git reset HEAD'
alias grhh='git reset HEAD --hard'

alias git-userlinecount="git ls-files | xargs -n1 -d'\n' -i git blame {} | perl -n -e '/\s\((.*?)\s[0-9]{4}/ && print \"$1\n\"' | sort -f | uniq -c -w3 | sort -r"

# Lists all files that have been ignored with git update-index
alias git-ignorelog='git ls-files -v | grep "^h"'

# Displays some git related author statistics
function authorstats {
    gitauth=$1;
    git log --author=$gitauth --pretty=tformat: --numstat | gawk '{ add += $1 ; subs += $2 ; loc += $1 - $2 } END { printf "added lines: %s removed lines : %s total lines: %s",add,subs,loc }' -
    echo ''
}

############################################
#  Modules & Completions
############################################
autoload -U compinit && compinit
#autoload -U bashcompinit && bashcompinit
#source /usr/share/bash-completion/completions/nmcli
# zmodload zsh/complist

if [[ "$TERM" != 'dumb' ]]; then
    setopt COMPLETE_IN_WORD    # Complete from both ends of a word.
    setopt ALWAYS_TO_END       # Move cursor to the end of a completed word.
    setopt PATH_DIRS           # Perform path search even on command names with slashes.
    setopt AUTO_MENU           # Show completion menu on a succesive tab press.
    setopt AUTO_LIST           # Automatically list choices on ambiguous completion.
    setopt AUTO_PARAM_SLASH    # If completed parameter is a directory, add a trailing slash.
    unsetopt MENU_COMPLETE     # Do not autoselect the first completion entry.
    unsetopt FLOW_CONTROL      # Disable start/stop characters in shell editor.

    # Use caching to make completion for commands such as dpkg and apt usable.
    zstyle ':completion::complete:*' use-cache on
    zstyle ':completion::complete:*' cache-path "${ZDOTDIR:-$HOME}/.zcompcache"

    # Case-insensitive (all), partial-word, and then substring completion.
    zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z}' 'r:|[._-]=* r:|=*' 'l:|=* r:|=*'
    unsetopt CASE_GLOB

    # Group matches and describe.
    zstyle ':completion:*:*:*:*:*' menu select
    zstyle ':completion:*:matches' group 'yes'
    zstyle ':completion:*:options' description 'yes'
    zstyle ':completion:*:options' auto-description '%d'
    zstyle ':completion:*:corrections' format ' %F{green}-- %d (errors: %e) --%f'
    zstyle ':completion:*:descriptions' format ' %F{yellow}-- %d --%f'
    zstyle ':completion:*:messages' format ' %F{purple} -- %d --%f'
    zstyle ':completion:*:warnings' format ' %F{red}-- no matches found --%f'
    zstyle ':completion:*:default' list-prompt '%S%M matches%s'
    zstyle ':completion:*' format ' %F{yellow}-- %d --%f'
    zstyle ':completion:*' group-name ''
    zstyle ':completion:*' verbose yes

    # Fuzzy match mistyped completions.
    zstyle ':completion:*' completer _complete _match _approximate
    zstyle ':completion:*:match:*' original only
    zstyle ':completion:*:approximate:*' max-errors 1 numeric

    # Increase the number of errors based on the length of the typed word.
    zstyle -e ':completion:*:approximate:*' max-errors 'reply=($((($#PREFIX+$#SUFFIX)/3))numeric)'

    # Don't complete unavailable commands.
    zstyle ':completion:*:functions' ignored-patterns '(_*|pre(cmd|exec))'

    # Array completion element sorting.
    zstyle ':completion:*:*:-subscript-:*' tag-order indexes parameters

    # Directories
    zstyle ':completion:*:default' list-colors ${(s.:.)LS_COLORS}
    zstyle ':completion:*:*:cd:*' tag-order local-directories directory-stack path-directories
    zstyle ':completion:*:*:cd:*:directory-stack' menu yes select
    zstyle ':completion:*:-tilde-:*' group-order 'named-directories' 'path-directories' 'users' 'expand'
    zstyle ':completion:*' squeeze-slashes true

    # History
    zstyle ':completion:*:history-words' stop yes
    zstyle ':completion:*:history-words' remove-all-dups yes
    zstyle ':completion:*:history-words' list false
    zstyle ':completion:*:history-words' menu yes

    # Environmental Variables
    zstyle ':completion::*:(-command-|export):*' fake-parameters ${${${_comps[(I)-value-*]#*,}%%,*}:#-*-}

    # Populate hostname completion.
    zstyle -e ':completion:*:hosts' hosts 'reply=(
  ${=${=${=${${(f)"$(cat {/etc/ssh_,~/.ssh/known_}hosts(|2)(N) 2>/dev/null)"}%%[#| ]*}//\]:[0-9]*/ }//,/ }//\[/ }
  ${=${(f)"$(cat /etc/hosts(|)(N) <<(ypcat hosts 2>/dev/null))"}%%\#*}
  ${=${${${${(@M)${(f)"$(cat ~/.ssh/config 2>/dev/null)"}:#Host *}#Host }:#*\**}:#*\?*}}
)'

    # Don't complete uninteresting users...
    zstyle ':completion:*:*:*:users' ignored-patterns                     \
           adm amanda apache avahi beaglidx bin cacti canna clamav daemon \
           dbus distcache dovecot fax ftp games gdm gkrellmd gopher       \
           hacluster haldaemon halt hsqldb ident junkbust ldap lp mail    \
           mailman mailnull mldonkey mysql nagios                         \
           named netdump news nfsnobody nobody nscd ntp nut nx openvpn    \
           operator pcap postfix postgres privoxy pulse pvm quagga radvd  \
           rpc rpcuser rpm shutdown squid sshd sync uucp vcsa xfs '_*'

    # ... unless we really want to.
    zstyle '*' single-ignored show

    # Ignore multiple entries.
    zstyle ':completion:*:(rm|kill|diff):*' ignore-line other
    zstyle ':completion:*:rm:*' file-patterns '*:all-files'

    # Kill
    zstyle ':completion:*:*:*:*:processes' command 'ps -u $LOGNAME -o pid,user,command -w'
    zstyle ':completion:*:*:kill:*:processes' list-colors '=(#b) #([0-9]#) ([0-9a-z-]#)*=01;36=0=01'
    zstyle ':completion:*:*:kill:*' menu yes select
    zstyle ':completion:*:*:kill:*' force-list always
    zstyle ':completion:*:*:kill:*' insert-ids single

    # Man
    zstyle ':completion:*:manuals' separate-sections true
    zstyle ':completion:*:manuals.(^1*)' insert-sections true

    # Media Players
    zstyle ':completion:*:*:mpg123:*' file-patterns '*.(mp3|MP3):mp3\ files *(-/):directories'
    zstyle ':completion:*:*:mpg321:*' file-patterns '*.(mp3|MP3):mp3\ files *(-/):directories'
    zstyle ':completion:*:*:ogg123:*' file-patterns '*.(ogg|OGG|flac):ogg\ files *(-/):directories'
    zstyle ':completion:*:*:mocp:*' file-patterns '*.(wav|WAV|mp3|MP3|ogg|OGG|flac):ogg\ files *(-/):directories'

    # Mutt
    if [[ -s "$HOME/.mutt/aliases" ]]; then
        zstyle ':completion:*:*:mutt:*' menu yes select
        zstyle ':completion:*:mutt:*' users ${${${(f)"$(<"$HOME/.mutt/aliases")"}#alias[[:space:]]}%%[[:space:]]*}
    fi

    # SSH/SCP/RSYNC
    zstyle ':completion:*:(scp|rsync):*' tag-order 'hosts:-host:host hosts:-domain:domain hosts:-ipaddr:ip\ address *'
    zstyle ':completion:*:(scp|rsync):*' group-order users files all-files hosts-domain hosts-host hosts-ipaddr
    zstyle ':completion:*:ssh:*' tag-order 'hosts:-host:host hosts:-domain:domain hosts:-ipaddr:ip\ address *'
    zstyle ':completion:*:ssh:*' group-order users hosts-domain hosts-host users hosts-ipaddr
    zstyle ':completion:*:(ssh|scp|rsync):*:hosts-host' ignored-patterns '*(.|:)*' loopback ip6-loopback localhost ip6-localhost broadcasthost
    zstyle ':completion:*:(ssh|scp|rsync):*:hosts-domain' ignored-patterns '<->.<->.<->.<->' '^[-[:alnum:]]##(.[-[:alnum:]]##)##' '*@*'
    zstyle ':completion:*:(ssh|scp|rsync):*:hosts-ipaddr' ignored-patterns '^(<->.<->.<->.<->|(|::)([[:xdigit:].]##:(#c,2))##(|%*))' '127.0.0.<->' '255.255.255.255' '::1' 'fe80::*'
fi

#############################################
# Directories
#############################################
setopt AUTO_CD              # Auto changes to a directory without typing cd.
setopt AUTO_PUSHD           # Push the old directory onto the stack on cd.
setopt PUSHD_IGNORE_DUPS    # Do not store duplicates in the stack.
setopt PUSHD_SILENT         # Do not print the directory stack after pushd or popd.
setopt PUSHD_TO_HOME        # Push to home directory when no argument is given.
setopt CDABLE_VARS          # Change directory to a path stored in a variable.
setopt AUTO_NAME_DIRS       # Auto add variable-stored paths to ~ list.
setopt MULTIOS              # Write to multiple descriptors.
setopt EXTENDED_GLOB        # Use extended globbing syntax.
unsetopt CLOBBER            # Do not overwrite existing files with > and >>. Use >! and >>! to bypass.

#############################################
# History
#############################################
HISTFILE=$HOME/.zsh_history

# Lines to store
HISTSIZE=$((2**14 - 1))
SAVEHIST=$((2**14 - 1))

setopt BANG_HIST                 # Treat the '!' character specially during expansion.
setopt EXTENDED_HISTORY          # Write the history file in the ':start:elapsed;command' format.
setopt INC_APPEND_HISTORY        # Write to the history file immediately, not when the shell exits.
setopt SHARE_HISTORY             # Share history between all sessions.
setopt HIST_EXPIRE_DUPS_FIRST    # Expire a duplicate event first when trimming history.
setopt HIST_IGNORE_DUPS          # Do not record an event that was just recorded again.
setopt HIST_IGNORE_ALL_DUPS      # Delete an old recorded event if a new event is a duplicate.
setopt HIST_FIND_NO_DUPS         # Do not display a previously found event.
setopt HIST_IGNORE_SPACE         # Do not record an event starting with a space.
setopt HIST_SAVE_NO_DUPS         # Do not write a duplicate event to the history file.
setopt HIST_VERIFY               # Do not execute immediately upon history expansion.
setopt HIST_BEEP                 # Beep when accessing non-existent history.

# Lists the ten most used commands.
alias history-stat="history 0 | awk '{print \$2}' | sort | uniq -c | sort -n -r | head"

#############################################
# Other
#############################################
export NVM_DIR="/home/zv/.nvm"
source $HOME/.gnupg/gpg-agent-wrapper
export RUST_SRC_PATH=$HOME/Development/rust/src
export GTAGSLIBPATH=$gnu_global_dir

[ -x "$NVM_DIR/nvm.sh" ] && . "$NVM_DIR/nvm.sh"  # This loads nvm
