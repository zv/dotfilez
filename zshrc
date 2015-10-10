#!/usr/bin/env zsh
# This is zv's ZSHRC.
# meow nyan meow nyan meow
# 2009

#  Environment
# go
export GOROOT=$HOME/Development/go
export GOPATH=$HOME/Development/golang

path=(
    $GOROOT/bin
    $GOPATH/bin
    $path
    /home/zv/Downloads/firefox
    /usr/local/pgsql/bin
    /usr/local/heroku/bin
    $HOME/.bin
    /usr/local/{bin,sbin}
    /usr/local/lib/
    /usr/local/plan9/bin # Userspace From Plan9 binaries
    # $HOME/depot_tools
)

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

typeset -gU cdpath fpath path mailpath

setopt BRACE_CCL          # Allow brace character class list expansion.
setopt COMBINING_CHARS    # Combine zero-length punctuation characters
                          # with the base character.
setopt RC_QUOTES          # Allow 'Zephyr''s Rad Pad' instead of 'Zephyrs'\''s Rad Pad'.
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

# Disable correction.
for cmd (
    ack cd cp ebuild gcc gist grep heroku
    ln man mkdir mv mysql rm nmcli ip ag
    git npm ember dnf
) alias $cmd="nocorrect $cmd"

# Disable globbing.
for cmd (
    fc find ftp history locate rake rsync
    scp sftp git
) alias $cmd="noglob $cmd"

export EDITOR='vim'
export VISUAL='vim'
export PAGER='less'
export LANG='en_US.UTF-8'
export LESS='-F -g -i -M -R -S -w -X -z-4'
export LC_CTYPE=$LANG

if [[ -x google-chrome ]]; then
    export BROWSER==google-chrome
fi

export MANPAGER="/bin/sh -c \"sed -r 's/\x1B\[([0-9]{1,2}(;[0-9]{1,2})?)?[m|K]//g' | vim -c 'set ft=man ts=8 norelativenumber nomod nolist nonu noma showtabline=0' -\""


# Configure GPG
# GPG_TTY=$(tty)
# export GPG_TTY

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
if [[ $TERM == "dumb" ]]; then
    PS1='%(?..[%?])%!:%~%# '
    unsetopt zle
    unsetopt prompt_cr
    unsetopt prompt_subst
    # Ensure we don't set preexec && precmd
else
    export KEYTIMEOUT=1 # Immediately switch to vicmd/viinst
    # shows a slightly different prompt for vicmd vs other ZLE command modes to let
    # you know what you are dealing with.
    baseprompt='>>'
    zle_vim_prompt_notifier() {
        if [[ "$KEYMAP" == vicmd ]]; then
            print "%F{red}$baseprompt%f"
        else
            print "$baseprompt"
        fi
    }

    print_node_version() {
        if is_node_project; then
            print "$node_info[version]"
        fi
    }

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

    PROMPT='[%n@%m] %B%2~%b${vcs_info_msg_0_} $(zle_vim_prompt_notifier) '
    # RPROMPT='$(print_node_version)'
fi

# ls colors
autoload -U colors && colors;

# Stop the beep insanity
setopt no_beep

# Automatic CD on directory specification
setopt auto_cd

# CD into variable from var name
setopt cdablevarS

# Setup the prompt with pretty colors
setopt prompt_subst

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

bindkey "^[m" copy-prev-shell-word

# Edit command in an external editor.
bindkey -M vicmd "v" edit-command-line

autoload -U edit-command-line
zle -N edit-command-line
bindkey '\C-x\C-e' edit-command-line

# Undo/Redo
bindkey -M vicmd "u" undo
bindkey -M vicmd "\C-R" redo
bindkey -M vicmd "?" history-incremental-pattern-search-backward
bindkey -M vicmd "/" history-incremental-pattern-search-forward

# Expand history on space.
bindkey -M "viins" ' ' magic-space

autoload -U url-quote-magic
zle -N self-insert url-quote-magic

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
# Aliasing
#############################################

alias ...='cd ../..'     # Basic directory operations
alias -- -='cd -'        # This has always irritated me
# Define general aliases.
alias _='sudo'
alias e="emacsclient -t"
alias edit="emacs -nw"
alias mail='emacs -nw --eval="(gnus)"'
alias cp="${aliases[cp]:-cp} -i"
alias ln="${aliases[ln]:-ln} -i"
alias mkdir="${aliases[mkdir]:-mkdir} -p"
alias type='type -a'

alias info="info --vi-keys"
alias -g pr="print"
alias -g gr="grep"

# I occassionally want to convert man pages to pdfs, this is my hacky way to do it.
function man2pdf {
    man -Tps "$argv[1]" | ps2pdf - "$argv[1]"_man.pdf
}
# Wikipedia over DNS.
function firewalkipedia {
    dig +short txt "$argv[1]".wp.dg.cx
}

# do a quick check on the fastest DNS servers around
function DNScheck() {
    for x in "208.67.222.222" "208.67.220.220" "198.153.192.1" "198.153.194.1" "156.154.70.1" "156.154.71.1" "8.8.8.8" "8.8.4.4" "4.2.2.2"; do
    (echo -n "$x "; dig @"$x" "$*"|grep Query);
    done;
}


# Openssl
alias ssl="openssl"
# Tmux
alias ta="tmux attach-session -t"
alias tl="tmux list-sessions"
# Thesaurus
alias ths2='dict -d moby-thesaurus'
alias ths='aiksaurus'
# esxape ANSI sequences
alias stresc='sed -r "s/\x1B\[([0-9]{1,2}(;[0-9]{1,2})?)?[m|K]//g"'
# Switch Users, maining RTP.
alias sudovim='sudo -E vim' # -c "set runtimepath+=$HOME/.vim" -u $HOME/.vimrc'
alias screenlock='xscreensaver-command -lock'
alias gdb="gdb -q"
alias gdbtui="gdb -tui -ex 'set \$SHOW_CONTEXT = 0' -ex 'set \$USECOLOR = 0' -ex 'set extended-prompt (gdb) '"

# # mkdir & cd to it
function mcd() {
  mkdir -p "$1" && cd "$1";
}

# Displays user owned processes status.
function psu {
  ps -U "${1:-$USER}" -o 'pid,%cpu,%mem,command' "${(@)argv[2,-1]}"
}

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

# Sort connection state
sortcons() {
    netstat -nat |awk '{print $6}'|sort|uniq -c|sort -rn
}

# On the connected IP sorted by the number of connections
sortconip() {
    netstat -ntu | awk '{print $5}' | cut -d: -f1 | sort | uniq -c | sort -n
}

# top20 of Find the number of requests on 80 port
req20() {
    netstat -anlp|grep 80|grep tcp|awk '{print $5}'|awk -F: '{print $1}'|sort|uniq -c|sort -nr|head -n20
}

# top20 of Using tcpdump port 80 access to view
http20() {
    sudo tcpdump -i eth0 -tnn dst port 80 -c 1000 | awk -F"." '{print $1"."$2"."$3"."$4}' | sort | uniq -c | sort -nr |head -20
}

# Printing process according to the port number
port_pro() {
    netstat -ntlp | grep "$(retval $1)" | awk '{print $7}' | cut -d/ -f1
}

# gather external ip address
getextaddr() {
    curl http://ifconfig.me
}

alias ping='ping -c 5'
alias clr='clear;echo "Currently logged in on $(tty), as $USER in directory $PWD."'
alias psmem='ps -e -orss=,args= | sort -b -k1,1n'
alias psmem10='ps -e -orss=,args= | sort -b -k1,1n| head -10'
# get top process eating cpu if not work try excute : export LC_ALL='C'
alias pscpu='ps -e -o pcpu,cpu,nice,state,cputime,args|sort -k1 -nr'
alias pscpu10='ps -e -o pcpu,cpu,nice,state,cputime,args|sort -k1 -nr | head -10'
# top10 of the history
alias hist10='print -l ${(o)history%% *} | uniq -c | sort -nr | head -n 10'

## alias node='env NODE_NO_READLINE=1 rlwrap -S "node >>> " node'
typeset -a harmony_args
harmony_args=(--harmony
 --harmony_shipping
 --harmony_modules
 --harmony_arrays
 --harmony_array_includes
 --harmony_regexps
 --harmony_arrow_functions
 --harmony_proxies
 --harmony_sloppy
 --harmony_unicode
 --harmony_tostring
 --harmony_classes
 --harmony_object_literals
 --harmony_numeric_literals
 --harmony_strings
 --harmony_scoping
 --harmony_templates)

alias harmony="node ${(j: :)harmony_args}"


typeset -A jumplist
jumplist=(
    fed "$(dirname `readlink $HOME/.zshrc`)"
    fez "vim ~/.zshrc"
    fev "vim ~/.vimrc"
)

## xclipboard ##############################################
if (( $+commands[xclip] )); then
    alias xccopy='xclip -selection clipboard -in'
    alias xcpaste='xclip -selection clipboard -out'
elif (( $+commands[xsel] )); then
    alias xccopy='xsel --clipboard --input'
    alias xcpaste='xsel --clipboard --output'
fi

## ls ######################################################
alias ls='ls --group-directories-first --color=auto'
alias l='ls -1A'         # Lists in one column, hidden files.
alias ll='ls -lh'        # Lists human readable sizes.
alias lr='ll -R'         # Lists human readable sizes, recursively.
alias la='ll -A'         # Lists human readable sizes, hidden files.
alias lm='la | "$PAGER"' # Lists human readable sizes, hidden files through pager.
alias lx='ll -XB'        # Lists sorted by extension (GNU only).
alias lk='ll -Sr'        # Lists sorted by size, largest last.
alias lt='ll -tr'        # Lists sorted by date, most recent last.
alias lc='lt -c'         # Lists sorted by date, most recent last, shows change time.
alias lu='lt -u'         # Lists sorted by date, most recent last, shows access time.

############################################
#  Mix
############################################

alias mxc='mix compile'
alias mxd='mix deps'
alias mxg='mix deps.get'
alias mxdc='mix deps.compile'
alias mxdu='mix deps.update'
alias mxt='mix test'

############################################
# SystemD
############################################

typeset sc_user_commands sc_sudo_commands
sc_user_commands=(
  list-units is-active status show help list-unit-files
  is-enabled list-jobs show-environment)

sc_sudo_commands=(
  start stop reload restart try-restart isolate kill
  reset-failed enable disable reenable preset mask unmask
  link load cancel set-environment unset-environment)

for c in $sc_user_commands; do; alias sc-$c="systemctl $c"; done
for c in $sc_sudo_commands; do; alias sc-$c="sudo systemctl $c"; done

############################################
#  Cargo
############################################
local -A rust_commands

rust_commands=(
    ccd 'cargo build' # Compile the current project
    ccl 'cargo clean' # Remove the target directory
    ccr 'cargo run'   # Build and execute src/main.rs
    rc 'rustc'        # Rust compiler
)

for c in ${(@k)rust_commands}; do; alias $c="$rust_commands[$c]"; done

############################################
#  Package Management (dnf)
############################################
if (( $+commands[dnf] )); then
    alias dnfc='sudo dnf clean all'    # Cleans the cache.
    alias dnfh='dnf history'           # Displays history.
    alias dnfi='sudo dnf install'      # Installs package(s).
    alias dnfl='dnf list'              # Lists packages.
    alias dnfL='dnf list installed'    # Lists installed packages.
    alias dnfq='dnf info'              # Displays package information.
    alias dnfr='sudo dnf remove'       # Removes package(s).
    alias dnfs='dnf search'            # Searches for a package.
    alias dnfu='sudo dnf update'       # Updates packages.
    alias dnfU='sudo dnf upgrade'      # Upgrades packages.
fi

############################################
#  Package Management (npm)
############################################
typeset -A npm_commands

npm_commands=(
    nps 'npm search'  # Search NPM
    npi 'npm install' # Install a package
)

for c in ${(@k)npm_commands}; do; alias $c="$npm_commands[$c]"; done


############################################
#  Grep
############################################
alias grep="$aliases[grep] --color=auto"
export GREP_COLORS="37;45"

############################################
#  Amazon AWS
############################################
typeset -A aws_env_vars
aws_env_vars=(
    AWS_AUTO_SCALING_HOME "$HOME/.aws/aws-autoscaling"
    AWS_ELB_HOME          "$HOME/.aws/aws-elastic_load_balacing"
    AWS_CLOUDWATCH_HOME   "$HOME/.aws/aws-cloudwatch"
    EC2_HOME              "$HOME/.aws/ec2-api-tools"
    # Build up the various credential files & environment variables required by the
    # AWS toolchain.
    AWS_CONFIG_FILE       "$HOME/.aws/config"
    AWS_CREDENTIAL_FILE   "$HOME/.aws/credentials"
    # # For awscli
    AWS_DEFAULT_REGION    "us-west-1"
    EC2_URL               "https://ec2.us-west-1.amazonaws.com"
    #AWS_ACCESS_KEY        "$(cat $AWS_CONFIG_FILE | grep -A 2 'default'  | grep '^aws_access_key_id' | cut -d  -f2)"
    #AWS_SECRET_KEY        "$(cat $AWS_CONFIG_FILE  | grep -A 2 'default' | grep '^aws_secret_access_key' | cut -d  -f2)"
)

# `touch $AWS_CREDENTIAL_FILE`
# if [ -w $AWS_CREDENTIAL_FILE ]; then
#     printf "AWSAccessKeyId=%s\nAWSSecretKey=%s\n", $AWS_ACCESS_KEY, $AWS_SECRET_KEY > $AWS_CREDENTIAL_FILE;
# fi

# # Add all the AWS binaries to our path
# for p ($AWS_CLOUDWATCH_HOME $AWS_ELB_HOME $AWS_AUTO_SCALING_HOME $EC2_HOME) path+=$p/bin

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

zmodload zsh/complist
zmodload zsh/mathfunc
zmodload zsh/system

setopt COMPLETE_IN_WORD    # Complete from both ends of a word.
setopt ALWAYS_TO_END       # Move cursor to the end of a completed word.
setopt PATH_DIRS           # Perform path search even on command names with slashes.
setopt AUTO_MENU           # Show completion menu on a succesive tab press.
setopt AUTO_LIST           # Automatically list choices on ambiguous completion.
setopt AUTO_PARAM_SLASH    # If completed parameter is a directory, add a trailing slash.
unsetopt MENU_COMPLETE     # Do not autoselect the first completion entry.
unsetopt FLOW_CONTROL      # Disable start/stop characters in shell editor.

zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}

# Use caching to make completion for cammands such as dpkg and apt usable.
zstyle ':completion::complete:*' use-cache on
zstyle ':completion::complete:*' cache-path "${ZDOTDIR:-$HOME}/.zcompcache"

## case-insensitive (all),partial-word and then substring completion
zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z}' 'r:|[._-]=* r:|=*' 'l:|=* r:|=*'
unsetopt CASE_GLOB

zstyle ':completion:*' list-colors ''

zstyle ':completion:*:*:*:*:*' menu select

zstyle ':completion:*:*:*:*:processes' command 'ps -u $USER -o pid,user,command -w'
zstyle ':completion:*:*:kill:*:processes' list-colors '=(#b) #([0-9]#) ([0-9a-z-]#)*=01;36=0=01'
zstyle ':completion:*:*:kill:*' menu yes select
zstyle ':completion:*:*:kill:*' force-list always
zstyle ':completion:*:*:kill:*' insert-ids single


# disable named-directories autocompletion
zstyle ':completion:*:cd:*' tag-order local-directories directory-stack path-directories
cdpath=(.)

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
zstyle ':completion:*:*:*:users' ignored-patterns \
  adm amanda apache avahi beaglidx bin cacti canna clamav daemon \
  dbus distcache dovecot fax ftp games gdm gkrellmd gopher \
  hacluster haldaemon halt hsqldb ident junkbust ldap lp mail \
  mailman mailnull mldonkey mysql nagios \
  named netdump news nfsnobody nobody nscd ntp nut nx openvpn \
  operator pcap postfix postgres privoxy pulse pvm quagga radvd \
  rpc rpcuser rpm shutdown squid sshd sync uucp vcsa xfs '_*'

# ... unless we really want to.
zstyle '*' single-ignored show

# Ignore multiple entries.
zstyle ':completion:*:(rm|kill|diff):*' ignore-line other
zstyle ':completion:*:rm:*' file-patterns '*:all-files'

# Kill
zstyle ':completion:*:*:*:*:processes' command 'ps -u $USER -o pid,user,command -w'
zstyle ':completion:*:*:kill:*:processes' list-colors '=(#b) #([0-9]#) ([0-9a-z-]#)*=01;36=0=01'
zstyle ':completion:*:*:kill:*' menu yes select
zstyle ':completion:*:*:kill:*' force-list always
zstyle ':completion:*:*:kill:*' insert-ids single

# Man
zstyle ':completion:*:manuals' separate-sections true
zstyle ':completion:*:manuals.(^1*)' insert-sections true

# SSH/SCP/RSYNC
zstyle ':completion:*:(scp|rsync):*' tag-order 'hosts:-host:host hosts:-domain:domain hosts:-ipaddr:ip\ address *'
zstyle ':completion:*:(scp|rsync):*' group-order users files all-files hosts-domain hosts-host hosts-ipaddr
zstyle ':completion:*:ssh:*' tag-order 'hosts:-host:host hosts:-domain:domain hosts:-ipaddr:ip\ address *'
zstyle ':completion:*:ssh:*' group-order users hosts-domain hosts-host users hosts-ipaddr
zstyle ':completion:*:(ssh|scp|rsync):*:hosts-host' ignored-patterns '*(.|:)*' loopback ip6-loopback localhost ip6-localhost broadcasthost
zstyle ':completion:*:(ssh|scp|rsync):*:hosts-domain' ignored-patterns '<->.<->.<->.<->' '^[-[:alnum:]]##(.[-[:alnum:]]##)##' '*@*'
zstyle ':completion:*:(ssh|scp|rsync):*:hosts-ipaddr' ignored-patterns '^(<->.<->.<->.<->|(|::)([[:xdigit:].]##:(#c,2))##(|%*))' '127.0.0.<->' '255.255.255.255' '::1' 'fe80::*'

## NPM
# eval "$(npm completion 2>/dev/null)"

#############################################
# Directories
#############################################

setopt auto_name_dirs
setopt auto_pushd
setopt pushd_ignore_dups
setopt pushd_silent
setopt multios
setopt extended_glob

alias d='dirs -v'
for index ({1..9}) alias "$index"="cd +${index}"; unset index

local gnu_global_dir=$HOME/.gtags/
if [[ ! -d $gnu_global_dir ]]; then
    mkdir $gnu_global_dir
fi

export GTAGSLIBPATH=$gnu_global_dir

#############################################
# History
#############################################
HISTFILE=$HOME/.zsh_history

# Lines to store
HISTSIZE=10000
SAVEHIST=10000

setopt append_history
setopt extended_history
setopt hist_expire_dups_first
setopt hist_ignore_dups # ignore duplication command history list
setopt hist_verify
setopt inc_append_history
setopt share_history # share command history data
setopt hist_ignore_all_dups      # delete an old recorded event if a new event is a duplicate.
setopt hist_find_no_dups         # do not display a previously found event.
setopt hist_ignore_space         # do not record an event starting with a space.
setopt hist_save_no_dups         # do not write a duplicate event to the history file.

# Lists the ten most used commands.
alias history-stat="history 0 | awk '{print \$2}' | sort | uniq -c | sort -n -r | head"

export NVM_DIR="/home/zv/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && . "$NVM_DIR/nvm.sh"  # This loads nvm

alias serialt=screen /dev/ttyS0 115200,cs8,-ixon,-ixon,istrip
function whatserialt {
    echo "screen /dev/ttyS0 115200,cs8,-ixon,-ixon,istrip"
}


alias gpg=gpg2

source $HOME/.gnupg/gpg-agent-wrapper
