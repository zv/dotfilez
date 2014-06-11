#!/usr/bin/env zsh
# This is zv's ZSHRC.
# haq the plan8
# meow nyan meow nyan meow
# 2013

############################################
#  Environment
############################################

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

#####################################################################
#  Spectrum (A script to make using 256 colors in zsh less painful.)
#  P.C. Shyamshankar <sykora@lucentbeing.com>
#  http://github.com/sykora/etc/blob/master/zsh/functions/spectrum/
#####################################################################

typeset -gA FX FG BG

FX=(
  none                      "\e[00m"
  normal                    "\e[22m"
  bold                      "\e[01m"    no-bold                      "\e[22m"
  faint                     "\e[02m"    no-faint                     "\e[22m"
  standout                  "\e[03m"    no-standout                  "\e[23m"
  underline                 "\e[04m"    no-underline                 "\e[24m"
  blink                     "\e[05m"    no-blink                     "\e[25m"
  fast-blink                "\e[06m"    no-fast-blink                "\e[25m"
  reverse                   "\e[07m"    no-reverse                   "\e[27m"
  conceal                   "\e[08m"    no-conceal                   "\e[28m"
  strikethrough             "\e[09m"    no-strikethrough             "\e[29m"
  gothic                    "\e[20m"    no-gothic                    "\e[22m"
  double-underline          "\e[21m"    no-double-underline          "\e[22m"
  proportional              "\e[26m"    no-proportional              "\e[50m"
  overline                  "\e[53m"    no-overline                  "\e[55m"

                                        no-border                    "\e[54m"
  border-rectangle          "\e[51m"    no-border-rectangle          "\e[54m"
  border-circle             "\e[52m"    no-border-circle             "\e[54m"

                                        no-ideogram-marking          "\e[65m"
  underline-or-right        "\e[60m"    no-underline-or-right        "\e[65m"
  double-underline-or-right "\e[61m"    no-double-underline-or-right "\e[65m"
  overline-or-left          "\e[62m"    no-overline-or-left          "\e[65m"
  double-overline-or-left   "\e[63m"    no-double-overline-or-left   "\e[65m"
  stress                    "\e[64m"    no-stress                    "\e[65m"

                                        font-default                 "\e[10m"
  font-first                "\e[11m"    no-font-first                "\e[10m"
  font-second               "\e[12m"    no-font-second               "\e[10m"
  font-third                "\e[13m"    no-font-third                "\e[10m"
  font-fourth               "\e[14m"    no-font-fourth               "\e[10m"
  font-fifth                "\e[15m"    no-font-fifth                "\e[10m"
  font-sixth                "\e[16m"    no-font-sixth                "\e[10m"
  font-seventh              "\e[17m"    no-font-seventh              "\e[10m"
  font-eigth                "\e[18m"    no-font-eigth                "\e[10m"
  font-ninth                "\e[19m"    no-font-ninth                "\e[10m"
)

FG[none]="$FX[none]"
BG[none]="$FX[none]"
colors=(black red green yellow blue magenta cyan white)
for color in {0..255}; do
    if (( $color >= 0 )) && (( $color < $#colors )); then
        index=$(( $color + 1 ))
        FG[$colors[$index]]="\e[38;5;${color}m"
        BG[$colors[$index]]="\e[48;5;${color}m"
    fi

    FG[$color]="\e[38;5;${color}m"
    BG[$color]="\e[48;5;${color}m"
done
unset color{s,} index

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
    if [[ "$KEYMAP" == vicmd ]]; then
        print "%F{red}>>%f"
    else
        print ">>"
    fi
}

PROMPT='[%n@%m]%2~ $(prompt_fix_for_git) $(zle_vim_prompt_notifier) '

# I am not predisposed to using multikey commands, as a consequence I would
# prefer it if vicmd immediately changed when I hit <ESC> <C-]> etc.
# If `bindkey -L` shows that you do not share this predisposition, removing
# this will simply just slow down mode switching marginally.
export KEYTIMEOUT=1

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

# bindkey -e is emacs mode
bindkey -v

# Edit command in an external editor.
bindkey -M vicmd "v" edit-command-line

autoload -U edit-command-line
zle -N edit-command-line
bindkey '\C-x\C-e' edit-command-line

# Edit command in an external editor.
bindkey -M vicmd "v" edit-command-line

# Undo/Redo
bindkey -M vicmd "u" undo
bindkey -M vicmd "\C-R" redo
bindkey -M vicmd "?" history-incremental-pattern-search-backward
bindkey -M vicmd "/" history-incremental-pattern-search-forward

# Expand history on space.
bindkey -M "viins" ' ' magic-space

#############################################
# Aliasing
#############################################

alias pu='pushd'         # Push on directory stack
alias po='popd'          # Pop dstack
alias ...='cd ../..'     # Basic directory operations
alias -- -='cd -'        # This has always irritated me
alias _='sudo'           # Super user
alias _bex='bundle exec' # Bundle Exec
alias history='fc -l 1'  # Show history
# LS
alias l='ls -1A'         # Lists in one column, hidden files.
alias ll='ls -lh'        # Lists human readable sizes.
alias lr='ll -R'         # Lists human readable sizes, recursively.
alias la='ll -A'         # Lists human readable sizes, hidden files.
alias lm='la | "$PAGER"' # Lists human readable sizes, hidden files through pager.
alias lx='ll -XB'        # Lists sorted by extension (GNU only).
alias lk='ll -Sr'        # Lists sorted by size, largest last.
alias lc='lt -c'         # Lists sorted by date, most recent last, shows change time.
alias lu='lt -u'         # Lists sorted by date, most recent last, shows access time.
alias sl='ls'            # I often screw this up.

alias edittmp='vim $(mktemp)' # I often edit temporary files
# I occassionally want to convert man pages to pdfs, this is my hacky way to do
# it.
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

# Network Manager aliases
alias wifi="nmcli -f SSID,BSSID,CHAN,SIGNAL,BARS,ACTIVE dev wifi"
alias connect="nmcli dev wifi connect"

#############################################
# Convienence Funcs
#############################################

# Tmux
alias ta="tmux attach-session -t"
alias tl="tmux list-sessions"

# esxape ANSI sequences
alias stresc='sed -r "s/\x1B\[([0-9]{1,2}(;[0-9]{1,2})?)?[m|K]//g"'
# Switch Users, maining RTP.
alias sudovim='sudo vim -c "set runtimepath+=$HOME/.vim" -u $HOME/.vimrc'
alias mongostart='sudo service mongod start'    # so sick of this
# alias node="env NODE_NO_READLINE=1 rlwrap node" # rlwrap node

alias xslock='xscreensaver-command -lock'

# NMCLI aliases

alias lstwifi='nmcli dev wifi'
alias conwifi='nmcli dev wifi connect'

# # mkdir & cd to it
function mcd() {
  mkdir -p "$1" && cd "$1";
}

# Changes to a directory and lists its contents.
function cdls {
  builtin cd "$argv[-1]" && ls "${(@)argv[1,-2]}"
}

# Prints columns 1 2 3 ... n.
function slit {
  awk "{ print ${(j:,:):-\$${^@}} }"
}

# Displays user owned processes status.
function psu {
  ps -U "${1:-$USER}" -o 'pid,%cpu,%mem,command' "${(@)argv[2,-1]}"
}

# Displays some git related author statistics
function authorstats {
    gitauth=$1;
    git log --author=$gitauth --pretty=tformat: --numstat | gawk '{ add += $1 ; subs += $2 ; loc += $1 - $2 } END { printf "added lines: %s removed lines : %s total lines: %s",add,subs,loc }' -
    echo ''
}


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

user_commands=(
  list-units is-active status show help list-unit-files
  is-enabled list-jobs show-environment)

sudo_commands=(
  start stop reload restart try-restart isolate kill
  reset-failed enable disable reenable preset mask unmask
  link load cancel set-environment unset-environment)

for c in $user_commands; do; alias sc-$c="systemctl $c"; done
for c in $sudo_commands; do; alias sc-$c="sudo systemctl $c"; done

############################################
#  Package Management (Yum)
############################################

alias yumc='sudo yum clean all'    # Cleans the cache.
alias yumh='yum history'           # Displays history.
alias yumi='sudo yum install'      # Installs package(s).
alias yuml='yum list'              # Lists packages.
alias yumL='yum list installed'    # Lists installed packages.
alias yumq='yum info'              # Displays package information.
alias yumr='sudo yum remove'       # Removes package(s).
alias yums='yum search'            # Searches for a package.
alias yumsc='yum search -C'        # Search in cache
alias yumu='sudo yum update'       # Updates packages.
alias yumU='sudo yum upgrade'      # Upgrades packages.
alias yumfl='repoquery -lq'        # (f)ile (l)ist a package

############################################
#  Grep
############################################

#export GREP_COLOR='37;45'
export GREP_COLORS="38;5;230:sl=38;5;240:cs=38;5;100:mt=38;5;161:fn=38;5;197:ln=38;5;212:bn=38;5;44:se=38;5;166"
export GREP_OPTIONS='--color=auto'

############################################
#  GPG
############################################

source "$HOME/.zsh/functions/gpg.zsh"

############################################
#  Amazon AWS
############################################
export AWS_AUTO_SCALING_HOME="$HOME/.aws/aws-autoscaling"
export AWS_ELB_HOME="$HOME/.aws/aws-elastic_load_balacing"
export AWS_CLOUDWATCH_HOME="$HOME/.aws/aws-cloudwatch"
export EC2_HOME="$HOME/.aws/ec2-api-tools"

# Build up the various credential files & environment variables required by the
# AWS toolchain.
export AWS_CONFIG_FILE=$HOME/.aws/config
export AWS_CREDENTIAL_FILE="$HOME/.aws/credentials"
export AWS_ACCESS_KEY=$(cat $AWS_CONFIG_FILE | grep -A 2 "default"  |
                   grep "^aws_access_key_id" | cut -d= -f2)

export AWS_SECRET_KEY=$(cat $AWS_CONFIG_FILE  | grep -A 2 "default" |
                grep "^aws_secret_access_key" | cut -d= -f2)

`touch $AWS_CREDENTIAL_FILE`
if [ -w $AWS_CREDENTIAL_FILE ]; then
    printf "AWSAccessKeyId=%s\nAWSSecretKey=%s\n", $AWS_ACCESS_KEY, $AWS_SECRET_KEY > $AWS_CREDENTIAL_FILE;
fi

# For awscli
export AWS_DEFAULT_REGION=us-west-1
export EC2_URL=https://ec2.us-west-1.amazonaws.com

PATH+=$AWS_CLOUDWATCH_HOME/bin:$AWS_ELB_HOME/bin:$AWS_AUTO_SCALING_HOME/bin:$EC2_HOME/bin

if [ -e "/usr/lib/jvm/java-1.7.0-openjdk-1.7.0.60-2.4.3.0.fc20.x86_64/jre" ]; then
    export JAVA_HOME="/usr/lib/jvm/java-1.7.0-openjdk-1.7.0.60-2.4.3.0.fc20.x86_64/jre"
else
    # give it our best shot
    echo "Couldn't find java"
    #echo "WE'RE TRYING TO FIND JAVA MANUALLY, CHANGE THIS PLEASE"
    #echo "LOOK IN ~/.ZSHRC AND CHANGE THIS RIGHT NOW"
    # export JAVA_HOME=$(dirname `find / -wholename "*/jre/bin/java" 2>/dev/null`)
fi

############################################
#  Completions
############################################
# Amazon AWS completion
source $HOME/.zsh/functions/amazon_ec2_completion.zsh

# Load our completion functions
fpath=($HOME/.zsh/zsh-completions/src $fpath)

unsetopt menu_complete   # do not autoselect the first completion entry
unsetopt flowcontrol
setopt auto_menu         # show completion menu on succesive tab press
setopt complete_in_word
setopt always_to_end

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
# zstyle ':completion:*:*:kill:*:processes' list-colors '=(#b) #([0-9]#) ([0-9a-z-]#)*=01;34=0=01'
zstyle ':completion:*:*:kill:*:processes' command 'ps --forest -e -o pid,user,tty,cmd'
zstyle ':completion:*:*:*:*:processes' command "ps -u `whoami` -o pid,user,comm -w -w"


# disable named-directories autocompletion
zstyle ':completion:*:cd:*' tag-order local-directories directory-stack path-directories
cdpath=(.)

# Use caching so that commands like apt and dpkg complete are useable
zstyle ':completion::complete:*' use-cache 1
zstyle ':completion::complete:*' cache-path ~/.zsh/cache/

# Use caching to make completion for cammands such as dpkg and apt usable.
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
# zstyle ':completion::*:(-command-|export):*' fake-parameters ${${${_comps[(I)-value-*]#*,}%%,*}:#-*-}

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
zstyle ':completion:*:*:*:*:processes' command 'ps -u $USER -o pid,user,comm -w'
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

#############################################
#  Corrections
#############################################

setopt correct_all

# Disable correction.
alias ack='nocorrect ack'
alias cd='nocorrect cd'
alias cp='nocorrect cp'
alias ebuild='nocorrect ebuild'
alias gcc='nocorrect gcc'
alias gist='nocorrect gist'
alias grep='nocorrect grep'
alias heroku='nocorrect heroku'
alias ln='nocorrect ln'
alias man='nocorrect man'
alias mkdir='nocorrect mkdir'
alias mv='nocorrect mv'
alias mysql='nocorrect mysql'
alias rm='nocorrect rm'
alias nmcli'nocorrect nmcli'
alias ip='nocorrect ip'

# Disable globbing.
alias fc='noglob fc'
alias find='noglob find'
alias ftp='noglob ftp'
alias history='noglob history'
alias locate='noglob locate'
alias rake='noglob rake'
alias rsync='noglob rsync'
alias scp='noglob scp'
alias sftp='noglob sftp'

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

#############################################
### History #################################
#############################################
# Where I store my shit
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

#############################################
### Bindings ################################
#############################################

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
### Colors from the beyond! #################
#############################################

# ls colors
autoload -U colors && colors;
export LSCOLORS="Gxfxcxdxbxegedabagacad"

# Stop the beep insanity
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

# The crazier the better!
eval `dircolors ~/.zsh/LS_COLORS`

#############################################
### Utilities et al.
#############################################

# ZSHENV stuff
#export EDITOR='vim' # didn't see that one coming.
#export VISUAL'vim'
export PAGER='less'
export LANG='en_US.UTF-8'
export LESS='-F -g -i -M -R -S -w -X -z-4'

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
# for some reason col -b no longer correctly strips ANSI escape characters.
export MANPAGER="/bin/sh -c \"sed -r 's/\x1B\[([0-9]{1,2}(;[0-9]{1,2})?)?[m|K]//g' | vim -c 'set ft=man ts=8 nomod nolist nonu noma' -\""

alias info="info --vi-keys"

##########################################
#### Build and Automake settings #########
##########################################
export CXX=g++
export CC=gcc
export AR=ar
export CFLAGS="-march=native -mtune=native -O2 -pipe -Wall"
export CXXFLAGS="$CFLAGS"
# export LDFLAGS="--cref -O2"
export LIBS="-lm -lcrypto"
export MAKEFLAGS="-j4"

############################################
#### Autocompletion and associates #########
############################################

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

# Add go to path
export GOPATH=$HOME/Development/go

PATH+=:$GOPATH/bin
PATH="$HOME/bin:/usr/local/bin:$PATH"

# Recursively add anything I've got in ~/bin
for home_bin in ~/bin; do
    PATH+=":$home_bin"
done

export prakts=$HOME/Development/practice
export prvumain=$HOME/Development/purveu-main

PATH+=:$HOME/.rvm/bin # Add RVM to PATH for scripting
PATH+=:/usr/local/plan9/bin # Plan9 binaries

export PATH
