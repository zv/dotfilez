#!/usr/bin/env zsh
# This is zv's ZSHRC.
# meow nyan meow nyan meow
# 2009
setopt BRACE_CCL          # Allow brace character class list expansion.
setopt COMBINING_CHARS    # Combine zero-length punctuation characters
                          # with the base character.
# setopt RC_QUOTES          # Allow 'Zephyr''s Rad Pad' instead of 'Zephyrs'\''s Rad Pad'.
setopt C_BASES            # Output hex numbers in 0xFF instead of 16#FF
unsetopt MAIL_WARNING     # Don't print a warning message if a mail file has been accessed.
setopt LONG_LIST_JOBS     # List jobs in the long format by default.
setopt AUTO_RESUME        # Attempt to resume existing job before creating a new process.
setopt NOTIFY             # Report status of background jobs immediately.
unsetopt BG_NICE          # Don't run all background jobs at a lower priority.
unsetopt HUP              # Don't kill jobs on shell exit.
unsetopt CHECK_JOBS       # Don't report on jobs when shell exit.

setopt EXTENDED_GLOB
setopt BARE_GLOB_QUAL

############################################
#  Theme
#############################################
# If we're in a dumb terminal then dont play fancy pants with our prompt
local baseprompt='>>'
if [[ $TERM == 'dumb' ]]; then
    PROMPT="%2~ $baseprompt "
    # unsetopt zle
    unsetopt prompt_cr
    unsetopt prompt_subst
    # Ensure we don't set preexec && precmd
elif [[ $TERM == 'eterm-color' ]]; then
    # unsetopt zle
    unsetopt prompt_cr
    unsetopt prompt_subst
    PROMPT="[%y] %2~ $baseprompt "
else
    export KEYTIMEOUT=1 # Immediately switch to vicmd/viinst
    # shows a slightly different prompt for vicmd vs other ZLE command modes to let
    # you know what you are dealing with.
    autoload -Uz vcs_info
    base_vcs_style='%c%b%u%f'
    zstyle ':vcs_info:*' enable git hg # svn cvs fossil bzr
    zstyle ':vcs_info:*' check-for-changes true
    zstyle ':vcs_info:*' actionformats "- [$base_vcs_style|%F{green}%a%f] "
    zstyle ':vcs_info:*' stagedstr '%F{028}'
    zstyle ':vcs_info:*' unstagedstr '*'
    zstyle ':vcs_info:*' formats " [$base_vcs_style]"
    zstyle ':vcs_info:git:*' branchformat '%b%F{1}:%F{3}%r'
    precmd () { vcs_info }

    zle_vim_prompt_notifier() {
        if [[ "$KEYMAP" == vicmd ]]; then
            print "%F{red}>>%f"
        else
            print ">>"
        fi
    }

    PROMPT='[%n@%m] %B%2~%b${vcs_info_msg_0_} `zle_vim_prompt_notifier` '
fi

# ls colors
autoload -U colors && colors;

setopt no_beep      # Stop the beep insanity
setopt auto_cd      # Automatic CD on directory specification
setopt cdablevarS   # CD into variable from var name
setopt prompt_subst # Setup the prompt with pretty colors

#############################################
# Misc. Env Vars
#############################################
# Editors
export VISUAL='vim'


# Set the default Less options.
# Mouse-wheel scrolling has been disabled by -X (disable screen clearing).
# Remove -X and -F (exit if the content fits on one screen) to enable it.
#export LESS='-F -g -i -M -R -S -w -X -z-4'

# Set the Less input preprocessor.
# Try both `lesspipe` and `lesspipe.sh` as either might exist on a system.
if (( $#commands[(i)lesspipe(|.sh)] )); then
  export LESSOPEN="| /usr/bin/env $commands[(i)lesspipe(|.sh)] %s 2>&-"
fi


#############################################
# Aliasing
#############################################
# Disable correction.
for cmd (ack cd cp ebuild gcc gist grep heroku
        ln man mkdir mv mysql rm nmcli ip ag
        git npm ember dnf jekyll) alias $cmd="nocorrect $cmd"

# Define general aliases.
alias p="print"
alias pr="print -l"

alias _='sudo'
alias e="emacsclient -t"
alias edit="emacs -nw"
alias info="info --vi-keys"
alias jkl="jekyll serve -D"

# Tmux
alias ta="tmux attach-session -t"
alias tl="tmux list-sessions"

alias zthree="z3 -in"
alias gpg=gpg2

# Bullshit workaround to unlock my yubikey
alias gpginit="date | gpg -a --encrypt -r zv@nxvr.org | gpg --decrypt"

function vman {
    vim -c "SuperMan $1" -c "set nonu"
}

# Moves the last created file to $2
function mvlast {
    local last_file=($1/*(.oc[1]))
    mv $last_file $2 && print $last_file
}

# Removes the last created file in the specified directory, defaulting to the
# current directory.
function rmlast {
  [[ $# -eq 0 ]] && rm -i *(.oc[1]) || rm -i $1/*(.oc[1])
}

nocorrect noglob function calc () {
    if [ $# -ne 0 ]; then
         local wrapped_args="\"${argv}\""
         emacsclient --eval "(calc-eval $wrapped_args)" | sed 's/^\"//' |  sed 's/\"$//'
    else
        emacsclient -t -c --eval "(full-calc)"
    fi
}

alias calc="noglob calc"

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

# # mkdir & cd to it
function mcd() { mkdir -p "$1" && cd "$1"; }

function ptrsp {
    if [[ !$+commands[xinput] ]] return;
    # Grep through for the actual ID. Hack.
    local prop_id=$(xinput list --short | \
        grep 'Lenovo.*pointer' | \
        awk 'BEGIN { FS="[\t]+" } ; {print $2}' | \
        sed 's/id\=//')
   xinput --set-prop $prop_id 140 $1 0 0 0 $1 0 0 0 $2
}

alias ls='ls --group-directories-first --color=auto'
alias l='ls -1A'         # Lists in one column, hidden files.
alias ll='ls -lh'        # Lists human readable sizes.
alias la='ll -A'         # Lists human readable sizes, hidden files.
alias grep="$aliases[grep] --color=auto"

# dnf
if (( $+commands[dnf] )); then
    alias dnfs="dnf search"                       # search package
    alias dnfp="dnf info"                         # show package info
    alias dnfl="dnf list"                         # list packages
    alias dnfcu="dnf check-update"                # check updates
    alias dnfh="sudo dnf history"                 # get dnf history
    alias dnfu="sudo dnf upgrade"                 # upgrade packages
    alias dnfi="sudo dnf install"                 # install package
    alias dnfr="sudo dnf remove"                  # remove package
    alias dnfgr="sudo dnf groupremove"            # remove pagage group
    alias dnfrl="sudo dnf remove --remove-leaves" # remove package and leaves
    alias dnfc="sudo dnf clean all"               # clean cachefi
fi

# Git
alias g='git'
alias gst='git status'
alias gl='git pull'
alias gc='git commit -v'
alias gca='git commit -v -a'
alias gco='git checkout'
alias gcm='git checkout master'
alias ga='git add'
alias chr="google-chrome"

alias git-userlinecount="git ls-files | xargs -n1 -d'\n' -i git blame {} | perl -n -e '/\s\((.*?)\s[0-9]{4}/ && print \"$1\n\"' | sort -f | uniq -c -w3 | sort -r"

# Lists all files that have been ignored with git update-index
alias git-ignorelog='git ls-files -v | grep "^h"'

# Displays some git related author statistics
function authorstats {
    gitauth=$1;
    git log --author=$gitauth --pretty=tformat: --numstat | gawk '{ add += $1 ; subs += $2 ; loc += $1 - $2 } END { printf "added lines: %s removed lines : %s total lines: %s",add,subs,loc }' -
    echo ''
}

####
# Expansion and Globbing
####

# Directories
setopt AUTO_CD              # Auto changes to a directory without typing cd.
setopt AUTO_PUSHD           # Push the old directory onto the stack on cd.
setopt PUSHD_IGNORE_DUPS    # Do not store duplicates in the stack.
setopt PUSHD_SILENT         # Do not print the directory stack after pushd or popd.
setopt PUSHD_TO_HOME        # Push to home directory when no argument is given.
setopt CDABLE_VARS          # Change directory to a path stored in a variable.
setopt AUTO_NAME_DIRS       # Auto add variable-stored paths to ~ list.
setopt MULTIOS              # Write to multiple descriptors.
unsetopt CLOBBER            # Do not overwrite existing files with > and >>. Use >! and >>! to bypass.
setopt RC_EXPAND_PARAM # xx=(a b c) && echo 'foo${xx}bar' -> fooabar, foobbar, foocbar instead of the default fooa b cbar
setopt EXTENDED_GLOB   # Use extended globbing syntax.

# History
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

# History
export HISTFILE=$HOME/.zsh_history

# Lines to store
export HISTSIZE=$((2**14 - 1))
export SAVEHIST=$((2**14 - 1))

# Lists the ten most used commands.
alias history-stat="history 0 | awk '{print \$2}' | sort | uniq -c | sort -n -r | head"

# Man
# export MANPAGER="/bin/sh -c \"sed -r 's/\x1B\[([0-9]{1,2}(;[0-9]{1,2})?)?[m|K]//g' | vim -c 'set ft=man nomod nolist nonu noma showtabline=0 shortmess+=T' -\""

############################################
#  Modules & Completions
############################################
autoload -U compinit && compinit

setopt ALWAYS_TO_END       # Move cursor to the end of a completed word.
setopt AUTO_LIST           # Automatically list choices on ambiguous completion.
setopt AUTO_MENU           # Show completion menu on a succesive tab press.
setopt AUTO_PARAM_SLASH    # If completed parameter is a directory, add a trailing slash.
setopt COMPLETE_IN_WORD    # Complete from both ends of a word.
setopt PATH_DIRS           # Perform path search even on command names with slashes.

unsetopt MENU_COMPLETE     # Do not autoselect the first completion entry.
unsetopt FLOW_CONTROL      # Disable start/stop characters in shell editor.

# Use caching to make completion for commands such as dpkg and apt usable.
zstyle ':completion::complete:*' use-cache on
zstyle ':completion::complete:*' cache-path "${ZDOTDIR:-$HOME}/.zcompcache"

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

# SSH/SCP/RSYNC
zstyle ':completion:*:(scp|rsync):*' tag-order 'hosts:-host:host hosts:-domain:domain hosts:-ipaddr:ip\ address *'
zstyle ':completion:*:(scp|rsync):*' group-order users files all-files hosts-domain hosts-host hosts-ipaddr
zstyle ':completion:*:ssh:*' tag-order 'hosts:-host:host hosts:-domain:domain hosts:-ipaddr:ip\ address *'
zstyle ':completion:*:ssh:*' group-order users hosts-domain hosts-host users hosts-ipaddr
zstyle ':completion:*:(ssh|scp|rsync):*:hosts-host' ignored-patterns '*(.|:)*' loopback ip6-loopback localhost ip6-localhost broadcasthost
zstyle ':completion:*:(ssh|scp|rsync):*:hosts-domain' ignored-patterns '<->.<->.<->.<->' '^[-[:alnum:]]##(.[-[:alnum:]]##)##' '*@*'
zstyle ':completion:*:(ssh|scp|rsync):*:hosts-ipaddr' ignored-patterns '^(<->.<->.<->.<->|(|::)([[:xdigit:].]##:(#c,2))##(|%*))' '127.0.0.<->' '255.255.255.255' '::1' 'fe80::*'

# Tmux Pane Completion
_tmux_pane_words() {local expl
  local -a w
  if [[ -z "$TMUX_PANE" ]]; then
    _message "not running inside tmux!"
    return 1
  fi
  # capture current pane first
  w=( ${(u)=$(tmux capture-pane -J -p)} )
  for i in $(tmux list-panes -F '#P'); do
    # skip current pane (handled above)
    [[ "$TMUX_PANE" = "$i" ]] && continue
    w+=( ${(u)=$(tmux capture-pane -J -p -t $i)} )
  done
  _wanted values expl 'words from current tmux pane' compadd -a w
}

zle -C tmux-pane-words-prefix   complete-word _generic
zle -C tmux-pane-words-anywhere complete-word _generic
bindkey '^X^X' tmux-pane-words-prefix
bindkey '^X^T' tmux-pane-words-anywhere
zstyle ':completion:tmux-pane-words-(prefix|anywhere):*' completer _tmux_pane_words
zstyle ':completion:tmux-pane-words-(prefix|anywhere):*' ignore-line current
# display the (interactive) menu on first execution of the hotkey
zstyle ':completion:tmux-pane-words-(prefix|anywhere):*' menu yes select interactive
zstyle ':completion:tmux-pane-words-anywhere:*' matcher-list 'b:=* m:{A-Za-z}={a-zA-Z}'


# **********************************************************
# Don't do any of the following if we have a dumb terminal *
# **********************************************************
if [[ "$TERM" == 'dumb' ]]; then
    return 1
fi

# Prompt for correction
setopt correct_all

# The crazier the better!
if [[ -x =dircolors && -e ~/.zsh/LS_COLORS ]]; then
    eval `dircolors --sh ~/.zsh/LS_COLORS`
fi

#############################################
#  Vim & ZSH Line Editor
############################################
bindkey -v
WORDCHARS='*?_-.[]~&;!#$%^(){}<>'

function zle-line-init zle-keymap-select {
  zle reset-prompt
}

# ZSH line editor stuff needed for bindkey to work
zle -N zle-line-init
zle -N zle-keymap-select

autoload -U url-quote-magic
zle -N self-insert url-quote-magic


bindkey -M viins "^P" up-line-or-search
bindkey -M viins "^N" down-line-or-search

# Some convienent alt bindings
bindkey -M viins "\eh" vi-backward-blank-word
bindkey -M vicmd "\eh" vi-backward-blank-word
bindkey -M viins "\el" vi-forward-blank-word
bindkey -M vicmd "\el" vi-forward-blank-word

bindkey -M viins "\ed" delete-word
bindkey -M viins "^F" vi-cmd-mode

# Edit command in an external editor.
autoload -U edit-command-line
zle -N edit-command-line
bindkey -M vicmd "v" edit-command-line

# History
bindkey "^R" history-incremental-pattern-search-backward
bindkey "^S" history-incremental-pattern-search-forward
bindkey -M vicmd "?" vi-history-search-forward
bindkey -M vicmd "/" vi-history-search-backward

# *-or-search searches for existing history items currently in the command
# *-string, while '*-or-history' ignores this.
bindkey -M vicmd "k" up-line-or-search # up-line-or-history
bindkey -M vicmd "j" down-line-or-search # down-line-or-history

bindkey -s '\eu' "..\n"
bindkey -s '\es' "git status\n"
# bindkey '\ew' kill-region
#bindkey '^r' history-incremental-search-backward
#bindkey "^[[5~" up-line-or-history
#bindkey "^[[6~" down-line-or-history

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

# Alt-Backspace deletes a word
bindkey -M viins "\e^?" backward-delete-word

# Insert the last argument
bindkey -M viins "\e0" insert-last-word

bindkey ' ' magic-space # [Space] - do history expansion

#############################################
# Other
#############################################
# Run our external modules
for fn (~/.zsh/modules/*.zsh) source $fn

#  Gnupg Agent Wrapper
source $HOME/.gnupg/gpg-agent-wrapper

# Configure Ocaml package manager
. /home/zv/.opam/opam-init/init.zsh > /dev/null 2> /dev/null || true

# cdpath=($cdpath)

export N_PREFIX="$HOME/n"; [[ :$PATH: == *":$N_PREFIX/bin:"* ]] || PATH+=":$N_PREFIX/bin"  # Added by n-install (see http://git.io/n-install-repo).

if [[ -x /usr/bin/virtualenvwrapper.sh ]]; then
    source /usr/bin/virtualenvwrapper.sh # Virtualenvwrapper
fi

export RUST_SRC_PATH=/usr/local/src/rust/src

# The next line updates PATH for the Google Cloud SDK.
source '/home/zv/extern/google-cloud-sdk/path.zsh.inc'

# The next line enables shell command completion for gcloud.
source '/home/zv/extern/google-cloud-sdk/completion.zsh.inc'
