local GPG_ENV=$HOME/.gnupg/gpg-agent.env

function start_agent_nossh {
    eval $(/usr/bin/env gpg-agent --quiet --daemon --write-env-file ${GPG_ENV} 2> /dev/null)
    chmod 600 ${GPG_ENV}
    export GPG_AGENT_INFO
}

function start_agent_withssh {
    eval $(/usr/bin/env gpg-agent --quiet --daemon --enable-ssh-support --write-env-file ${GPG_ENV} 2> /dev/null)
    chmod 600 ${GPG_ENV}
    export GPG_AGENT_INFO
    export SSH_AUTH_SOCK
    export SSH_AGENT_PID
}

# check if another agent is running
if ! gpg-connect-agent --quiet /bye > /dev/null 2> /dev/null; then
    # source settings of old agent, if applicable
    if [ -f "${GPG_ENV}" ]; then
        . ${GPG_ENV} > /dev/null
        export GPG_AGENT_INFO
        export SSH_AUTH_SOCK
        export SSH_AGENT_PID
    fi

    # check again if another agent is running using the newly sourced settings
    if ! gpg-connect-agent --quiet /bye > /dev/null 2> /dev/null; then
        # check for existing ssh-agent
        if ssh-add -l > /dev/null 2> /dev/null; then
            # ssh-agent running, start gpg-agent without ssh support
            start_agent_nossh;
        else
            # otherwise start gpg-agent with ssh support
            start_agent_withssh;
        fi
    fi
fi

GPG_TTY=$(tty)
export GPG_TTY




# #
# # Return if requirements are not found.
# if [ ! -e `which gpg-agent` ]; then
#   return 1
# fi

# # Set the default paths to gpg-agent files.
# _gpg_agent_conf="$HOME/.gnupg/gpg-agent.conf"
# _gpg_agent_env="/tmp/gpg-agent.env"

# # Start gpg-agent if not started.
# if ! ps -U "$USER" -o ucomm | grep -q gpg-agent; then
#   eval "$(gpg-agent --daemon | tee "$_gpg_agent_env")"
# else
#   # Export environment variables.
#   source "$_gpg_agent_env" 2> /dev/null
# fi

# # Inform gpg-agent of the current TTY for user prompts.
# export GPG_TTY="$(tty)"

# # Integrate with the SSH module.
# if grep 'enable-ssh-support' "$_gpg_agent_conf" &> /dev/null; then
#   # Override the ssh-agent environment file default path.
#   _ssh_agent_env="$_gpg_agent_env"

#   # Load the SSH module for additional processing.
#   pmodload 'ssh'
# fi

# # Clean up.
# unset _gpg_agent_{conf,env}

# # Disable GUI prompts inside SSH.
# if [[ -n "$SSH_CONNECTION" ]]; then
#   export PINENTRY_USER_DATA='USE_CURSES=1'
# fi
