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
ssh_identities=("id_rsa")
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
