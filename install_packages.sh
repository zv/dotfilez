PACKAGES="valgrind gprof vim zsh curl erlang tmux golang"

if [ -f /etc/debian_version ]; then
  apt-get update 
  sudo apt-get install build-essential
  eval sudo apt-get install "$PACKAGES"
elif [ -f /etc/redhat-release ]; then
  yum groupinstall 'Development Tools'
  eval sudo yum install "$PACKAGES"
fi

if [ ! -f `which rvm` ]; then 
  curl -L https://get.rvm.io | bash -s stable --rails --autolibs=enabled 
fi 

if [ ! $SHELL == "/bin/zsh"]; then
  chsh -s `which zsh`
fi
