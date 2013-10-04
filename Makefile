DOTFILES = aprc gemrc gitconfig erlang pryrc \
	  zshrc xmobarrc xsession vimrc pentadactylrc \
	  Xdefaults xmodmaprc gdbinit tmux.conf xpdfrc \
	  iex ctags editrc gitignore

DOTDIRS = vim xmonad zsh pentadactyl tmux

install:
	git submodule update --init --recursive
	# Install our files
	$(foreach file, $(DOTFILES), ln -sf $(PWD)/$(file) $(HOME)/.$(file);)
	# Install our directories
	$(foreach directory, $(DOTDIRS), ln -sf $(PWD)/$(directory) $(HOME)/.$(directory);)
	ln -s $(HOME)/.Xdefaults $(HOME)/.Xresources
	ln -s $(HOME)/.Xsession $(HOME)/.xinit
	# Run Vundle from the command line
	vim +BundleInstall +qall

select:

# Will backup your existing stuff to ~/.dotfilez_backups
# does not work well at this point.
backup:
	mkdir -p ~/.dotfilez_backups/`date %+F`
	ls $(DOTFILES) | xargs -n 1 readlink -f | xargs -n 1 basename | xargs -t -n 1 -I {} cp -f `pwd`/{} ~/.dotfilez_backups/`date %+F`
	cp -r ~/.vim ~/.dotfilez_backups/`date %+F`/.vim
	cp -r ~/.xmonad ~/.dotfilez_backups/`date %+F`/.xmonad

key:
	cp id_rsa.gpg ~/.ssh/id_rsa.gpg
	cp id_rsa.pub ~/.ssh/id_rsa.pub
	ln -sf `pwd`/ssh_config ~/.ssh/config
	gpg ~/.ssh/id_rsa.gpg
	chown `whoami` ~/.ssh/config
	chmod 0600 ~/.ssh/config ~/.ssh/id_rsa

# Clean out the dotfiles I installed. This could also kill your stuff
clean:
	$(foreach file, $(DOTFILES), rm -f $(HOME)/.$(file);)
	$(foreach directory, $(DOTDIRS), rm -rf $(HOME)/.$(directory);)
	rm -f $(HOME)/.xinit # a dangerous game
	rm -f $(HOME)/.Xresources
# make a tar of these nice dotfiles
dotfilez.tar.gz:
	tar cvf dotfilez.tar *
	gzip -9c dotfilez.tar >> dotfilez.tar.gz
	rm dotfilez.tar
