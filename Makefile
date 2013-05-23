DOTFILES = aprc gemrc gitconfig erlang pryrc \
	  zshrc xmobarrc xsession vimrc pentadactylrc \
	  Xdefaults xmodmaprc gdbinit 

DOTDIRS = vim xmonad

install: 
	git submodule update --init
	$(foreach file, $(DOTFILES), ln -sf $(PWD)/$(file) $(HOME)/.$(file);)
	$(foreach directory, $(DOTDIRS), ln -sf $(PWD)/$(directory) $(HOME)/.$(directory);)
	ln -sf $(HOME)/.Xdefaults $(HOME)/.Xresources


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

clean: 
	$(foreach file, $(DOTFILES), rm -f ~/.$(file);)
	rm -rf ~/.vim
	rm -rf ~/.xmonad

dotfilez.tar.gz:
	tar cvf dotfilez.tar * 
	gzip -9c dotfilez.tar >> dotfilez.tar.gz
	rm dotfilez.tar

