objects = aprc gemrc gitconfig erlang pryrc \
	  zshrc xmobarrc xsession vimrc pentadactylrc

install: clean 
	# I would love to know a better way to achieve this
	ls $(objects) | xargs -n 1 readlink -f | xargs -n 1 basename | xargs -t -n 1 -I {} ln -sf `pwd`/{} ~/.{}
	ln -sf -t ~/.vim `pwd`/xmonad  ~
	ln -sf -t ~/.vim `pwd`/vim 
	ln -sf ~/.xsession ~/.xinitrc
	git submodule update --init

backup: 
	mkdir -p ~/.dotfilez_backups/`date %+F`
	ls $(objects) | xargs -n 1 readlink -f | xargs -n 1 basename | xargs -t -n 1 -I {} cp -f `pwd`/{} ~/.dotfilez_backups/`date %+F` 
	cp -r ~/.vim ~/.dotfilez_backups/`date %+F`/.vim
	cp -r ~/.xmonad ~/.dotfilez_backups/`date %+F`/.xmonad

key:
	cp id_rsa.gpg ~/.ssh/id_rsa.gpg 
	cp id_rsa.pub ~/.ssh/id_rsa.pub 
	ln -sf `pwd`/ssh_config ~/.ssh/config
	gpg ~/.ssh/id_rsa.gpg 
	chmod 0600 ~/.ssh/config ~/.ssh/id_rsa

clean: 
	rm ~/.$(objects)
	rm -rf ~/.vim
	rm -rf ~/.xmonad

dotfilez.tar.gz:
	tar cvf dotfilez.tar * 
	gzip -9c dotfilez.tar >> dotfilez.tar.gz
	rm dotfilez.tar

