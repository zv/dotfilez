objects = aprc gemrc gitconfig erlang pryrc \
	  zshrc xmobarrc xsession vimrc 

install: 	$(objects)
		# find all the rc files, throw them in readlink to get the absolute path, put them in basename because of limitations expanding out the -I option being passed in (in this case -l). 
		ls $(objects) | xargs -n 1 readlink -f | xargs -n 1 basename | xargs -t -n 1 -I {} ln -sf `pwd`/{} ~/.{}
		ln -sf `pwd`/xmonad ~/.xmonad
		ln -sf `pwd`/vim ~/.vim
		git submodule update --init


# watch yourself, this is naive
backupconfig:
		mv ~/.$(objects) ~/$i.backup

privatekey:
		cp id_rsa.gpg ~/.ssh/id_rsa.gpg 
		cp id_rsa.pub ~/.ssh/id_rsa.pub 
		ln -sf `pwd`/ssh_config ~/.ssh/config
		gpg ~/.ssh/id_rsa.gpg 
		chmod 0600 ~/.ssh/config ~/.ssh/id_rsa

clean:

dotfilez.tar.gz:
	tar cvf dotfilez.tar * 
	gzip -9c dotfilez.tar > dotfilez.tar.gz
	rm dotfilez.tar

