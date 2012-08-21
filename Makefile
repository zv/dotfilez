objects = aprc gemrc gitconfig erlang pryrc \
	  zshrc vimrc

install: 	$(objects)
		# find all the rc files, throw them in readlink to get the absolute path, put them in basename because of limitations expanding out the -I option being passed in (in this case -l). 
		# whew
		ls $(objects) | xargs -n 1 readlink -f | xargs -n 1 basename | xargs -t -n 1 -I {} ln -sf `pwd`/{} ~/.{}
		ln -sf `pwd`/vim ~/.vim
		git submodule update --init


# watch yourself, this is naive
backupconfig:
		mv ~/.$(objects) ~/$i.backup

privatekey:
		mv ~/.id_rsa.gpg ~/.ssh/id_rsa.gpg 
		mv ~/.id_rsa.pub ~/.ssh/id_rsa.pub 
		gpg ~/.ssh/id_rsa.gpg 

clean:

dotfilez.tar.gz:
	tar cvf dotfilez.tar * 
	gzip -9c dotfilez.tar > dotfilez.tar.gz
	rm dotfilez.tar

