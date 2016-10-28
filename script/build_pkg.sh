#!/bin/zsh


################################################################################
# Javascript
################################################################################
install_javascript () {
    pushd
    ## Build nodejs
    if [[ -x =node ]]; then
        mkdir -p ~/Development/node/local
        cd ~/Development/node
        curl https://nodejs.org/dist/node-latest.tar.gz | tar xz --strip-components=1
        ./configure --prefix=~/Development/node/local
        make install
        rehash node
    fi

    # Build / install npm
    [[ -x =npm ]] && curl https://www.npmjs.org/install.sh | sh
    rehash npm

    ## Install our NPM packages
    npm_packages=(tern js-beautify yo ember-cli gulp lodash
                  bower babel-core babel-cli webpack webpack-dev-server)

    npm install -g ${(@)npm_packages}

    popd
}

################################################################################
# Rust
################################################################################
install_rust () {
    curl -sf https://raw.githubusercontent.com/brson/multirust/master/blastoff.sh | sh
}

################################################################################
# Chrome
################################################################################
install_chrome () {
    https://dl.google.com/linux/direct/google-chrome-stable_current_x86_64.rpm
}

################################################################################
# OCaml
################################################################################
install_ocaml () {
    if [[ -x =ocaml ]]; then
        # Install
        install_by_tag "ocaml"
        # Install opam
        wget https://raw.github.com/ocaml/opam/master/shell/opam_installer.sh -O - | sh -s /usr/local/bin/
        # Now install utop
    fi
}

################################################################################
# Python
################################################################################
install_python () { sudo pip install virtualenvwrapper }

################################################################################
# Emacs
################################################################################
install_emacs () {
    echo "Created build-directory"
    local destination=~/extern
    pushd .
    if [[ ! -e $destination ]]; then
        mkdir $destination
    fi
    cd $destination
    echo "Installing dependencies"
    sudo dnf builddep emacs
    ## Emacs Developers Keyring
    echo "Install our toolchain"
    # gpg --import =(wget -q "http://savannah.gnu.org/project/memberlist-gpgkeys.php?group=emacs&download=1" -O -)
    git clone --depth=1 -b emacs-25 git://git.savannah.gnu.org/emacs.git && \
        cd emacs && \
        ./autogen.sh && ./configure && make && sudo make install && make clean
    popd
}

################################################################################
# OCaml
################################################################################
install_ocaml () {
    echo "Downloading OPAM"
    wget https://raw.github.com/ocaml/opam/master/shell/opam_installer.sh -O - | sh -s /usr/local/bin

    hash -r # ensure we can fetch opam here

    if hash opam 2>/dev/null; then
        opam switch 4.02.1
        eval `opam config env`
        opam install batteries core
    else
        echo "jkgg"
    fi

}
