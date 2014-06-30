filetype off

set rtp+=~/.vim/bundle/vundle/
call vundle#rc()

Bundle "gmarik/vundle"
Bundle "altercation/vim-colors-solarized"

" Languages and Scripts
Bundle "jnwhiteh/vim-golang"
Bundle "jimenezrick/vimerl"
Bundle "elixir-lang/vim-elixir"
Bundle "mattonrails/vim-mix"
Bundle "pangloss/vim-javascript"
Bundle "kchmck/vim-coffee-script"
Bundle "nono/vim-handlebars"
Bundle 'wting/rust.vim'

" Snippets
Bundle 'Shougo/neosnippet'
Bundle 'honza/vim-snippets'
Bundle 'Shougo/neosnippet-snippets'

" Tools
Bundle "airblade/vim-gitgutter"
Bundle "AndrewRadev/splitjoin.vim"
Bundle "bling/vim-airline"
Bundle "edkolev/tmuxline.vim"
Bundle "godlygeek/tabular"

Bundle "kien/ctrlp.vim"
Bundle "majutsushi/tagbar"
Bundle "scrooloose/nerdtree"
Bundle "scrooloose/syntastic"
Bundle "kien/ctrlp.vim"
Bundle "sgur/ctrlp-extensions.vim"
Bundle "sjl/gundo.vim"
Bundle "skwp/vim-easymotion"
Bundle "terryma/vim-multiple-cursors"
Bundle "tpope/vim-commentary"
Bundle "tpope/vim-endwise.git"
Bundle "tpope/vim-repeat.git"
Bundle "tpope/vim-surround.git"
Bundle "tpope/vim-unimpaired"
Bundle "vim-scripts/camelcasemotion.git"
Bundle "vim-scripts/matchit.zip.git"
Bundle "justinmk/vim-sneak"

if executable('nasm')
  Bundle "vim-scripts/asmx86"
endif

if executable('ipl')
  Bundle "adimit/prolog.vim"
endif

if ( has('lua') && (v:version > 703 || v:version == 703 && has('patch885')) )
  Bundle 'Shougo/neocomplete'
  Bundle 'Rip-Rip/clang_complete'
endif

if executable('ag')
  Bundle 'mileszs/ack.vim'
  let g:ackprg = 'ag --nogroup --nocolor --column --smart-case'
elseif executable('ack-grep')
  let g:ackprg="ack-grep -H --nocolor --nogroup --column"
  Bundle 'mileszs/ack.vim'
elseif executable('ack')
  Bundle 'mileszs/ack.vim'
endif

if executable('git')
  Bundle "gregsexton/gitv"
  Bundle "tpope/vim-fugitive"
  Bundle "tpope/vim-git"
endif

"Filetype plugin indent on is required by vundle
filetype plugin indent on
