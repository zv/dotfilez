filetype off

set rtp+=~/.vim/bundle/vundle/
call vundle#rc()

Bundle "gmarik/vundle"


Bundle "AndrewRadev/splitjoin.vim"
Bundle "Raimondi/delimitMate"
Bundle "austintaylor/vim-indentobject"
Bundle "claco/jasmine.vim"
Bundle "coderifous/textobj-word-column.vim"
Bundle "elixir-lang/vim-elixir"
Bundle "godlygeek/tabular"
Bundle "gregsexton/gitv"
Bundle "groenewege/vim-less.git"
Bundle "itspriddle/vim-jquery.git"
Bundle "jimenezrick/vimerl"
Bundle "jnwhiteh/vim-golang"
Bundle "kana/vim-textobj-datetime"
Bundle "kana/vim-textobj-entire"
Bundle "kana/vim-textobj-function"
Bundle "kana/vim-textobj-line"
Bundle "kana/vim-textobj-user"
Bundle "kchmck/vim-coffee-script"
Bundle "kien/ctrlp.vim"
Bundle "lucapette/vim-textobj-underscore"
Bundle "majutsushi/tagbar"
Bundle "mattonrails/vim-mix"
Bundle "nelstrom/vim-visual-star-search"
Bundle "nono/vim-handlebars"
Bundle "pangloss/vim-javascript"
Bundle "scrooloose/nerdtree"
Bundle "scrooloose/syntastic"
Bundle "shougo/neocomplcache"
Bundle "sjl/gundo.vim"
Bundle "skwp/greplace.vim"
Bundle "skwp/vim-easymotion"
Bundle "skwp/vim-powerline.git"
Bundle "terryma/vim-multiple-cursors"
Bundle "tjennings/git-grep-vim"
Bundle "tomtom/tcomment_vim"
Bundle "tpope/vim-endwise.git"
Bundle "tpope/vim-fugitive"
Bundle "tpope/vim-git"
Bundle "tpope/vim-liquid.git"
Bundle "tpope/vim-rails.git"
Bundle "tpope/vim-rake.git"
Bundle "tpope/vim-repeat.git"
Bundle "tpope/vim-rvm.git"
Bundle "tpope/vim-surround.git"
Bundle "tpope/vim-surround.git"
Bundle "tpope/vim-unimpaired"
Bundle "vim-ruby/vim-ruby.git"
Bundle "vim-scripts/AnsiEsc.vim.git"
Bundle "vim-scripts/AutoTag.git"
Bundle "vim-scripts/TagHighlight.git"
Bundle "vim-scripts/ZoomWin"
Bundle "vim-scripts/argtextobj.vim"
Bundle "vim-scripts/asmx86"
Bundle "vim-scripts/camelcasemotion.git"
Bundle "vim-scripts/matchit.zip.git"
Bundle "xsunsmile/showmarks.git"
"Filetype plugin indent on is required by vundle
filetype plugin indent on

" I wrote this little plugin to quickly pull up the github 
" for a Bundle.
function! WhatsThatPluginAgain()
  let uri = matchstr(getline("."), '^Bundle ')
  echo uri
  if uri != ""
    let isbundle = substitute(getline("."), '^Bundle "', "", 'g')
    let unbundled = substitute(isbundle, '"', "", 'g')
    let unbundled = substitute(unbundled, '\.git', "", 'g')
    let unbundled = substitute(unbundled, '\.vim', "", 'g')
    let url = substitute(unbundled, "^", "https://github.com/", 'g')
    silent exec "!google-chrome '".url."'"
  else
    echo "I can't even pretend this line has a URL in it"
  endif
endfunction

" ,fb for find bundle
nnoremap <leader>fb :call WhatsThatPluginAgain()<cr>
