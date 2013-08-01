filetype off

set rtp+=~/.vim/bundle/vundle/
call vundle#rc()

Bundle "gmarik/vundle"
Bundle "altercation/vim-colors-solarized" 
"Bundle 'amdt/vim-niji'

Bundle "coderifous/textobj-word-column.vim"
Bundle "kana/vim-textobj-datetime"
Bundle "kana/vim-textobj-entire"
Bundle "kana/vim-textobj-function"
Bundle "kana/vim-textobj-line"
Bundle "austintaylor/vim-indentobject"
Bundle "kana/vim-textobj-user"
Bundle "lucapette/vim-textobj-underscore"

Bundle "jimenezrick/vimerl"
Bundle "jnwhiteh/vim-golang"
Bundle "elixir-lang/vim-elixir"
Bundle "mattonrails/vim-mix"
Bundle "vim-scripts/asmx86"
Bundle "Rip-Rip/clang_complete"
Bundle "kchmck/vim-coffee-script"
Bundle "adimit/prolog.vim"
Bundle "nono/vim-handlebars"
Bundle "claco/jasmine.vim"
Bundle "pangloss/vim-javascript"
Bundle "tpope/vim-liquid.git"
Bundle "tpope/vim-rails.git"
Bundle "vim-ruby/vim-ruby.git"
Bundle "groenewege/vim-less.git"
Bundle "itspriddle/vim-jquery.git"

Bundle "garbas/vim-snipmate.git"
Bundle "honza/vim-snippets"
Bundle "MarcWeber/vim-addon-mw-utils.git"
Bundle "tomtom/tlib_vim.git"

Bundle "AndrewRadev/splitjoin.vim"
Bundle "godlygeek/tabular"
Bundle "gregsexton/gitv"
Bundle "kien/ctrlp.vim"
Bundle "majutsushi/tagbar"
Bundle "nelstrom/vim-visual-star-search"
Bundle "scrooloose/nerdtree"
Bundle "scrooloose/syntastic"
Bundle "shougo/neocomplcache"

Bundle "sjl/gundo.vim"
Bundle "skwp/greplace.vim"
Bundle "skwp/vim-easymotion"
Bundle 'bling/vim-airline'
Bundle "terryma/vim-multiple-cursors"
Bundle "tjennings/git-grep-vim"
Bundle "tomtom/tcomment_vim"
Bundle "tpope/vim-endwise.git"
Bundle "tpope/vim-fugitive"
Bundle "tpope/vim-git"
Bundle "tpope/vim-repeat.git"
Bundle "tpope/vim-surround.git"
Bundle "tpope/vim-unimpaired"
Bundle "vim-scripts/AnsiEsc.vim.git"
Bundle "vim-scripts/AutoTag.git"
Bundle "vim-scripts/TagHighlight.git"
Bundle "vim-scripts/ZoomWin"
Bundle "vim-scripts/argtextobj.vim"
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
    " let unbundled = substitute(unbundled, '\.vim', "", 'g')
    let url = substitute(unbundled, "^", "https://github.com/", 'g')
    silent exec "!google-chrome '".url."'"
  else
    echo "I can't even pretend this line has a URL in it"
  endif
endfunction

" ,fb for find bundle
nnoremap ,fb :call WhatsThatPluginAgain()<cr>
