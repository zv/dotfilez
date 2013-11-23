filetype off

set rtp+=~/.vim/bundle/vundle/
call vundle#rc()

Bundle 'gmarik/vundle'
Bundle "altercation/vim-colors-solarized"

"@BeginType: Languages and Scripts
" golang
Bundle "jnwhiteh/vim-golang"
" erlang & elixir
Bundle "jimenezrick/vimerl"
Bundle "elixir-lang/vim-elixir"
Bundle "mattonrails/vim-mix"
" rubes
Bundle "vim-ruby/vim-ruby.git"
" javascript; things like javascript.
Bundle 'jelera/vim-javascript-syntax'
Bundle "pangloss/vim-javascript"
" Bundle "marijnh/tern_for_vim"
Bundle "nono/vim-handlebars"
Bundle "digitaltoad/vim-jade"
Bundle "kchmck/vim-coffee-script"
Bundle "itspriddle/vim-jquery.git"

" Completion
" At this point I've given up on all autocompletion and gone back to Wildmenu.
" Good riddance (Although I keep config files that will automatically work
" with YCM & NeoComplete & Neocomplcache)
"Bundle "Valloric/YouCompleteMe"
"Bundle "Shougo/neocomplete"
"Bundle "Shougo/neocomplcache"
"Bundle "Shougo/neosnippet"
"Bundle "honza/vim-snippets"

" Filename completion
Bundle "kien/ctrlp.vim"
Bundle "sgur/ctrlp-extensions.vim"

" Tools
Bundle "rking/ag.vim"

"Bundle "godlygeek/tabular"
Bundle 'junegunn/vim-easy-align'

Bundle "majutsushi/tagbar"
Bundle "scrooloose/nerdtree"
Bundle "scrooloose/syntastic"
Bundle "sjl/gundo.vim"
Bundle "skwp/greplace.vim"
Bundle "skwp/vim-easymotion"
Bundle "justinmk/vim-sneak"
Bundle "bling/vim-airline"
Bundle "terryma/vim-multiple-cursors"
Bundle "mattn/emmet-vim"

" Git tools (Fugitive is later)
Bundle "gregsexton/gitv"
Bundle "airblade/vim-gitgutter"

" tpope"s various bundle"s of joy
Bundle "tpope/vim-endwise.git"
Bundle "tpope/vim-fugitive"
Bundle "tpope/vim-git"
Bundle "tpope/vim-repeat.git"
Bundle "tpope/vim-commentary"
Bundle "tpope/vim-surround.git"
Bundle "tpope/vim-unimpaired"
" end tpope

Bundle "vim-scripts/camelcasemotion.git"
Bundle "vim-scripts/matchit.zip.git"


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
