filetype off

set rtp+=~/.vim/bundle/vundle/
call vundle#rc()

Bundle "gmarik/vundle"
Bundle "altercation/vim-colors-solarized"

"@BeginType: TextObjects
Bundle "coderifous/textobj-word-column.vim"
Bundle "kana/vim-textobj-datetime"
Bundle "kana/vim-textobj-entire"
Bundle "kana/vim-textobj-function"
Bundle "kana/vim-textobj-line"
Bundle "austintaylor/vim-indentobject"
Bundle "kana/vim-textobj-user"
Bundle "lucapette/vim-textobj-underscore"
Bundle "vim-scripts/argtextobj.vim"
"@EndType

"@BeginType: Languages and Scripts
" golang
Bundle "jnwhiteh/vim-golang"
" erlang & elixir
Bundle "jimenezrick/vimerl"
Bundle "elixir-lang/vim-elixir"
Bundle "mattonrails/vim-mix"

" rubes
" Disabled for the time being
"Bundle "tpope/vim-rails.git"
Bundle "vim-ruby/vim-ruby.git"
" javascript; things like javascript.
Bundle 'jelera/vim-javascript-syntax'
Bundle "pangloss/vim-javascript"
Bundle "marijnh/tern_for_vim"
Bundle "nono/vim-handlebars"
Bundle "kchmck/vim-coffee-script"
Bundle "itspriddle/vim-jquery.git"
Bundle "dsawardekar/ember.vim"

" other
" These below have been disabled for performance
"Bundle "uarun/vim-protobuf"
"Bundle "tpope/vim-liquid.git"
"Bundle "groenewege/vim-less.git"
"Bundle "claco/jasmine.vim"
"Bundle "vim-scripts/asmx86"
"Bundle "adimit/prolog.vim"
"@EndType


" So here's what going on here. YouCompleteMe requires newer features in VIM,
" not (at this point) readily available in most package managers. As a
" consequence, when using this on systems that I only spend brief amounts of
" time on, it is not worth it to compile and build NPM. On the other hand,
" neocomplcache is still pretty good but can get very slow  (still good
" autocompletion). Even more confusing is the third dark knight (Neocomplete)
" which is arguably the best of all of the but requires some bleeding edge
" stuff that requires you compile your own build on practically every
" distribution (save OSX, again, go figures).
"
" Neo* plays best with other Neo* software. YCM plays best with ultisnips.

" Completion
"Bundle "Valloric/YouCompleteMe"
Bundle "Shougo/neocomplete"
"Bundle "Shougo/neocomplcache"

" Skippets
Bundle "Shougo/neosnippet"
"Bundle "MarcWeber/ultisnips"
Bundle "honza/vim-snippets"

" Filename completion
Bundle "kien/ctrlp.vim"
Bundle "sgur/ctrlp-extensions.vim"

" Tools
Bundle "rking/ag.vim"
Bundle "AndrewRadev/splitjoin.vim"
Bundle "godlygeek/tabular"
Bundle "majutsushi/tagbar"
Bundle "scrooloose/nerdtree"
Bundle "scrooloose/syntastic"
Bundle "sjl/gundo.vim"
Bundle "skwp/greplace.vim"
Bundle "skwp/vim-easymotion"
Bundle "bling/vim-airline"

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

Bundle "beloglazov/vim-online-thesaurus"


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
" nnoremap ,fb :call WhatsThatPluginAgain()<cr>
