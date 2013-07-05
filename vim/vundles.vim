filetype off

set rtp+=~/.vim/bundle/vundle/
call vundle#rc()

Bundle "gmarik/vundle"

"##################################################
" Utilities
"##################################################
" ,t ; ,b fuzzy file finder
Bundle "kien/ctrlp.vim" 
" ,a tabularize to align items by chars
Bundle "godlygeek/tabular" 
" sk/sj to split and merge hashs, arrays, etc.
Bundle "AndrewRadev/splitjoin.vim" 
" ,T to get yourself a ctag bar
Bundle "majutsushi/tagbar" 
" comment things out (gcc) 
Bundle "tomtom/tcomment_vim" 
" ,N to pop open this classic 
Bundle "scrooloose/nerdtree" 
" Tab completion
Bundle "shougo/neocomplcache" 
" Visual undo tree
Bundle "sjl/gundo.vim" 
" Hard to explain, easy to think about
Bundle "terryma/vim-multiple-cursors" 
" ,,w or ,,b to get vimium like movements
Bundle "skwp/vim-easymotion" 
" that little thing at the bottom
Bundle "skwp/vim-powerline.git" 
" :AnsiEsc to escape the ansi codez
Bundle "vim-scripts/AnsiEsc.vim.git"

"##################################################
" Enhancement
"##################################################
Bundle "vim-scripts/AutoTag.git"
Bundle "vim-scripts/camelcasemotion.git"
Bundle "tomtom/tlib_vim" 
Bundle "vim-scripts/ZoomWin"
Bundle "xsunsmile/showmarks.git"
Bundle "scrooloose/syntastic" 
Bundle "skwp/greplace.vim"
Bundle "tpope/vim-repeat.git"
Bundle "tpope/vim-surround.git"
Bundle "tpope/vim-unimpaired"
Bundle "nelstrom/vim-visual-star-search"
Bundle "briandoll/change-inside-surroundings.vim.git"
Bundle "vim-scripts/TagHighlight.git"
Bundle "nathanaelkane/vim-indent-guides.git"
" snips
Bundle "garbas/vim-snipmate.git"
Bundle "honza/vim-snippets"
Bundle "carlosgaldino/elixir-snippets"
Bundle "vim-addon-mw-utils"

"##################################################
" Git
"##################################################
Bundle "gregsexton/gitv"
Bundle "tjennings/git-grep-vim"
Bundle "tpope/vim-fugitive"
Bundle "tpope/vim-git"

"##################################################
" Language specific (syntax, :make, etc)
"##################################################
"" x86
Bundle "vim-scripts/asmx86"
"
"" erlang & elixir
Bundle "elixir-lang/vim-elixir"
Bundle "mattonrails/vim-mix"
Bundle "jimenezrick/vimerl"

"" go
Bundle "jnwhiteh/vim-golang"

"" rubes
Bundle "vim-ruby/vim-ruby.git"
Bundle "tpope/vim-rails.git"
Bundle "tpope/vim-rvm.git"

Bundle "tpope/vim-endwise.git"
Bundle "tpope/vim-rake.git"

"" js
Bundle "pangloss/vim-javascript"
Bundle "itspriddle/vim-jquery.git"
Bundle "kchmck/vim-coffee-script"
Bundle "claco/jasmine.vim"

"" less
Bundle "groenewege/vim-less.git"

" templates
Bundle "nono/vim-handlebars"
Bundle "tpope/vim-liquid.git"

"##################################################
" Text objects
"##################################################
Bundle "kana/vim-textobj-user"
Bundle "kana/vim-textobj-datetime"
Bundle "kana/vim-textobj-entire"
Bundle "kana/vim-textobj-function"
Bundle "kana/vim-textobj-line" 
Bundle "coderifous/textobj-word-column.vim"
Bundle "lucapette/vim-textobj-underscore"
Bundle "vim-scripts/argtextobj.vim"
Bundle "austintaylor/vim-indentobject"

"Filetype plugin indent on is required by vundle
filetype plugin indent on
