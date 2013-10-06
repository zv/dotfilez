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
Bundle "jimenezrick/vimerl"
Bundle "jnwhiteh/vim-golang"
Bundle "elixir-lang/vim-elixir"
Bundle "mattonrails/vim-mix"
Bundle "Rip-Rip/clang_complete"
Bundle "kchmck/vim-coffee-script"
Bundle "nono/vim-handlebars"
Bundle "pangloss/vim-javascript"
Bundle "tpope/vim-rails.git"
Bundle "vim-ruby/vim-ruby.git"
" These below have been disabled for performance
"Bundle "tpope/vim-liquid.git"
"Bundle "groenewege/vim-less.git"
"Bundle "claco/jasmine.vim"
"Bundle "vim-scripts/asmx86"
"Bundle "adimit/prolog.vim"
"Bundle "itspriddle/vim-jquery.git"
"@EndType

" Completion
Bundle "Valloric/YouCompleteMe"
Bundle "Shougo/neosnippet"
Bundle "honza/vim-snippets"


" These are part of garba's snipmate system. I am using neosnip now 
"Bundle "garbas/vim-snipmate.git"
"Bundle "MarcWeber/vim-addon-mw-utils.git"
"Bundle "tomtom/tlib_vim.git"
"@EndType


" Tools
Bundle "rking/ag.vim"
Bundle "AndrewRadev/splitjoin.vim"
"@utility: ,a to align text by a character
Bundle "godlygeek/tabular"
"@utility: :Gitv to get a gitk-alike history layout
Bundle "gregsexton/gitv"
"@utility: CTRLP is an amazing utility for automatically pulling up things.
"Try it with ,t or ,b to fuzzy search for files or buffers.
"@image: 'https://a248.e.akamai.net/camo.github.com/0a0b4c0d24a44d381cbad420ecb285abc2aaa4cb/687474703a2f2f692e696d6775722e636f6d2f7949796e722e706e67 '
Bundle "kien/ctrlp.vim"
Bundle "sgur/ctrlp-extensions.vim"
"Bundle "tacahiroy/ctrlp-funky"
"Bundle "ivalkeen/vim-ctrlp-tjump"
"@utility: ,T to bring up a tagbar on the right.
Bundle "majutsushi/tagbar"


"@utility: Select more than a single word with * or #.
Bundle "nelstrom/vim-visual-star-search"
"@utility: The original NERDTree file browser
Bundle "scrooloose/nerdtree"
"@utilty: A utility
"@image: 'https://github.com/scrooloose/syntastic/raw/master/_assets/screenshot_1.png
Bundle "scrooloose/syntastic"

"@utility: Get a complete tree of your undo history
"@image: 'https://a248.e.akamai.net/camo.github.com/02d40619ef8e5559acabb28df959a42e0c783d74/687474703a2f2f6661726d352e7374617469632e666c69636b722e636f6d2f343131332f353039333131343630355f656263343664363439342e6a7067' 
Bundle "sjl/gundo.vim"
"@utility: Replace in all files with :Gsearch and :Greplace
Bundle "skwp/greplace.vim"
"@utility: Make motions easier.
"@image: 'https://a248.e.akamai.net/camo.github.com/311e2034c078b3d7a53497020cda7b3bedda249d/687474703a2f2f6f6935342e74696e797069632e636f6d2f3279797365666d2e6a7067' 
Bundle "skwp/vim-easymotion"
"@image: 'https://a248.e.akamai.net/camo.github.com/63f9947cac196ec7e6e3d790fd3cd1e1463a7b9b/687474703a2f2f692e696d6775722e636f6d2f4d737549422e706e67'
Bundle 'bling/vim-airline'
" Airline is my replacement for powerline
"Bundle "skwp/vim-powerline.git"
"@utility: 'Multiple cursors (Use with Ctrl-N)'
"@image: 'https://github.com/terryma/vim-multiple-cursors/raw/master/assets/example1.gif?raw=true'
Bundle "terryma/vim-multiple-cursors"
"@utility: ',gg to git grep.'
Bundle "tjennings/git-grep-vim"
"@utility: gc to comment (Got tired of NERDCommenter).
Bundle "tomtom/tcomment_vim"
"@utility: End statements automatically.
Bundle "tpope/vim-endwise.git"
"@utility: Tpope's great git plugin.
Bundle "tpope/vim-fugitive"
Bundle "tpope/vim-git"
Bundle "tpope/vim-repeat.git"
"@utility: Make surrounding things easy, try it with `,{` or `,'` to surround
"vselected text with {} or '' appropriately. Look in the keymap file for all
"of my definitions.
Bundle "tpope/vim-surround.git"
"@utility: Ying-Yang options carry the same key prefixed by [ .`]a` is next, `[a`
"is previous for example)
Bundle "tpope/vim-unimpaired"
"@utility: :AnsiEsc to escape ansicodes into vim coloring
" Bundle "vim-scripts/AnsiEsc.vim.git"
"@utility: Automatically ctag when you save
Bundle "vim-scripts/AutoTag.git"
"@utility: Highlight valid tags (Notice all the external items here in the
"linxu kernel are tagged)
"@image: 'http://www.cgtk.co.uk/vim-scripts/taghighlight/images/module_with_ctags_highlighter.png'
Bundle "vim-scripts/TagHighlight.git"
"@utility: 'Zoomwin permits for ,gz, which zooms in and out of a particular
""window so you can temporarily focus.'
Bundle "vim-scripts/ZoomWin"
"@utility: 'Adds argument text objects (i.e, daa to delete an argument, via
"" to delete an inner argument, etc.'
Bundle "vim-scripts/argtextobj.vim"
"@utility: 'Adds motions for CamelCase objects with the capitalized words (W,
""B, etc.) .'
Bundle "vim-scripts/camelcasemotion.git"
"@utility: 'Matchit adds some extended % matching facilties for vim'
Bundle "vim-scripts/matchit.zip.git"
"@utility: 'ShowMarks shows marks in a file (added with 'X where x is any
"char) '

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
"nnoremap ,fb :call WhatsThatPluginAgain()<cr>
