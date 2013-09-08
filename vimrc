filetype on " long story.
filetype off

set nocompatible " rube would be proud

syntax enable "turn on syntax highlighting
filetype plugin indent on

" ================ General Config ====================

set number                       " Line numbers are good:
set ruler " but where you are is better
set backspace=indent,eol,start " Allow backspace in insert mode
set history=1000               " Store lots of :cmdline history
set showcmd                      " Show incomplete cmds down the bottom
set shortmess=aIoO             " Show short messages, no intro.
set showmode                     " Show current mode down the bottom
set directory-=.                 " I don't want my swap files in `cwd`!
set gcr=a:blinkon0             " Disable cursor blink
set visualbell                   " No sounds
set autoread                     " Reload files changed outside vim
set hidden                       " Hidden Buffers
set encoding=utf8              " Yes, really
set columns=80                 " Set column width at 80
set selection=old              " Stop that annoying thing where vim selects the next line
set clipboard=unnamed          " yank and paste with the system clipboard where normal systems don't do this

set list                                                     " show trailing whitespace
set listchars=tab:▸\ ,trail:▫



" Sets light/dark background
let color_scheme='solarized_dark'

" =============== Vundle Initialization ===============
if filereadable(expand("~/.vim/vundles.vim"))
  source ~/.vim/vundles.vim
endif

" ================ Search Settings  =================
set incsearch        "Find the next match as we type the search
set hlsearch         "Hilight searches by default
set viminfo='100,f1  "Save up to 100 marks, enable capital marks
set smartcase        "Smart Case Search
set ignorecase       "Ignore Case

" ================ Persistent Undo ==================
" Keep undo history across sessions, by storing in file.

silent !mkdir ~/.vim/backups > /dev/null 2>&1
set undodir=~/.vim/backups
set undofile

" ==================  Ctags  ========================
set tags=./tags;$HOME

" ================ Indentation ======================

set shiftwidth=2
set softtabstop=2 " insert mode tab and backspace use 2 spaces
set tabstop=2     " actual tabs occupy 8 characters

set autoindent
set smartindent
set smarttab

set expandtab
set nowrap       "Don't wrap lines
set linebreak    "Wrap lines at convenient points

" ================ Folds ============================

set foldmethod=indent "fold based on indent
set foldnestmax=3       "deepest fold is 3 levels
set nofoldenable        "dont fold by default

" ================ Completion =======================

" ================ Scrolling ========================

set scrolloff=3         "Start scrolling when we're 8 lines away from margins
set sidescrolloff=15
set sidescroll=1

" ================ External Settings ========================
for f in split(glob('~/.vim/plugin/settings/*.vim'), '\n')
  exe 'source' f
endfor

" ================ Vim as MAN ========================
runtime ftplugin/man.vim
