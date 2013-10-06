filetype on " long story.
filetype off

set nocompatible " rube would be proud

syntax enable "turn on syntax highlighting
filetype plugin indent on

" ================ General Config ====================

set number                     " Line numbers are good:
set ruler                      " but where you are is better
set backspace=indent,eol,start " Allow backspace in insert mode
set history=1000               " Store lots of :cmdline history
set showcmd                    " Show incomplete cmds down the bottom
set shortmess=aIoO             " Show short messages, no intro.
set showmode                   " Show current mode down the bottom
set gcr=a:blinkon0             " Disable cursor blink
set visualbell                 " No sounds
set directory-=.               " No .swp/swo etc. in `cwd`
set autoread                   " Reload files changed outside vim
set hidden                     " Hidden Buffers
set encoding=utf8              " Yes, really
set columns=80                 " Set column width at 80
set selection=old              " Stop that annoying thing where vim selects the next line

set list                       " show trailing whitespace
" thanks florian fritz :)
set listchars=tab:▸\ ,trail:▫

" I enjoy the use of comma as my leader
let mapleader=","

" check my spelling
set spelllang=en_us

" The terminal handles colors differently so we need to have a bit of unclean
" logic in another file handle the loading based on if we're in a GUI or not.
" 'desert' or 'default' give what you'd expect, 'solarized_dark' and
" 'solarized_light' are special codes for setting the colorscheme and
" background.
let color_scheme='solarized_light'
" Don't need this w/ solarized, but useful to set sometimes with other params.
"set background=dark

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
" silent !mkdir ~/.vim/backups > /dev/null 2>&1
" set undodir=~/.vim/backups
"
" While it is nice to have this stuff available across boots, tmpfs is a
" better place for this sort of data on a machine with degrading writes.
silent !mkdir /tmp/.backups > /dev/null 2>&1
set undodir=/tmp/.backups
set undofile

" ==================  Ctags  ========================
set tags=./tags;$HOME

" ================ Indentation ======================

set softtabstop=2 " insert mode tab and backspace use 2 spaces
set shiftwidth=2
set tabstop=8     " actual tabs occupy 8 characters
set smarttab

set autoindent   " copy previous line's indent
set smartindent  " intdent intelligently
set expandtab    " I prefer spaces to tabs.
set nowrap       "Don't wrap lines
set linebreak    "Wrap lines at convenient points

" ================ Folds ============================

set foldmethod=indent "fold based on indent
set foldnestmax=3       "deepest fold is 3 levels
set nofoldenable        "dont fold by default

" ================ Completion =======================

" Display candidate supplement.
set nowildmenu
set wildmode=longest,list,full
set wildignore+=*vim/backups*
set wildignore+=*sass-cache*
set wildignore+=*.beam
set wildignore+=vendor/rails/**
set wildignore+=vendor/cache/**
set wildignore+=*.gem
set wildignore+=log/**
set wildignore+=tmp/**
set wildignore+=*.beam
set wildignore+=*.aux,*.out,*.toc
set wildignore+=*.jpg,*.bmp,*.gif,*.png,*.jpeg
set wildignore+=*.luac
set wildignore+=*.o,*.obj,*.exe,*.dll,*.manifest
set wildignore+=*.pyc
set wildignore+=*.spl
set wildignore+=*~,#*#,*.sw?,%*,*=

" Can supplement a tag in a command-line.
set wildoptions=tagfile


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
