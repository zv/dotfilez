set nocompatible " rube would be proud

" ================ General Config ====================

set number                     " Line numbers are good:
set backspace=indent,eol,start " Allow backspace in insert mode
set history=1000               " Store lots of :cmdline history
set showcmd                    " Show incomplete cmds down the bottom
set shortmess=aIoO             " Show short messages, no intro.
set showmode                   " Show current mode down the bottom
set gcr=a:blinkon0             " Disable cursor blink
set visualbell                 " No sounds
set autoread                   " Reload files changed outside vim
set hidden                     " Hidden Buffers
set encoding=utf8              " Yes, really
set columns=80                 " Set column width at 80
set selection=old                " Stop that annoying thing where vim selects the next line
set virtualedit+=block           " permit virtual edits

syntax on "turn on syntax highlighting

" Sets light/dark background
let color_scheme = 'solarized_dark'

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

" ================ Turn Off Swap Files ==============
set noswapfile
set nobackup
set nowb

" ================ Persistent Undo ==================
" Keep undo history across sessions, by storing in file.

silent !mkdir ~/.vim/backups > /dev/null 2>&1
set undodir=~/.vim/backups
set undofile

" ==================  Ctags  ========================
set tags=./tags;$HOME

" ================ Indentation ======================
filetype plugin on
filetype indent on
set autoindent
set smartindent
set smarttab
set shiftwidth=2
set softtabstop=2
set tabstop=2
set expandtab
set nowrap       "Don't wrap lines
set linebreak    "Wrap lines at convenient points

" ================ Completion =======================

set wildmode=list:longest,
set wildmenu                "enable ctrl-n and ctrl-p to scroll thru matches
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
"

" ================ Scrolling ========================

set scrolloff=8         "Start scrolling when we're 8 lines away from margins
set sidescrolloff=15 
set sidescroll=1

" ================ External Settings ========================
for f in split(glob('~/.vim/plugin/settings/*.vim'), '\n')
  exe 'source' f
endfor

" ================ Vim as MAN ========================
runtime ftplugin/man.vim
