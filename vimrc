set nocompatible " rube would be proud
syntax enable "turn on syntax highlighting
filetype plugin indent on
let mapleader=","

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
set relativenumber
set list                       " show trailing whitespace
set lazyredraw "  Lazy is as lazy does!

" thanks florian fritz :)
set listchars=tab:▸\ ,trail:▫

set pastetoggle=<F2>            " when in insert mode, press <F2> to go to paste mode

set switchbuf=useopen           " reveal already opened files from the
                                " quickfix window instead of opening new
                                " buffers

" check my spelling
set spelllang=en_us

" Long lines make syntax reallyyyy slow.
set synmaxcol=512

" The terminal handles colors differently so we need to have a bit of unclean
" logic in another file handle the loading based on if we're in a GUI or not.
" 'desert' or 'default' give what you'd expect, 'solarized_dark' and
" 'solarized_light' are special codes for setting the colorscheme and
" background.
let color_scheme='solarized_dark'
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

" ================ Swap & Persistent Undo ==================
" Keep undo history across sessions, by storing in file.
" silent !mkdir ~/.vim/backups > /dev/null 2>&1
" set undodir=~/.vim/backups
"
" While it is nice to have this stuff available across boots, tmpfs is a
" better place for this sort of data on a machine with degrading writes.
set undofile
set backup
set noswapfile " welcome to the year 2000.

silent !mkdir /tmp/.backups > /dev/null 2>&1
silent !mkdir /tmp/.undodir > /dev/null 2>&1
set undodir=/tmp/.undodir
set backupdir=/tmp/.backups
set directory=/tmp/.backups
set backupskip=/tmp/*,/private/tmp/*

" ==================  Ctags  ========================
set tags=./tags;$HOME

" ================ Indentation ======================

set softtabstop=2 " insert mode tab and backspace use 2 spaces
set shiftwidth=2
set tabstop=8     " actual tabs occupy 8 characters
set smarttab      " tab inserts tab when already tabbed!
set autoindent    " copy previous line's indent
set smartindent   " intdent intelligently
set expandtab     " I prefer spaces to tabs.
set nowrap        " Don't wrap lines
set linebreak     " Wrap lines at convenient points
set cindent       " start c lang indent mode

" thanks lang-guides!
if has("autocmd")
  autocmd FileType apache     setlocal sw=4 sts=4 ts=4 et
  autocmd FileType aspvbs     setlocal sw=4 sts=4 ts=4 et
  autocmd FileType cpp        setlocal sw=4 sts=4 ts=4 et
  autocmd FileType c          setlocal sw=4 sts=4 ts=4 et
  autocmd FileType cs         setlocal sw=4 sts=4 ts=4 et
  autocmd FileType css        setlocal sw=2 sts=2 ts=2 et
  autocmd FileType diff       setlocal sw=4 sts=4 ts=4 et
  autocmd FileType erlang     setlocal sw=2 sts=2 ts=2 et
  autocmd FileType eruby      setlocal sw=4 sts=4 ts=4 et
  autocmd FileType go         setlocal sw=4 sts=4 ts=4 et
  autocmd FileType haml       setlocal sw=2 sts=2 ts=2 et
  autocmd FileType html       setlocal sw=2 sts=2 ts=2 et
  autocmd FileType javascript setlocal sw=2 sts=2 ts=2 et
  autocmd FileType java       setlocal sw=4 sts=4 ts=4 et
  autocmd FileType perl       setlocal sw=4 sts=4 ts=4 et
  autocmd FileType php        setlocal sw=4 sts=4 ts=4 et
  autocmd FileType python     setlocal sw=4 sts=4 ts=4 et
  autocmd FileType ruby       setlocal sw=2 sts=2 ts=2 et
  autocmd FileType scala      setlocal sw=2 sts=2 ts=2 et
  autocmd FileType sh         setlocal sw=4 sts=4 ts=4 et
  autocmd FileType sql        setlocal sw=4 sts=4 ts=4 et
  autocmd FileType vb         setlocal sw=4 sts=4 ts=4 et
  autocmd FileType vim        setlocal sw=2 sts=2 ts=2 et
  autocmd FileType wsh        setlocal sw=4 sts=4 ts=4 et
  autocmd FileType xhtml      setlocal sw=4 sts=4 ts=4 et
  autocmd FileType xml        setlocal sw=4 sts=4 ts=4 et
  autocmd FileType yaml       setlocal sw=2 sts=2 ts=2 et
  autocmd FileType zsh        setlocal sw=4 sts=4 ts=4 et
endif


" ================ Folds ============================

set nofoldenable        "dont fold by default
set foldmethod=indent "fold based on indent
set foldnestmax=3       "deepest fold is 3 levels

" ================ Completion =======================

" The default is ".,w,b,u,t,i", which means to scan:
"    1. the current buffer
"    2. buffers in other windows
"    3. other loaded buffers
"    4. unloaded buffers
"    5. tags
"    6. included files
set complete=.,w,b,u,t
" I want to only match the longest substring so I can type to complete.
set completeopt=longest,menuone,preview
" Maximum is infinite entries, I am ok with just 25 completions.
set pumheight=35

" Display candidate supplement.
set wildmenu
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
set wildignore+=.hg,.git,.svn
set wildignore+=*.DS_Store    " OSX bullshit
set wildignore+=migrations    " Django migrations
set wildignore+=*.pyc         " Python byte code
set wildignore+=*.orig        " Merge resolution files


" Can supplement a tag in a command-line.
set wildoptions=tagfile

" ================ Scrolling ========================

set scrolloff=3     "Start scrolling when we're 8 lines away from margins
set sidescrolloff=15
set sidescroll=1

" ================ External Settings ========================
for f in split(glob('~/.vim/plugin/settings/*.vim'), '\n')
  exe 'source' f
endfor

" ================ Vim as MAN ========================
runtime ftplugin/man.vim
