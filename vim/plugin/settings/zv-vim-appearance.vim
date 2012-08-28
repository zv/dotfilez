" Make it beautiful - colors and fonts
" 
set t_Co=256

if has("gui_running")
  "tell the term has 256 colors
  
  " http://ethanschoonover.com/solarized/vim-colors-solarized
  colorscheme default 
  
  set guioptions=agit
  " If you want your toolbar and tabs back just add 
  " set guioptions=aegitTm

  set guiheadroom=0
  " Show tab number (useful for Cmd-1, Cmd-2.. mapping)
  " For some reason this doesn't work as a regular set command,
  " (the numbers don't show up) so I made it a VimEnter event
  autocmd VimEnter * set guitablabel=%N:\ %t\ %M

  set lines=60
  set columns=190

  set guifont=Terminus\ 12
else
  "dont load csapprox if we no gui support - silences an annoying warning
  let g:CSApprox_loaded = 1
endif

