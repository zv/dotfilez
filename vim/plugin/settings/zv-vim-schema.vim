if has("gui_running")
  " fix these bizzare fqn borders problem in gtk vim
  let s:border = synIDattr(synIDtrans(hlID('Normal')), 'bg', 'gui')
  exe 'silent !echo ''style "vimfix" { bg[NORMAL] = "' . escape(s:border, '#') . '" }'''.
              \' > ~/.gtkrc-2.0'
  exe 'silent !echo ''widget "vim-main-window.*GtkForm" style "vimfix"'''.
              \' >> ~/.gtkrc-2.0'
  " finally! after all these years those borders go away.

  set t_Co=256
  " set guifont=Terminus\ 12
  set guifont=Inconsolata\ 12
  set guioptions=agit
  autocmd VimEnter * set guitablabel=%N:\ %t\ %M
else

  "dont load csapprox if we no gui support - silences an annoying warning
  let g:CSApprox_loaded = 1
endif

match ErrorMsg '^\(<\|=\|>\)\{7\}\([^=].\+\)\?$' " Highlight git cflct markers
