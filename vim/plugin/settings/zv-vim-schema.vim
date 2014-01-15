if has("gui_running")
  " fix these bizzare fqn borders problem in gtk vim
  let s:border = synIDattr(synIDtrans(hlID('Normal')), 'bg', 'gui')
  exe 'silent !echo ''style "vimfix" { bg[NORMAL] = "' . escape(s:border, '#') . '" }'''.
              \' > ~/.gtkrc-2.0'
  exe 'silent !echo ''widget "vim-main-window.*GtkForm" style "vimfix"'''.
              \' >> ~/.gtkrc-2.0'
  " finally! after all these years those borders go away.

  " Just a quick old switch-a-roo.
  if color_scheme == 'solarized_dark'
    colorscheme solarized
    set background=dark
  elseif color_scheme == 'solarized_light'
    colorscheme solarized
    set background=light
  else
    " a whack hack, cannot provide vimscript args to colorscheme.
    execute 'colorscheme '.color_scheme
  end

  set t_Co=256
  " set guifont=Terminus\ 12
  set guifont=Inconsolata\ 11
  set guioptions=agit
  autocmd VimEnter * set guitablabel=%N:\ %t\ %M
else
  " colorscheme desert
  colorscheme solarized
  set background=dark
  "dont load csapprox if we no gui support - silences an annoying warning
  let g:CSApprox_loaded = 1
endif

match ErrorMsg '^\(<\|=\|>\)\{7\}\([^=].\+\)\?$' " Highlight git cflct markers
