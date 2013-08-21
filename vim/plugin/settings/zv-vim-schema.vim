" Make it beautiful - colors and fonts

if has("gui_running")
  "colorscheme desert 
  colorscheme solarized

  if color_scheme == 'solarized_dark'
    set background=dark
  elseif color_scheme == 'solarized_light'
    set background=light
  end

  set t_Co=256
  set guifont=Monospace\ 11
  set guioptions=agit
  " If you want your toolbar and tabs back just add 
  " set guioptions=aegitTm

  set guiheadroom=0
  " Show tab number (useful for Cmd-1, Cmd-2.. mapping)
  " For some reason this doesn't work as a regular set command,
  " (the numbers don't show up) so I made it a VimEnter event
  autocmd VimEnter * set guitablabel=%N:\ %t\ %M
else

  if color_scheme == 'solarized_dark'
    colorscheme solarized 
    set background=dark
  elseif color_scheme == 'solarized_light'
    colorscheme solarized 
    set background=light
  else
    colorscheme default
    set background=light
  end

  "dont load csapprox if we no gui support - silences an annoying warning
  let g:CSApprox_loaded = 1
endif

match ErrorMsg '^\(<\|=\|>\)\{7\}\([^=].\+\)\?$' " Highlight git cflct markers 
