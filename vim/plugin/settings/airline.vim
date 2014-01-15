let g:airline_enable_tagbar    = 1
let g:airline_enable_syntastic = 1
let g:airline_enable_branch    = 1
let g:airline_powerline_fonts  = 0

let g:airline#extensions#tabline#enabled = 1

"" Whitespace Machine
"  enable/disable detection of whitespace errors.
  let g:airline#extensions#whitespace#enabled = 1

"" Tmuxline
let g:tmuxline_powerline_separators = 0
let g:tmuxline_preset = 'powerline'
let g:airline#extensions#tmuxline#snapshot_file = "~/.tmux.snapshot"

let g:tmuxline_separators = {
    \ 'left' : '',
    \ 'left_alt': '»',
    \ 'right' : '',
    \ 'right_alt' : '-',
    \ 'space' : ' '}

if !has("gui_running") && exists(":Tmuxline")
  autocmd VimEnter * :Tmuxline powerline full
endif
