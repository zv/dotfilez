let g:airline_enable_tagbar = 1
let g:airline_enable_syntastic = 1
let g:airline_theme = 'solarized'

let g:airline_enable_branch = 1

if has("gui_running")
  if has('multi_byte')
    let g:airline_left_sep = ''
    let g:airline_left_alt_sep = ''
    let g:airline_right_sep = ''
    let g:airline_right_alt_sep = ''
    let g:airline_branch_prefix = ' '
    let g:airline_readonly_symbol = ''
    let g:airline_linecolumn_prefix = ' '
  endif
else
  let g:airline_left_sep = '»'
  let g:airline_left_sep = '▶'
  let g:airline_right_sep = '«'
  let g:airline_right_sep = '◀'
  let g:airline_linecolumn_prefix = 'lf '
  let g:airline_linecolumn_prefix = 'nl '
  let g:airline_linecolumn_prefix = '¶ '
  let g:airline_branch_prefix = '->'
  let g:airline_paste_symbol = 'ρ'
  let g:airline_paste_symbol = 'Þ'
  let g:airline_paste_symbol = '∥'
endif
