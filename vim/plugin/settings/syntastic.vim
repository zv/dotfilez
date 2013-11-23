let g:syntastic_check_on_open         = 1
let g:syntastic_enable_signs          = 1        " Put errors on left side
let g:syntastic_quiet_warnings        = 1        " Only errors, not warnings please
let g:syntastic_auto_loc_list         = 2        " Only show errors when I ask
let g:syntastic_disabled_filetypes    = ['html']
let g:syntastic_cpp_check_header      = 1
let g:syntastic_cpp_no_include_search = 1
let g:syntastic_coffee_checkers       = ['coffee']

if has('unix')
  let g:syntastic_error_symbol         = '★'
  let g:syntastic_style_error_symbol   = '>'
  let g:syntastic_warning_symbol       = '⚠'
  let g:syntastic_style_warning_symbol = '>'
else
  let g:syntastic_error_symbol         = '!'
  let g:syntastic_style_error_symbol   = '>'
  let g:syntastic_warning_symbol       = '.'
  let g:syntastic_style_warning_symbol = '>'
endif


