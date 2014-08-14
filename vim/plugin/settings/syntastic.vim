" ensure our assembly is 64 bitaroos when we have em
if executable('arch')
  if system('arch') == 'x86_64'
    let g:syntastic_nasm_nasm_post_args = '-X gnu -f elf64'
  endif
endif

"mark syntax errors with :signs
let g:syntastic_enable_signs=1
"automatically jump to the error when saving the file
let g:syntastic_auto_jump=0
"show the error list automatically
let g:syntastic_auto_loc_list=0

"don't care about warnings
" let g:syntastic_quiet_warnings=0

" let g:syntastic_check_on_open         = 1
" let g:syntastic_enable_signs          = 0        " Put errors on left side
" let g:syntastic_quiet_warnings        = 1        " Only errors, not warnings please
" let g:syntastic_auto_loc_list         = 2        " Only show errors when I ask
let g:syntastic_disabled_filetypes    = ['html']
" let g:syntastic_cpp_check_header      = 1
" let g:syntastic_cpp_no_include_search = 1
"
let g:syntastic_coffee_checkers     = ['coffee']
let g:syntastic_javascript_checkers = ['jshint']

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
