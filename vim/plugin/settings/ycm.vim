if exists(":YcmCompleter")
  " These are the tweaks I apply to YCM's config, you don't need them but they might help.
  " YCM gives you popups and splits by default that some people might not like, so these should tidy it up a bit for you.

  let g:ycm_key_list_select_completion = ['<C-n>', '<Down>']
  let g:ycm_key_list_previous_completion = ['<C-p>', '<Up>']
  let g:ycm_key_invoke_completion = '<C-Space>'
  " let g:ycm_global_ycm_extra_conf = '~/.vim/plugin/settings/ycm_extra_conf.py'



  " Automatically close the scratch buffer / 'preview window'
  let g:ycm_autoclose_preview_window_after_completion = 1
endif
