" These are the tweaks I apply to YCM's config, you don't need them but they might help.
" YCM gives you popups and splits by default that some people might not like, so these should tidy it up a bit for you.

" I think this should be at least 3.
let g:ycm_min_num_of_chars_for_completion = 3

" Unlimited is pretty ridiculous, We should top out at 25
let g:ycm_min_num_identifier_candidate_chars = 25

" This is disabled by default because of the slowness associated with reading
" ctags off of the network.
let g:ycm_collect_identifiers_from_tags_files = 1

let g:ycm_global_ycm_extra_conf = '~/.vim/plugin/settings/ycm_extra_conf.py'

" Automatically close the scratch buffer / 'preview window'
let g:ycm_autoclose_preview_window_after_completion = 1
