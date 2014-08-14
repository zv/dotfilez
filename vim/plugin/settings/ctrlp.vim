let g:ctrlp_extensions = ['yankring', 'changes']

" See if we can replace CtrlP w/ CtrlPMixed
nnoremap ,p :CtrlPMixed<cr>
" Additional mapping for buffer search
nnoremap ,b :CtrlPBuffer<cr>

" Yankring (no longer can use ctrlp because of kdl threading changes, thanks
" obama)
command! CtrlPYankring call ctrlp#init(ctrlp#yankring#id())
nnoremap ,yr :CtrlPYankring<CR>

" Use the git index if we're in a git repo.
let g:ctrlp_user_command = ['.git', 'cd %s && git ls-files . --cached --exclude-standard --others']

let g:ctrlp_cache_dir = '/tmp/.cache/ctrlp'
let g:ctrlp_use_caching = 1
let g:ctrlp_match_window = 'order:ttb,max:20'
let g:ctrlp_cache_dir = '/tmp/.cache/ctrlp'

" Default to filename searches - so that appctrl will find application controller
let g:ctrlp_by_filename = 1

let g:ctrlp_custom_ignore = {
    \ 'dir':  '\v[\/]\.(git|hg|svn)$',
    \ 'file': '\v\.(exe|so|dll)$',
\ }
