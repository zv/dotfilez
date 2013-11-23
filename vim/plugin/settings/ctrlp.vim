let g:ctrlp_extensions = ['yanking']
let g:ctrlp_custom_ignore = '\.git$\|\.hg$\|\.svn$'

" Use the git index if we're in a git repo.
let g:ctrlp_user_command = ['.git', 'cd %s && git ls-files . --cached --exclude-standard --others']

let g:ctrlp_cache_dir = '/tmp/.cache/ctrlp'

let g:ctrlp_match_window = 'order:ttb,max:20'

" Default to filename searches - so that appctrl will find application controller
let g:ctrlp_by_filename = 1

" See if we can replace CtrlP w/ CtrlPMixed
"nnoremap ,p :CtrlPMixed<cr>
" Additional mapping for buffer search
" nnoremap ,b :CtrlPBuffer<cr>
" nnoremap ,m :CtrlPMRU<cr>

" Yankring
nnoremap ,yr :CtrlPYankring<CR>

let g:ctrlp_buftag_types = {
      \ 'erlang'     : '--language-force=erlang --erlang-types=drmf',
      \ 'javascript' : {
      \ 'bin': 'jsctags',
      \ 'args': '-f -',
      \ },
\ }
