let g:ctrlp_extensions = ['yanking']
let g:ctrlp_custom_ignore = '\.git$\|\.hg$\|\.svn$'

" Use the git index if we're in a git repo.
let g:ctrlp_user_command = ['.git', 'cd %s && git ls-files . --cached --exclude-standard --others']

let g:ctrlp_match_window = 'order:ttb,max:20'

" Default to filename searches - so that appctrl will find application controller
let g:ctrlp_by_filename = 1

" We don't want to use Ctrl-p as the mapping because
" it interferes with YankRing (paste, then hit ctrl-p)
let g:ctrlp_map = ',t'

" Additional mapping for buffer search
nnoremap ,b :CtrlPBuffer<cr>
" Tags (4 letters) 
map ,4 :CtrlPBufTag<CR>
" Yankring
map ,yr :CtrlPYankring<CR>

