let g:ctrlp_custom_ignore = '\.git$\|\.hg$\|\.svn$'
" Use the git index if we're in a git repo.
let g:ctrlp_user_command = ['.git', 'cd %s && git ls-files . --cached --exclude-standard --others']

" Default to filename searches - so that appctrl will find application
" controller
let g:ctrlp_by_filename = 1

" We don't want to use Ctrl-p as the mapping because
" it interferes with YankRing (paste, then hit ctrl-p)
let g:ctrlp_map = ',t'

" Additional mapping for buffer search
nnoremap ,b :CtrlPBuffer<cr>
nnoremap <C-b> :CtrlPBuffer<cr>

"Cmd-(m)ethod - jump to a method (tag in current file)
map ,m :CtrlPBufTag<CR>

"Ctrl-(M)ethod - jump to a method (tag in all files)
map ,M :CtrlPBufTagAll<CR>
