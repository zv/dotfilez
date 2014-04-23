" Execute one command in normal mode
inoremap <C-l> <C-o>

set pastetoggle=<F2>            " when in insert mode, press <F2> to go to paste mode
nnoremap <F3> :set list!<CR>
nnoremap <F4> :GitGutterToggle<CR>

" Easier to type, and I never use the default behavior.
noremap H ^
noremap L $
vnoremap L g_

imap <D-k> _

inoremap <C-D> <Esc>:call setline(".",substitute(getline(line(".")),'^\s*',matchstr(getline(line(".")-1),'^\s*'),''))<CR>I

" Sneak 1-char inclusive bindings
"replace 'f' with inclusive 1-char Sneak
nmap f <Plug>Sneak_f
nmap F <Plug>Sneak_F
xmap f <Plug>Sneak_f
xmap F <Plug>Sneak_F
omap f <Plug>Sneak_f
omap F <Plug>Sneak_F
"replace 't' with exclusive 1-char Sneak
nmap t <Plug>Sneak_t
nmap T <Plug>Sneak_T
xmap t <Plug>Sneak_t
xmap T <Plug>Sneak_T
omap t <Plug>Sneak_t
omap T <Plug>Sneak_T

"" Vim completion mapping
" Complete filename
inoremap <C-f> <C-x><C-f>
" C-o for omnicomplete
inoremap <C-o> <C-x><C-o>

" Insert word currently under cursor in command mode
cnoremap <C-k> <C-r><C-w>
" tcsh style!
cnoremap <C-a> <Home>

" stop the visual paste insanity
xnoremap p pgvy

" ,ow = 'overwrite word', replace a word with what's in the yank buffer
" FIXME: will not properly repeat when you use a dot (tie into repeat.vim)
nnoremap ,ow "_diwhp

" y copies line
nnoremap Y y$

" Faster substitute.
nnoremap ,S :%s//g<left><left>
vmap ,s :s//g<left><left>

" Space to move a half page down and backspace to reverse
nmap <Space> <C-d>
nmap <BS> <C-u>

" A Classic
cnoremap w!! w !sudo tee % >/dev/null

" Re hard wrap paragraph.
nnoremap ,qw gqip

" Set ourselves as modifiable
nnoremap ,sm :set modifiable<CR>

" Reselect text after indent/unindent.
vnoremap < <gv
vnoremap > >gv

" mnemonic -- 'lose lines'
" This gets rid of all those inexplictable double newlines that keep appearing
" all over my scripts.
nnoremap ,ll :%s#^\n^\n#\r#g<CR>

" Surround with ", ', (), [], {} and #{} by simply ,CHARACTER
""""""""""""""""""""""""""""""""""""""""""""""""""
map ,# ysiw#
vmap ,# c#{<C-R>"}<ESC>
map ," ysiw"
vmap ," c"<C-R>""<ESC>
map ,' ysiw'
vmap ,' c'<C-R>"'<ESC>
map ,( ysiw(
map ,) ysiw)
vmap ,( c( <C-R>" )<ESC>
vmap ,) c(<C-R>")<ESC>
map ,] ysiw]
map ,[ ysiw[
vmap ,[ c[ <C-R>" ]<ESC>
vmap ,] c[<C-R>"]<ESC>
map ,} ysiw}
map ,{ ysiw{
vmap ,} c{ <C-R>" }<ESC>
vmap ,{ c{<C-R>"}<ESC>
"

" give paste a chance (don't overwrite your register with whats pasted)
function! RestoreRegister()
  let @" = s:restore_reg
  return ''
endfunction

function! s:Repl()
    let s:restore_reg = @"
    return "p@=RestoreRegister()\<cr>"
endfunction

" NB: this supports "rp that replaces the selection by the contents of @r
vnoremap <silent> <expr> P <sid>Repl()

" toggle your qfix
nmap <silent> ,qc :cclose<CR>
nmap <silent> ,qo :copen<CR>
nmap <silent> ,lc :lclose<CR>
nmap <silent> ,lo :lopen<CR>

" use ,F to jump to tag in a vertical split
nnoremap <silent> ,F :let word=expand("<cword>")<CR>:vsp<CR>:wincmd w<cr>:exec("tag ". word)<cr>

" use ,gf to go to file in a vertical split
nnoremap <silent> ,gf :vertical botright wincmd f<CR>

" move us around by functions
nnoremap <silent> <C-j> }
nnoremap <silent> <C-k> {

" ==============================
" Window/Tab/Split Manipulation
" ==============================
nnoremap <silent> <C-h> <C-w>h
nnoremap <silent> <C-l> <C-w>l
nnoremap <silent> <C-k> <C-w>k
nnoremap <silent> <C-j> <C-w>j

" Create window splits easier.
nnoremap <silent> vv <C-w>v
" nnoremap <silent> ss <C-w>s

" Resize windows with arrow keys
nnoremap <C-Up>        10<C-w>+
nnoremap <C-Down>      10<C-w>-
nnoremap <C-Left>      10<C-w><
nnoremap <C-Right>     10<C-w>>

" copy current filename into system clipboard - mnemonic: (c)urrent(f)ilename
" this is helpful to paste someone the path you're looking at
nnoremap <silent> ,cf :let @* = expand("%:~")<CR>
nnoremap <silent> ,cn :let @* = expand("%:t")<CR>

"Clear current search highlight by double tapping //
nmap <silent> // :nohlsearch<CR>

" These are very similar keys. Typing 'a will jump to the line in the current
" file marked with ma. However, `a will jump to the line and column marked
" with ma.  It’s more useful in any case I can imagine, but it’s located way
" off in the corner of the keyboard. The best way to handle this is just to
" swap them: http://items.sjbach.com/319/configuring-vim-right
nnoremap ' `
nnoremap ` '

"reload your file here
nmap <silent> ,vr :so %<CR>

" run script
nmap <silent> ,rs yy:<C-f>p<C-c><CR>

" Hit leader a then type a character you want to align by
nmap ,a :Tabularize /
vmap ,a :Tabularize /
