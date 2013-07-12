let mapleader=","

" stop the visual paste insanity 
xnoremap p pgvy

" alias yw to yank the entire word 'yank inner word'
nnoremap ,yw yiww

" ,ow = 'overwrite word', replace a word with what's in the yank buffer
" FIXME: will not properly repeat when you use a dot (tie into repeat.vim)
nnoremap ,ow "_diwhp

" y copies line 
nnoremap Y y$

" Faster substitute.
nnoremap ,S :%s//g<left><left>

" Re hard wrap paragraph.
nnoremap ,qw gqip

" Reselect text ater indent/unindent.
vnoremap < <gv
vnoremap > >gv

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

" find your file in the nerdtree 
nnoremap <silent> <C-\> :NERDTreeFind<CR>

" grep in a git repoz :)
nnoremap ,gg :GitGrep ""<left>

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

" Zoom in and out of current window with ,gz
map <silent> ,gz <C-w>o

" Create window splits easier. 
nnoremap <silent> vv <C-w>v
nnoremap <silent> ss <C-w>s

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

" turn {\n} into {} 
nmap sj :SplitjoinSplit<cr>

"turn {} into { " }
nmap sk :SplitjoinJoin<cr>
