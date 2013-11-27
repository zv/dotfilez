" TODO: `which` the location of ag instead of statically filling it in.
" ag is pretty fast.
set grepprg=/usr/local/bin/ag
let g:agprg="/usr/local/bin/ag --column"


" git grep
nnoremap ,gg :Ggrep ""<left>
" ag
nnoremap ,gr Ag ""<left>
