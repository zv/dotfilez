" TODO: `which` the location of ag instead of statically filling it in.
" ag is pretty fast.
set grepprg=/usr/local/bin/ag

" git grep
nnoremap ,gg :Ggrep ""<left>
" ag
nnoremap ,gr :Ack ""<left>
