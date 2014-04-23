let b:javascript_fold=0

function! UseRegExpEngine()
 set regexpengine=1
 syntax enable
endfunction

nnoremap <F3> :call UseRegExpEngine()<CR>
