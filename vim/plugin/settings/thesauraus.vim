let g:online_thesaurus_map_keys = 0
nnoremap ,j :OnlineThesaurusLookup<CR>



"
" for a Bundle.
" function! WhatsThatPluginAgain()
"   let uri = matchstr(getline("."), '^Bundle ')
"   echo uri
"   if uri != ""
"     let isbundle = substitute(getline("."), '^Bundle "', "", 'g')
"     let unbundled = substitute(isbundle, '"', "", 'g')
"     let unbundled = substitute(unbundled, '\.git', "", 'g')
"     " let unbundled = substitute(unbundled, '\.vim', "", 'g')
"     let url = substitute(unbundled, "^", "https://github.com/", 'g')
"     silent exec "!google-chrome '".url."'"
"   else
"     echo "I can't even pretend this line has a URL in it"
"   endif
" endfunction
" 
" ,fb for find bundle
"nnoremap ,fb :call WhatsThatPluginAgain()<cr>
