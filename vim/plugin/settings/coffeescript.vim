" fix for terminal  
"au BufWritePost *.coffee silent CoffeeMake! -b | cwindow | redraw!
"autocmd BufWritePost,FileWritePost *.html call !coffeelint % | cwindow | redraw!
let coffee_compiler = '/usr/local/bin/coffee'
let coffee_linter     = '/usr/local/bin/coffeelint'
map <leader>cl :redir @a<CR>:silent !coffeelint %<CR>:redir END<CR>:new<CR>:put! a<CR>:AnsiEsc<CR>:%s/^M$//g<CR>
