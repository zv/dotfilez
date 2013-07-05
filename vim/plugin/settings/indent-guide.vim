let g:indent_guides_auto_colors = 0
autocmd VimEnter,Colorscheme * :hi IndentGuidesOdd  guibg=white ctermbg=3
autocmd VimEnter,Colorscheme * :hi IndentGuidesEven guibg=black ctermbg=4
let g:indent_guides_start_level = 2
let g:indent_guides_guide_size = 1
