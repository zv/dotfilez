" Make nerdtree look nice
let NERDTreeMinimalUI = 1
let NERDTreeDirArrows = 0
let g:NERDTreeWinSize = 30

" Close if NERDTree is only open 
autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTreeType") && b:NERDTreeType == "primary") | q | endif
