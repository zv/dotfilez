" ==== NERD tree
" Leader-Shift-N for nerd tree
nmap ,N :NERDTreeToggle<CR>

" Make nerdtree look nice
let NERDTreeMinimalUI = 1
let NERDTreeDirArrows = 0
let g:NERDTreeWinSize = 35

" Close if NERDTree is only open 
autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTreeType") && b:NERDTreeType == "primary") | q | endif

" Disable the scrollbars (NERDTree)
set guioptions-=r
set guioptions-=L
