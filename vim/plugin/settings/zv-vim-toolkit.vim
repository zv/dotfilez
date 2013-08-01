" Toolkit index 
" Change ruby hash syntax - :ChangeHashSyntax
" Jump to tag in vertical split - ,F
" Open Changed files - ,ocf
" Strip trailing whitespace - ,w
" Intelligently close window - Q
" Automatically append semicolon - filetypes: c, p

" Change ruby hash syntax 1.9 to 2.0 hash syntax
function! s:ChangeHashSyntax(line1,line2)
    let l:save_cursor = getpos(".")
    silent! execute ':' . a:line1 . ',' . a:line2 . 's/\([^:]\):\([a-z0-9_]\+\)\s\+=>/\1\2:/g'
    call setpos('.', l:save_cursor)
endfunction

command! -range=% ChangeHashSyntax call <SID>ChangeHashSyntax(<line1>,<line2>)

" use ,F to jump to tag in a vertical split
nnoremap <silent> ,F :let word=expand("<cword>")<CR>:vsp<CR>:wincmd w<cr>:exec("tag ". word)<cr>

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" OpenChangedFiles COMMAND
" Open a split for each dirty file in git
"
" Shamelessly stolen from Gary Bernhardt: https://github.com/garybernhardt/dotfiles
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
function! OpenChangedFiles()
  only " Close all windows, unless they're modified
  let status = system('git status -s | grep "^ \?\(M\|A\)" | cut -d " " -f 3')
  let filenames = split(status, "\n")
  if len(filenames) > 0
    exec "edit " . filenames[0]
    for filename in filenames[1:]
      exec "sp " . filename
    endfor
  end
endfunction
command! OpenChangedFiles :call OpenChangedFiles()

nnoremap ,ocf :OpenChangedFiles<CR>


" via: http://rails-bestpractices.com/posts/60-remove-trailing-whitespace
" Strip trailing whitespace
function! <SID>StripTrailingWhitespaces()
    " Preparation: save last search, and cursor position.
    let _s=@/
    let l = line(".")
    let c = col(".")
    " Do the business:
    %s/\s\+$//e
    " Clean up: restore previous search history, and cursor position
    let @/=_s
    call cursor(l, c)
endfunction
command! StripTrailingWhitespaces call <SID>StripTrailingWhitespaces()
nmap ,w :StripTrailingWhitespaces<CR>


" Use Q to intelligently close a window
function! CloseWindowOrKillBuffer()
  let number_of_windows_to_this_buffer = len(filter(range(1, winnr('$')), "winbufnr(v:val) == bufnr('%')"))

  if number_of_windows_to_this_buffer > 1
    wincmd c
  else
    bdelete
  endif
endfunction

nnoremap <silent> Q :call CloseWindowOrKillBuffer()<CR>


" If there isn't one, append a semi colon to the end of the current line.
function! s:appendSemiColon()
  if getline('.') !~ ';$'
    let original_cursor_position = getpos('.')
    exec("s/$/;/")
    call setpos('.', original_cursor_position)
  endif
endfunction

" For programming languages using a semi colon at the end of statement.
autocmd FileType c,cpp,css,java,javascript,perl,jade nmap <silent> ;; :call <SID>appendSemiColon()<CR>
autocmd FileType c,cpp,css,java,javascript,perl,jade inoremap <silent> ;; <ESC>:call <SID>appendSemiColon()<CR>a



" Convert Items in selection to Camel/Snake case 
"
":tabe
function! ConvertToSnake()
  execute '%s#\C\(\<\u[a-z0-9]\+\|[a-z0-9]\+\)\(\u\)#\l\1_\l\2#g' 
endfunction
function! ConvertToCamel()
  execute '%s#_\(\l\)#\u\1#g'
endfunction

function! ConvertLineToSnake()
  execute 's#\C\(\<\u[a-z0-9]\+\|[a-z0-9]\+\)\(\u\)#\l\1_\l\2#g' 
endfunction
function! ConvertLineToCamel()
  execute 's#_\(\l\)#\u\1#g'
endfunction

command! ConvertSnake :call ConvertToSnake()
command! ConvertCamel :call ConvertToCamel()
command! ConvertSnakeLine :call ConvertLineToSnake()
command! ConvertCamelLine :call ConvertLineToCamel()
