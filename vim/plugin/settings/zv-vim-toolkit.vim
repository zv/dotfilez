""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Change ruby hash syntax - :ChangeHashSyntax
" Jump to tag in vertical split - ,F
" Open Changed files - ,ocf
" Strip trailing whitespace - ,w
"
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""" Toolkit index

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

command! SnakeConv :call ConvertToSnake()
command! CamelConv :call ConvertToCamel()
command! SnakeConvLine :call ConvertLineToSnake()
command! CamelConvLine :call ConvertLineToCamel()

" Space parens before and after func args
function! SpaceFuncArgs()
  execute '%s/(\(\S.*\S\))/( \1 )/g'
endfunction

command! ArgSpace :call SpaceFuncArgs()
command! SpaceArgs :call SpaceFuncArgs()

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" git_diff_aware_gf
" Gdiff aware gf!
"
" Shamelessly borrowed from yasuaoza
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
nnoremap <expr> gf  <SID>do_git_diff_aware_gf('gf')
nnoremap <expr> gF  <SID>do_git_diff_aware_gf('gF')
nnoremap <expr> <C-w>f  <SID>do_git_diff_aware_gf('<C-w>f')
nnoremap <expr> <C-w><C-f>  <SID>do_git_diff_aware_gf('<C-w><C-f>')
nnoremap <expr> <C-w>F  <SID>do_git_diff_aware_gf('<C-w>F')
nnoremap <expr> <C-w>gf  <SID>do_git_diff_aware_gf('<C-w>gf')
nnoremap <expr> <C-w>gF  <SID>do_git_diff_aware_gf('<C-w>gF')

function! s:do_git_diff_aware_gf(command)
  let target_path = expand('<cfile>')
  if target_path =~# '^[ab]/'  " with a peculiar prefix of git-diff(1)?
    if filereadable(target_path) || isdirectory(target_path)
      return a:command
    else
      " BUGS: Side effect - Cursor position is changed.
      let [_, c] = searchpos('\f\+', 'cenW')
      return c . '|' . 'v' . (len(target_path) - 2 - 1) . 'h' . a:command
    endif
  else
    return a:command
  endif
endfunction

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Diff saved version and current buffer
"
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

function! s:DiffWithSaved()
  let filetype=&ft
  diffthis
  vnew | r # | normal! 1Gdd
  diffthis
  exe "setlocal bt=nofile bh=wipe nobl noswf ro ft=" . filetype
endfunction
com! DiffSaved call s:DiffWithSaved()

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Identify what a bundle plugin is by hovering over it.
"
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
function! WhatsThatPluginAgain()
  let uri = matchstr(getline("."), '^Bundle ')
  echo uri
  if uri != ""
    let isbundle = substitute(getline("."), '^Bundle "', "", 'g')
    let unbundled = substitute(isbundle, '"', "", 'g')
    let unbundled = substitute(unbundled, '\.git', "", 'g')
    " let unbundled = substitute(unbundled, '\.vim', "", 'g')
    let url = substitute(unbundled, "^", "https://github.com/", 'g')
    silent exec "!google-chrome '".url."'"
  else
    echo "I can't even pretend this line has a URL in it"
  endif
endfunction

" ,fb for find bundle
nnoremap ,fb :call WhatsThatPluginAgain()<cr>


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Use Q to intelligently close a window
" (if there are multiple windows into the same buffer)
" or kill the buffer entirely if it's the last window looking into that buffer
"
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
function! CloseWindowOrKillBuffer()
  let number_of_windows_to_this_buffer = len(filter(range(1, winnr('$')), "winbufnr(v:val) == bufnr('%')"))

  " We should never bdelete a nerd tree
  if matchstr(expand("%"), 'NERD') == 'NERD'
    wincmd c
    return
  endif

  if number_of_windows_to_this_buffer > 1
    wincmd c
  else
    bdelete
  endif
endfunction

nnoremap <C-q><C-k> :q!<CR>

" nnoremap <silent> Q :call CloseWindowOrKillBuffer()<CR>


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Insert multiple inclusion gates for IFDEFs (h, hpp)
"
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
function! s:insert_gates()
  let gatename = substitute(toupper(expand("%:t")), "\\.", "_", "g")
  execute "normal! i#ifndef " . gatename
  execute "normal! o#define " . gatename . " "
  execute "normal! Go#endif /* " . gatename . " */"
  normal! kk
endfunction
autocmd BufNewFile *.{h,hpp} call <SID>insert_gates()

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Prevent everything from going wrong when I load a huge file.
"
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Protect large files from sourcing and other overhead.
" Files become read only
if !exists("my_auto_commands_loaded")
  let my_auto_commands_loaded = 1
  " Large files are > 10M
  " Set options:
  " eventignore+=FileType (no syntax highlighting etc
  " assumes FileType always on)
  " noswapfile (save copy of file)
  " bufhidden=unload (save memory when other file is viewed)
  " buftype=nowritefile (is read-only)
  " undolevels=-1 (no undo possible)
  let g:LargeFile = 1024 * 1024 * 10
  augroup LargeFile
    autocmd BufReadPre * let f=expand("<afile>") | if getfsize(f) > g:LargeFile | set eventignore+=FileType | setlocal noswapfile bufhidden=unload buftype=nowrite undolevels=-1 | else | set eventignore-=FileType | endif
    augroup END
  endif
