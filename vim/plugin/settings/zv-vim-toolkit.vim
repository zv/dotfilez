" Toolkit index
" Change ruby hash syntax - :ChangeHashSyntax
" Jump to tag in vertical split - ,F
" Open Changed files - ,ocf
" Strip trailing whitespace - ,w

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

command! ConvertSnake :call ConvertToSnake()
command! ConvertCamel :call ConvertToCamel()
command! ConvertSnakeLine :call ConvertLineToSnake()
command! ConvertCamelLine :call ConvertLineToCamel()

" Space parens before and after func args
function! SpaceFuncArgs()
  execute '%s/(\(\S.*\S\))/( \1 )/g'
endfunction

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
