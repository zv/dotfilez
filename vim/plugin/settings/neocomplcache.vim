if exists(":NeoComplCacheEnable")
  let g:acp_enableAtStartup = 0
  " Use neocomplcache.
  let g:neocomplcache_enable_at_startup = 1
  " Set minimum syntax keyword length.
  let g:neocomplcache_min_syntax_length = 3
  " Enable smart case
  let g:neocomplcache_enable_smart_case = 1
  " Enable camel case
  let g:neocomplcache_enable_camel_case_completion = 1
  " Enable completion split by _
  let g:neocomplcache_enable_underbar_completion = 1

  let g:neocomplcache_lock_buffer_name_pattern = '\*ku\*'

  " default # of completions is 100, that's crazy
  let g:neocomplcache_max_list = 7

  " Snagged from yasuoza/dotfiles
  let g:neocomplcache_dictionary_filetype_lists = {
      \ 'default' : '',
      \ 'vimshell' : $HOME.'/.vimshell_hist',
      \ 'java' : $HOME.'/.vim/dict/java.dict',
      \ 'c' : $HOME.'/.vim/dict/c.dict',
      \ 'cpp' : $HOME.'/.vim/dict/cpp.dict',
      \ 'javascript' : $HOME.'/.vim/dict/javascript.dict',
      \ 'ocaml' : $HOME.'/.vim/dict/ocaml.dict',
      \ 'perl' : $HOME.'/.vim/dict/perl.dict',
      \ 'php' : $HOME.'/.vim/dict/php.dict',
      \ 'scheme' : $HOME.'/.vim/dict/scheme.dict',
      \ 'vm' : $HOME.'/.vim/dict/vim.dict'
      \ }

  " Cache ourselves here.
  "autocmd BufReadPost,BufEnter,BufWritePost :NeoComplCacheCachingBuffer <buffer>

  " This makes sure we use neocomplcache completefunc instead of
  " the one in rails.vim, otherwise this plugin will crap out
  let g:neocomplcache_force_overwrite_completefunc = 1

  " Recommended key-mappings.
  " <CR>: close popup and save indent.
  inoremap <silent> <CR> <C-r>=<SID>my_cr_function()<CR>
  function! s:my_cr_function()
    " return neocomplcache#smart_close_popup() . "\<CR>"
    " For no inserting <CR> key.
    return pumvisible() ? neocomplcache#close_popup() : "\<CR>"
  endfunction

  " <C-h>, <BS>: close popup and delete backword char.
  inoremap <expr><BS> neocomplcache#smart_close_popup()."\<C-h>"

  " Enable omni completion.
  autocmd FileType css           setlocal omnifunc=csscomplete#CompleteCSS
  autocmd FileType html,markdown setlocal omnifunc=htmlcomplete#CompleteTags
  autocmd FileType javascript    setlocal omnifunc=tern#Complete
  autocmd FileType coffee        setlocal omnifunc=javascriptcomplete#CompleteJS
  autocmd FileType python        setlocal omnifunc=pythoncomplete#Complete
  autocmd FileType ruby          setlocal omnifunc=rubycomplete#Complete
  autocmd FileType erlang        setlocal omnifunc=erlangcomplete#Complete
  autocmd FileType c             setlocal omnifunc=ccomplete#Complete

  if !exists('g:neocomplcache_omni_patterns')
    let g:neocomplcache_omni_patterns = {}
  endif
  " XML?
  " autocmd FileType xml setlocal omnifunc=xmlcomplete#CompleteTags
  let g:neocomplcache_omni_patterns['xml'] = ''

endif
