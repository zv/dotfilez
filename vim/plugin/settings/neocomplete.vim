if exists(":NeoCompleteEnable")
  let g:acp_enableAtStartup = 0
  " Use neocomplete.  
  let g:neocomplete#enable_at_startup = 1
  " Use smartcase.
  let g:neocomplete#enable_smart_case = 1

  " Set minimum syntax keyword length.
  let g:neocomplete#sources#syntax#min_keyword_length = 3

  let g:neocomplete#auto_completion_start_length = 2

  let g:neocomplete#lock_buffer_name_pattern = '\*ku\*'
  let g:neocomplete#sources#buffer#disabled_pattern = '\.log'

  " Use smartcase.
  let g:neocomplete#enable_smart_case       = 1
  " Use fuzzy completion.
  let g:neocomplete#enable_fuzzy_completion = 1

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

  " Define keyword.
  if !exists('g:neocomplete#keyword_patterns')
      let g:neocomplete#keyword_patterns = {}
  endif

  let g:neocomplete#keyword_patterns['default'] = '\h\w*'

  if !exists('g:neocomplete#delimiter_patterns')
    let g:neocomplete#delimiter_patterns= {}
  endif
  let g:neocomplete#delimiter_patterns.vim = ['#']
  let g:neocomplete#delimiter_patterns.cpp = ['::']
  let g:neocomplete#delimiter_patterns.ruby = ['.', '::']


  " Plugin key-mappings.
  inoremap <expr><C-u>     neocomplete#undo_completion()
  inoremap <expr><C-l>     neocomplete#complete_common_string()

  " Recommended key-mappings.
  " <CR>: close popup and save indent.
  inoremap <silent> <CR> <C-r>=<SID>my_cr_function()<CR>
  function! s:my_cr_function()
    return neocomplete#smart_close_popup() . "\<CR>"
    " For no inserting <CR> key.
    "return pumvisible() ? neocomplete#close_popup() : "\<CR>"
  endfunction
  " <TAB>: completion.
  inoremap <expr><TAB>  pumvisible() ? "\<C-n>" : "\<TAB>"
  " <C-h>, <BS>: close popup and delete backword char.
  inoremap <expr><C-h> neocomplete#smart_close_popup()."\<C-h>"
  inoremap <expr><BS> neocomplete#smart_close_popup()."\<C-h>"
  inoremap <expr><C-y>  neocomplete#close_popup()
  inoremap <expr><C-e>  neocomplete#cancel_popup()

  " Enable omni completion.
  autocmd FileType css setlocal omnifunc=csscomplete#CompleteCSS
  autocmd FileType html,markdown setlocal omnifunc=htmlcomplete#CompleteTags
  autocmd FileType javascript setlocal omnifunc=javascriptcomplete#CompleteJS
  autocmd FileType python setlocal omnifunc=pythoncomplete#Complete
  autocmd Filetype c,cpp  setlocal omnifunc=omni#cpp#complete#Main
  autocmd FileType erlang setlocal omnifunc=erlangcomplete#Complete
  autocmd FileType ruby setlocal omnifunc=rubycomplete#Complete

  " if !exists('g:neocomplete#force_omni_input_patterns')
  "   let g:neocomplete#force_omni_input_patterns = {}
  " endif

  " let g:neocomplete#force_overwrite_completefunc = 1
  " let g:neocomplete#force_omni_input_patterns.ruby = '[^. *\t]\.\w*\|\h\w*::'
  " let g:neocomplete#force_omni_input_patterns.c = '[^.[:digit:] *\t]\%(\.\|->\)\w*'
  " let g:neocomplete#force_omni_input_patterns.cpp = '[^.[:digit:] *\t]\%(\.\|->\)\w*\|\h\w*::\w*'
  " let g:neocomplete#force_omni_input_patterns.objc = '[^.[:digit:] *\t]\%(\.\|->\)\w*'
  " let g:neocomplete#force_omni_input_patterns.objcpp = '[^.[:digit:] *\t]\%(\.\|->\)\w*\|\h\w*::\w*'
endif
