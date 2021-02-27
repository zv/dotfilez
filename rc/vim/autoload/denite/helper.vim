"=============================================================================
" FILE: helper.vim
" AUTHOR: Shougo Matsushita <Shougo.Matsu at gmail.com>
" License: MIT license
"=============================================================================

function! denite#helper#complete(arglead, cmdline, cursorpos) abort
  let _ = []

  if a:arglead =~# ':'
    " Todo: source arguments completion.
  elseif a:arglead =~# '^-'
    " Option names completion.
    let bool_options = keys(filter(copy(denite#init#_user_options()),
          \ 'type(v:val) == type(v:true) || type(v:val) == type(v:false)'))
    let _ += map(copy(bool_options), "'-' . tr(v:val, '_', '-')")
    let string_options = keys(filter(copy(denite#init#_user_options()),
          \ 'type(v:val) != type(v:true) && type(v:val) != type(v:false)'))
    let _ += map(copy(string_options), "'-' . tr(v:val, '_', '-') . '='")

    " Add "-no-" option names completion.
    let _ += map(copy(bool_options), "'-no-' . tr(v:val, '_', '-')")
  else
    " Source name completion.
    let _ += denite#helper#_get_available_sources()
  endif

  return uniq(sort(filter(_, 'stridx(v:val, a:arglead) == 0')))
endfunction
function! denite#helper#complete_actions(arglead, cmdline, cursorpos) abort
  return uniq(sort(filter(copy(g:denite#_actions),
        \ 'stridx(v:val, a:arglead) == 0')))
endfunction

function! denite#helper#call_denite(command, args, line1, line2) abort
  let [args, context] = denite#helper#_parse_options_args(a:args)

  let context.command = a:command
  let context.firstline = a:line1
  let context.lastline = a:line2

  call denite#start(args, context)
endfunction

function! denite#helper#preview_file(context, filename) abort
  let preview_width = str2nr(a:context.preview_width)
  let preview_height = str2nr(a:context.preview_height)

  if a:context.vertical_preview
    let pos = win_screenpos(win_getid())
    let win_width = winwidth(0)

    call denite#util#execute_path(
          \ 'silent rightbelow vertical pedit!', a:filename)
    wincmd P

    if a:context.floating_preview && exists('*nvim_win_set_config')
      if a:context['split'] ==# 'floating'
        let win_row = str2nr(a:context['winrow'])
        let win_col = str2nr(a:context['wincol'])
      else
        let win_row = pos[0] - 1
        let win_col = pos[1] - 1
      endif
      let win_col += win_width
      if a:context.split !=# 'floating'
        let win_col -= preview_width
      endif

      call nvim_win_set_config(win_getid(), {
           \ 'relative': 'editor',
           \ 'row': win_row,
           \ 'col': win_col,
           \ 'width': preview_width,
           \ 'height': preview_height,
           \ })
    else
      execute 'vert resize ' . preview_width
    endif
  else
    let previewheight_save = &previewheight
    try
      let &previewheight = preview_height
      call denite#util#execute_path('silent aboveleft pedit!', a:filename)
    finally
      let &previewheight = previewheight_save
    endtry

    wincmd P
  endif

  if exists('#User#denite-preview')
    doautocmd User denite-preview
  endif
endfunction

function! denite#helper#options() abort
  return map(keys(denite#init#_user_options()), "tr(v:val, '_', '-')")
endfunction

function! denite#helper#_parse_options_args(cmdline) abort
  let _ = []
  let [args, options] = s:parse_options(a:cmdline)

  for arg in args
    " Add source name.
    let source_name = matchstr(arg, '^[^:]*')
    let source_arg = arg[len(source_name)+1 :]
    let source_args = []
    if source_arg !=# ''
      for s in split(source_arg, s:re_unquoted_match('\\\@<!:'), 1)
        call add(source_args, s:remove_quote_pairs(s))
      endfor
    endif
    call add(_, { 'name': source_name, 'args': source_args })
  endfor

  return [_, options]
endfunction
function! s:re_unquoted_match(match) abort
  " Don't match a:match if it is located in-between unescaped single or double
  " quotes
  return a:match . '\v\ze([^"' . "'" . '\\]*(\\.|"([^"\\]*\\.)*[^"\\]*"|'
        \ . "'" . '([^' . "'" . '\\]*\\.)*[^' . "'" . '\\]*' . "'" . '))*[^"'
        \ . "'" . ']*$'
endfunction
function! s:remove_quote_pairs(s) abort
  " remove leading/ending quote pairs
  let s = a:s
  if s[0] ==# '"' && s[len(s) - 1] ==# '"'
    let s = s[1: len(s) - 2]
  elseif s[0] ==# "'" && s[len(s) - 1] ==# "'"
    let s = s[1: len(s) - 2]
  else
    let s = substitute(a:s, '\\\(.\)', "\\1", 'g')
  endif
  return s
endfunction
function! s:parse_options(cmdline) abort
  let args = []
  let options = {}

  " Eval
  let cmdline = (a:cmdline =~# '\\\@<!`.*\\\@<!`') ?
        \ s:eval_cmdline(a:cmdline) : a:cmdline

  for s in split(cmdline, s:re_unquoted_match('\%(\\\@<!\s\)\+'))
    let arg = substitute(s, '\\\( \)', '\1', 'g')
    let arg_key = substitute(arg, '=\zs.*$', '', '')

    let name = substitute(tr(arg_key, '-', '_'), '=$', '', '')[1:]
    if name =~# '^no_'
      let name = name[3:]
      let value = v:false
    else
      let value = (arg_key =~# '=$') ?
            \ s:remove_quote_pairs(arg[len(arg_key) :]) : v:true
    endif

    if index(keys(denite#init#_user_options())
          \ + keys(denite#init#_deprecated_options()), name) >= 0
      let options[name] = value
    else
      call add(args, arg)
    endif
  endfor

  return [args, options]
endfunction
function! s:eval_cmdline(cmdline) abort
  let cmdline = ''
  let prev_match = 0
  let eval_pos = match(a:cmdline, '\\\@<!`.\{-}\\\@<!`')
  while eval_pos >= 0
    if eval_pos - prev_match > 0
      let cmdline .= a:cmdline[prev_match : eval_pos - 1]
    endif
    let prev_match = matchend(a:cmdline,
          \ '\\\@<!`.\{-}\\\@<!`', eval_pos)
    let cmdline .= escape(eval(a:cmdline[eval_pos+1 : prev_match - 2]), '\ ')

    let eval_pos = match(a:cmdline, '\\\@<!`.\{-}\\\@<!`', prev_match)
  endwhile
  if prev_match >= 0
    let cmdline .= a:cmdline[prev_match :]
  endif

  return cmdline
endfunction

function! denite#helper#has_cmdline() abort
  if !exists('*getcompletion')
    return 0
  endif

  try
    call getcompletion('', 'cmdline')
  catch
    return 0
  endtry

  return 1
endfunction


function! denite#helper#_set_oldfiles(oldfiles) abort
  let v:oldfiles = a:oldfiles
endfunction
function! denite#helper#_get_oldfiles() abort
  return filter(copy(v:oldfiles),
        \ 'filereadable(fnamemodify(v:val, ":p")) || buflisted(v:val)')
endfunction


function! denite#helper#_get_available_sources() abort
  if exists('s:source_names')
    return copy(s:source_names)
  endif
  let s:source_names = map(
        \ globpath(&runtimepath, 'rplugin/python3/denite/source/**/*.py', 1, 1),
        \ 's:_get_source_name(v:val)',
        \)
  return copy(filter(s:source_names, '!empty(v:val)'))
endfunction
function! denite#helper#_set_available_sources(source_names) abort
  " Called from rplugin/python3/denite/denite.py#load_sources
  let s:source_names = a:source_names
endfunction
function! s:_get_source_name(path) abort
  let path_f = fnamemodify(a:path, ':gs?\\?/?')
  if path_f ==# '__init__.py' || path_f ==# 'base.py'
    return ''
  elseif path_f[-12:] ==# '/__init__.py'
    if getfsize(path_f) == 0
      " Probably the file exists for making a namespace so ignore
      return ''
    endif
    return fnamemodify(path_f, ':h:s?.*/rplugin/python3/denite/source/??:r')
  endif
  return fnamemodify(path_f, ':s?.*/rplugin/python3/denite/source/??:r')
endfunction

function! denite#helper#_get_wininfo() abort
  let wininfo = getwininfo(win_getid())[0]
  return {
        \ 'bufnr': wininfo['bufnr'],
        \ 'winnr': wininfo['winnr'],
        \ 'winid': wininfo['winid'],
        \ 'tabnr': wininfo['tabnr'],
        \}
endfunction
function! denite#helper#_get_preview_window() abort
  " Note: For popup preview feature
  if exists('*popup_findpreview') && popup_findpreview() > 0
    return 1
  endif

  return len(filter(range(1, winnr('$')),
        \ "getwinvar(v:val, '&previewwindow') ==# 1"))
endfunction


function! denite#helper#_start_update_candidates_timer(bufnr) abort
  return timer_start(100,
        \ {-> denite#call_async_map('update_candidates')},
        \ {'repeat': -1})
endfunction
function! denite#helper#_start_update_buffer_timer(bufnr) abort
  return timer_start(50,
        \ {-> denite#_update_map('update_buffer', a:bufnr, v:false)},
        \ {'repeat': -1})
endfunction

function! denite#helper#_get_temp_file(bufnr) abort
  let temp = tempname()
  call writefile(getbufline(a:bufnr, 1, '$'), temp)
  return temp
endfunction
