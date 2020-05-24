"=============================================================================
" FILE: denite.vim
" AUTHOR:  Shougo Matsushita <Shougo.Matsu at gmail.com>
" License: MIT license
"=============================================================================

if exists('g:loaded_denite')
  finish
endif
let g:loaded_denite = 1

command! -nargs=+ -range -bar -complete=customlist,denite#helper#complete
      \ Denite
      \ call denite#helper#call_denite('Denite',
      \                                <q-args>, <line1>, <line2>)
command! -nargs=+ -range -bar -complete=customlist,denite#helper#complete
      \ DeniteCursorWord
      \ call denite#helper#call_denite('DeniteCursorWord',
      \                                <q-args>, <line1>, <line2>)
command! -nargs=+ -range -bar -complete=customlist,denite#helper#complete
      \ DeniteBufferDir
      \ call denite#helper#call_denite('DeniteBufferDir',
      \                                <q-args>, <line1>, <line2>)
command! -nargs=+ -range -bar -complete=customlist,denite#helper#complete
      \ DeniteProjectDir
      \ call denite#helper#call_denite('DeniteProjectDir',
      \                                <q-args>, <line1>, <line2>)



" ------------ Denite ------------ {{{{
function! s:setup_denite() abort
	call denite#custom#option('default', {
				\ 'mode' : 'insert',
				\ 'winheight': 15,
				\ 'start_filter' : 1,
				\ 'direction': 'bot',
				\ 'statusline' : v:false,
				\ 'prompt' : '> '
				\})
	call denite#custom#var('buffer', 'date_format', '%m-%d-%Y %H:%M:%S')
	call denite#custom#alias('source', 'file/rec/git', 'file/rec')
	call denite#custom#var('file/rec/git', 'command', ['git', 'ls-files', '-co', '--exclude-standard'])

	let l:globs = ['-g', '!*/.git/**']
	for l:v in split(&wildignore, ',')
		call extend(l:globs, ['-g', '!' . l:v])
	endfor

	if executable('rg')
		call denite#custom#var('grep', {
			\ 'command': ['rg'],
			\ 'default_opts': [
				\ '--color', 'never',
				\ '--vimgrep', '--no-heading'
				\ ],
			\ 'recursive_opts': [],
			\ 'pattern_opt': ['--regexp'],
			\ 'separator': ['--'],
			\ 'final_opts': [],
			\ })

		if !has('win32') && !has('win64')
			call denite#custom#var('file/rec', 'command', [
						\ 'rg', 
						\ '--max-count', '256', 
						\ '--max-depth', '3',
						\ '--hidden', '--files'] + l:globs)
		endif
	endif

	function! s:open_parent_rec(context)
		let l:parent = fnamemodify(a:context['path'], ':h')
		return {'sources_queue': [[{'name': 'file', 'args': [v:false, parent]}]], 'path': parent }
	endfunction

	call denite#custom#action('buffer,directory,file,openable', 
				\'open_parent', function('s:open_parent_rec'))

	autocmd FileType denite call s:denite_my_settings()
	autocmd FileType denite-filter call s:denite_filter_my_settings()

	function! s:denite_my_settings() abort
		nnoremap <nowait><silent><buffer><expr> <Esc> denite#do_map('quit')
		nnoremap <nowait><silent><buffer><expr> <C-g> denite#do_map('quit')
		nnoremap <silent><buffer><expr> i denite#do_map('open_filter_buffer')
		nnoremap <silent><buffer><expr> t denite#do_map('toggle_select').'j'
		nnoremap <silent><buffer><expr> q denite#do_map('quit')
		inoremap <silent><buffer><expr> <C-h> denite#do_map('do_action', 'open_parent')
		nnoremap <silent><buffer><expr> <C-t> denite#do_map('do_action', 'tabopen')
		nnoremap <silent><buffer><expr> <C-v> denite#do_map('do_action', 'vsplit')
		nnoremap <silent><buffer><expr> <C-s> denite#do_map('do_action', 'split')
		nnoremap <silent><buffer><expr> <CR> denite#do_map('do_action')
		nnoremap <silent><buffer><expr> p denite#do_map('do_action', 'preview')
		nnoremap <silent><buffer><Tab> j
		nnoremap <silent><buffer><S-Tab> k
	endfunction

	function! s:denite_filter_my_settings() abort
		imap <nowait><silent><buffer> <Esc> <Plug>(denite_filter_quit):q<CR>
		imap <nowait><silent><buffer> <C-g> <Plug>(denite_filter_quit):q<CR>
		inoremap <silent><buffer><expr> <C-h> denite#do_map('do_action', 'open_parent')
		inoremap <silent><buffer> <C-j> <Esc><C-w>p:call cursor((line('.') % line('$')) + 1, 0)<CR><C-w>pA
		inoremap <silent><buffer> <C-k> <Esc><C-w>p:call cursor(((line('$') + line('.') - 2) % line('$')) + 1, 0)<CR><C-w>pA
		imap <silent><buffer> <Tab> <C-j>
		imap <silent><buffer> <S-Tab> <C-k>
		inoremap <silent><buffer><expr> <C-l> denite#do_map('do_action')
	endfunction

	nnoremap <leader>ff :Denite file<CR>
	nnoremap <leader>fr :Denite file/old<CR>
	nnoremap <leader>/ :DeniteProjectDir grep<CR>
	nnoremap <leader>bb :Denite buffer<CR>
	nnoremap <leader>pf :DeniteProjectDir file/rec<CR>
	nnoremap <silent> <Leader>fj :<C-u>Denite jump<CR>
endfunction

call s:setup_denite()
" }}}}
