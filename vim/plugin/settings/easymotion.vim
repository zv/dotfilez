" This remaps easymotion to show us only the left
" hand home row keys as navigation optiotyns which 
" may mean more typing to get to a particular spot
" but it'll all be isolated to one area of the keyboard
call EasyMotion#InitOptions({
\   'leader_key'      : '<Leader><Leader>'
\ , 'keys'            : 'asdfjkl;ghwertyuinmvbop'
\ , 'do_shade'        : 1
\ , 'do_mapping'      : 1
\ , 'grouping'        : 1
\
\ , 'hl_group_target' : 'Type'
\ , 'hl_group_shade'  : 'Comment'
\ })

" Make EasyMotion more yellow, less red
hi clear EasyMotionTarget
hi! EasyMotionTarget guifg=red term=standout

nmap ,<ESC> ,,w
nmap ,<S-ESC> ,,b
