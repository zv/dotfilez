" abbreviations, believe it.
autocmd BufRead,BufNewFile *.c* iabbr packed __attribute__ ((__packed__))
autocmd BufRead,BufNewFile *.c* iabbr packedstruct struct __attribute__ ((__packed__)) PACKED_STRUCT {<CR>};

abbr cdl console.log
abbr pdl Purveu.log

" Guillemets
abbr lbraket «
abbr rbraket »

" close our stuff
iabbr (C ( )<Left><Left>
iabbr {C { }<Left><Left>
iabbr [C [ ]<Left><Left>
