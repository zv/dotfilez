autocmd BufRead,BufNewFile *.asm set ft=nasm syntax=nasm

abbr asmstart global _start ; section .text; _start:

" generates a pause instruction
abbr x86pause void __builtin_ia32_pause (void)

