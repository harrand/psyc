" Vim syntax file
" Language:     psy
" Maintainer:   harrand

if version < 600
    syntax clear
elseif exists("b:current_syntax")
    finish
endif

" Syntax definitions {{{1

syntax match psy_builtin /\<__\w\+\>/
syntax match psy_funcname /\<\h\w*\>\ze\s*:\s*func/
syntax match psy_asmname /\<\h\w*\>\ze\s*:\s*asm/
syntax match psy_typename /\<\h\w*\>\ze\s*:\s*\(struct\|enum\)/
syntax match psy_comments /\/\/.*/ containedin=ALL
syntax match psy_operators /::=\|:=\|<=\|>=\|==\|[-+*\/=<>@#]/

" Number literals
syn match psy_dec_number display "\<[0-9][0-9_]*\%([iu]\%(size\|8\|16\|32\|64\|128\)\)\="
syn match psy_hex_number display "\<0x[a-fA-F0-9_]\+\%([iu]\%(size\|8\|16\|32\|64\|128\)\)\="
syn match psy_bin_number display "\<0b[01_]\+\%([iu]\%(size\|8\|16\|32\|64\|128\)\)\="
syn match psy_float display "\<[0-9][0-9_]*\.\%([^[:cntrl:][:space:][:punct:][:digit:]]\|_\|\.\)\@!"
syn match psy_float display "\<[0-9][0-9_]*\%(\.[0-9][0-9_]*\)\%([eE][+-]\=[0-9_]\+\)\=\(f32\|f64\)\="
syn match psy_float display "\<[0-9][0-9_]*\%(\.[0-9][0-9_]*\)\=\%([eE][+-]\=[0-9_]\+\)\(f32\|f64\)\="
syn match psy_float display "\<[0-9][0-9_]*\%(\.[0-9][0-9_]*\)\=\%([eE][+-]\=[0-9_]\+\)\=\(f32\|f64\)"
syn match psy_call "\w\(\w\)*("he=e-1,me=e-1

syn region psy_string_literal start=+"+ skip=+\\"+ end=+"+
syn region psy_char_literal start=+'+ skip=+\\'+ end=+'+

syntax keyword psy_keywords defer ref if static_if else while for return sizeof alignof countof nameof typeof
syntax keyword psy_modifiers mut weak static
syntax keyword psy_constants true false zero extern _config _win32 _linux
syntax keyword psy_funcs func macro asm
syntax keyword psy_types struct enum v0 s8 s16 s32 s64 u8 u16 u32 u64 bool embed_data srcloc program_args _atomic_op _atomic_ordering barrier

hi link psy_funcname Function
hi link psy_asmname Function
hi link psy_typename Type
hi link psy_types Type
hi link psy_comments Comment
hi link psy_builtin Function
hi link psy_call Function
hi link psy_funcs Type
hi link psy_keywords Keyword
hi link psy_modifiers Storage
hi link psy_constants Number
hi link psy_operators Operator

hi def link psy_dec_number Number
hi def link psy_hex_number Number
hi def link psy_float Float
hi def link psy_string_literal String
hi def link psy_char_literal String
syn sync minlines=200
syn sync maxlines=500

let b:current_syntax = "psy"
