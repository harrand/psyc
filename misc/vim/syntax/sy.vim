" Vim syntax file
" Language:     sy
" Maintainer:   harrand

if version < 600
    syntax clear
elseif exists("b:current_syntax")
    finish
endif

" Syntax definitions {{{1

syntax match sy_builtin /\<__\w\+\>/
syntax match sy_funcname /\<\h\w*\>\ze\s*:\s*func/
syntax match sy_asmname /\<\h\w*\>\ze\s*:\s*asm/
syntax match sy_typename /\<\h\w*\>\ze\s*:\s*\(struct\|enum\)/
syntax match sy_comments /\/\/.*/ containedin=ALL
syntax match sy_operators /::=\|:=\|<=\|>=\|==\|[-+*\/=<>@#]/

" Number literals
syn match sy_dec_number display "\<[0-9][0-9_]*\%([iu]\%(size\|8\|16\|32\|64\|128\)\)\="
syn match sy_hex_number display "\<0x[a-fA-F0-9_]\+\%([iu]\%(size\|8\|16\|32\|64\|128\)\)\="
syn match sy_bin_number display "\<0b[01_]\+\%([iu]\%(size\|8\|16\|32\|64\|128\)\)\="
syn match sy_float display "\<[0-9][0-9_]*\.\%([^[:cntrl:][:space:][:punct:][:digit:]]\|_\|\.\)\@!"
syn match sy_float display "\<[0-9][0-9_]*\%(\.[0-9][0-9_]*\)\%([eE][+-]\=[0-9_]\+\)\=\(f32\|f64\)\="
syn match sy_float display "\<[0-9][0-9_]*\%(\.[0-9][0-9_]*\)\=\%([eE][+-]\=[0-9_]\+\)\(f32\|f64\)\="
syn match sy_float display "\<[0-9][0-9_]*\%(\.[0-9][0-9_]*\)\=\%([eE][+-]\=[0-9_]\+\)\=\(f32\|f64\)"
syn match sy_call "\w\(\w\)*("he=e-1,me=e-1
syn match sy_attrib /\$\k*/

syn region sy_string_literal start=+"+ skip=+\\"+ end=+"+
syn region sy_char_literal start=+'+ skip=+\\'+ end=+'+

syntax keyword sy_keywords ref if static_if else while for return unreachable sizeof alignof countof nameof typeof clobber
syntax keyword sy_modifiers mut weak static abi_sysv
syntax keyword sy_constants true false zero extern _config _win32 _linux
syntax keyword sy_funcs func macro asm
syntax keyword sy_types struct enum v0 s8 s16 s32 s64 u8 u16 u32 u64 bool embed_data srcloc program_args _atomic_op _atomic_ordering barrier

hi link sy_funcname Function
hi link sy_asmname Function
hi link sy_typename Type
hi link sy_types Type
hi link sy_comments Comment
hi link sy_builtin Function
hi link sy_call Function
hi link sy_funcs Type
hi link sy_keywords Keyword
hi link sy_modifiers Storage
hi link sy_attrib Operator
hi link sy_constants Number
hi link sy_operators Operator

hi def link sy_dec_number Number
hi def link sy_hex_number Number
hi def link sy_float Float
hi def link sy_string_literal String
hi def link sy_char_literal String
syn sync minlines=200
syn sync maxlines=500

let b:current_syntax = "sy"
