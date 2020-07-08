let g:VL_T = '#t'
let g:VL_F = '#f'
let g:VL_INITIAL_ENV = vlenv#BuildInitialEnv()

function! VlEvalCommand(expr) abort
    echo vl#Eval(a:expr)
endfunction

function! VlRunTests() abort
    call tests#vl#TestVimLisp()
endfunction

command! -nargs=* VlEval :call VlEvalCommand(<q-args>)
