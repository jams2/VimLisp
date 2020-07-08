let g:VL_T = '#t'
let g:VL_F = '#f'
let g:VL_INITIAL_ENV = vlenv#BuildInitialEnv()

function! VlEvalCommand(expr) abort
    let val = vl#Eval(a:expr)
    echo vlutils#PrettyPrint(val)
endfunction

function! VlRunTests() abort
    call tests#vl#TestVimLisp()
endfunction

command! -nargs=* VlEval :call VlEvalCommand(<q-args>)
