let g:vl_t_pair = "t_pair"
let g:vl_t_sym = "t_sym"
let g:vl_t_lstr = "t_lstr"
let g:vl_t_void = "t_void"
let g:vl_bool_t = "#t"
let g:vl_bool_f = "#f"
let g:VL_INITIAL_ENV = vlenv#BuildInitialEnv()

function! VlEvalCommand(expr) abort
    try
        let val = vl#Eval(a:expr)
        echo vlutils#PrettyPrint(val)
    catch
        echohl WarningMsg
        echomsg "[!] vl#Eval: "..v:exception
        echohl SpellBad
        echomsg a:expr
        echohl Normal
    endtry
endfunction

function! VlRunTests() abort
    call tests#vl#TestVimLisp()
endfunction

command! -nargs=* VlEval :call VlEvalCommand(<q-args>)
