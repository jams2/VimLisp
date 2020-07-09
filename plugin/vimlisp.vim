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

function! VlEvalFile(path) abort
    let fname = fnameescape(a:path)
    if !filereadable(fname)
        throw "file not readable: "..fname
    endif
    let lines = map(readfile(fname), {_, l -> trim(l)})
    let lines = filter(lines, {_, l -> match(l, '^;\+') == -1})
    let lines = filter(lines, {_, l -> strlen(l) > 0})
    let source = join(lines)
    let exprs = vl#SplitExprs(source)
    for expr in exprs
        call vl#Eval(expr)
    endfor
    echomsg "[+] VlEvalFile(\""..fname.."\"): ok"
endfunction

command! -nargs=* VlEval :call VlEvalCommand(<q-args>)
