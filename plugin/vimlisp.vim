let g:vl_t_pair = "t_pair"
let g:vl_t_sym = "t_sym"
let g:vl_t_lstr = "t_lstr"
let g:vl_t_void = "t_void"
let g:vl_bool_t = "#t"
let g:vl_bool_f = "#f"
let g:VL_INITIAL_ENV = vlenv#BuildInitialEnv()
let s:D_QUOTE = 34
let s:SEMICOLON = 59

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
    call VlEvalMultiline(readfile(fname))
    echomsg "[+] VlEvalFile(\""..fname.."\"): ok"
endfunction

function! VlEvalMultiline(lines) abort
    let lines = map(a:lines, {_, l -> s:CleanLine(l)})
    let lines = filter(lines, {_, l -> match(l, '\(^;\+\)\|^$') == -1})
    let source = join(lines)
    let exprs = vl#SplitExprs(source)
    for expr in exprs
        call vl#Eval(expr)
    endfor
endfunction

function! s:CleanLine(line) abort
    " 1. Strip leading/trailing whitespace.
    " 2. Include up to the leftmost semicolon
    "       (if any) preceded by an odd number
    "       of doublequotes.
    let chars = str2list(trim(a:line))
    let sc_count = count(chars, s:SEMICOLON)
    let leftmost = 0
    while sc_count > 0
        let sc_index = s:Rindex(chars[:leftmost-1], s:SEMICOLON)
        let sc_count = count(chars[:sc_index-1], s:D_QUOTE)
        if sc_index == -1
            break
        elseif s:Even(count(chars[:sc_index-1], s:D_QUOTE))
            let leftmost = sc_index
            let sc_count -= 1
        else
            break
        endif
    endwhile
    return list2str(chars[:leftmost-1])
endfunction

function! s:Even(num) abort
    return a:num == 2 || a:num / 2.0 == 0
endfunction

function! s:Rindex(list, pattern) abort
    " Find the rightmost index of pattern in list.
    if type(a:list) != v:t_list
        throw "Invalid argument: Rindex requires list"
    elseif len(a:list) == 0
        return -1
    endif
    let i = len(a:list) - 1
    while i > -1
        if a:list[i] == a:pattern
            return i
        endif
        let i -= 1
    endwhile
    return -1
endfunction

command! -nargs=* VlEval :call VlEvalCommand(<q-args>)
