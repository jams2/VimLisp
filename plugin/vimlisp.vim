let s:PREV_FRAME_KEY = "__prev_frame"
let s:END_CONT = {val -> val}
let s:OUTER_PARENS_R ='\(^(\)\|\()\)$'
let s:VARNAME_R = '^[a-z][a-z0-9-?!*^]*$'
let s:PRIMOP_R = '^[+*/=-]$'
let s:STRING_CONST_R = '^".*"$'
let s:NUMBER_R = '^\d\+$'
let s:STR_DELIM = 34
let s:LEFT_PAREN = 40
let s:RIGHT_PAREN = 41
let s:SPACE = 32
let s:TAB = 9
let s:NEWLINE = 13

function! NilP(obj) abort
    return type(a:obj) == v:t_list && a:obj == []
endfunction

function! IsWhiteSpace(char) abort
    return a:char == s:SPACE || a:char == s:TAB || a:char == s:NEWLINE
endfunction

function! DeepSchemeList(elts) abort
    if NilP(a:elts)
        return []
    elseif len(a:elts) == 1
        if type(a:elts[0]) == v:t_list
            return Cons(DeepSchemeList(a:elts[0]), [])
        endif
        return add(a:elts, [])
    elseif type(a:elts[0]) == v:t_list
        return Cons(DeepSchemeList(a:elts[0]), DeepSchemeList(a:elts[1:]))
    else
        return Cons(a:elts[0], DeepSchemeList(a:elts[1:]))
    endif
endfunction

function! SchemeList(elts) abort
    if NilP(a:elts)
        return []
    endif
    return Cons(a:elts[0], SchemeList(a:elts[1:]))
endfunction

function! Cons(a, d) abort
    return [a:a, a:d]
endfunction

function! Car(list) abort
    return a:list[0]
endfunction

function! Cdr(list) abort
    return a:list[1]
endfunction

function! Cadr(list) abort
    return a:list[1][0]
endfunction

function! Cddr(list) abort
    return a:list[1][1]
endfunction

function! Caddr(list) abort
    return a:list[1][1][0]
endfunction

function! Cdddr(list) abort
    return a:list[1][1][1]
endfunction

function! Cadddr(list) abort
    return a:list[1][1][1][0]
endfunction

function! SchemeMap(proc, l) abort
    if NilP(a:l)
        return []
    endif
    return Cons(a:proc(Car(a:l)), SchemeMap(a:proc, Cdr(a:l)))
endfunction

function! SubExprLen(str) abort
    let open_count = 0
    for i in range(strlen(a:str))
        let char = strgetchar(a:str, i)
        if char == s:LEFT_PAREN
            let open_count += 1
        elseif char == s:RIGHT_PAREN
            let open_count -= 1
            if open_count == 0
                return i + 1
            endif
        endif
    endfor
    return -1
endfunction

function! ApplyEnv(env, var) abort
    if has_key(a:env, a:var)
        return get(a:env, a:var)
    elseif has_key(a:env, s:PREV_FRAME_KEY)
        return ApplyEnv(get(a:env, s:PREV_FRAME_KEY), a:var)
    else
        throw "Unbound variable: "..a:var
    endif
endfunction

function! ExtendEnv(env, vars, vals) abort
    let new_frame = {s:PREV_FRAME_KEY: a:env}
    let var_l = a:vars
    let val_l = a:vals
    while !NilP(var_l)
        let new_frame[Car(var_l)] = Car(val_l)
        let var_l = Cdr(var_l)
        let val_l = Cdr(val_l)
    endwhile
    return new_frame
endfunction

function! SetVar(env, var, val) abort
    if has_key(a:env, a:var)
        let a:env[a:var] = a:val
    elseif has_key(a:env, s:PREV_FRAME_KEY)
        return SetVar(get(a:env, s:PREV_FRAME_KEY), a:var)
    else
        throw "Unbound variable: "..a:var
    endif
endfunction

function! DefineVar(env, var, val) abort
    let a:env[a:var] = a:val
endfunction

function! RemoveOuterParens(expr) abort
    return substitute(a:expr, s:OUTER_PARENS_R, '', 'g')
endfunction

function! StrToVim(expr) abort
    if a:expr =~ s:NUMBER_R
        return a:expr - 0
    elseif a:expr =~ s:STRING_CONST_R
        return a:expr
    elseif a:expr =~? s:VARNAME_R || a:expr =~? s:PRIMOP_R
        return a:expr
    elseif a:expr =~ '^(.*'
        return DeepSchemeList(StringToList(a:expr))
    else
        throw "Invalid expr: "..a:expr
    endif
endfunction

function! ParseStringLiteral(expr) abort
endfunction

function! StringToList(expr) abort
    let l = []
    let buf = []
    let chars = RemoveOuterParens(a:expr)
    let i = 0
    let parsing_string = 0
    while i < strlen(chars)
        let char = strgetchar(chars, i)
        if char == s:LEFT_PAREN
            "new list
            let subexpr_len = SubExprLen(strcharpart(chars, i))
            let sublist = StringToList(strcharpart(chars, i, subexpr_len))
            let l = add(l, sublist)
            let i += subexpr_len
        elseif IsWhiteSpace(char) && !parsing_string
            if len(buf) > 0
                let l = add(l, StrToVim(list2str(buf)))
                let buf = []
            endif
            let i += 1
        else
            if char == s:STR_DELIM
                let parsing_string = !parsing_string
            endif
            let buf = add(buf, char)
            let i += 1
        endif
    endwhile
    if len(buf) > 0
        let l = add(l, StrToVim(list2str(buf)))
    endif
    return l
endfunction

function! VlEval(expr) abort
    let C = VlAnalyze(StrToVim(a:expr))
    let r = C(s:INITIAL_ENV, s:END_CONT)
    return r
endfunction

function! Sequentially(proc1, proc2) abort
    return {env, k -> a:proc1(env, {x -> a:proc2(env, k)})}
endfunction

function! GenSequence(expr) abort
    let Analyzer = {x -> VlAnalyze(x)}
    let closures = SchemeMap(Analyzer, a:expr)
    let C1 = Car(closures)
    while Cdr(closures) != []
        let C2 = Cadr(closures)
        let C1 = Sequentially(C1, C2)
        let closures = Cdr(closures)
    endwhile
    return C1
endfunction

function! ProcParams(proc) abort
    return Cadr(a:proc)
endfunction

function! ProcEnv(proc) abort
    return Cadddr(a:proc)
endfunction

function! ProcBody(proc) abort
    return Caddr(a:proc)
endfunction

function! ExecProc(rator, rands, k)
    if Car(a:rator) =~? '^primitive$'
        return a:k(Cdr(a:rator)(a:rands))
    elseif Car(a:rator) =~? '^proc$'
        let Body = ProcBody(a:rator)
        let env = ProcEnv(a:rator)
        let params = ProcParams(a:rator)
        let env2 = ExtendEnv(env, params, a:rands)
        let result = Body(env2, a:k)
        return result
    endif
endfunction

function! EvalClosureList(l, env, k) abort
    if a:l == []
        return a:k([])
    endif
    let InnerCont = {arg -> {args -> a:k(Cons(arg, args))}}
    let OuterCont = {arg -> EvalClosureList(Cdr(a:l), a:env, InnerCont(arg))}
    return Car(a:l)(a:env, OuterCont)
endfunction

function! RandsCont(rator, k) abort
    return {args -> ExecProc(a:rator, args, a:k)}
endfunction

function! RatorCont(rands, env, k) abort
    return {rator -> EvalClosureList(a:rands, a:env, RandsCont(rator, a:k))}
endfunction

function! GenApplication(expr) abort
    let Rator = VlAnalyze(Car(a:expr))
    let rands = SchemeMap({x -> VlAnalyze(x)}, Cdr(a:expr))
    return {env, k -> Rator(env, RatorCont(rands, env, k))}
endfunction

function! GenDefine(expr) abort
    let ValClosure = VlAnalyze(Caddr(a:expr))
    let InnerClosure = {env, k -> {val -> k(DefineVar(env, Cadr(a:expr), val))}}
    return {env, k -> ValClosure(env, InnerClosure(env, k))}
endfunction

function! VlAnalyze(expr) abort
    if type(a:expr) == v:t_number
        return {env, k -> k(a:expr)}
    elseif type(a:expr) == v:t_string && a:expr =~ s:STRING_CONST_R
        return {env, k -> k(a:expr)}
    elseif type(a:expr) == v:t_string
        return {env, k -> k(ApplyEnv(env, a:expr))}
    elseif type(Car(a:expr)) == v:t_list
        return GenApplication(a:expr)
    elseif Car(a:expr) =~? '^lambda$'
        let params = Cadr(a:expr)
        let Body = VlAnalyze(Caddr(a:expr))
        return {env, k -> k(SchemeList(["proc", params, Body, env]))}
    elseif Car(a:expr) =~? '^begin$'
        return GenSequence(Cdr(a:expr))
    elseif Car(a:expr) =~? '^define$'
        return GenDefine(a:expr)
    elseif Car(a:expr) =~? '^set!$'
        return {env, k -> k(SetVar(env, Cadr(a:expr), VlAnalyze(Caddr(a:expr))))}
    elseif type(a:expr) == v:t_list
        return GenApplication(a:expr)
    else
        throw "Invalid expression: "..a:expr
    endif
endfunction

function! VlAdd(args) abort
    let total = 0
    let l = a:args
    while l != []
        let total += Car(l)
        let l = Cdr(l)
    endwhile
    return total
endfunction

let s:INITIAL_ENV = {'+': ['primitive', funcref('VlAdd')]}
