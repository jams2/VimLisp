let g:VL_TRANSFORMERS = {'let': funcref('TransformLet')}
let s:PREV_FRAME_KEY = "__prev_frame"
let s:END_CONT = {val -> val}
let s:OUTER_PARENS_R ='\(^(\)\|\()\)$'
let s:VARNAME_R = '^[a-z][a-z0-9-?!*^]*$'
let s:PRIMOP_R = '^[+*/=-]$\|^\(call/cc\)$'
let s:STRING_CONST_R = '^".*"$'
let s:NUMBER_R = '^-\?\d\+$'
let s:STR_DELIM = 34
let s:LEFT_PAREN = 40
let s:RIGHT_PAREN = 41
let s:SPACE = 32
let s:TAB = 9
let s:NEWLINE = 13

function! IsEmptyList(obj) abort
    return type(a:obj) == v:t_list && a:obj == []
endfunction

function! IsWhiteSpace(char) abort
    return a:char == s:SPACE || a:char == s:TAB || a:char == s:NEWLINE
endfunction

function! DeepLispList(elts) abort
    if IsEmptyList(a:elts)
        return []
    elseif len(a:elts) == 1
        if type(a:elts[0]) == v:t_list
            return Cons(DeepLispList(a:elts[0]), [])
        endif
        return add(a:elts, [])
    elseif type(a:elts[0]) == v:t_list
        return Cons(DeepLispList(a:elts[0]), DeepLispList(a:elts[1:]))
    else
        return Cons(a:elts[0], DeepLispList(a:elts[1:]))
    endif
endfunction

function! LispList(elts) abort
    if IsEmptyList(a:elts)
        return []
    endif
    return Cons(a:elts[0], LispList(a:elts[1:]))
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

function! LispMap(proc, l) abort
    if IsEmptyList(a:l)
        return []
    endif
    return Cons(a:proc(Car(a:l)), LispMap(a:proc, Cdr(a:l)))
endfunction

function! DeepLispMap(proc, l) abort
    if IsEmptyList(a:l)
        return []
    elseif type(Car(a:l)) ==  v:t_list
        return Cons(DeepLispMap(a:proc, Car(a:l)), DeepLispMap(a:proc, Cdr(a:l)))
    else
        return Cons(a:proc(Car(a:l)), DeepLispMap(a:proc, Cdr(a:l)))
    endif
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
    while !IsEmptyList(var_l)
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

function! StrToVim(expr) abort
    if a:expr =~ s:NUMBER_R
        return a:expr - 0
    elseif a:expr =~ s:STRING_CONST_R
        return a:expr
    elseif a:expr =~? s:VARNAME_R || a:expr =~? s:PRIMOP_R
        return a:expr
    elseif a:expr =~ '^(.*'
        return DeepLispList(StringToList(a:expr))
    else
        throw "Invalid expr: "..a:expr
    endif
endfunction

function! ParseStringLiteral(expr) abort
    let end = strlen(a:expr)
    let buf = [strgetchar(a:expr, 0)]
    let i = 1
    let char = strgetchar(a:expr, i)
    while i < end
        let buf = add(buf, char)
        if char == s:STR_DELIM
            break
        endif
        let i += 1
        let char = strgetchar(a:expr, i)
    endwhile
    if buf[-1] != s:STR_DELIM
        throw "Unterminated string: "..a:expr
    endif
    return [buf, i+1]
endfunction

function! StringToList(expr) abort
    let expr_len = SubExprLen(a:expr)
    if expr_len == -1
        throw "Unterminated expression: "..string(a:expr)
    elseif expr_len != strlen(a:expr)
        throw "Unbalanced parentheses: "..string(a:expr)
    endif
    let l = []
    let buf = []
    let chars = substitute(a:expr, s:OUTER_PARENS_R, '', 'g')
    let i = 0
    while i < strlen(chars)
        let char = strgetchar(chars, i)
        if char == s:LEFT_PAREN
            let subexpr_len = SubExprLen(strcharpart(chars, i))
            let sublist = StringToList(strcharpart(chars, i, subexpr_len))
            let l = add(l, sublist)
            let i += subexpr_len
        elseif IsWhiteSpace(char)
            if len(buf) > 0
                let l = add(l, StrToVim(list2str(buf)))
                let buf = []
            endif
            let i += 1
        elseif char == s:STR_DELIM
            if !IsEmptyList(buf)
                throw "Invalid string literal: "..strcharpart(chars, i)
            endif
            let [string_chars, length] = ParseStringLiteral(strcharpart(chars, i))
            let l = add(l, StrToVim(list2str(string_chars)))
            let i += length
        else
            let buf = add(buf, char)
            let i += 1
        endif
    endwhile
    if len(buf) > 0
        let l = add(l, StrToVim(list2str(buf)))
    endif
    return l
endfunction

function! MapTransformers(l) abort
    if IsEmptyList(a:l)
        return []
    elseif type(Car(a:l)) ==  v:t_list
        return Cons(MapTransformers(Car(a:l)), MapTransformers(Cdr(a:l)))
    else
        return ApplyTransformers(a:l)
    endif
endfunction

function ApplyTransformers(expr) abort
    if has_key(g:VL_TRANSFORMERS, Car(a:expr))
        return get(g:VL_TRANSFORMERS, Car(a:expr))(a:expr)
    endif
    return a:expr
endfunction

function! VlEval(expr, env=g:VL_INITIAL_ENV) abort
    let vim_repr = StrToVim(a:expr)
    let vim_repr = MapTransformers(vim_repr)
    return VlAnalyze(vim_repr)(a:env, s:END_CONT)
endfunction

function! Sequentially(proc1, proc2) abort
    return {env, k -> a:proc1(env, {x -> a:proc2(env, k)})}
endfunction

function! GenSequence(expr) abort
    let Analyzer = {x -> VlAnalyze(x)}
    let closures = LispMap(Analyzer, a:expr)
    let C1 = Car(closures)
    while !IsEmptyList(Cdr(closures))
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
    elseif Car(a:rator) =~? '^cont-primitive$'
        return Cdr(a:rator)(Car(a:rands))
    elseif Car(a:rator) =~? '^proc$'
        let Body = ProcBody(a:rator)
        let env = ProcEnv(a:rator)
        let params = ProcParams(a:rator)
        let env2 = ExtendEnv(env, params, a:rands)
        let result = Body(env2, a:k)
        return result
    elseif Car(a:rator) =~? '^cont$'
        let Body = ProcBody(a:rator)
        let env = ProcEnv(a:rator)
        let params = ProcParams(a:rator)
        let args = Cons(extend(['cont-primitive'], a:rands), [])
        let env2 = ExtendEnv(env, params, args)
        let result = Body(env2, a:k)
        return result
    endif
endfunction

function! EvalClosureList(l, env, k) abort
    if IsEmptyList(a:l)
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
    let rands = LispMap({x -> VlAnalyze(x)}, Cdr(a:expr))
    return {env, k -> Rator(env, RatorCont(rands, env, k))}
endfunction

function! GenDefine(expr) abort
    let ValClosure = VlAnalyze(Caddr(a:expr))
    let InnerClosure = {env, k -> {val -> k(DefineVar(env, Cadr(a:expr), val))}}
    return {env, k -> ValClosure(env, InnerClosure(env, k))}
endfunction

function! AnalyzeCallCCProc(expr) abort
    let params = Cadr(a:expr)
    let Body = GenSequence(Cddr(a:expr))
    return {env, k -> k(LispList(["cont", params, Body, env]))}
endfunction

function! GenCallCC(expr) abort
    let Proc = AnalyzeCallCCProc(Cadr(a:expr))
    return {env, k -> Proc(env, {rator -> ExecProc(rator, [k], {x -> x})})}
endfunction

function! GenProc(expr) abort
    let params = Cadr(a:expr)
    let Body = GenSequence(Cddr(a:expr))
    return {env, k -> k(LispList(["proc", params, Body, env]))}
endfunction

function! TransformLet(expr) abort
    let bindings = Cadr(a:expr)
    let body = Cddr(a:expr)
    let vars = LispMap({x -> Car(x)}, bindings)
    let vals = LispMap({x -> Cadr(x)}, bindings)
    let lambda = Cons('lambda', Cons(vars, body))
    return Cons(lambda, vals)
endfunction

function! VlAnalyze(expr) abort
    if type(a:expr) == v:t_number
        return {env, k -> k(a:expr)}
    elseif type(a:expr) == v:t_string && a:expr =~ s:STRING_CONST_R
        return {env, k -> k(a:expr)}
    elseif type(a:expr) == v:t_string
        return {env, k -> k(ApplyEnv(env, a:expr))}
    elseif type(a:expr[0]) == v:t_list
        return GenApplication(a:expr)
    elseif a:expr[0] =~? '^lambda$'
        return GenProc(a:expr)
    elseif a:expr[0] =~? '^begin$'
        return GenSequence(Cdr(a:expr))
    elseif a:expr[0] =~? '^define$'
        return GenDefine(a:expr)
    elseif a:expr[0] =~? '^set!$'
        return {env, k -> k(SetVar(env, Cadr(a:expr), VlAnalyze(Caddr(a:expr))))}
    elseif a:expr[0] =~? '^call/cc$'
        return GenCallCC(a:expr)
    elseif type(a:expr) == v:t_list
        return GenApplication(a:expr)
    else
        throw "Invalid expression: "..a:expr
    endif
endfunction

function! VlAdd(args) abort
    let total = 0
    let l = a:args
    while !IsEmptyList(l)
        let total += Car(l)
        let l = Cdr(l)
    endwhile
    return total
endfunction

let g:VL_INITIAL_ENV = {'+': ['primitive', funcref('VlAdd')]}
