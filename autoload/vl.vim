let g:VL_TRANSFORMERS = {}
let s:PREV_FRAME_KEY = "__prev_frame"
let s:END_CONT = {val -> val}
let s:SYMBOL_R = '^[a-zA-Z0-9?!*^/\\+-]\+$'
let s:STRING_CONST_R = '^".*"$'
let s:NUMBER_R = '^-\?\d\+$'
let s:BOOL_R = '^\(#t\)\|\(#f\)$'

function! vl#Eval(expr, env=g:VL_INITIAL_ENV) abort
    let tokens = split(substitute(a:expr, '\([()"'']\)', ' \1 ', 'g'))
    let syntax = s:Parse(tokens)
    return s:Analyze(syntax)(a:env, s:END_CONT)
endfunction

function! vl#TypeOf(obj) abort
    if type(a:obj) == v:t_dict
        return get(a:obj, "_t")
    elseif type(a:obj) == v:t_list
        if type(vl#Cdr(a:obj)) == v:t_list
            return v:t_list
        endif
        return g:vl_t_pair
    elseif type(a:obj) == v:t_string
        return g:vl_t_sym
    else
        return type(a:obj)
    endif
endfunction

function! s:Parse(tokens) abort
    if len(a:tokens) == 0
        throw "Unterminated expression"
    elseif len(a:tokens) == 1
        return s:ParseAtom(a:tokens[0])
    elseif a:tokens[0] == "("
        return vl#LispList(s:ParseList(a:tokens))
    elseif a:tokens[0] == '"'
        return vl#LispList(s:ParseString(a:tokens))
    elseif a:tokens[0] == "'"
        "if a:tokens[1] == "(" && index(a:tokens, ".") > 1
            "return vl#LispList(["quote", s:ParsePair(a:tokens[1:])])
        "endif
        return vl#LispList(["quote", s:Parse(a:tokens[1:])])
    endif
endfunction

function! s:ParseList(tokens) abort
    if a:tokens[0] != "(" || a:tokens[-1] != ")"
        throw "Invalid list structure -- s:ParseList"
    endif
    let tokens = a:tokens[1:-2]
    let exprlist = []
    let i = 0  "skip open paren
    while i < len(tokens)
        let token = tokens[i]
        if token == ")"
            throw "Unexpected list termination -- s:ParseList"
        elseif token == "("
            let sublistlen = s:NestedNonTerminalLen(tokens[i:])
            call add(exprlist, s:Parse(tokens[i:i+sublistlen-1]))
            let i += sublistlen
        elseif token == '"'
            let stringlen = s:NonTerminalLen(tokens[i:], '"')
            call add(exprlist, s:Parse(tokens[i:i+stringlen-1]))
            let i += stringlen
        elseif token == "'"
            if tokens[i+1] == "("
                let quotelen = s:NestedNonTerminalLen(tokens[i+1:])
                call add(exprlist, s:Parse(tokens[i:i+quotelen]))
                let i += 1 + quotelen
            else
                call add(exprlist, s:Parse(tokens[i:i+1]))
                let i += 2
            endif
        else
            call add(exprlist, s:Parse(tokens[i:i]))
            let i += 1
        endif
    endwhile
    return exprlist
endfunction

function! s:ParsePair(tokens) abort
    if a:tokens[0] != "(" || index(a:tokens, ".") == -1
        throw "Invalid pair structure -- s:ParseList"
    endif
    let exprlist = s:ParseList(tokens)
    if len(exprlist) != 3
    endif
endfunction

function! s:ParseString(tokens) abort
    if a:tokens[0] != '"' || a:tokens[-1] != '"'
        throw "Invalid string literal: "..string(join(a:tokens))
    endif
    return s:StrFactory(join(a:tokens[1:-2]))
endfunction

function! s:StrFactory(str) abort
    return ["vlobj", #{_t: g:vl_t_lstr, _chars: str2list(a:str)}]
endfunction

function! s:NestedNonTerminalLen(tokens, open="(", close=")") abort
    let open_count = 0
    for i in range(len(a:tokens))
        let token = a:tokens[i]
        if token == a:open
            let open_count += 1
        elseif token == a:close
            let open_count -= 1
            if open_count == 0
                return i + 1
            endif
        endif
    endfor
    return -1
endfunction

function! s:NonTerminalLen(tokens, delim) abort
    let delim_count = 0
    for i in range(len(a:tokens))
        let token = a:tokens[i]
        if token == a:delim
            let delim_count += 1
            if delim_count == 2
                return i + 1
            endif
        endif
    endfor
    return -1
endfunction

function! s:ParseAtom(token) abort
    if a:token =~ s:NUMBER_R
        return a:token - 0
    elseif a:token =~ s:STRING_CONST_R
        return a:token
    elseif a:token =~? s:SYMBOL_R
        return a:token
    elseif a:token =~? s:BOOL_R
        return a:token
    elseif a:token == "."
        return a:token
    else
        throw "Invalid token: "..a:token
    endif
endfunction

function! s:Analyze(expr) abort
    let expr = s:ApplyTransformers(a:expr)
    if type(expr) == v:t_number
        return {env, k -> k(expr)}
    elseif type(expr) == v:t_string
        return {env, k -> k(s:ApplyEnv(env, expr))}
    elseif type(expr[0]) == v:t_list
        return s:GenApplication(expr)
    elseif expr[0] == "vlobj"
        return {env, k -> k(vl#Cadr(expr))}
    elseif expr[0] == "quote"
        return {env, k -> k(vl#Cadr(expr))}
    elseif expr[0] == "lambda"
        return s:GenProc(expr)
    elseif expr[0] == "if"
        return s:GenCond(expr)
    elseif expr[0] == "begin"
        return s:GenSequence(vl#Cdr(expr))
    elseif expr[0] == "define"
        return s:GenDefine(expr)
    elseif expr[0] == "set!"
        return s:GenSetBang(expr)
    elseif expr[0] == "call/cc"
        return s:GenCallCC(expr)
    elseif type(expr) == v:t_list
        return s:GenApplication(expr)
    else
        throw "Invalid expression: "..expr
    endif
endfunction

function! s:DeepLispList(elts) abort
    if vlutils#IsEmptyList(a:elts)
        return []
    elseif len(a:elts) == 1
        if type(a:elts[0]) == v:t_list
            return vl#Cons(s:DeepLispList(a:elts[0]), [])
        endif
        return add(a:elts, [])
    elseif type(a:elts[0]) == v:t_list
        return vl#Cons(s:DeepLispList(a:elts[0]), s:DeepLispList(a:elts[1:]))
    else
        return vl#Cons(a:elts[0], s:DeepLispList(a:elts[1:]))
    endif
endfunction

function! s:ExecProc(rator, rands, k) abort
    if vl#Car(a:rator) =~? '^primitive$'
        return a:k(vl#Cdr(a:rator)(a:rands))
    elseif vl#Car(a:rator) =~? '^cont-primitive$'
        return vl#Cdr(a:rator)(vl#Car(a:rands))
    elseif vl#Car(a:rator) =~? '^proc$'
        let Body = s:ProcBody(a:rator)
        let env = s:ProcEnv(a:rator)
        let params = s:ProcParams(a:rator)
        let result = Body(s:ExtendEnv(env, params, a:rands), a:k)
        return result
    elseif vl#Car(a:rator) =~? '^cont$'
        let Body = s:ProcBody(a:rator)
        let env = s:ProcEnv(a:rator)
        let params = s:ProcParams(a:rator)
        let args = vl#Cons(extend(["cont-primitive"], a:rands), [])
        let result = Body(s:ExtendEnv(env, params, args), a:k)
        return result
    endif
endfunction

function! vl#LispList(elts) abort
    if vlutils#IsEmptyList(a:elts)
        return []
    endif
    return vl#Cons(a:elts[0], vl#LispList(a:elts[1:]))
endfunction

function! s:LispLen(l) abort
    let list = a:l
    let total = 0
    while !vlutils#IsEmptyList(list)
        let total += 1
        let list = vl#Cdr(list)
    endwhile
    return total
endfunction

function! vl#Cons(a, d) abort
    return [a:a, a:d]
endfunction

function! vl#Car(list) abort
    return a:list[0]
endfunction

function! vl#Cdr(list) abort
    return a:list[1]
endfunction

function! vl#Cadr(list) abort
    return a:list[1][0]
endfunction

function! vl#Cddr(list) abort
    return a:list[1][1]
endfunction

function! vl#Caddr(list) abort
    return a:list[1][1][0]
endfunction

function! vl#Cdddr(list) abort
    return a:list[1][1][1]
endfunction

function! vl#Cadddr(list) abort
    return a:list[1][1][1][0]
endfunction

function! s:LispMap(proc, l) abort
    if vlutils#IsEmptyList(a:l)
        return []
    endif
    return vl#Cons(a:proc(vl#Car(a:l)), s:LispMap(a:proc, vl#Cdr(a:l)))
endfunction

function! s:DeepLispMap(proc, l) abort
    if vlutils#IsEmptyList(a:l)
        return []
    elseif type(vl#Car(a:l)) ==  v:t_list
        return vl#Cons(s:DeepLispMap(a:proc, vl#Car(a:l)), s:DeepLispMap(a:proc, vl#Cdr(a:l)))
    else
        return vl#Cons(a:proc(vl#Car(a:l)), s:DeepLispMap(a:proc, vl#Cdr(a:l)))
    endif
endfunction

function! s:ApplyEnv(env, var) abort
    if has_key(a:env, a:var)
        return get(a:env, a:var)
    elseif has_key(a:env, s:PREV_FRAME_KEY)
        return s:ApplyEnv(get(a:env, s:PREV_FRAME_KEY), a:var)
    else
        throw "Unbound variable: "..a:var
    endif
endfunction

function! s:ExtendEnv(env, vars, vals) abort
    let new_frame = {s:PREV_FRAME_KEY: a:env}
    let var_l = a:vars
    let val_l = a:vals
    while !vlutils#IsEmptyList(var_l)
        let new_frame[vl#Car(var_l)] = vl#Car(val_l)
        let var_l = vl#Cdr(var_l)
        let val_l = vl#Cdr(val_l)
    endwhile
    return new_frame
endfunction

function! s:SetVar(env, var, val) abort
    if has_key(a:env, a:var)
        let a:env[a:var] = a:val
    elseif has_key(a:env, s:PREV_FRAME_KEY)
        return s:SetVar(get(a:env, s:PREV_FRAME_KEY), a:var)
    else
        throw "Unbound variable: "..a:var
    endif
endfunction

function! s:DefineVar(env, var, val) abort
    let a:env[a:var] = a:val
endfunction

function! s:ApplyTransformers(expr) abort
    if type(vl#Car(a:expr)) == v:t_list
        return a:expr
    elseif has_key(g:VL_TRANSFORMERS, vl#Car(a:expr))
        return get(g:VL_TRANSFORMERS, vl#Car(a:expr))(a:expr)
    endif
    return a:expr
endfunction

function! s:Sequentially(proc1, proc2) abort
    return {env, k -> a:proc1(env, {x -> a:proc2(env, k)})}
endfunction

function! s:GenSequence(expr) abort
    let Analyzer = {x -> s:Analyze(x)}
    let closures = s:LispMap(Analyzer, a:expr)
    let C1 = vl#Car(closures)
    while !vlutils#IsEmptyList(vl#Cdr(closures))
        let C2 = vl#Cadr(closures)
        let C1 = s:Sequentially(C1, C2)
        let closures = vl#Cdr(closures)
    endwhile
    return C1
endfunction

function! s:ProcParams(proc) abort
    return vl#Cadr(a:proc)
endfunction

function! s:ProcEnv(proc) abort
    return vl#Cadddr(a:proc)
endfunction

function! s:ProcBody(proc) abort
    return vl#Caddr(a:proc)
endfunction

function! s:EvalClosureList(l, env, k) abort
    if vlutils#IsEmptyList(a:l)
        return a:k([])
    endif
    let InnerCont = {arg -> {args -> a:k(vl#Cons(arg, args))}}
    let OuterCont = {arg -> s:EvalClosureList(vl#Cdr(a:l), a:env, InnerCont(arg))}
    return vl#Car(a:l)(a:env, OuterCont)
endfunction

function! s:RandsCont(rator, k) abort
    return {args -> s:ExecProc(a:rator, args, a:k)}
endfunction

function! s:RatorCont(rands, env, k) abort
    return {rator -> s:EvalClosureList(a:rands, a:env, s:RandsCont(rator, a:k))}
endfunction

function! s:GenApplication(expr) abort
    let Rator = s:Analyze(vl#Car(a:expr))
    let rands = s:LispMap({x -> s:Analyze(x)}, vl#Cdr(a:expr))
    return {env, k -> Rator(env, s:RatorCont(rands, env, k))}
endfunction

function! s:GenDefine(expr) abort
    let ValClosure = s:Analyze(vl#Caddr(a:expr))
    let InnerClosure = {env, k -> {val -> k(s:DefineVar(env, vl#Cadr(a:expr), val))}}
    return {env, k -> ValClosure(env, InnerClosure(env, k))}
endfunction

function! s:AnalyzeCallCCProc(expr) abort
    let params = vl#Cadr(a:expr)
    let Body = s:GenSequence(vl#Cddr(a:expr))
    return {env, k -> k(vl#LispList(["cont", params, Body, env]))}
endfunction

function! s:GenCallCC(expr) abort
    let Proc = s:AnalyzeCallCCProc(vl#Cadr(a:expr))
    return {env, k -> Proc(env, {rator -> s:ExecProc(rator, [k], {x -> x})})}
endfunction

function! s:GenProc(expr) abort
    let params = vl#Cadr(a:expr)
    let Body = s:GenSequence(vl#Cddr(a:expr))
    return {env, k -> k(vl#LispList(["proc", params, Body, env]))}
endfunction

function! s:LetToLambda(expr) abort
    let bindings = vl#Cadr(a:expr)
    let body = vl#Cddr(a:expr)
    let vars = s:LispMap({x -> vl#Car(x)}, bindings)
    let vals = s:LispMap({x -> vl#Cadr(x)}, bindings)
    let lambda = vl#Cons("lambda", vl#Cons(vars, body))
    return vl#Cons(lambda, vals)
endfunction

function! s:TransformCond(clauses) abort
    if vlutils#IsEmptyList(a:clauses)
        return []
    endif
    let first = vl#Car(a:clauses)
    let rest = vl#Cdr(a:clauses)
    if vl#Car(first) =~? '^else$'
        if !vlutils#IsEmptyList(rest)
            throw "Invalid cond expression: else clause must be last"
        endif
        if vlutils#IsEmptyList(vl#Cddr(first))
            return vl#Cadr(first)
        endif
        return vl#Cons("begin", vl#Cdr(first))
    endif
    if vlutils#IsEmptyList(vl#Cddr(first))
        let consequent = vl#Cadr(first)
    else
        let consequent = vl#Cons("begin", vl#Cdr(first))
    endif
    if vlutils#IsEmptyList(rest)
        return vl#LispList(["if", vl#Car(first), consequent, g:vl_bool_f])
    else
        return vl#LispList(["if", vl#Car(first), consequent, s:TransformCond(rest)])
    endif
endfunction

function! s:CondToIf(expr) abort
    let clauses = vl#Cdr(a:expr)
    return s:TransformCond(clauses)
endfunction

function! s:IsTrue(expr) abort
    return a:expr != g:vl_bool_f
endfunction

function! s:GenCond(expr) abort
    let P_clsr = s:Analyze(vl#Cadr(a:expr))
    let C_clsr = {env, k -> s:Analyze(vl#Caddr(a:expr))(env, k)}
    let alt = vlutils#IsEmptyList(vl#Cdddr(a:expr)) ? g:vl_bool_f : vl#Cadddr(a:expr)
    let A_clsr = {env, k -> s:Analyze(alt)(env, k)}
    let Cont = {env, k -> {res -> s:IsTrue(res) ? C_clsr(env, k) : A_clsr(env, k)}}
    return {env, k -> P_clsr(env, Cont(env, k))}
endfunction

function! s:GenSetBang(expr) abort
    let Val_closure = s:Analyze(vl#Caddr(a:expr))
    let Cont = {env, k -> {val -> k(s:SetVar(env, vl#Cadr(a:expr), val))}}
    return {env, k -> Val_closure(env, Cont(env, k))}
endfunction

function! s:RegisterTransformer(name, funcref) abort
    if has_key(g:VL_TRANSFORMERS, a:name)
        throw "Duplicate syntax transformer: "..a:name
    endif
    let g:VL_TRANSFORMERS[a:name] = a:funcref
endfunction

function! s:RegisterTransformers(transformers) abort
    for [name, Transformer] in a:transformers
        call s:RegisterTransformer(name, Transformer)
    endfor
endfunction

call s:RegisterTransformers([
            \["let", funcref("s:LetToLambda")],
            \["cond", funcref("s:CondToIf")],
            \])
