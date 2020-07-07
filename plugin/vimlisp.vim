let g:VL_TRANSFORMERS = {}
let s:PREV_FRAME_KEY = "__prev_frame"
let s:END_CONT = {val -> val}
let s:OUTER_PARENS_R ='\(^(\)\|\()\)$'
let s:SYMBOL_R = '^[a-z][a-z0-9-?!*^]*$'
let s:PRIMOP_R = '^[+*/=-]$\|^\(call/cc\)$'
let s:STRING_CONST_R = '^".*"$'
let s:NUMBER_R = '^-\?\d\+$'
let s:BOOL_R = '^\(#t\)\|\(#f\)$'
let s:QUOTE = 39
let s:STR_DELIM = 34
let s:LEFT_PAREN = 40
let s:RIGHT_PAREN = 41
let s:SPACE = 32
let s:TAB = 9
let s:NEWLINE = 13
let s:TRUE = '#t'
let s:FALSE = '#f'

function! VlEval(expr, env=g:VL_INITIAL_ENV) abort
    let tokens = split(substitute(a:expr, '\([()"'']\)', ' \1 ', 'g'))
    let syntax = VlParse(tokens)
    if type(syntax) == v:t_list
        let syntax = DeepLispList(syntax)
    endif
    return VlAnalyze(syntax)(a:env, s:END_CONT)
endfunction

function! VlParse(tokens) abort
    if len(a:tokens) == 0
        throw "Unterminated expression"
    elseif len(a:tokens) == 1
        return VlParseAtom(a:tokens[0])
    elseif a:tokens[0] == "("
        return VlParseList(a:tokens)
    elseif a:tokens[0] == '"'
        return VlParseString(a:tokens)
    elseif a:tokens[0] == "'"
        return ['quote', VlParse(a:tokens[1:])]
    endif
endfunction

function! VlParseList(tokens) abort
    if a:tokens[0] != "("
        throw "Invalid list structure -- VlParseList"
    endif
    let exprlist = []
    let i = 1  "skip open paren
    while i < len(a:tokens)
        let token = a:tokens[i]
        if token == "("
            let listlen = NestedNonTerminalLen(a:tokens[i:], "(", ")")
            call add(exprlist, VlParseList(a:tokens[i:i+listlen]))
            let i += listlen
        elseif token == ")"
            return exprlist
        elseif token == '"'
            let stringlen = NonTerminalLen(a:tokens[i:], '"')
            call add(exprlist, VlParseString(a:tokens[i:i+stringlen-1]))
            let i += stringlen
        elseif token == "'"
            if a:tokens[i+1] == "("
                let listlen = NestedNonTerminalLen(a:tokens[i+1:], "(", ")")
                call add(exprlist, ["quote", VlParseList(a:tokens[i+1:i+listlen])])
                let i += listlen + 1
            else
                call add(exprlist, ["quote", a:tokens[i+1]])
                let i += 2
            endif
        else
            call add(exprlist, VlParseAtom(token))
            let i += 1
        endif
    endwhile
    return exprlist
endfunction

function! VlParseString(tokens) abort
    if a:tokens[0] != '"' || a:tokens[-1] != '"'
        throw "Invalid string literal: "..string(join(a:tokens))
    endif
    return StrFactory(join(a:tokens[1:-1]))
endfunction

function! StrFactory(str) abort
    return ['str', #{_chars: str2list(a:str)}]
endfunction

function! NestedNonTerminalLen(tokens, open, close) abort
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

function! NonTerminalLen(tokens, delim) abort
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

function! VlParseAtom(token) abort
    if a:token =~ s:NUMBER_R
        return a:token - 0
    elseif a:token =~ s:STRING_CONST_R
        return a:token
    elseif a:token =~? s:SYMBOL_R
        return a:token
    elseif a:token =~? s:PRIMOP_R
        return a:token
    elseif a:token =~? s:BOOL_R
        return a:token
    else
        throw "Invalid token: "..a:token
    endif
endfunction

function! VlAnalyze(expr) abort
    let expr = ApplyTransformers(a:expr)
    if type(expr) == v:t_number
        return {env, k -> k(expr)}
    elseif type(expr) == v:t_string
        return {env, k -> k(ApplyEnv(env, expr))}
    elseif type(expr[0]) == v:t_list
        return GenApplication(expr)
    elseif expr[0] =~? '^str$'
        return {env, k -> k(get(Cadr(expr), "_chars"))}
    elseif expr[0] =~? '^quote$'
        return {env, k -> k(Cadr(expr))}
    elseif expr[0] =~? '^lambda$'
        return GenProc(expr)
    elseif expr[0] =~? '^if$'
        return GenCond(expr)
    elseif expr[0] =~? '^begin$'
        return GenSequence(Cdr(expr))
    elseif expr[0] =~? '^define$'
        return GenDefine(expr)
    elseif expr[0] =~? '^set!$'
        return GenSetBang(expr)
    elseif expr[0] =~? '^call/cc$'
        return GenCallCC(expr)
    elseif type(expr) == v:t_list
        return GenApplication(expr)
    else
        throw "Invalid expression: "..expr
    endif
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

function! ExecProc(rator, rands, k)
    if Car(a:rator) =~? '^primitive$'
        return a:k(Cdr(a:rator)(a:rands))
    elseif Car(a:rator) =~? '^cont-primitive$'
        return Cdr(a:rator)(Car(a:rands))
    elseif Car(a:rator) =~? '^proc$'
        let Body = ProcBody(a:rator)
        let env = ProcEnv(a:rator)
        let params = ProcParams(a:rator)
        let result = Body(ExtendEnv(env, params, a:rands), a:k)
        return result
    elseif Car(a:rator) =~? '^cont$'
        let Body = ProcBody(a:rator)
        let env = ProcEnv(a:rator)
        let params = ProcParams(a:rator)
        let args = Cons(extend(['cont-primitive'], a:rands), [])
        let result = Body(ExtendEnv(env, params, args), a:k)
        return result
    endif
endfunction

function! IsEmptyList(obj) abort
    return type(a:obj) == v:t_list && a:obj == []
endfunction

function! IsWhiteSpace(char) abort
    return a:char == s:SPACE || a:char == s:TAB || a:char == s:NEWLINE
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

function! PrimitiveCons(l) abort
    return [Car(a:l), Cadr(a:l)]
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

function ApplyTransformers(expr) abort
    if type(Car(a:expr)) == v:t_list
        return a:expr
    elseif has_key(g:VL_TRANSFORMERS, Car(a:expr))
        return get(g:VL_TRANSFORMERS, Car(a:expr))(a:expr)
    endif
    return a:expr
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

function! LetToLambda(expr) abort
    let bindings = Cadr(a:expr)
    let body = Cddr(a:expr)
    let vars = LispMap({x -> Car(x)}, bindings)
    let vals = LispMap({x -> Cadr(x)}, bindings)
    let lambda = Cons('lambda', Cons(vars, body))
    return Cons(lambda, vals)
endfunction

function! TransformCond(clauses) abort
    if IsEmptyList(a:clauses)
        return []
    endif
    let first = Car(a:clauses)
    let rest = Cdr(a:clauses)
    if Car(first) =~? '^else$'
        if !IsEmptyList(rest)
            throw "Invalid cond expression: else clause must be last"
        endif
        if IsEmptyList(Cddr(first))
            return Cadr(first)
        endif
        return Cons('begin', Cdr(first))
    endif
    if IsEmptyList(Cddr(first))
        let consequent = Cadr(first)
    else
        let consequent = Cons('begin', Cdr(first))
    endif
    if IsEmptyList(rest)
        return LispList(['if', Car(first), consequent, s:FALSE])
    else
        return LispList(['if', Car(first), consequent, TransformCond(rest)])
    endif
endfunction

function! CondToIf(expr) abort
    let clauses = Cdr(a:expr)
    return TransformCond(clauses)
endfunction

function! IsTrue(expr) abort
    return a:expr != s:FALSE
endfunction

function! GenCond(expr) abort
    let P_clsr = VlAnalyze(Cadr(a:expr))
    let C_clsr = {env, k -> VlAnalyze(Caddr(a:expr))(env, k)}
    let alt = IsEmptyList(Cdddr(a:expr)) ? s:FALSE : Cadddr(a:expr)
    let A_clsr = {env, k -> VlAnalyze(alt)(env, k)}
    let Cont = {env, k -> {res -> IsTrue(res) ? C_clsr(env, k) : A_clsr(env, k)}}
    return {env, k -> P_clsr(env, Cont(env, k))}
endfunction

function! GenSetBang(expr) abort
    let Val_closure = VlAnalyze(Caddr(a:expr))
    let Cont = {env, k -> {val -> k(SetVar(env, Cadr(a:expr), val))}}
    return {env, k -> Val_closure(env, Cont(env, k))}
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

function! RegisterTransformer(name, funcref) abort
    if has_key(g:VL_TRANSFORMERS, a:name)
        throw "Duplicate syntax transformer: "..a:name
    endif
    let g:VL_TRANSFORMERS[a:name] = a:funcref
endfunction

function! RegisterTransformers(transformers) abort
    for [name, Transformer] in a:transformers
        call RegisterTransformer(name, Transformer)
    endfor
endfunction

let g:VL_INITIAL_ENV = {
            \'+': ['primitive', funcref('VlAdd')],
            \'cons': ['primitive', funcref('PrimitiveCons')],
            \'#t': s:TRUE,
            \'#f': s:FALSE,
            \}

function! VlEvalCommand(expr) abort
    echo VlEval(a:expr)
endfunction

call RegisterTransformers([
            \['let', funcref('LetToLambda')],
            \['cond', funcref('CondToIf')],
            \])

command! -nargs=* VlEval :call VlEvalCommand(<q-args>)
