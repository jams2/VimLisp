let s:PREV_FRAME_KEY = "__prev_frame"
let s:END_CONT = {val -> val}
let s:SYMBOL_R = '^[a-zA-Z0-9?!*^/\\><=+_-]\+$'
let s:STRING_CONST_R = '^".*"$'
let s:NUMBER_R = '^-\?\d\+$'
let s:BOOL_R = '^\(#t\)\|\(#f\)$'
let s:TOKEN_R = '\([()"'']\)'
let s:R_PAREN = ")"
let s:L_PAREN = "("
let s:D_QUOTE = '"'
let s:BACKTICK = "`"
let s:QUOTE = "'"
let s:DOT = "."
let s:COUNTER = 0

let s:EXPR_R = -1
let s:ENV_R = -1
let s:K_STACK = []
let s:KVAL_STACK = []
let s:PROC_R = -1

function! s:InitRegisters() abort
    let s:EXPR_R = -1
    let s:ENV_R = -1
    let s:K_STACK = []
    let s:KVAL_STACK = []
    let s:PROC_R = -1
endfunction

function! s:PushK(k) abort
    call add(s:K_STACK, a:k)
endfunction

function! s:PopK() abort
    let Kn = s:K_STACK[-1]
    let s:K_STACK = s:K_STACK[:-2]
    return Kn
endfunction

function! s:PushKVal(val) abort
    call add(s:KVAL_STACK, a:val)
endfunction

function! s:PopKVal() abort
    let val = s:KVAL_STACK[-1]
    let s:KVAL_STACK = s:KVAL_STACK[:-2]
    return val
endfunction

function! s:EndCont(val) abort
    return a:val
endfunction

function! s:Trampoline(bounce) abort
    if type(a:bounce) == v:t_func
        let Val = a:bounce()
        while type(Val) == v:t_func
            let Val = Val()
        endwhile
        return Val
    else
        return a:bounce
    endif
endfunction

function! s:ApplyK() abort
    return s:PopK()(s:PopKVal())
endfunction

function! vl#Eval(expr, env=g:VL_INITIAL_ENV) abort
    call s:InitRegisters()
    let tokens = vl#Tokenize(a:expr)
    let syntax = vl#Parse(tokens)
    let Program = vl#Analyze(syntax)
    let Bounce = Program(a:env, funcref("s:EndCont"))
    return s:Trampoline(Bounce)
endfunction

function! vl#Tokenize(expr) abort
    return split(substitute(a:expr, s:TOKEN_R, ' \1 ', 'g'))
endfunction

function! vl#SplitExprs(source) abort
    let remaining = a:source
    let exprs = []
    while len(remaining) > 0
        if remaining[0] =~ '\s'
            let remaining = remaining[1:]
            continue
        endif
        let exprlen = vl#ExprLen(remaining)
        let nextexpr = remaining[:exprlen-1]
        call add(exprs, nextexpr)
        let remaining = remaining[exprlen:]
    endwhile
    return exprs
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

function! vl#Parse(tokens) abort
    if len(a:tokens) == 0
        throw "unterminated expression"
    elseif a:tokens[0] == s:QUOTE
        return vl#LispList(["quote", s:ParseQuoted(a:tokens[1:])])
    elseif index(a:tokens, s:DOT) > -1
        throw "invalid syntax"
    elseif len(a:tokens) == 1
        return s:ParseAtom(a:tokens[0])
    elseif a:tokens[0] == s:L_PAREN
        return vl#LispList(s:ParseList(a:tokens))
    elseif a:tokens[0] == s:D_QUOTE
        return vl#LispList(s:ParseString(a:tokens))
    endif
endfunction

function! vl#ExprLen(tokens) abort
    let first = a:tokens[0]
    if first == s:L_PAREN
        return vl#NestedNonTerminalLen(a:tokens)
    elseif first == s:D_QUOTE
        return vl#NonTerminalLen(a:tokens)
    elseif first == s:QUOTE
        if a:tokens[1] == s:L_PAREN
            return 1 + vl#NestedNonTerminalLen(a:tokens[1:])
        endif
        return 2
    else
        return 1
    endif
endfunction

function! s:ParseQuoted(tokens) abort
    if a:tokens[0] == s:L_PAREN && index(a:tokens, s:DOT) > 0
        return s:ParsePair(a:tokens)
    else
        return vl#Parse(a:tokens)
    endif
endfunction

function! s:ParseList(tokens) abort
    if a:tokens[0] != s:L_PAREN || a:tokens[-1] != s:R_PAREN
        throw "Invalid list structure -- s:ParseList"
    endif
    let tokens = a:tokens[1:-2]  " remove outer parens
    let exprlist = []
    let i = 0
    while i < len(tokens)
        let token = tokens[i]
        if token == s:R_PAREN
            throw "Unexpected list termination -- s:ParseList"
        elseif token == s:L_PAREN
            let sublistlen = vl#NestedNonTerminalLen(tokens[i:])
            call add(exprlist, vl#Parse(tokens[i:i+sublistlen-1]))
            let i += sublistlen
        elseif token == s:D_QUOTE
            let stringlen = vl#NonTerminalLen(tokens[i:])
            call add(exprlist, vl#Parse(tokens[i:i+stringlen-1]))
            let i += stringlen
        elseif token == s:QUOTE
            if tokens[i+1] == s:L_PAREN
                let quotelen = vl#NestedNonTerminalLen(tokens[i+1:])
                call add(exprlist, vl#Parse(tokens[i:i+quotelen]))
                let i += 1 + quotelen
            else
                call add(exprlist, vl#Parse(tokens[i:i+1]))
                let i += 2
            endif
        else
            call add(exprlist, vl#Parse(tokens[i:i]))
            let i += 1
        endif
    endwhile
    return exprlist
endfunction

function! s:ParsePair(tokens) abort
    let dotpos = index(a:tokens, s:DOT)
    if a:tokens[0] != s:L_PAREN || dotpos == -1
        throw "Invalid pair structure -- s:ParseList"
    endif
    let lhs = s:ParseQuoted(a:tokens[1:dotpos-1])
    let rhs = s:ParseQuoted(a:tokens[dotpos+1:-2])
    return vl#Cons(lhs, rhs)
endfunction

function! s:ParseString(tokens) abort
    if a:tokens[0] != s:D_QUOTE || a:tokens[-1] != s:D_QUOTE
        throw "Invalid string literal: "..string(join(a:tokens))
    endif
    return s:StrFactory(join(a:tokens[1:-2]))
endfunction

function! s:StrFactory(str) abort
    return ["vlobj", #{_t: g:vl_t_lstr, _chars: str2list(a:str)}]
endfunction

function! vl#NestedNonTerminalLen(tokens, open=s:L_PAREN, close=s:R_PAREN) abort
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

function! vl#NonTerminalLen(tokens, delim=s:D_QUOTE) abort
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
    elseif a:token == s:DOT
        return a:token
    else
        throw "Invalid token: "..a:token
    endif
endfunction

function! vl#Analyze(expr) abort
    let expr = vltrns#Transform(a:expr)
    let s:COUNTER += 1
    if type(expr) == v:t_number
        return s:GenConst(expr)
    elseif type(expr) == v:t_string
        return s:GenLookup(expr)
    elseif type(expr[0]) == v:t_list
        return s:GenApplication(expr)
    elseif expr[0] == "vlobj"
        return s:GenConst(vl#Cadr(expr))
    elseif expr[0] == "quote"
        return s:GenConst(vl#Cadr(expr))
    elseif expr[0] == "lambda"
        return s:GenProc(expr)
    elseif expr[0] == "if"
        return s:GenCond(expr)
    elseif expr[0] == "and"
        return s:GenAnd(expr)
    elseif expr[0] == "while"
        return s:GenWhile(expr)
    elseif expr[0] == "begin"
        return s:GenSequence(vl#Cdr(expr), funcref("s:Sequentially"))
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

function! s:GenConst(expr) abort
    let fname = "Closure"..s:COUNTER
    function! {fname}(env, k) closure abort
        call s:PushK(a:k)
        call s:PushKVal(a:expr)
        return s:ApplyK()
    endfunction
    return funcref(fname)
endfunction

function! s:GenLookup(expr) abort
    let fname = "Closure"..s:COUNTER
    function! {fname}(env, k) closure abort
        call s:PushK(a:k)
        call s:PushKVal(s:ApplyEnv(a:env, a:expr))
        return s:ApplyK()
    endfunction
    return funcref(fname)
endfunction

function! s:AndSequentially(p1, p2) abort
    let fname = "AndClosureCont"..s:COUNTER
    let contfactname = "AndContFactory"..s:COUNTER

    function! {contfactname}(env, k) closure abort
        function! {fname}(val) closure abort
            if s:IsTrue(a:val)
                return a:p2(a:env, a:k)
            else
                call s:PushK(a:k)
                call s:PushKVal(g:vl_bool_f)
                return s:ApplyK()
            endif
        endfunction
        return funcref(fname)
    endfunction

    return {env, k -> a:p1(env, function(contfactname)(env, k))}
endfunction

function! s:GenAnd(expr)
    return s:GenSequence(vl#Cdr(a:expr), funcref("s:AndSequentially"))
endfunction

function! s:GenWhile(expr) abort
    " 1. Analyze predicates (AND).
    " 2. Analyze body.
    " 3. Return closure that repeatedly executes body closure
    "       while all predicates are true.
    " The intermediate calculations get passed the end_cont
    "   so as not to continue execution until the loop is
    "   complete.
    let Preds = vl#Analyze(vl#Cons("and", vl#Cadr(a:expr)))
    let Body = vl#Analyze(vl#Cons("begin", vl#Cddr(a:expr)))
    function! WhileClosure(env, k) abort closure
        let result = g:vl_bool_f
        while s:IsTrue(s:Trampoline(Preds(a:env, s:END_CONT)))
            let result = s:Trampoline(Body(a:env, s:END_CONT))
        endwhile
        call s:PushK(a:k)
        call s:PushKVal(result)
        return s:ApplyK()
    endfunction
    return funcref("WhileClosure")
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

function! s:ApplyProc(rator, rands, k) abort
    if vl#Car(a:rator) =~? '^prim$'
        let fname = "PrimitiveApply"..s:COUNTER
        function! {fname}() closure abort
            call s:PushK(a:k)
            call s:PushKVal(vl#Cdr(a:rator)(a:rands))
            return s:ApplyK()
        endfunction
        return funcref(fname)
    elseif vl#Car(a:rator) =~? '^cont-prim$'
        return {-> vl#Cdr(a:rator)(vl#Car(a:rands))}
    elseif vl#Car(a:rator) =~? '^proc$'
        let Body = s:ProcBody(a:rator)
        let env = s:ProcEnv(a:rator)
        let params = s:ProcParams(a:rator)
        return {-> Body(s:ExtendEnv(env, params, a:rands), a:k)}
    elseif vl#Car(a:rator) =~? '^cont$'
        let Body = s:ProcBody(a:rator)
        let env = s:ProcEnv(a:rator)
        let params = s:ProcParams(a:rator)
        let args = vl#Cons(extend(["cont-prim"], a:rands), [])
        return {-> Body(s:ExtendEnv(env, params, args), a:k)}
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

function! vl#LispMap(proc, l) abort
    if vlutils#IsEmptyList(a:l)
        return []
    endif
    return vl#Cons(a:proc(vl#Car(a:l)), vl#LispMap(a:proc, vl#Cdr(a:l)))
endfunction

function! s:DeepLispMap(proc, l) abort
    if vlutils#IsEmptyList(a:l)
        return []
    elseif type(vl#Car(a:l)) ==  v:t_list
        let first = s:DeepLispMap(a:proc, vl#Car(a:l))
        let rest = s:DeepLispMap(a:proc, vl#Cdr(a:l))
        return vl#Cons(first, rest)
    else
        let first = a:proc(vl#Car(a:l))
        let rest = s:DeepLispMap(a:proc, vl#Cdr(a:l))
        return vl#Cons(first, rest)
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

function! s:Sequentially(proc1, proc2) abort
    return {env, k -> a:proc1(env, {x -> a:proc2(env, k)})}
endfunction

function! s:GenSequence(expr, sequencer) abort
    let Analyzer = {x -> vl#Analyze(x)}
    let closures = vl#LispMap(Analyzer, a:expr)
    let C1 = vl#Car(closures)
    while !vlutils#IsEmptyList(vl#Cdr(closures))
        let C2 = vl#Cadr(closures)
        let C1 = a:sequencer(C1, C2)
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
    let s:COUNTER += 1
    if vlutils#IsEmptyList(a:l)
        call s:PushK(a:k)
        call s:PushKVal([])
        return s:ApplyK()
    endif
    let outercontname = "EvalClosureListOuterCont"..s:COUNTER
    let innercontname = "EvalClosureListInnerCont"..s:COUNTER
    function! {outercontname}(arg) closure abort
        function! {innercontname}(args) closure abort
            call s:PushK(a:k)
            call s:PushKVal(vl#Cons(a:arg, a:args))
            return s:ApplyK()
        endfunction
        return s:EvalClosureList(vl#Cdr(a:l), a:env, funcref(innercontname))
    endfunction
    return vl#Car(a:l)(a:env, funcref(outercontname))
endfunction

function! s:RandsCont(rator, k) abort
    return {args -> s:ApplyProc(a:rator, args, a:k)}
endfunction

function! s:RatorCont(rands, env, k) abort
    return {rator -> s:EvalClosureList(a:rands, a:env, s:RandsCont(rator, a:k))}
endfunction

function! s:GenApplication(expr) abort
    let Rator = vl#Analyze(vl#Car(a:expr))
    let rands = vl#LispMap({x -> vl#Analyze(x)}, vl#Cdr(a:expr))
    return {env, k -> Rator(env, s:RatorCont(rands, env, k))}
endfunction

function! s:GenDefine(expr) abort
    let ValClosure = vl#Analyze(vl#Caddr(a:expr))
    let contname = "DefineCont"..s:COUNTER
    let fname = "DefineClosure"..s:COUNTER
    function! {fname}(env, k) closure abort
        function! {contname}(val) closure abort
            call s:PushK(a:k)
            call s:PushKVal(s:DefineVar(a:env, vl#Cadr(a:expr), a:val))
            return s:ApplyK()
        endfunction
        return ValClosure(a:env, funcref(contname))
    endfunction
    return funcref(fname)
endfunction

function! s:AnalyzeCallCCProc(expr) abort
    let params = vl#Cadr(a:expr)
    let Body = s:GenSequence(vl#Cddr(a:expr), funcref("s:Sequentially"))
    let fname = "CallCCK"..s:COUNTER
    function! {fname}(env, k) closure abort
        call s:PushK(a:k)
        call s:PushKVal(vl#LispList(["cont", params, Body, a:env]))
        return s:ApplyK()
    endfunction
    return funcref(fname)
endfunction

function! s:GenCallCC(expr) abort
    let Proc = s:AnalyzeCallCCProc(vl#Cadr(a:expr))
    return {env, k -> Proc(env, {rator -> s:ApplyProc(rator, [k], {x -> x})})}
endfunction

function! s:GenProc(expr) abort
    let params = vl#Cadr(a:expr)
    let Body = s:GenSequence(vl#Cddr(a:expr), funcref("s:Sequentially"))
    let fname = "ProcClosure"..s:COUNTER
    function! {fname}(env, k) closure abort
        call s:PushK(a:k)
        call s:PushKVal(vl#LispList(["proc", params, Body, a:env]))
        return s:ApplyK()
    endfunction
    return funcref(fname)
endfunction

function! s:IsTrue(expr) abort
    return a:expr != g:vl_bool_f
endfunction

function! s:GenCond(expr) abort
    let P_clsr = vl#Analyze(vl#Cadr(a:expr))
    let C_clsr = {env, k -> vl#Analyze(vl#Caddr(a:expr))(env, k)}
    let alt = vlutils#IsEmptyList(vl#Cdddr(a:expr)) ? g:vl_bool_f : vl#Cadddr(a:expr)
    let A_clsr = {env, k -> vl#Analyze(alt)(env, k)}
    let Cont = {env, k -> {res -> s:IsTrue(res) ? C_clsr(env, k) : A_clsr(env, k)}}
    return {env, k -> P_clsr(env, Cont(env, k))}
endfunction

function! s:GenSetBang(expr) abort
    let ValClosure = vl#Analyze(vl#Caddr(a:expr))
    let contname = "SetBangCont"..s:COUNTER
    let fname = "SetBangClosure"..s:COUNTER
    function! {fname}(env, k) closure abort
        function! {contname}(val) closure abort
            call s:PushK(a:k)
            call s:PushKVal(s:SetVar(a:env, vl#Cadr(a:expr), a:val))
            return s:ApplyK()
        endfunction
        return ValClosure(a:env, funcref(contname))
    endfunction
    return funcref(fname)
endfunction

function! vl#TopLevelHandler(exception)
    let e = a:exception[0]
    echohl WarningMsg
    echomsg "Uncaught exception: "..vlutils#PrettyPrint(e)
    echohl Normal
endfunction
