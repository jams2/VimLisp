let s:PREV_FRAME_KEY = "__prev_frame"
let s:COUNTER = 0

function! s:InitRegisters() abort
    let s:EXPR_R = -1
    let s:CLOSURE_R = -1
    let s:CLOSURE_LIST_R = -1
    let s:ENV_R = -1
    let s:K_STACK = []
    let s:KVAL_STACK = []
    let s:HANDLER_STACK = []
    let s:ERR_R = -1
    let s:PROC_RATOR_R = -1
    let s:PROC_ARGS_R = -1
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

function! s:PushHandler(val) abort
    call add(s:HANDLER_STACK, a:val)
endfunction

function! s:PopHandler() abort
    let Handler = s:HANDLER_STACK[-1]
    let s:HANDLER_STACK = s:HANDLER_STACK[:-2]
    return Handler
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

function! s:ApplyHandler() abort
    call s:PushK(s:PopHandler())
    call s:PushKVal(s:ERR_R)
    return s:ApplyK()
endfunction

function! s:EvalClosure() abort
    return s:CLOSURE_R()
endfunction

function! vl#Eval(expr, env=g:VL_INITIAL_ENV) abort
    call s:InitRegisters()
    let tokens = vlparse#Tokenize(a:expr)
    let syntax = vlparse#Parse(tokens)
    call s:PushHandler(funcref("vl#TopLevelHandler"))
    let s:EXPR_R = syntax
    let s:CLOSURE_R = vl#Analyze()
    let s:ENV_R = a:env
    let Bounce = s:EvalClosure()
    return s:Trampoline(Bounce)
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

function! vl#Analyze() abort
    call s:PushK(funcref("s:EndCont"))
    let expr = vltrns#Transform(s:EXPR_R)
    let s:COUNTER += 1
    if type(expr) == v:t_number
        return s:GenConst(expr)
    elseif type(expr) == v:t_string
        return s:GenLookup(expr)
    elseif type(expr[0]) == v:t_list
        return s:GenApplication(expr)
    elseif expr[0] == "raise"
        return s:GenRaise(expr)
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

function! s:GenRaise(condition) abort
    let fname = "Condition"..s:COUNTER
    function! {fname}() closure abort
        let s:ERR_R = a:condition
        return s:ApplyHandler()
    endfunction
    return funcref(fname)
endfunction

function! s:GenConst(expr) abort
    let fname = "Closure"..s:COUNTER
    function! {fname}() closure abort
        call s:PushKVal(a:expr)
        return s:ApplyK()
    endfunction
    return funcref(fname)
endfunction

function! s:GenLookup(expr) abort
    let fname = "Closure"..s:COUNTER
    function! {fname}() closure abort
        call s:PushKVal(s:ApplyEnv(s:ENV_R, a:expr))
        return s:ApplyK()
    endfunction
    return funcref(fname)
endfunction

function! s:Sequentially(closures) abort
    let fname = "SequenceClosure"..s:COUNTER
    let closures = a:closures
    function! {fname}() closure abort
        call s:PushK({x -> x})
        while vl#Cdr(closures) != []
            let s:CLOSURE_R = closures[0]
            call s:EvalClosure()
            let closures = closures[1]
        endwhile
        let s:CLOSURE_R = closures[0]
        return s:EvalClosure()
    endfunction
    return funcref(fname)
endfunction

function! s:AndSequentially(closures) abort
    let fname = "AndClosure"..s:COUNTER
    let closures = a:closures
    function! {fname}() closure abort
        if closures == []
            call s:PushKVal(g:vl_bool_t)
            return s:ApplyK()
        endif
        call s:PushK({x -> x})
        while vl#Cdr(closures) != []
            let s:CLOSURE_R = closures[0]
            if !s:IsTrue(s:EvalClosure())
                call s:PushKVal(g:vl_bool_f)
                return s:ApplyK()
            endif
            let closures = closures[1]
        endwhile
        let s:CLOSURE_R = closures[0]
        return s:EvalClosure()
    endfunction
    return funcref(fname)
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
    let s:EXPR_R = vl#Cons("and", vl#Cadr(a:expr))
    let Preds = vl#Analyze()
    let s:EXPR_R = vl#Cons("begin", vl#Cddr(a:expr))
    let Body = vl#Analyze()
    let fname = "WhileClosure"..s:COUNTER
    function! {fname}() closure abort
        let result = g:vl_bool_f
        let s:CLOSURE_R = Preds
        let s:SAVED_CONT = s:NEXT_CONT
        let s:NEXT_CONT = s:END_CONT
        while s:IsTrue(s:Trampoline(s:EvalClosure()))
            let s:CLOSURE_R = Body
            let result = s:Trampoline(s:EvalClosure())
            let s:CLOSURE_R = Preds
            let s:NEXT_CONT = s:END_CONT
        endwhile
        let s:NEXT_CONT = s:SAVED_CONT
        call s:PushK(s:NEXT_CONT)
        call s:PushKVal(result)
        return s:ApplyK()
    endfunction
    return funcref(fname)
endfunction

function! s:ApplyProc() abort
    let rator = s:PROC_RATOR_R
    let rands = s:PROC_ARGS_R
    if vl#Car(rator) =~? '^prim$'
        let fname = "PrimitiveApply"..s:COUNTER
        function! {fname}() closure abort
            call s:PushKVal(vl#Cdr(rator)(rands))
            return s:ApplyK()
        endfunction
        return funcref(fname)
    elseif vl#Car(rator) =~? '^cont-prim$'
        let fname = "ApplyContPrim"..s:COUNTER
        function {fname}() closure abort
            call s:PopK()
            call s:PushKVal(vl#Car(rands))
            return s:ApplyK()
        endfunction
        return funcref(fname)
    elseif vl#Car(rator) =~? '^proc$'
        let Body = s:ProcBody(rator)
        let env = s:ProcEnv(rator)
        let params = s:ProcParams(rator)
        let fname = "ProcBounce"..s:COUNTER
        function! {fname}() closure abort
            let s:CLOSURE_R = Body
            let s:ENV_R = s:ExtendEnv(env, params, rands)
            return s:EvalClosure()
        endfunction
        return funcref(fname)
    elseif vl#Car(rator) =~? '^cont$'
        let Body = s:ProcBody(rator)
        let env = s:ProcEnv(rator)
        let params = s:ProcParams(rator)
        let rands = vl#Cons(extend(["cont-prim"], rands), [])
        let fname = "ContBounce"..s:COUNTER
        function! {fname}() closure abort
            let s:CLOSURE_R = Body
            let s:ENV_R = s:ExtendEnv(env, params, rands)
            return s:EvalClosure()
        endfunction
        return funcref(fname)
    endif
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

function! vl#LispList(elts) abort
    if vlutils#IsEmptyList(a:elts)
        return []
    endif
    return vl#Cons(a:elts[0], vl#LispList(a:elts[1:]))
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

function! s:MapAnalyze(expr) abort
    let s:EXPR_R = a:expr
    return vl#Analyze()
endfunction

function! s:GenSequence(expr, sequencer) abort
    let closures = vl#LispMap(funcref("s:MapAnalyze"), a:expr)
    return a:sequencer(closures)
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

function! s:EvalClosureList() abort
    let s:COUNTER += 1
    let closures = s:CLOSURE_LIST_R
    if vlutils#IsEmptyList(closures)
        call s:PushKVal([])
        return s:ApplyK()
    endif
    let outercontname = "EvalClosureListOuterCont"..s:COUNTER
    let innercontname = "EvalClosureListInnerCont"..s:COUNTER
    function! {outercontname}(arg) closure abort
        function! {innercontname}(args) closure abort
            call s:PushKVal(vl#Cons(a:arg, a:args))
            return s:ApplyK()
        endfunction
        let s:CLOSURE_LIST_R = vl#Cdr(closures)
        call s:PushK(funcref(innercontname))
        return s:EvalClosureList()
    endfunction
    let s:CLOSURE_R = vl#Car(closures)
    call s:PushK(funcref(outercontname))
    return s:EvalClosure()
endfunction

function! s:RandsCont(rator) abort
    let fname = "RandsCont"..s:COUNTER
    function! {fname}(args) closure abort
        let s:PROC_RATOR_R = a:rator
        let s:PROC_ARGS_R = a:args
        return s:ApplyProc()
    endfunction
    return funcref(fname)
endfunction

function! s:RatorCont(rands) abort
    let fname = "RatorCont"..s:COUNTER
    function! {fname}(rator) closure abort
        call s:PushK(s:RandsCont(a:rator))
        let s:CLOSURE_LIST_R = a:rands
        return s:EvalClosureList()
    endfunction
    return funcref(fname)
endfunction

function! s:GenApplication(expr) abort
    let s:EXPR_R = vl#Car(a:expr)
    let Rator = vl#Analyze()
    let rands = vl#LispMap(funcref("s:MapAnalyze"), vl#Cdr(a:expr))
    let fname = "ApplicationClosure"..s:COUNTER
    function! {fname}() closure abort
        let s:CLOSURE_R = Rator
        call s:PushK(s:RatorCont(rands))
        return s:EvalClosure()
    endfunction
    return funcref(fname)
endfunction

function! s:GenDefine(expr) abort
    let s:EXPR_R = vl#Caddr(a:expr)
    let ValClosure = vl#Analyze()
    let contname = "DefineCont"..s:COUNTER
    let fname = "DefineClosure"..s:COUNTER
    function! {fname}() closure abort
        function! {contname}(val) closure abort
            call s:PushKVal(s:DefineVar(s:ENV_R, vl#Cadr(a:expr), a:val))
            return s:ApplyK()
        endfunction
        let s:CLOSURE_R = ValClosure
        call s:PushK(funcref(contname))
        return s:EvalClosure()
    endfunction
    return funcref(fname)
endfunction

function! s:AnalyzeCallCCProc() abort
    let expr = s:EXPR_R
    let params = vl#Cadr(expr)
    let Body = s:GenSequence(vl#Cddr(expr), funcref("s:Sequentially"))
    let fname = "CallCCK"..s:COUNTER
    function! {fname}() closure abort
        call s:PushKVal(vl#LispList(["cont", params, Body, s:ENV_R]))
        return s:ApplyK()
    endfunction
    return funcref(fname)
endfunction

function! s:GenCallCC(expr) abort
    let s:EXPR_R = vl#Cadr(a:expr)
    let Proc = s:AnalyzeCallCCProc()
    let fname = "CallCC"..s:COUNTER
    let contname = "CallCCK"..s:COUNTER
    function! {fname}() closure abort
        function! {contname}(rator) closure abort
            let s:PROC_RATOR_R = a:rator
            let s:PROC_ARGS_R = []
            return s:ApplyProc()
        endfunction
        let s:CLOSURE_R = Proc
        call s:PushK(funcref(contname))
        return s:EvalClosure()
    endfunction
    return funcref(fname)
endfunction

function! s:GenProc(expr) abort
    let params = vl#Cadr(a:expr)
    let Body = s:GenSequence(vl#Cddr(a:expr), funcref("s:Sequentially"))
    let fname = "ProcClosure"..s:COUNTER
    function! {fname}() closure abort
        call s:PushKVal(vl#LispList(["proc", params, Body, s:ENV_R]))
        return s:ApplyK()
    endfunction
    return funcref(fname)
endfunction

function! s:IsTrue(expr) abort
    return a:expr != g:vl_bool_f
endfunction

function! s:GenCond(expr) abort
    let s:EXPR_R = vl#Cadr(a:expr)
    let P_clsr = vl#Analyze()
    let consequentname = "CondConsequent"..s:COUNTER
    function! {consequentname}() closure abort
        let s:EXPR_R = vl#Caddr(a:expr)
        let s:CLOSURE_R = vl#Analyze()
        return s:EvalClosure()
    endfunction
    let C_clsr = funcref(consequentname)

    let alt = vlutils#IsEmptyList(vl#Cdddr(a:expr)) ? g:vl_bool_f : vl#Cadddr(a:expr)
    let altname = "CondAlt"..s:COUNTER
    function! {altname}() closure abort
        let s:EXPR_R = alt
        let s:CLOSURE_R = vl#Analyze()
        return s:EvalClosure()
    endfunction
    let A_clsr = funcref(altname)

    let fname = "CondClosure"..s:COUNTER
    let contname = "CondCont"..s:COUNTER
    function! {fname}() closure abort
        function! {contname}(res) closure abort
            if s:IsTrue(a:res)
                let s:CLOSURE_R = C_clsr
                return s:EvalClosure()
            else
                let s:CLOSURE_R = A_clsr
                return s:EvalClosure()
            endif
        endfunction
        let s:CLOSURE_R = P_clsr
        call s:PushK(funcref(contname))
        return s:EvalClosure()
    endfunction
    return funcref(fname)
endfunction

function! s:GenSetBang(expr) abort
    let s:EXPR_R = vl#Caddr(a:expr)
    let ValClosure = vl#Analyze()
    let contname = "SetBangCont"..s:COUNTER
    let fname = "SetBangClosure"..s:COUNTER
    function! {fname}() closure abort
        function! {contname}(val) closure abort
            call s:PushKVal(s:SetVar(s:ENV_R, vl#Cadr(a:expr), a:val))
            return s:ApplyK()
        endfunction
        let s:CLOSURE_R = ValClosure
        call s:PushK(funcref(contname))
        return s:EvalClosure()
    endfunction
    return funcref(fname)
endfunction

function! vl#TopLevelHandler(exception)
    return ["condition", vl#Cdr(a:exception)]
endfunction
