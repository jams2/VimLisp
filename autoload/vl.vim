function! s:InitRegisters() abort
    let s:COUNTER = 0
    let s:EXPR_R = -1
    let s:CLOSURE_R = -1
    let s:CLOSURE_LIST_R = -1
    let s:ENV_R = -1
    let s:K_STACK = []
    let s:KVAL_STACK = []
    let s:HANDLER_STACK = []
    let s:LABELS = []
    let s:ERR_R = -1
    let s:PROC_RATOR_R = -1
    let s:PROC_ARGS_R = -1
endfunction

function! s:GenLabel(name) abort
    let label = "s:"..a:name..s:COUNTER
    call add(s:LABELS, label)
    return label
endfunction

function! s:Cleanup() abort
    for label in uniq(s:LABELS)
        eval("delf "..label)
    endfor
endfunction

function! s:PushHandler(val) abort
    call add(s:HANDLER_STACK, a:val)
endfunction

function! s:PopHandler() abort
    return remove(s:HANDLER_STACK, -1)
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
    return remove(s:K_STACK, -1)(remove(s:KVAL_STACK, -1))
endfunction

function! s:ApplyHandler() abort
    return s:PopHandler()(s:ERR_R)
endfunction

function! s:EvalClosure() abort
    return s:CLOSURE_R()
endfunction

function! vl#Eval(expr, env=vlenv#BuildInitialEnv()) abort
    call s:InitRegisters()
    let tokens = vlparse#Tokenize(a:expr)
    let program = vlparse#Parse(tokens)
    let program = vlparse#ToLisp(program)
    "call vltrns#ScanLambdas(syntax)
    let s:EXPR_R = program
    call add(s:K_STACK, funcref("s:EndCont"))
    call s:PushHandler(funcref("vl#TopLevelHandler"))
    let s:CLOSURE_R = vl#Analyze()
    let s:ENV_R = a:env
    let Bounce = s:EvalClosure()
    let val = s:Trampoline(Bounce)
    call s:Cleanup()
    return val
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
    let s:COUNTER += 1
    let expr = vltrns#Desugar(s:EXPR_R)
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
    let fname = s:GenLabel("Condition")
    function! {fname}() closure abort
        let s:ERR_R = a:condition
        return s:ApplyHandler()
    endfunction
    return funcref(fname)
endfunction

function! s:GenConst(expr) abort
    let fname = s:GenLabel("Closure")
    function! {fname}() closure abort
        call add(s:KVAL_STACK, a:expr)
        return s:ApplyK()
    endfunction
    return funcref(fname)
endfunction

function! s:GenLookup(expr) abort
    let fname = s:GenLabel("Closure")
    function! {fname}() closure abort
        call add(s:KVAL_STACK, s:ApplyEnv(s:ENV_R, a:expr))
        return s:ApplyK()
    endfunction
    return funcref(fname)
endfunction

function! s:Sequentially(closures) abort
    " The first n-1 expressions in a sequence should be
    " executed only for effect. The value of the last
    " expression becomes the value of the sequence.
    " We therefore need to push an 'empty' continuation
    " for each of the first n-1 expressions.
    " Evaluating a procedure application returns a bounce,
    " so we need to trampoline the evaluation of each closure.
    let fname = s:GenLabel("SequenceClosure")
    let closures = a:closures
    function! {fname}() closure abort
        let K = {_ -> "done"}
        while vl#Cdr(closures) != []
            call add(s:K_STACK, K)
            let s:CLOSURE_R = closures[0]
            call s:Trampoline(s:EvalClosure())
            let closures = closures[1]
        endwhile
        let s:CLOSURE_R = closures[0]
        return s:EvalClosure()
    endfunction
    return funcref(fname)
endfunction

function! s:AndSequentially(closures) abort
    let fname = s:GenLabel("AndClosure")
    let closures = a:closures
    function! {fname}() closure abort
        if closures == []
            call add(s:KVAL_STACK, g:vl_bool_t)
            return s:ApplyK()
        endif
        while vl#Cdr(closures) != []
            call add(s:K_STACK, {x -> x})
            let s:CLOSURE_R = closures[0]
            if !s:IsTrue(s:EvalClosure())
                call add(s:KVAL_STACK, g:vl_bool_f)
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

function! s:ApplyProc() abort
    let rator = s:PROC_RATOR_R
    let rands = s:PROC_ARGS_R
    if vl#Car(rator) =~? '^prim$'
        let fname = s:GenLabel("PrimitiveApply")
        function! {fname}() closure abort
            call add(s:KVAL_STACK, vl#Cdr(rator)(rands))
            return s:ApplyK()
        endfunction
        return funcref(fname)
    elseif vl#Car(rator) =~? '^cont-prim$'
        let fname = s:GenLabel("ApplyContPrim")
        function! {fname}() closure abort
            call add(s:KVAL_STACK, vl#Car(rands))
            return s:ApplyK()
        endfunction
        return funcref(fname)
    elseif vl#Car(rator) =~? '^proc$'
        let Body = vl#ProcBody(rator)
        let env = s:ProcEnv(rator)
        let params = vlutils#FlattenList(vl#ProcParams(rator))
        let rands = vlutils#FlattenList(rands)
        let fname = s:GenLabel("ProcBounce")
        function! {fname}() closure abort
            let s:CLOSURE_R = Body
            let s:ENV_R = vl#ExtendEnv(env, params, rands)
            return s:EvalClosure()
        endfunction
        return funcref(fname)
    elseif vl#Car(rator) =~? '^cont$'
        let Body = vl#ProcBody(rator)
        let env = s:ProcEnv(rator)
        let params = vlutils#FlattenList(vl#ProcParams(rator))
        let rands = [extend(["cont-prim"], vlutils#FlattenList(rands))]
        let fname = s:GenLabel("ContBounce")
        function! {fname}() closure abort
            let s:CLOSURE_R = Body
            let s:ENV_R = vl#ExtendEnv(env, params, rands)
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
    let e = a:env
    while e != []
        for j in range(len(e[0][0]))
            if e[0][0][j] == a:var
                return e[0][1][j]
            endif
        endfor
        let e = e[1]
    endwhile
    throw "Unbound variable: "..a:var
endfunction

function! vl#ExtendEnv(env, vars, vals) abort
    return [[a:vars, a:vals], a:env]
endfunction

function! s:SetVar(env, var, val) abort
    let e = a:env
    while e != []
        for j in range(len(e[0][0]))
            if e[0][0][j] == a:var
                let e[0][1][j] = a:val
                return 1
            endif
        endfor
        let e = e[1]
    endwhile
    throw "Unbound variable: "..a:var
endfunction

function! s:DefineVar(env, var, val) abort
    let a:env[0][0] = add(a:env[0][0], a:var)
    let a:env[0][1] = add(a:env[0][1], a:val)
    return 1
endfunction

function! s:MapAnalyze(expr) abort
    let s:EXPR_R = a:expr
    return vl#Analyze()
endfunction

function! s:GenSequence(expr, sequencer) abort
    let closures = vl#LispMap(funcref("s:MapAnalyze"), a:expr)
    return a:sequencer(closures)
endfunction

function! vl#ProcParams(proc) abort
    return vl#Cadr(a:proc)
endfunction

function! s:ProcEnv(proc) abort
    return vl#Cadddr(a:proc)
endfunction

function! vl#ProcBody(proc) abort
    return vl#Caddr(a:proc)
endfunction

function! s:EvalClosureList() abort
    let s:COUNTER += 1
    let closures = s:CLOSURE_LIST_R
    if vlutils#IsEmptyList(closures)
        call add(s:KVAL_STACK, [])
        return s:ApplyK()
    endif
    let outercontname = s:GenLabel("EvalClosureListOuterCont")
    let innercontname = s:GenLabel("EvalClosureListInnerCont")
    function! {outercontname}(arg) closure abort
        function! {innercontname}(args) closure abort
            call add(s:KVAL_STACK, vl#Cons(a:arg, a:args))
            return s:ApplyK()
        endfunction
        let s:CLOSURE_LIST_R = vl#Cdr(closures)
        call add(s:K_STACK, funcref(innercontname))
        return s:EvalClosureList()
    endfunction
    let s:CLOSURE_R = vl#Car(closures)
    call add(s:K_STACK, funcref(outercontname))
    return s:EvalClosure()
endfunction

function! s:RandsCont(rator) abort
    let fname = s:GenLabel("RandsCont")
    function! {fname}(args) closure abort
        let s:PROC_RATOR_R = a:rator
        let s:PROC_ARGS_R = a:args
        return s:ApplyProc()
    endfunction
    return funcref(fname)
endfunction

function! s:RatorCont(rands) abort
    let fname = s:GenLabel("RatorCont")
    function! {fname}(rator) closure abort
        call add(s:K_STACK, s:RandsCont(a:rator))
        let s:CLOSURE_LIST_R = a:rands
        return s:EvalClosureList()
    endfunction
    return funcref(fname)
endfunction

function! s:GenApplication(expr) abort
    let s:EXPR_R = vl#Car(a:expr)
    let Rator = vl#Analyze()
    let rands = vl#LispMap(funcref("s:MapAnalyze"), vl#Cdr(a:expr))
    let fname = s:GenLabel("ApplicationClosure")
    function! {fname}() closure abort
        let s:CLOSURE_R = Rator
        call add(s:K_STACK, s:RatorCont(rands))
        return s:EvalClosure()
    endfunction
    return funcref(fname)
endfunction

function! s:GenDefine(expr) abort
    let s:EXPR_R = vl#Caddr(a:expr)
    let ValClosure = vl#Analyze()
    let contname = s:GenLabel("DefineCont")
    let fname = s:GenLabel("DefineClosure")
    function! {fname}() closure abort
        function! {contname}(val) closure abort
            call add(s:KVAL_STACK, s:DefineVar(s:ENV_R, vl#Cadr(a:expr), a:val))
            return s:ApplyK()
        endfunction
        let s:CLOSURE_R = ValClosure
        call add(s:K_STACK, funcref(contname))
        return s:EvalClosure()
    endfunction
    return funcref(fname)
endfunction

function! s:AnalyzeCallCCProc() abort
    let expr = s:EXPR_R
    let params = vl#Cadr(expr)
    let Body = s:GenSequence(vl#Cddr(expr), funcref("s:Sequentially"))
    let fname = s:GenLabel("CallCCK")
    function! {fname}() closure abort
        call add(s:KVAL_STACK, vl#LispList(["cont", params, Body, s:ENV_R]))
        return s:ApplyK()
    endfunction
    return funcref(fname)
endfunction

function! s:GenCallCC(expr) abort
    let s:EXPR_R = vl#Cadr(a:expr)
    let Proc = s:AnalyzeCallCCProc()
    let fname = s:GenLabel("CallCC")
    let contname = s:GenLabel("CallCCK")
    function! {fname}() closure abort
        function! {contname}(rator) closure abort
            let s:PROC_RATOR_R = a:rator
            let s:PROC_ARGS_R = []
            return s:ApplyProc()
        endfunction
        let s:CLOSURE_R = Proc
        call add(s:K_STACK, funcref(contname))
        return s:EvalClosure()
    endfunction
    return funcref(fname)
endfunction

function! s:GenProc(expr) abort
    let params = vl#Cadr(a:expr)
    let Body = s:GenSequence(vl#Cddr(a:expr), funcref("s:Sequentially"))
    let fname = s:GenLabel("ProcClosure")
    function! {fname}() closure abort
        call add(s:KVAL_STACK, vl#LispList(["proc", params, Body, s:ENV_R]))
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
    let consequentname = s:GenLabel("CondConsequent")
    function! {consequentname}() closure abort
        let s:EXPR_R = vl#Caddr(a:expr)
        let s:CLOSURE_R = vl#Analyze()
        return s:EvalClosure()
    endfunction
    let C_clsr = funcref(consequentname)

    let alt = vlutils#IsEmptyList(vl#Cdddr(a:expr)) ? g:vl_bool_f : vl#Cadddr(a:expr)
    let altname = s:GenLabel("CondAlt")
    function! {altname}() closure abort
        let s:EXPR_R = alt
        let s:CLOSURE_R = vl#Analyze()
        return s:EvalClosure()
    endfunction
    let A_clsr = funcref(altname)

    let fname = s:GenLabel("CondClosure")
    let contname = s:GenLabel("CondCont")
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
        call add(s:K_STACK, funcref(contname))
        return s:EvalClosure()
    endfunction
    return funcref(fname)
endfunction

function! s:GenSetBang(expr) abort
    let s:EXPR_R = vl#Caddr(a:expr)
    let ValClosure = vl#Analyze()
    let contname = s:GenLabel("SetBangCont")
    let fname = s:GenLabel("SetBangClosure")
    function! {fname}() closure abort
        function! {contname}(val) closure abort
            call add(s:KVAL_STACK, s:SetVar(s:ENV_R, vl#Cadr(a:expr), a:val))
            return s:ApplyK()
        endfunction
        let s:CLOSURE_R = ValClosure
        call add(s:K_STACK, funcref(contname))
        return s:EvalClosure()
    endfunction
    return funcref(fname)
endfunction

function! vl#TopLevelHandler(exception)
    return ["condition", vl#Cdr(a:exception)]
endfunction
