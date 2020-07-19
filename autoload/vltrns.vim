let s:VL_TRANSFORMERS = {}
let s:LAMBDA = "lambda"
let s:DEFINE = "define"
let s:SETBANG = "set!"

function! s:RegisterTransformer(name, funcref) abort
    if has_key(s:VL_TRANSFORMERS, a:name)
        throw "Duplicate syntax transformer: "..a:name
    endif
    let s:VL_TRANSFORMERS[a:name] = a:funcref
endfunction

function! s:RegisterTransformers(transformers) abort
    for [name, Transformer] in a:transformers
        call s:RegisterTransformer(name, Transformer)
    endfor
endfunction

function! vltrns#Desugar(expr) abort
    if vlutils#IsEmptyList(a:expr) || type(a:expr) != v:t_list
        return a:expr
    elseif type(a:expr[0]) == v:t_string
        if has_key(s:VL_TRANSFORMERS, a:expr[0])
            return get(s:VL_TRANSFORMERS, a:expr[0])(a:expr)
        endif
    endif
    return a:expr
endfunction

function! s:ProcessLambda(lambda, scope) abort
    let boundvars = extend([a:lambda[1]], a:scope)
    let body = s:SubLambdaBodyRefs(a:lambda[2:], boundvars)
    return extend(["lambda", a:lambda[1]], body)
endfunction

function! s:SubLambdaBodyRefs(body, scope) abort
    " We optimise variable lookup in lambda expressions by
    " representing the env containing that expression's bound
    " variables as a list of values. Every time a new lexical scope
    " is introduced, the env is updated by pushing the newly bound
    " values onto the front of the list of frames. We can then
    " provide an index pair of [frame-index, value-index] in place
    " of every variable reference inside a lambda body.
    " See Dybvig '87, "Three Implementation Models for Scheme.
    " Definitions inside a lambda body must be before any other 
    " expressions. When encountered, the new var is pushed onto the
    " front of the list of vars for the current lexical scope, so
    " the index of the value can be computed. At runtime, the new
    " value is pushed onto the front of the current frame when
    " the define expression is executed.
    let newbody = []
    let scope = a:scope
    let i = 0
    while i < len(a:body) && type(a:body[i]) == v:t_list
        if type(a:body[i][0]) == v:t_string && a:body[i][0] == s:DEFINE
            " Add the var to be defined to the front of the current frame.
            " Any binding over a function parameter will then be shadowed
            " by the new definition.
            let scope[0] = extend(a:body[i][1:1], scope[0])
            call add(newbody, s:NestedDefineAddVar(a:body[i], scope))
            let i += 1
        else
            break
        endif
    endwhile
    for expr in a:body[i:]
        if type(expr) == v:t_list
            if type(expr[0]) == v:t_string
                if expr[0] == s:LAMBDA
                    call add(newbody, s:ProcessLambda(expr, scope))
                elseif expr[0] == s:DEFINE
                    throw "define in invalid body position"
                elseif expr[0] == s:SETBANG
                    call add(newbody, s:NestedDefineSetVar(expr, scope))
                endif
            else
                call add(newbody, s:SubLambdaBodyRefs(expr, scope))
            endif
        elseif type(expr) == v:t_string
            call add(newbody, s:GetLambdaEnvRef(expr, scope))
        else
            call add(newbody, expr)
        endif
    endfor
    return newbody
endfunction

function! s:GetLambdaEnvRef(expr, scope) abort
    for frame in range(len(a:scope))
        let pos = index(a:scope[frame], a:expr)
        if pos > -1
            return ["#refer", [frame, pos]]
        endif
    endfor
    return a:expr
endfunction

function! s:NestedDefineAddVar(expr, scope) abort
    let val = vltrns#ScanLambdas(a:expr[2], a:scope)
    return ["#defvar", a:expr[1], val]
endfunction

function! s:NestedDefineSetVar(expr, scope) abort
    let val = vltrns#ScanLambdas(a:expr[2], a:scope)
    let envref = s:GetLambdaEnvRef(a:expr[1], a:scope)
    if type(envref) == v:t_string
        " var not found in lambda scope. Return a set!,
        " which will try to mutate a variable in the top
        " level environment.
        return add(a:expr[:1], val)
    else
        return ["#setvar", envref, val]
    endif
endfunction

function! vltrns#ScanLambdas(expr, scope=[]) abort
    if type(a:expr) != v:t_list || a:expr == []
        return a:expr
    endif
    let newexpr = []
    for e in a:expr
        if type(e) == v:t_list
            call add(newexpr, vltrns#ScanLambdas(e, a:scope))
        elseif type(e) == v:t_string && e == s:LAMBDA
            return extend(newexpr, s:ProcessLambda(a:expr, a:scope))
        else
            call add(newexpr, e)
        endif
    endfor
    return newexpr
endfunction

function! s:CondToIf(expr) abort
    let clauses = vl#Cdr(a:expr)
    return s:TransformCond(clauses)
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

function! vltrns#LetToLambda(expr) abort
    let bindings = a:expr[1]
    let body = a:expr[2:]
    let vars = map(deepcopy(bindings), {_, x -> x[0]})
    let vals = map(deepcopy(bindings), {_, x -> x[1]})
    let lambda = extend(["lambda", vars], body)
    return extend([lambda], vals)
endfunction

call s:RegisterTransformers([
            \["cond", funcref("s:CondToIf")],
            \])
