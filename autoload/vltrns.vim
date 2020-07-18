let s:VL_TRANSFORMERS = {}
let s:LAMBDA = "lambda"

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

function! s:SubBoundVarRefs(lambda, scope) abort
    let boundvars = extend([a:lambda[1]], a:scope)
    let body = s:SubLambdaBodyRefs(a:lambda[2:], boundvars)
    return extend(["lambda", a:lambda[1]], body)
endfunction

function! s:SubLambdaBodyRefs(body, scope) abort
    let newbody = []
    for expr in a:body
        if type(expr) == v:t_list
            if type(expr[0]) == v:t_string && expr[0] == s:LAMBDA
                call add(newbody, s:SubBoundVarRefs(expr, a:scope))
            else
                call add(newbody, s:SubLambdaBodyRefs(expr, a:scope))
            endif
        elseif type(expr) == v:t_string
            let next = expr
            for frame in range(len(a:scope))
                let pos = index(a:scope[frame], expr)
                if pos > -1
                    let next = ["refer", [frame, pos]]
                    break
                endif
            endfor
            call add(newbody, next)
        else
            call add(newbody, expr)
        endif
    endfor
    return newbody
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
            return extend(newexpr, s:SubBoundVarRefs(a:expr, a:scope))
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
