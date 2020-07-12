let s:VL_TRANSFORMERS = {}

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

function! vltrns#Transform(expr) abort
    if type(vl#Car(a:expr)) == v:t_list
        return a:expr
    elseif has_key(s:VL_TRANSFORMERS, vl#Car(a:expr))
        return get(s:VL_TRANSFORMERS, vl#Car(a:expr))(a:expr)
    endif
    return a:expr
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

function! s:LetToLambda(expr) abort
    let bindings = vl#Cadr(a:expr)
    let body = vl#Cddr(a:expr)
    let vars = vl#LispMap({x -> vl#Car(x)}, bindings)
    let vals = vl#LispMap({x -> vl#Cadr(x)}, bindings)
    let lambda = vl#Cons("lambda", vl#Cons(vars, body))
    return vl#Cons(lambda, vals)
endfunction

call s:RegisterTransformers([
            \["let", funcref("s:LetToLambda")],
            \["cond", funcref("s:CondToIf")],
            \])