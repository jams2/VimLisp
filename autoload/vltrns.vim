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
    if vlutils#IsEmptyList(a:expr) || type(a:expr) != v:t_list
        return a:expr
    elseif type(a:expr[0]) == v:t_list
        return [vltrns#Transform(a:expr[0]), vltrns#Transform(a:expr[1])]
    elseif type(a:expr[0]) != v:t_string || a:expr[0] == "vlobj"
        return [a:expr[0], vltrns#Transform(a:expr[1])]
    elseif has_key(s:VL_TRANSFORMERS, a:expr[0])
        let transformed = get(s:VL_TRANSFORMERS, a:expr[0])(a:expr)
        if type(transformed) != v:t_list
            return transformed
        endif
        return [vltrns#Transform(transformed[0]), vltrns#Transform(transformed[1])]
    else
        return [a:expr[0], vltrns#Transform(a:expr[1])]
    endif
endfunction

function! s:SubRefer(expr, bound=[])
    let boundvars = extend([vlutils#FlattenList(vl#Cadr(a:expr))], a:bound)
    let body = vl#Caddr(a:expr)
    while body != []
        if type(body[0]) == v:t_list
            call vltrns#ScanLambdas(body[0], boundvars)
        elseif type(body[0]) == v:t_string
            let reference = []
            for i in range(len(boundvars))
                for j in range(len(boundvars[i]))
                    if boundvars[i][j] == body[0]
                        let reference = ["refer", [i, j]]
                        break
                    endif
                endfor
                if reference != []
                    let body[0] = reference
                    break
                endif
            endfor
        endif
        let body = body[1]
    endwhile
endfunction

function! vltrns#ScanLambdas(expr, scope=[]) abort
    if vlutils#IsEmptyList(a:expr)
        return
    elseif type(a:expr[0]) == v:t_list
        call vltrns#ScanLambdas(a:expr[0], a:scope)
    elseif a:expr[0] == "quote"
        return
    elseif a:expr[0] == "lambda"
        call s:SubRefer(a:expr, a:scope)
        return
    endif
    call vltrns#ScanLambdas(a:expr[1], a:scope)
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
