let s:LIST_OPS = {
            \'eq?': {x, y -> x is y ? g:VL_T : g:VL_F},
            \'equal?': {x, y -> x == y ? g:VL_T : g:VL_F},
            \}

let s:NUMBER_OPS = {
            \'=': {x, y -> x == y ? g:VL_T : g:VL_F},
            \'equal?': {x, y -> x == y ? g:VL_T : g:VL_F},
            \}

let s:STRING_OPS = {
            \'eq?': {x, y -> x is y ? g:VL_T : g:VL_F},
            \'equal?': {x, y -> x == y ? g:VL_T : g:VL_F},
            \}

let s:UNTYPED_OPS = {
            \'eq?': {x, y -> g:VL_F},
            \'equal?': {x, y -> g:VL_F},
            \}

let s:BUILTINS = {
            \v:t_list: s:LIST_OPS,
            \v:t_number: s:NUMBER_OPS,
            \v:t_string: s:STRING_OPS,
            \'untyped': s:UNTYPED_OPS,
            \}

function! vlbuiltins#ApplyGeneric(op, args) abort
    let args = vlutils#FlattenList(a:args)
    let types = uniq(map(deepcopy(args), {_, x -> type(x)}))
    let type = len(types) == 1? types[0] : 'untyped'
    if !has_key(s:BUILTINS[type], a:op)
        throw "Undefined operation on type "..type.." ("..a:op..")"
    endif
    return call(get(s:BUILTINS[type], a:op), args)
endfunction

function! vlbuiltins#VlAdd(args) abort
    let total = 0
    let l = a:args
    while !vlutils#IsEmptyList(l)
        let total += vl#Car(l)
        let l = vl#Cdr(l)
    endwhile
    return total
endfunction

function! vlbuiltins#PrimitiveCons(l) abort
    return [vl#Car(a:l), vl#Cadr(a:l)]
endfunction
