let s:PAIR_OPS = {
            \'to_str': funcref("vlutils#PrintPair"),
            \}

let s:SYM_OPS = {
            \'eq?': {x, y -> x == y ? g:vl_bool_t : g:vl_bool_f},
            \'equal?': {x, y -> x == y ? g:vl_bool_t : g:vl_bool_f},
            \}

let s:LIST_OPS = {
            \'eq?': {x, y -> x is y ? g:vl_bool_t : g:vl_bool_f},
            \'equal?': {x, y -> x == y ? g:vl_bool_t : g:vl_bool_f},
            \'to_str': funcref("vlutils#PrintList"),
            \}

let s:NUMBER_OPS = {
            \'=': {x, y -> x == y ? g:vl_bool_t : g:vl_bool_f},
            \'equal?': {x, y -> x == y ? g:vl_bool_t : g:vl_bool_f},
            \'to_str': function("string"),
            \}

let s:STRING_OPS = {
            \'eq?': {x, y -> x is y ? g:vl_bool_t : g:vl_bool_f},
            \'equal?': {x, y -> x == y ? g:vl_bool_t : g:vl_bool_f},
            \'to_str': funcref("vlutils#PrintString")
            \}

let s:UNTYPED_OPS = {
            \'eq?': {x, y -> g:vl_bool_f},
            \'equal?': {x, y -> g:vl_bool_f},
            \}

let s:BUILTINS = {
            \v:t_list: s:LIST_OPS,
            \v:t_number: s:NUMBER_OPS,
            \v:t_string: s:SYM_OPS,
            \g:vl_t_lstr: s:STRING_OPS,
            \g:vl_t_void: s:UNTYPED_OPS,
            \g:vl_t_pair: s:PAIR_OPS,
            \g:vl_t_sym: s:SYM_OPS,
            \}

function! vlbuiltins#ApplyGeneric(op, args) abort
    let args = vlutils#FlattenList(a:args)
    let types = uniq(map(deepcopy(args), {_, x -> vl#TypeOf(x)}))
    let type = len(types) == 1? types[0] : g:vl_t_void
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
