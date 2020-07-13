let s:add2 = {x, y -> x + y}
let s:sub2 = {x, y -> x - y}
let s:mult2 = {x, y -> x * y}
let s:div2 = {x, y -> x / y}
let s:gt = {x, y -> x > y ? g:vl_bool_t : g:vl_bool_f}
let s:lt = {x, y -> x < y ? g:vl_bool_t : g:vl_bool_f}
let s:gte = {x, y -> x >= y ? g:vl_bool_t : g:vl_bool_f}
let s:lte = {x, y -> x <= y ? g:vl_bool_t : g:vl_bool_f}

function! s:PrettyPrintErr(err) abort
    return vlutils#PrettyPrint(vl#Cdr(a:err))
endfunction

let s:ERR_OPS = {
            \}

let s:PAIR_OPS = {
            \'to_str': funcref("vlutils#PrintPair"),
            \}

let s:SYM_OPS = {
            \'eq?': {x, y -> x == y ? g:vl_bool_t : g:vl_bool_f},
            \'equal?': {x, y -> x == y ? g:vl_bool_t : g:vl_bool_f},
            \'to_str': {x -> x},
            \}

let s:LIST_OPS = {
            \'eq?': {x, y -> x is y ? g:vl_bool_t : g:vl_bool_f},
            \'equal?': {x, y -> x == y ? g:vl_bool_t : g:vl_bool_f},
            \'to_str': funcref("vlutils#PrintList"),
            \}

let s:NUMBER_OPS = {
            \'add2': s:add2,
            \'sub2': s:sub2,
            \'mult2': s:mult2,
            \'div2': s:div2,
            \'gt': s:gt,
            \'lt': s:lt,
            \'lte': s:lte,
            \'gte': s:gte,
            \'=': {x, y -> x == y ? g:vl_bool_t : g:vl_bool_f},
            \'equal?': {x, y -> x == y ? g:vl_bool_t : g:vl_bool_f},
            \'to_str': function("string"),
            \}

let s:STRING_OPS = {
            \'add2': s:add2,
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
            \g:vl_t_err: s:ERR_OPS,
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

function! vlbuiltins#PrimitiveCons(l) abort
    return [vl#Car(a:l), vl#Cadr(a:l)]
endfunction

function! vlbuiltins#VimCall(args) abort
    let args = vlutils#FlattenList(a:args)
    return call(args[0], args[1:])
endfunction
