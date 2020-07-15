let s:vars = ["+", "-", "*", "/", ">", "<", ">=", "<=", "cons", "vimcall",
            \"equal?", "eq?", "#t", "#f"]

let s:vals = [
            \['prim', {args -> vlbuiltins#ApplyGeneric('add2', args)}],
            \['prim', {args -> vlbuiltins#ApplyGeneric('sub2', args)}],
            \['prim', {args -> vlbuiltins#ApplyGeneric('mult2', args)}],
            \['prim', {args -> vlbuiltins#ApplyGeneric('div2', args)}],
            \['prim', {args -> vlbuiltins#ApplyGeneric('gt', args)}],
            \['prim', {args -> vlbuiltins#ApplyGeneric('lt', args)}],
            \['prim', {args -> vlbuiltins#ApplyGeneric('gte', args)}],
            \['prim', {args -> vlbuiltins#ApplyGeneric('lte', args)}],
            \['prim', {args -> vlbuiltins#PrimitiveCons(args)}],
            \['prim', {args -> vlbuiltins#VimCall(args)}],
            \['prim', {args -> vlbuiltins#ApplyGeneric('equal?', args)}],
            \['prim', {args -> vlbuiltins#ApplyGeneric('eq?', args)}],
            \g:vl_bool_t,
            \g:vl_bool_f,
            \]

function! vlenv#BuildInitialEnv() abort
    let env = [[s:vars, s:vals]]
    return env
endfunction
