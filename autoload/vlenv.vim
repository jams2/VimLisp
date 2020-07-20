function! s:GetInitialEnv() abort
    return [
                \["+", ['prim', {args -> vlbuiltins#ApplyGeneric('add2', args)}]],
                \["-", ['prim', {args -> vlbuiltins#ApplyGeneric('sub2', args)}]],
                \["*", ['prim', {args -> vlbuiltins#ApplyGeneric('mult2', args)}]],
                \["/", ['prim', {args -> vlbuiltins#ApplyGeneric('div2', args)}]],
                \[">", ['prim', {args -> vlbuiltins#ApplyGeneric('gt', args)}]],
                \["<", ['prim', {args -> vlbuiltins#ApplyGeneric('lt', args)}]],
                \[">=", ['prim', {args -> vlbuiltins#ApplyGeneric('gte', args)}]],
                \["<=", ['prim', {args -> vlbuiltins#ApplyGeneric('lte', args)}]],
                \["cons", ['prim', {args -> vlbuiltins#PrimitiveCons(args)}]],
                \["vimcall", ['prim', {args -> vlbuiltins#VimCall(args)}]],
                \["equal?", ['prim', {args -> vlbuiltins#ApplyGeneric('equal?', args)}]],
                \["eq?", ['prim', {args -> vlbuiltins#ApplyGeneric('eq?', args)}]],
                \["#t", g:vl_bool_t],
                \["#f", g:vl_bool_f],
                \]
endfunction

function! vlenv#GetInitialVars() abort
    return [map(s:GetInitialEnv(), {_, x -> x[0]})]
endfunction

function! vlenv#GetInitialVals() abort
    return [map(s:GetInitialEnv(), {_, x -> x[1]})]
endfunction

function! vlenv#BuildInitialEnv() abort
    let g:VL_INIT_ENV_VALS = vlenv#GetInitialVals()
    let g:VL_INIT_ENV_VARS = vlenv#GetInitialVars()
endfunction

function! vlenv#ExtendInitialEnv(var, val) abort
    let g:VL_INIT_ENV_VARS[0] = extend([a:var], g:VL_INIT_ENV_VARS[0])
    let g:VL_INIT_ENV_VALS[0] = extend([a:val], g:VL_INIT_ENV_VALS[0])
endfunction
