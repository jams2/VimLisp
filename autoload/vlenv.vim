function! vlenv#BuildInitialEnv() abort
    return {
                \'+': ['prim', {args -> vlbuiltins#ApplyGeneric('add2', args)}],
                \'-': ['prim', {args -> vlbuiltins#ApplyGeneric('sub2', args)}],
                \'*': ['prim', {args -> vlbuiltins#ApplyGeneric('mult2', args)}],
                \'/': ['prim', {args -> vlbuiltins#ApplyGeneric('div2', args)}],
                \'>': ['prim', {args -> vlbuiltins#ApplyGeneric('gt', args)}],
                \'<': ['prim', {args -> vlbuiltins#ApplyGeneric('lt', args)}],
                \'>=': ['prim', {args -> vlbuiltins#ApplyGeneric('gte', args)}],
                \'<=': ['prim', {args -> vlbuiltins#ApplyGeneric('lte', args)}],
                \'cons': ['prim', {args -> vlbuiltins#PrimitiveCons(args)}],
                \'vimcall': ['prim', {args -> vlbuiltins#VimCall(args)}],
                \'equal?': ['prim',
                \{args -> vlbuiltins#ApplyGeneric('equal?', args)}],
                \'eq?': ['prim', {args -> vlbuiltins#ApplyGeneric('eq?', args)}],
                \'#t': g:vl_bool_t,
                \'#f': g:vl_bool_f,
                \}
endfunction
