function! vlenv#BuildInitialEnv() abort
    return {
                \'+': ['prim', {args -> vlbuiltins#VlAdd(args)}],
                \'cons': ['prim', {args -> vlbuiltins#PrimitiveCons(args)}],
                \'equal?': ['prim',
                \{args -> vlbuiltins#ApplyGeneric('equal?', args)}],
                \'eq?': ['prim', {args -> vlbuiltins#ApplyGeneric('eq?', args)}],
                \'#t': g:vl_bool_t,
                \'#f': g:vl_bool_f,
                \}
endfunction
