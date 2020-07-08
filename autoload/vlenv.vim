function! vlenv#BuildInitialEnv() abort
    return {
                \'+': ['primitive', {args -> vlbuiltins#VlAdd(args)}],
                \'cons': ['primitive', {args -> vlbuiltins#PrimitiveCons(args)}],
                \'equal?': ['primitive',
                \{args -> vlbuiltins#ApplyGeneric('equal?', args)}],
                \'eq?': ['primitive', {args -> vlbuiltins#ApplyGeneric('eq?', args)}],
                \'#t': g:vl_bool_t,
                \'#f': g:vl_bool_f,
                \}
endfunction
