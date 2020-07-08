function! vlenv#BuildInitialEnv() abort
    return {
                \'+': ['primitive', {args -> vlbuiltins#VlAdd(args)}],
                \'cons': ['primitive', {args -> vlbuiltins#PrimitiveCons(args)}],
                \'equal?': ['primitive',
                \{args -> vlbuiltins#ApplyGeneric('equal?', args)}],
                \'eq?': ['primitive', {args -> vlbuiltins#ApplyGeneric('eq?', args)}],
                \'#t': g:VL_T,
                \'#f': g:VL_F,
                \}
endfunction
