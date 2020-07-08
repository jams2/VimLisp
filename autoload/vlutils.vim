function! vlutils#IsEmptyList(obj) abort
    return type(a:obj) == v:t_list && a:obj == []
endfunction

function! vlutils#FlattenList(l) abort
    let list = a:l
    let flat = []
    while !vlutils#IsEmptyList(list)
        call add(flat, vl#Car(list))
        let list = vl#Cdr(list)
    endwhile
    return flat
endfunction
