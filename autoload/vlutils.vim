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

function! vlutils#PrintString(obj) abort
    return list2str(get(a:obj, "_chars"))
endfunction

function! vlutils#PrintPair(obj) abort
    return "("..a:obj[0].." . "..a:obj[1]..")"
endfunction

function! vlutils#PrintList(obj) abort
    let flat = vlutils#FlattenList(a:obj)
    let elts = map(flat, {_, x -> vlutils#PrettyPrint(x)})
    return "("..join(elts)..")"
endfunction!

function! vlutils#PrettyPrint(obj) abort
    return vlbuiltins#ApplyGeneric('to_str', vl#LispList([a:obj]))
endfunction
