let s:NODE_ELT = 0
let s:NODE_L = 1
let s:NODE_R = 2

function! vlutils#LispIndexOf(elt, l) abort
    let index = 0
    let found = -1
    let list = a:l
    while !vlutils#IsEmptyList(list)
        if a:elt == list[0]
            return index
        endif
        let list = list[1]
        let index += 1
    endwhile
    return found
endfunction

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

function! vlutils#TimeExecution(func) abort
    let start = system("date +%s.%N")
    call a:func()
    let end = system("date +%s.%N")
    return str2float(end) - str2float(start)
endfunction

function! s:MakeSetNode(x) abort
    " [str, left (<), right (>)]
    return [a:x, "", ""]
endfunction

function! vlutils#MakeStrSet() abort
    " B-Tree representation of set
    return s:MakeSetNode("")
endfunction

function! vlutils#StrSetMember(root, x) abort
    let n = a:root
    while 1
        if n[s:NODE_ELT] == ""
            return 0
        elseif a:x == n[s:NODE_ELT]
            return 1
        elseif a:x > n[s:NODE_ELT]
            let n = n[s:NODE_R]
        else
            let n = n[s:NODE_L]
        endif
    endwhile
endfunction

function! vlutils#StrSetInsert(root, x) abort
    let node = a:root
    if node[s:NODE_ELT] == ""
        let node[s:NODE_ELT] = a:x
        return 1
    endif
    while 1
        if a:x == node[s:NODE_ELT]
            return 0
        elseif a:x > node[s:NODE_ELT]
            if type(node[s:NODE_R]) == v:t_list
                let node = node[s:NODE_R]
            else
                let node[s:NODE_R] = s:MakeSetNode(a:x)
                return 1
            endif
        else
            if type(node[s:NODE_L]) == v:t_list
                let node = node[s:NODE_L]
            else
                let node[s:NODE_L] = s:MakeSetNode(a:x)
                return 1
            endif
        endif
    endwhile
endfunction

function! vlutils#StrSetForEach(proc, node) abort
    if a:node[s:NODE_ELT] == ""
        return
    else
        call a:proc(a:node)
        call vlutils#StrSetForEach(a:proc, a:node[s:NODE_L])
        call vlutils#StrSetForEach(a:proc, a:node[s:NODE_R])
    endif
endfunction
