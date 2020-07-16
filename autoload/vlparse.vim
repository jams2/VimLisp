let s:TOKEN_R = '\([()"'']\)'
let s:PREV_FRAME_KEY = "__prev_frame"
let s:SYMBOL_R = '^[a-zA-Z0-9?!*^/\\><=+_-]\+$'
let s:STRING_CONST_R = '^".*"$'
let s:NUMBER_R = '^-\?\d\+$'
let s:BOOL_R = '^\(#t\)\|\(#f\)$'
let s:R_PAREN = ")"
let s:L_PAREN = "("
let s:D_QUOTE = '"'
let s:BACKTICK = "`"
let s:QUOTE = "'"
let s:DOT = "."


function! vlparse#Tokenize(expr) abort
    return split(substitute(a:expr, s:TOKEN_R, ' \1 ', 'g'))
endfunction

function! vlparse#Parse(tokens) abort
    if len(a:tokens) == 0
        throw "unterminated expression"
    elseif a:tokens[0] == s:QUOTE
        return vl#LispList(["quote", s:ParseQuoted(a:tokens[1:])])
    elseif index(a:tokens, s:DOT) > -1
        throw "invalid syntax"
    elseif len(a:tokens) == 1
        return s:ParseAtom(a:tokens[0])
    elseif a:tokens[0] == s:L_PAREN
        return vl#LispList(s:ParseList(a:tokens))
    elseif a:tokens[0] == s:D_QUOTE
        return vl#LispList(s:ParseString(a:tokens))
    endif
endfunction

function! s:ParseQuoted(tokens) abort
    if a:tokens[0] == s:L_PAREN && index(a:tokens, s:DOT) > 0
        return s:ParsePair(a:tokens)
    else
        return vlparse#Parse(a:tokens)
    endif
endfunction

function! s:ParseList(tokens) abort
    if a:tokens[0] != s:L_PAREN || a:tokens[-1] != s:R_PAREN
        throw "Invalid list structure -- s:ParseList"
    endif
    let tokens = a:tokens[1:-2]  " remove outer parens
    let exprlist = []
    let i = 0
    while i < len(tokens)
        let token = tokens[i]
        if token == s:R_PAREN
            throw "Unexpected list termination -- s:ParseList"
        elseif token == s:L_PAREN
            let sublistlen = s:NestedNonTerminalLen(tokens[i:])
            call add(exprlist, vlparse#Parse(tokens[i:i+sublistlen-1]))
            let i += sublistlen
        elseif token == s:D_QUOTE
            let stringlen = s:NonTerminalLen(tokens[i:])
            call add(exprlist, vlparse#Parse(tokens[i:i+stringlen-1]))
            let i += stringlen
        elseif token == s:QUOTE
            if tokens[i+1] == s:L_PAREN
                let quotelen = s:NestedNonTerminalLen(tokens[i+1:])
                call add(exprlist, vlparse#Parse(tokens[i:i+quotelen]))
                let i += 1 + quotelen
            else
                call add(exprlist, vlparse#Parse(tokens[i:i+1]))
                let i += 2
            endif
        else
            call add(exprlist, vlparse#Parse(tokens[i:i]))
            let i += 1
        endif
    endwhile
    return exprlist
endfunction

function! s:ParsePair(tokens) abort
    let dotpos = index(a:tokens, s:DOT)
    if a:tokens[0] != s:L_PAREN || dotpos == -1
        throw "Invalid pair structure -- s:ParseList"
    endif
    let lhs = s:ParseQuoted(a:tokens[1:dotpos-1])
    let rhs = s:ParseQuoted(a:tokens[dotpos+1:-2])
    return vl#Cons(lhs, rhs)
endfunction

function! s:ParseString(tokens) abort
    if a:tokens[0] != s:D_QUOTE || a:tokens[-1] != s:D_QUOTE
        throw "Invalid string literal: "..string(join(a:tokens))
    endif
    return s:StrFactory(join(a:tokens[1:-2]))
endfunction

function! s:StrFactory(str) abort
    return ["vlobj", #{_t: g:vl_t_lstr, _chars: str2list(a:str)}]
endfunction

function! s:NestedNonTerminalLen(tokens, open=s:L_PAREN, close=s:R_PAREN) abort
    let open_count = 0
    for i in range(len(a:tokens))
        let token = a:tokens[i]
        if token == a:open
            let open_count += 1
        elseif token == a:close
            let open_count -= 1
            if open_count == 0
                return i + 1
            endif
        endif
    endfor
    return -1
endfunction

function! s:NonTerminalLen(tokens, delim=s:D_QUOTE) abort
    let delim_count = 0
    for i in range(len(a:tokens))
        let token = a:tokens[i]
        if token == a:delim
            let delim_count += 1
            if delim_count == 2
                return i + 1
            endif
        endif
    endfor
    return -1
endfunction

function! s:ParseAtom(token) abort
    if a:token =~ s:NUMBER_R
        return a:token - 0
    elseif a:token =~? s:SYMBOL_R
        return a:token
    elseif a:token =~? s:BOOL_R
        return a:token
    elseif a:token == s:DOT
        return a:token
    else
        throw "Invalid token: "..a:token
    endif
endfunction

function! s:DeepLispList(elts) abort
    if vlutils#IsEmptyList(a:elts)
        return []
    elseif len(a:elts) == 1
        if type(a:elts[0]) == v:t_list
            return vl#Cons(s:DeepLispList(a:elts[0]), [])
        endif
        return add(a:elts, [])
    elseif type(a:elts[0]) == v:t_list
        return vl#Cons(s:DeepLispList(a:elts[0]), s:DeepLispList(a:elts[1:]))
    else
        return vl#Cons(a:elts[0], s:DeepLispList(a:elts[1:]))
    endif
endfunction

function! vlparse#SplitExprs(source) abort
    let remaining = a:source
    let exprs = []
    while len(remaining) > 0
        if remaining[0] =~ '\s'
            let remaining = remaining[1:]
            continue
        endif
        let exprlen = vlparse#ExprLen(remaining)
        let nextexpr = remaining[:exprlen-1]
        call add(exprs, nextexpr)
        let remaining = remaining[exprlen:]
    endwhile
    return exprs
endfunction

function! vlparse#ExprLen(tokens) abort
    let first = a:tokens[0]
    if first == s:L_PAREN
        return s:NestedNonTerminalLen(a:tokens)
    elseif first == s:D_QUOTE
        return s:NonTerminalLen(a:tokens)
    elseif first == s:QUOTE
        if a:tokens[1] == s:L_PAREN
            return 1 + s:NestedNonTerminalLen(a:tokens[1:])
        endif
        return 2
    else
        return 1
    endif
endfunction
