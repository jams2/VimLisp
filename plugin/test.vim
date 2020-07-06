source plugin/vimlisp.vim

let s:TEST_ENV = #{x: 3, __prev_frame: g:VL_INITIAL_ENV}
let s:END_CONT = {val -> val}

function! TestVlEval(expr) abort
    return VlAnalyze(StrToVim(a:expr))(s:TEST_ENV, s:END_CONT)
endfunction

function! RunTests(tests) abort
    for [expected, actual] in a:tests
        if string(expected) == string(actual)
            echo "[+] Expected "..string(expected)..", got: "..string(actual)
        else
            echo "[ ] Expected "..string(expected)..", got: "..string(actual)
        endif
    endfor
endfunction

let s:tests = [
            \[3, TestVlEval('x')],
            \[-3, TestVlEval('-3')],
            \[123, TestVlEval(123)],
            \['"hello"', TestVlEval('"hello"')],
            \[0, TestVlEval("(define x 3)")],
            \[0, TestVlEval('(define x "hello, world")')],
            \[0, TestVlEval('(set! x 3)')],
            \['proc', TestVlEval('(lambda (x y) x y)')[0]],
            \[4, TestVlEval('(begin 1 2 4)')],
            \[3, TestVlEval('(begin (define x 3) x)')],
            \[3, VlEval('(+ 1 2)')],
            \[12, VlEval('((lambda (x y) (+ x y)) 5 7)')],
            \[7, TestVlEval('(call/cc (lambda (k) 7))')],
            \[7, TestVlEval('(call/cc (lambda (k) (k 7)))')],
            \[9, TestVlEval('((lambda () 5 7 9))')],
            \[0, VlEval('(+ 1 2 (call/cc (lambda (k) (k -3))))')],
            \[0, VlEval('(+ 1 2 (call/cc (lambda (k) 0)))')],
            \[6, VlEval('(+ 1 2 (call/cc (lambda (k) (k 3))))')],
            \[7, VlEval('(let ((x 3) (y 4)) (+ x y))')],
            \[12, VlEval('(begin (define y (lambda (x) (+ 5 7))) (y 3))')]]

call RunTests(s:tests)
