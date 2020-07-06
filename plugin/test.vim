source plugin/vimlisp.vim

let s:TEST_ENV = #{x: 3, __prev_frame: g:VL_INITIAL_ENV}

function! TestVlEval(expr) abort
    return VlEval(a:expr, s:TEST_ENV)
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
            \[3, TestVlEval('(+ 1 2)')],
            \[12, TestVlEval('((lambda (x y) (+ x y)) 5 7)')],
            \[7, TestVlEval('(call/cc (lambda (k) 7))')],
            \[7, TestVlEval('(call/cc (lambda (k) (k 7)))')],
            \[9, TestVlEval('((lambda () 5 7 9))')],
            \[0, TestVlEval('(+ 1 2 (call/cc (lambda (k) (k -3))))')],
            \[0, TestVlEval('(+ 1 2 (call/cc (lambda (k) 0)))')],
            \[6, TestVlEval('(+ 1 2 (call/cc (lambda (k) (k 3))))')],
            \[7, TestVlEval('(let ((x 3) (y 4)) (+ x y))')],
            \[7, TestVlEval('((lambda (x y) (+ x y)) 3 4)')],
            \[12, TestVlEval('(begin (define y (lambda (x) (+ 5 7))) (y 3))')],
            \[5, TestVlEval('(let ((x 3)) (let ((x 5)) x))')],
            \]

call RunTests(s:tests)
