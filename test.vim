source plugin/vimlisp.vim

let s:TEST_ENV = #{x: 3, __prev_frame: g:VL_INITIAL_ENV}

function! TestVlEval(expr) abort
    return VlEval(a:expr, s:TEST_ENV)
endfunction

function! RunTests(tests) abort
    for [expected, actual] in a:tests
        if string(expected) == string(actual)
            echo "[+] "..string(actual).."\t\t: "..string(expected)
        else
            echo "[ ] "..string(actual).."\t\t: "..string(expected)
        endif
    endfor
endfunction

function! RunEvalTests(tests) abort
    for [expected, expr] in a:tests
        let actual = TestVlEval(expr)
        if string(expected) == string(actual)
            echo "[+] "..string(actual).."\t\t: "..expr
        else
            echo "[ ] "..string(actual).."\t\t: "..expr
        endif
    endfor
endfunction

call RunTests([
            \[0, LispLen([])],
            \[3, LispLen([1, [2, [3, []]]])],
            \])

call RunEvalTests([
            \[3, 'x'],
            \[-3, '-3'],
            \[123, 123],
            \[str2list("hello"), '"hello"'],
            \[0, "(define x 3)"],
            \[0, '(define x "hello, world")'],
            \[0, '(set! x 3)'],
            \[4, '(begin 1 2 4)'],
            \[3, '(begin (define x 3) x)'],
            \[3, '(+ 1 2)'],
            \[12, '((lambda (x y) (+ x y)) 5 7)'],
            \[7, '(call/cc (lambda (k) 7))'],
            \[7, '(call/cc (lambda (k) (k 7)))'],
            \[9, '((lambda () 5 7 9))'],
            \[0, '(+ 1 2 (call/cc (lambda (k) (k -3))))'],
            \[0, '(+ 1 2 (call/cc (lambda (k) 0)))'],
            \[6, '(+ 1 2 (call/cc (lambda (k) (k 3))))'],
            \[7, '(let ((x 3) (y 4)) (+ x y))'],
            \[7, '((lambda (x y) (+ x y)) 3 4)'],
            \[12, '(begin (define y (lambda (x) (+ 5 7))) (y 3))'],
            \[5, '(let ((x 3)) (let ((x 5)) x))'],
            \[17, '(begin (define x 7) (set! x 17) x)'],
            \[120, '(if #t 120 121)'],
            \[121, '(if #f 120 121)'],
            \[11, '(cond (else 11))'],
            \[17, '(cond (#f 11) (#f 13) (else 17))'],
            \[17, '(cond (#f 11) (else (define x 7) (set! x 17) x))'],
            \["x", "'x"],
            \[[1, [2, [3, []]]], "'(1 2 3)"],
            \[[1, 2], "(cons 1 2)"],
            \["x", "((lambda () 'x))"],
            \[[1, [2, [3, []]]], "(cons 1 '(2 3))"],
            \[[1, [2, [3, []]]], "(cons 1 (quote (2 3)))"],
            \['#t', '(eq? 5 5)'],
            \['#f', '(eq? 5 6)'],
            \['#t', '(equal? 5 5)'],
            \['#f', '(equal? 5 6)'],
            \['#f', '(eq? "hello, world" "hello, world")'],
            \['#t', '(equal? "hello, world" "hello, world")'],
            \['#f', "(eq? '(1 2) '(1 2))"],
            \['#t', "(equal? '(1 2) '(1 2))"],
            \['#f', "(equal? '(1 2 3) '(1 2))"],
            \['#t', "(begin (define l '(1 2)) (eq? l l))"],
            \])
