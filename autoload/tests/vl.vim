function! tests#vl#TestVimLisp() abort
    function! TestVlEval(expr) abort
        let TEST_ENV = vl#ExtendEnv(vlenv#BuildInitialEnv(), ["x"], [3])
        return vl#Eval(a:expr, TEST_ENV)
    endfunction

    function! s:TestSubRefer() abort
        let e = "(lambda (x y) ((lambda (z) (x y z))))"
        let e = vlparse#Parse(vlparse#Tokenize(e))
        call vltrns#ScanLambdas(e)
        "echo e
    endfunction

    function! RunTests(tests) abort
        let [success, fail] = [0, 0]
        for [expected, actual] in a:tests
            if string(expected) == string(actual)
                let success += 1
            else
                echo "[ ] "..string(actual).."\t\t: "..string(expected)
                let fail += 1
            endif
        endfor
        let total = success + fail
        echo "[+] Ran "..total.." tests.\t("..success.."/"..total.." passed)"
    endfunction

    function! RunEvalTests(tests) abort
        let [success, fail] = [0, 0]
        for [expected, expr] in a:tests
            let actual = TestVlEval(expr)
            if string(expected) == string(actual)
                "echo "[+] "..string(expr)
                let success += 1
            else
                echo "[!] "..string(actual).."\t\t: "..expr
                let fail += 1
            endif
        endfor
        let total = success + fail
        echo "[+] Ran "..total.." vl#Eval tests.\t("..success.."/"..total.." passed)"
    endfunction

    call RunEvalTests([
                \[5, '((lambda (x) ((lambda (y) y x x) x)) 5)'],
                \[5, '(let ((x 3)) (let ((x 5)) x (+ 1 2) x))'],
                \[4, '(begin 1 (+ 1 2) 4)'],
                \[[1, 2], "'(1 . 2)"],
                \[[1, [2, [3, []]]], "'(1 . (2 3))"],
                \[[1, 2], '(cons 1 (call/cc (lambda (k) (k 2))))'],
                \[["condition", [123, []]], '(raise 123)'],
                \[[1, [2, []]], "'(1 . (2 . ()))"],
                \[[1, []], "'(1 . ())"],
                \[[], "'()"],
                \[-3, '-3'],
                \[123, 123],
                \[3, 'x'],
                \[3, '(- 5 2)'],
                \[3, '(+ 1 (+ 1 1))'],
                \[10, '(* 5 2)'],
                \[3, '(/ 12 4)'],
                \[3, '(+ 1 2)'],
                \[1, "(define x 3)"],
                \[12, '((lambda (x y) (+ x y)) 5 7)'],
                \[1, '(define x "hello, world")'],
                \[1, '(set! x 3)'],
                \[3, '(begin (define x 3) x)'],
                \[9, '((lambda () 5 7 9))'],
                \[7, '(call/cc (lambda (k) 7))'],
                \[7, '(call/cc (lambda (k) (k 7)))'],
                \[-2, '(+ 1 (call/cc (lambda (k) (k -3))))'],
                \[1, '(+ 1 (call/cc (lambda (k) 0)))'],
                \[4, '(+ 1 (call/cc (lambda (k) (k 3))))'],
                \[7, '(let ((x 3) (y 4)) (+ x y))'],
                \[7, '((lambda (x y) (+ x y)) 3 4)'],
                \[12, '(begin (define y (lambda (x) (+ 5 7))) (y 3))'],
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
                \['#t', '(and)'],
                \['#t', '(and (and 1 2 (and 3) (and) #t) #t)'],
                \['#f', '(and (and 1 2 (and #f) (and) #t) #t)'],
                \['#t', '(and (> 5 3))'],
                \['#f', '(and (> 5 3) (> 1 3))'],
                \['#f', '(and (> 1 3))'],
                \[3, '(and (> 5 3) (+ 1 2))'],
                \['#t', '(> 5 3)'],
                \['#f', '(> 2 3)'],
                \['#t', '(< 1 3)'],
                \['#f', '(< 4 3)'],
                \['#t', '(>= 3 3)'],
                \['#f', '(>= 2 3)'],
                \['#t', '(<= 3 3)'],
                \['#f', '(<= 4 3)'],
                \['#t', '(equal? 5 5)'],
                \['#f', '(equal? 5 6)'],
                \['#f', '(eq? "hello, world" "hello, world")'],
                \['#t', '(equal? "hello, world" "hello, world")'],
                \['#f', "(eq? '(1 2) '(1 2))"],
                \['#t', "(equal? '(1 2) '(1 2))"],
                \['#f', "(equal? '(1 2 3) '(1 2))"],
                \['#t', "(begin (define l '(1 2)) (eq? l l))"],
                \])
                "\[5, '(let ((x 0)) (while ((< x 5)) (set! x (+ x 1))) x)'],
                "\[0, '(let ((x 0) (y 0)) (while ((< x 5)) (set! x (+ x 1))) y)'],
                "\[50, '(let ((x 0) (y 0)) (while ((< (+ x y) 100)) (set! x (+ x 1)) (set! y (+ y 1))) y)'],

    call RunTests([
                \["hello, world", vlutils#PrettyPrint(vl#Eval('"hello, world"'))],
                \["(1 2 3)", vlutils#PrettyPrint(vl#Eval("'(1 2 3)"))],
                \["13", vlutils#PrettyPrint(vl#Eval("13"))],
                \])
endfunction

function! s:FloatAvg(list) abort
    let total = 0
    for x in a:list
        let total += x
    endfor
    return total / len(a:list)
endfunction

function! s:FloatMax(list) abort
    let max = 0
    for x in a:list
        if x > max
            let max = x
        endif
    endfor
    return max
endfunction

function! s:FloatMin(list) abort
    let min = a:list[0]
    for x in a:list
        if x < min
            let min = x
        endif
    endfor
    return min
endfunction

function! s:Benchmark(desc, func, n) abort
    let times = []
    for i in range(a:n)
        call add(times, vlutils#TimeExecution(a:func))
    endfor
    echo "Executed "..a:desc.." "..a:n.." times:"
    echo "\tMax: "..string(s:FloatMax(times))
    echo "\tMin: "..string(s:FloatMin(times))
    echo "\tAvg: "..string(s:FloatAvg(times))
endfunction

function! tests#vl#Benchmark() abort
"    let e = "(begin (define double (lambda (x) (+ x x)) (double (double (double 7)))))"
"    let Func = {-> vl#Eval(e)}
"    call s:Benchmark("nested double function call", Func, 200)
"    let e = '(let ((x 0) (y 0)) (while ((< (+ x y) 100)) (set! x (+ x 1)) (set! y (+ y 1))) y)'
"    let Func = {-> vl#Eval(e)}
"    call s:Benchmark("while loop 50 reps", Func, 200)
    let e = "(begin (define ! (lambda (n) (if (equal? n 0) 1 (* n (! (- n 1)))))) (! 20))"
    let Func = {-> vl#Eval(e)}
    call s:Benchmark("fact 20", Func, 1000)
endfunction
