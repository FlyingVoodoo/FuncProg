let rec factorial n = 
    if n <= 1.0 then 1.0
    else n * factorial (n - 1.0)

// Наивный способ
let nthTerm (n: int) (x: float) =
    let nF = float n
    let sign = (-1.0) ** (nF - 1.0)
    let numerator = (2.0 ** (2.0 * nF - 1.0)) * (x ** (2.0 * nF))
    let denominator = factorial (2.0 * nF)
    sign * (numerator / denominator)

let rec summarize f n eps currentSum =
    let term = f n
    if abs term < eps then
        (currentSum + term, n)
    else
        summarize f (n + 1) eps (currentSum + term)

// Умный способ
let rec smartTaylor x eps n prevTerm currentSum =
    if x = 0.0 then (0.0, 0)
    else
        let nF = float n
        let R = - (4.0 * x**2.0) / ((2.0 * nF + 1.0) * (2.0 * nF + 2.0))
        let nextTerm = prevTerm * R
        
        if abs nextTerm < eps then
            (currentSum + nextTerm, n + 1)
        else
            smartTaylor x eps (n + 1) nextTerm (currentSum + nextTerm)

let main () =
    let a = 0.0
    let b = 1.0
    let step = 0.1
    let eps = 0.000001

    printfn "%-5s | %-10s | %-12s | %-8s | %-12s | %-8s" "x" "Builtin" "Smart Taylor" "# terms" "Dumb Taylor" "# terms"
    printfn "%s" (String.replicate 75 "-")

    for i in 0 .. int((b - a) / step) do
        let x = a + float i * step
        
        let builtin = (sin x) ** 2.0
        
        let (dumbRes, dumbCount) = summarize (fun n -> nthTerm n x) 1 eps 0.0

        let firstTerm = x ** 2.0
        let (smartRes, smartCount) = 
            if x = 0.0 then (0.0, 0)
            else smartTaylor x eps 1 firstTerm firstTerm
        
        printfn "%-5.1f | %-10.6f | %-12.6f | %-8d | %-12.6f | %-8d" x builtin smartRes smartCount dumbRes dumbCount

main()