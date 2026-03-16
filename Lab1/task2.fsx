let eps = 0.000001

let rec dichotomy f a b eps =
    let c = (a + b) / 2.0
    if abs (b - a) < eps then c
    elif f a * f c < 0.0 then dichotomy f a c eps
    else dichotomy f c b eps

let rec iteration phi x eps =
    let nextX = phi x
    if abs (nextX - x) < eps then nextX
    else iteration phi nextX eps

let rec newton f df x eps =
    let nextX = x - (f x) / (df x)
    if abs (nextX - x) < eps then nextX
    else newton f df nextX eps

let tasks = [
    ("2x*sin(x)-cos(x)", 
     (fun x -> 2.0*x*sin x - cos x), 
     (fun x -> 3.0*sin x + 2.0*x*cos x), 
     (fun x -> 0.5 * (cos x / sin x)), 0.4, 1.0, 0.5);
    
    ("e^x + sqrt(1+e^2x) - 2", 
     (fun x -> exp x + sqrt(1.0 + exp (2.0*x)) - 2.0), 
     (fun x -> exp x + (exp (2.0*x)) / sqrt(1.0 + exp (2.0*x))), 
     (fun x -> log(2.0 - exp x) / 2.0), -1.0, 0.0, -0.5);
    
    ("ln(x) - x + 1.8", 
     (fun x -> log x - x + 1.8), 
     (fun x -> 1.0/x - 1.0), 
     (fun x -> log x + 1.8), 2.0, 3.0, 2.5)
]

let runTable () =
    printfn "%-25s | %-12s | %-12s | %-12s" "Уравнение" "Дихотомия" "Итерации" "Ньютон"
    printfn "%s" (String.replicate 70 "-")
    
    for (name, f, df, phi, a, b, x0) in tasks do
        let d = dichotomy f a b eps
        let i = iteration phi x0 eps
        let n = newton f df x0 eps
        printfn "%-25s | %-12.4f | %-12.4f | %-12.4f" name d i n

runTable()