module TdsParser.FParsecExtensions
    
    open FParsec

    let pipe11 p1 p2 p3 p4 p5 p6 p7 p8 p9 p10 p11 f =
        pipe3 (tuple4 p1 p2 p3 p4) (tuple4 p5 p6 p7 p8) (tuple3 p9 p10 p11)
            (fun (x1, x2, x3, x4) (x5, x6, x7, x8) (x9, x10, x11) -> f x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11)