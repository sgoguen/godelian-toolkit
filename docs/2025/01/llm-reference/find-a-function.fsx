// #quit

#load "../../../../godelian-toolkit.fsx"

//  Let's do arithmetic program synthesis with the Gödelian Toolkit

open GodelianTooklit

type Expr = 
    | Const of int
    | Var of int
    | Add of Expr * Expr
    | Mul of Expr * Expr
    | Sub of Expr * Expr
    | Div of Expr * Expr

    override this.ToString() =
        match this with
        | Var n -> sprintf "x%d" n
        | Const n -> n.ToString()
        | Add(l, r) -> sprintf "(%s + %s)" (l.ToString()) (r.ToString())
        | Mul(l, r) -> sprintf "(%s * %s)" (l.ToString()) (r.ToString())
        | Sub(l, r) -> sprintf "(%s - %s)" (l.ToString()) (r.ToString())
        | Div(l, r) -> sprintf "(%s / %s)" (l.ToString()) (r.ToString())

