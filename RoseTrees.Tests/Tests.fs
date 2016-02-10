module RoseTrees.Tests


open Xunit
open RoseTrees
open SumProduct


let xs = 0 %> [1 %> [2 %> [3 %> [4 %> [] ; 5 %> []]]] ; 6 %> [] ; 7 %> [8 %> [9 %> [10 %> []] ; 11 %> []] ; 12 %> [13 %> []]]]


[<Fact>]
let ``ex 1`` () =
    let tree = 'x' %> List.map (fun c -> c %> []) ['a'..'A']
    let ex1 = List.length (children tree)
    Assert.Equal( 0, ex1 )

[<Fact>]
let ``ex 2`` () =
    let ex2 = root << List.head << children << List.head << children << List.head << List.skip 2 <| children xs
    Assert.Equal( 9, ex2 )

[<Fact>]
let ``ex 3`` () =
    let tree = 1 %> List.map (fun c -> c %> []) [1..5]
    let ex3 = size tree
    Assert.Equal( 6, ex3 )

[<Fact>]
let ``ex 4`` () =
    let tree = 1 %> List.map (fun c -> c %> []) [1..5]
    let ex4 = size << List.head << children <| tree
    Assert.Equal( 1, ex4 )

[<Fact>]
let ``ex 5`` () =
    let tree = 1 %> List.map (fun c -> c %> []) [1..5]
    let ex5 = leaves tree
    Assert.Equal( 5, ex5 )

let product = List.fold (fun x y -> x * y) 1

[<Fact>]
let ``ex 6`` () =
    let tree = 1 %> List.map (fun c -> c %> []) [1..5]
    let ex6 = product (List.map leaves (children tree))
    Assert.Equal( 1, ex6 )

[<Fact>]
let ``ex 7`` () = 
    let ex7 = (leaves << List.head << children << List.head << children <| xs) * (product << List.map size << children << List.head << List.skip 2 << children <| xs)
    Assert.Equal( 16, ex7 )

[<Fact>]
let ``ex 8`` () =
    let tree = 1 %> List.map (fun c -> c %> []) [1..5]
    let ex8 = size (fmap leaves (fmap (fun x -> x %> []) tree))
    Assert.Equal( 6, ex8 )

[<Fact>]
let ``ex 10`` () =
    let ex10 = round << root << List.head << children << fmap (fun x -> if x > 0.5 then x else 0.0) <| fmap (fun x -> sin(float x)) xs
    Assert.Equal( 1.0, ex10 )

[<Fact>]
let ``ex 13`` () =
    let num1 = SumM.mappend (SumM.mappend (Sum 2) (SumM.mappend (SumM.mappend SumM.mempty (Sum 1)) SumM.mempty)) (SumM.mappend (Sum 2) (Sum 1))
    let num2 = SumM.mappend (Sum 3) (SumM.mappend SumM.mempty (SumM.mappend (SumM.mappend (SumM.mappend (Sum 2) SumM.mempty) (Sum (-1))) (Sum 3)))
    let ex13 = unSum (SumM.mappend (Sum 5) (Sum (unProduct (ProductM.mappend (Product (unSum num2)) (ProductM.mappend (Product (unSum num1)) (ProductM.mappend ProductM.mempty (ProductM.mappend (Product 2) (Product 3))))))))
    Assert.Equal( 257, ex13 )

[<Fact>]
let ``ex 15`` () =
    let sumxs = Sum 0 %> [Sum 13 %> [Sum 26 %> [Sum (-31) %> [Sum (-45) %> [] ; Sum 23 %> []]]] ; Sum 27 %> [] ; Sum 9 %> [Sum 15 %> [Sum 3 %> [Sum (-113) %> []] ; Sum 1 %> []] ; Sum 71 %> [Sum 55 %> []]]]
    let ex15 = unSum (SumM.mappend (SumM.mappend (fold sumxs) (SumM.mappend (fold << List.head << List.skip 2 << children <| sumxs) (Sum 30))) (fold << List.head << children <| sumxs))
    Assert.Equal( 111, ex15 )
