module SumProduct


type Sum<'a> = Sum of 'a

type Product<'a> = Product of 'a


// instance Num a => Monoid (Sum a) where
//   mempty = Sum 0
//   (Sum a) `mappend` (Sum b) = Sum (a + b)

type SumM () =
    static member mempty = Sum 0
    static member mappend (Sum a) (Sum b) = Sum (a + b)


// instance Num a => Monoid (Product a) where
//   mempty = Product 1
//   (Product a) `mappend` (Product b) = Product (a * b)

type ProductM () =
    static member mempty = Product 1
    static member mappend (Product a) (Product b) = Product (a * b)


// unSum :: Sum a -> a
let unSum (Sum a) = a

// unProduct :: Product a -> a
let unProduct (Product a) = a
