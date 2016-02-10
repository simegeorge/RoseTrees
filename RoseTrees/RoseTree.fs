module RoseTrees


open SumProduct

// A rose tree is a tree with a variable number of branches per node
//
// F# version of the following Haskell type 
//
//      data Rose a = Rose a [Rose a]

type Rose<'a> = Rose of 'a * Rose<'a> list

// Define an F# version of the Haskell inline type constructor
//
//      data Rose a = a :> [Rose a]
//
// Note that :> is the upcast operator in F# so we use %> instead

let (%>) a b = Rose (a,b)

// root :: Rose a -> a 
let root =
    function
    | Rose (x,_) -> x

// children :: Rose a -> [Rose a]
let children = 
    function
    | Rose (_,xs) -> xs

// size :: Rose a -> Int
let rec size = 
    function
    | Rose (_,[]) -> 1
    | Rose (_,xs) -> List.fold (fun acc subtree -> acc + size subtree) 1 xs

// leaves :: Rose a -> Int
let rec leaves = 
    function
    | Rose (_,[]) -> 1
    | Rose (_,xs) -> List.fold (fun acc subtree -> acc + leaves subtree) 0 xs


// class Functor f where 
//   fmap :: (a -> b) -> f a -> f b

// instance Functor Rose where
//   fmap :: (a -> b) -> Rose a -> Rose b

let rec fmap f =
    function
    | Rose (x,[]) -> (f x) %> []
    | Rose (x,xs) -> (f x) %> List.map (fmap f) xs


// class Functor f => Foldable f where
//   fold :: Monoid m => f m -> m
//   foldMap :: Monoid m => (a -> m) -> (f a -> m)
//   foldMap f = fold . fmap f
  
// instance Foldable Rose where

let rec fold =
    function
    | Rose (x,[]) -> x
    | Rose (x,xs) -> SumM.mappend x <| List.fold (fun acc el -> SumM.mappend acc (fold el)) SumM.mempty xs


