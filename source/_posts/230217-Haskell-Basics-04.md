---
title: Haskell Basics - Lists & Functions 03
id: haskell-basics-lists-&-funcs-03
tags:
	- haskell
	- basics
	- functions
categories:
	- haskell
date: 2023-03-09 01:00:00
---
In the {% post_link 230210-Haskell-Basics-03 'last post' %} we explored the definition of functions by defining a function that returns `Yep` (our own boolean definition, `Booly`) if we pass it an empty list (using our own list definition: `Lst`).  Here's the code with which we were working, and that we'll expand upon in this post. A friend of mine let me know about the Haskell Playground; I've added this as a [snippet on the playground](https://play.haskell.org/saved/s6dxVzd1), I'll try to add all my samples over there and see how it works out.

```haskell picking up from last time
{-# LANGUAGE NoImplicitPrelude #-}
module List230217 where

-- | The Prelude defines the most common data types and functions for
-- | us, but the LANGUAGE construct at the top of the page excludes the
-- | normally automatically imported Prelude. Here we add back in what
-- | we might need in this module.
import Prelude (Show)

-- | our own version of a list datatype
data Lst a 
  = NIL
  | Cons a (Lst a)
  deriving (Show)

-- | our own version of a boolean datatype
data Booly
  = Yep
  | Nope
  deriving (Show)

-- | Returns true if an instance of @Lst@ is empty, false otherwise
isEmpty :: Lst a -> Booly
isEmpty NIL = Yep
isEmpty _   = Nope
```

## List Length
Next function we'll tackle is finding the length of a list. Most (dare I say all) languages have a function to find the length of a list (or array, or vector, etc.), Haskell included. We, however, have been intentionally _excluded_ `Prelude`, Haskell's standard library, and we've been re-inventing the wheel by defining our own list and boolean types for the purpose of understanding how these fundamental types work. 

### Recursive Functions and Inductive Data Types
You'll recall that our definition for lists is an _inductive_ data type with two constructors, the `NIL` constructor that represents an empty list, and the `Cons a (Lst a)` constructor, which represents one element of the list and the rest of the list. When a sum type has one constructor that refers to it's own type (`Cons` has a reference to `Lst`), you should recognize it as an inductive type definition.

Inductive types and recursive functions are two sides of the same coin. The natural way to enumerate an inductive data type such as this is with a recursive function. Just like an inductive definition starts with a _base case_, which for our list definition is `NIL`, the same is true of the recursive function we use to manipulate it. This is a function for calculating the length of a list.

```haskell Length of a List
{-| An example list of integers 1 - 4 -}
ex01 :: Lst Int
ex01 = Cons 1 (Cons 2 (Cons 3 (Cons 4 NIL)))

{-| Compute the length of a list as an integer -}
lstLen :: Lst a -> Int
lstLen NIL = 0
lstLen (Cons x xs) = 1 + lstLen xs
```

([snippet](https://play.haskell.org/saved/al4dQ9oc) in the playground)

You remember that the first line of the function is the type declaration `lstLen :: Lst a -> Int`. This tells us that that function has one parameter of type `Lst a`, where the `a` is a type variable meaning this will work with lists of any type. 

The base case for the recursive function `lstLen` handles the case when the list (which is the only parameter) is `NIL`. When the list is `NIL`, the length is zero, because it's the empty list. The recursive case matches a list that is `(Cons x xs)`, this will match any non-empty list. You'll notice our matching case binds variables `x` and `xs`.  Get used to this syntax, it's **super** common, and Haskell has robust pattern matching and binding. 

The `x` in the `Cons` case is the first item in the list, and the `xs` is the rest of the list. Note that the rest of the list is another list. We can say verbally that the `Cons` is an item in the list and another list. The "another list" may be another `Cons` , which would be the head of _that_ list and another list. And so on, until we get to a `Nil`, which is the empty list.

To calculate the length of the list, when we get to a `Cons`, we'll say that the length of the list is one, plus whatever the length of the rest of the list is. In effect, our `lstLen` function replaces all the `Cons` that are chained together with a `1 +` and it replaces the `NIL` with zero:

```text From List to Expression
(Cons 1 (Cons 2 (Cons 3 (Cons 4 NIL))))  -- ex01
(1 +    (1 +    (1 +    (1 +    0  ))))  -- lstLen ex01
```

The relationship of the inductive type to the recursive function is obvious! You'll see this pattern over and over again, as inductive types are fundamental to many functional data structures, and many functional languages (Haskell included) don't even have constructs like for loops. In Haskell, when you have to loop, you have to use recursion. If you're familiar with functions like `map` and `fold` you might say "hey wait - I don't _have_ to use recursion, I can use functions like `map` and `fold` " ... well that's true but map and fold are recursive functions, so you may not have to _write_ recursive functions, but you will definitely _use_ recursive functions ;) 

### Double Every List Item
Now rather than counting every list item, we're going to _modify_ each item in a list of integers by doubling it. But Haskell is immutable, so we can't change existing values. Whenever you want to "change" the value of a variable you have to instead think of creating a new variable with the new value. 

```haskell Double Every Item
{-| Double every item in the list -}
doubler :: Lst Int -> Lst Int
doubler NIL = NIL
doubler (Cons x xs) = Cons (x * 2) (doubler xs)
```

([snippet](https://play.haskell.org/saved/Wtw54h4Y) in playground)

The `doubler` function follows the same pattern as `lstLen`. We handle the base case of the inductive type `Lst` by pattern matching `NIL`. Doubling every value in the empty list results in the empty list, no surprise there. The inductive case matches against `Cons x xs`, with the head of the list bound to `x` and the rest of the list is `xs`.  The result of this match is a _new_ `Cons`, with the head of the list being doubled, and calling `doubler` recursively on `xs` to get the new rest of the list.

Much like `lstLen`, the `doubler` function replaces each `Cons` as it walks its way down the list.

```text From List to Modified List
(Cons 1       (Cons 2       (Cons 3       (Cons 4       NIL))))  -- ex01
(1 +          (1 +          (1 +          (1 +          0  ))))  -- lstLen ex01

(Cons (1 * 2) (Cons (2 * 2) (Cons (3 * 2) (Cons (4 * 2) NIL))))  -- doubler ex01
```

### Generalizing with Map and Fold
You may recognize the `doubler` as doing a _map_ operation, and our `lstLen` is doing a _fold_. Let's start with a map operation.

#### Map
When we want to apply a function, for example one that double's an integer, to every item in a list (or other collection as we'll see), we can write the function directly as we did with `doubler`, or we can use a _higher order function_ called map. A **higher-order function** is one that either takes a function as an argument, returns a function, or both. It's a function that operates on functions.

`doubler` is a function that doubles every element of a list. If we just look at the doubling part of `doubler`, we have

```haskell double an integer
{-| Doubles the input parameter -}
dub :: Int -> Int
dub n = n * 2
```

We can rewrite `doubler` using this function:

```haskell doubler using dub
{-| Double every item in the list using dub -}
doubler :: Lst Int -> Lst Int
doubler NIL = NIL
doubler (Cons x xs) = Cons (dub x) (doubler xs)
```

Now we can see how we might be able to generalize this function, if we pass the `dub` function in as a parameter. We'll change the name of the function as now it could do other things to every value in the list. Note how we specify the type of the modifying function; it's the same type signature of our `dub` function.

```haskell Generalized list modifier 
{-| Apply a function to every element in the list to generate
    a new element -}
lstModder :: (Int -> Int) -> Lst Int -> Lst Int
lstModder _ NIL = NIL
lstModder modFn (Cons x xs) = Cons (modFn x) (lstModder modFn xs)

{-| Output list and doubled list -}
main :: IO ()
main = do
  putStrLn $ "ex1         : " ++ (show ex01) 
  putStrLn $ "doubled ex1 : " ++ (show (lstModder dub ex01))
```
[Snippet](https://play.haskell.org/saved/rIqKIt7i)

The first parameter is a function from `Int` to `Int`. We pass our integer doubling function `dub` to the `lstModder` function to do the work of modifying each element. Note how in the base case, when the list is `NIL` (empty), we use an underscore in the place of the first parameter. This tells Haskell to ignore the first parameter. In an empty list, we won't be using the modifier function. We could still name it, but Haskell would then warn us we have an unused variable.

The parentheses in the type signature are necessary because the arrow `->` which defines a function, binds to the right. That means without parentheses the order of precedence would look like the following, and the function would not type check. It would be expecting three parameters first and second parameters to be a `Int`s and the third an `Lst Int`. 

```haskell wrong order of precedence
{-| does not type check -}
lstModder :: (Int -> (Int -> (Lst Int -> Lst Int)))
lstModder _ NIL = NIL
lstModder modFn (Cons x xs) = Cons (modFn x) (lstModder modFn xs)
```

Now we have a general purpose modifier for lists of _integers_, but we could generalize further. What if we didn't specify `Int` as the input and output of our modifier function. And even better, what if the input and output of the modifier function didn't have to both be the same type?

```haskell general purpose lst modder
{-| apply a modifier function to a list of any type -}
lstModder :: (a -> b) -> Lst a -> Lst b
lstModder _ NIL = NIL
lstModder modFn (Cons x xs) = Cons (modFn x) (lstModder modFn xs)
```

We've already seen a _type variable_ used in the definition of `Lst` to enable the `Lst` inductive type to represent a list of any type. The same approach is used in the `lstModder` function, the lower case `a` and `b` can both represent any type, without restriction. The implication is that the modifier function can not only modify one of the elements of the list, it can return a completely different type! The modifier turns a single `a` into a `b`, and the `lstModder` turns a _list of_ `a` into a _list of_ `b`. It could be that `a` and `b` are the same type as they are in the `dub` function, but they can also be different.

It happens now that our list modifier function is exactly a **map** function for our `Lst` data type. 

```haskell list modder is map
{-| Apply a function to eeach element in a list 
    to produce a new list -}
lstMap :: (a -> b) -> Lst a -> Lst b
lstMap _ NIL         = NIL
lstMap f (Cons x xs) = Cons (f x) (lstMap f xs)
```
[Snippet](https://play.haskell.org/saved/QyPA1efd)

We can use our new `lstMap` with a different modifier function to create a sort of a histogram:

```haskell from a list of ints to a histogram
{-| Makes a string of repeated strings -}
makeStringOf :: String -> Int -> String
makeStringOf _ 0 = ""
makeStringOf s n = s ++ (makeStringOf s (n - 1))

{-| Makes a string of asterisks -}
makeStringOfStars :: Int -> String
makeStringOfStars n = makeStringOf "*" n

{-| Turn a list of integers to a list of strings of asterisks where
    each string of asterisks represents the integer from the 
    original list -}
histogramOf :: Lst Int -> Lst String
histogramOf nums = lstMap makeStringOfStars nums
```
[Snippet](https://play.haskell.org/saved/q4ihdfWk)

Let's introduce a couple of more concepts that you'd find in real Haskell code before moving on. Here's another way to produce our histogram.

```haskell another way to histogram
putStrLn $ show $ lstMap (makeStringOf "*") ex01
```
[Snippet](https://play.haskell.org/saved/iSchmZYf)

There are a few different things going on here. First, the `putStrLn $ show $` (look at the [snippet](https://play.haskell.org/saved/iSchmZYf) for the full context) - this is just a way to print our result to the output, we'll look at the details later.  More interestingly, we've gotten rid of `mkeStringOfStars`. The type of `makeStringOf` is `String -> Int -> String`. In Haskell, if we pass this function that expects 2 parameters just one, what would it return? 

```text curried function

makeStringOf :: String -> Int -> String
                 🡡         \__________/
                 |               |
                 |               |
makeStringOf    "*"              |
                                 |
                                 🡣
                          /-----------\
                          Int -> String
```

What we get back is another function, this time `Int -> String`. The asterisk string we pass to `makeStringOf` is _captured_ in this new function that is returned. The new function will turn any `Int` into a string of that many asterisks. We then pass this in our `lstMap` function as the modifier. This behavior is known as _partial application_, we're only partially applying the parameters the `makeStringOf` function is expecting. This is perfectly legal and canonical Haskell. 

#### Fold
While a map function is definitely a powerful tool, it is limited in that the structure being mapped over, a list in our case, is preserved in the output. Map doesn't allow us to change the output type. It will not only be a list, but be a list of the same size. What we need is a similar function that walks the data structure and allows more flexibility. Let's use this to refactor our `lstLen` function for finding the length of a list.

```haskell fold for our list type
{-| Fold for Lst -}
lstFold :: (b -> a -> b) -> b -> Lst a -> b
lstFold _ acc NIL = acc
lstFold f acc (Cons x xs) = lstFold f (f acc x) xs

...

{-| Get the length of a list by folding over the list with
    an accumulator function -}
lstLen :: Lst a -> Int
lstLen xs = lstFold (\acc _ -> 1 + acc) 0 xs
```
[Snippet](https://play.haskell.org/saved/lkigFcXR)

The type of `lstFold` is interesting. The first parenthesized parameter has type `(b -> a -> b)`. We saw earlier that the parentheses indicate that the first parameter is a function, in this case a function that takes two parameters `a` and `b`, and returns a value of the same type `b` as the first parameter. The first parameter, `b`, is usually referred to as the _accumulator_. This is the new value that we're building with our fold. Each application of `lstFold` will get the accumulator and the next value in the list. It should use these two parameters to calculate and return a new value for the accumulator. 

The next parameter after the accumulator function is also of type `b`, and this should be the value of the accumulator _before we start processing the list_. When counting the elements in a list for example, we should start at zero as the accumulator. We need this because even at the first element, the accumulator function needs a value for the accumulator. 

The last parameter to `lstFold` is the list over which we are folding. 

The new `lstLen` function now uses `lstFold` and another new concept, an anonymous function, to calculate the length of the list. The anonymous function saves us from having to write a named function just to add one to a number. In Haskell an anonymous function begins with the backslash followed by the arguments of the function separated by spaces, and the body of the function follows the right arrow `->`. 

```haskell anonymous number
-- instead of
add1ToX :: Int -> Int -> Int
add1ToX acc _ = acc + 1

-- we can use an anonymous function
\acc _ -> acc + 1
```

Note that in the case of counting the elements in the list, we don't actually care _what_ the element is, so in our accumulator function we ignore the second parameter that `lstFold` is looking for, the element in the list. But if we wanted to say sum up the values in an integer list, we could do that with this call to `lstFold`. The accumulator is carrying the running sum through the list.

```haskell sum the values in an integer list
putStrLn $ show $ lstFold (\acc x -> acc + x) 0 ex01
```
[Snippet](https://play.haskell.org/saved/6AGvoDnV)

#### η-conversion
There's another interesting optimization we can use, called  η-conversion, (eta conversion). This one is optional, and some people really don't care to see it in code, but it is a fundamental concept with which it is good to be comfortable. As a matter of fact, with the Visual Studio Code plugin, you'll get linter hints where there's an opportunity to use  η-conversion.

```haskell beta reduction for lstLen

{-| before η-conversion -}
lstLen1 :: Lst a -> Int
lstLen1 xs = lstFold (\acc _ -> 1 + acc) 0 xs

{-| with  η-conversion -}
lstLen :: Lst a -> Int
lstLen = lstFold (\acc _ -> 1 + acc) 0
```

Note that the type signature for `lstLen` hasn't changed, it's still looking for a `Lst a` and returning an `Int`. But in the definition, we've left off the parameter. We've also left the last parameter from body of the function, the call to `lstFold`. Look at this simple example:

```haskell η-conversion
-- these two definitions are the same (assume an abs function exists):

myAbs1 :: Int -> Int
myAbs1 x = abs x

myAbs2 :: Int -> Int
myAbs2 = abs
```

#### Partial Application of Infix Operators
Another common technique is using partial application of infix operators. Remembering that addition and multiplication are actually functions with an _infix_ syntax, which means the arguments of the function are placed on either side of the function:

```haskell infix functions
add :: Int -> Int
add a b = a + b

-- these are equiv
-- note that you can't redefine ans over and over, this is just an example

ans :: Int
ans = 2 + 3

ans = (+) 2 3

ans = add 2 3

ans = 2 `add` 3
```

When we want to treat an infix operator, like addition, as a normal prefix function, we wrap it in parentheses. And when we want to treat a standard prefix function like `add` above as an _infix_ function, we can wrap it in back ticks.

Knowing this, we can leverage partial application:

```haskell partial application of infix operators

{-| Add 2 to every integer in a list -}
ans1 :: Lst Int
ans1 = lstMap (2 +) ex01

{-| sum up every element in a list -}
ans2 :: Int
ans2 = lstFold (+) 0 ex01
```

## Wrapping up
This is already a long post ... let's wrap it up by switching from the custom types we've been using for bools and lists to the standard ones from the Prelude, and using anonymous functions and η-conversion where we'd typically find them. Although there are standard functions for list length and sum, we'll still use our own.

On difference to notice when looking at this converted code - since Haskell uses lists so extensively, there's some syntactic sugar for added convenience. We can use square brackets in the type signature `[Int]` rather than `List Int`, we can use empty brackets `[]` rather than `NIL`, and we can use an infix operator `:` rather than `Cons`:

```haskell list syntactic sugar
----- Lst ------------------------------------ 
emptyLst :: Lst Int
emptyLst = NIL

ex01 :: Lst Int
ex01 = (Cons 1 (Cons 2 (Cons 3 (Cons 4 NIL))))

mapLst :: (a -> b) -> Lst a -> Lst b
mapLst _ NIL = NIL
mapLst f (Cons x xs) = Cons (f x) (map f xs)

----- List -----------------------------------
emptyList :: [Int]
emptyList = []

ex01 = (1 :: (2 :: (3 :: (4 :: []))))
-- or --
ex01 = [1,2,3,4]

mapList :: (a -> b) -> [a] -> [b]
mapList _ [] = []
mapList f (x:xs) = (f x) : (map f xs)
```

And finally, the final code listing, using the `List` type and other functions available in the Prelude.

```haskell Exercises with built in types and functions
import Prelude 

{- Prelude has 
     null :: [a] -> Bool    that retunrs True for empty lists,
     length :: [a] -> Int    to get the length of litss, and
     sum :: [Int] -> Int to get the sum of elements in the list
     
  so these implementations could be simpler still -}

{-| An example list of integers 1 - 4 -}
ex01 :: [Int]
ex01 = [1,2,3,4]

{-| Makes a string of repeated strings -}
makeStringOf :: String -> Int -> String
makeStringOf _ 0 = ""
makeStringOf s n = s ++ (makeStringOf s (n - 1))

main :: IO ()
main = do
  putStrLn $ "ex1         : " ++ (show ex01) 
  putStrLn $ "ex1 length  : " ++ (show (foldl (\acc _ -> acc + 1) 0 ex01))
  putStrLn $ "doubled ex1 : " ++ (show (map (2 *) ex01))
  putStrLn $ "sum ex1     : " ++ (show (foldl (+) 0 ex01))
  putStrLn $ "histogram   : " ++ (show (map (makeStringOf "*") ex01))
  
  -- bonus - mapping using foldr for a right fold...
  putStrLn $ "doubled ex1 : " ++
    (show (foldr (\x acc -> (2 * x):acc) [] ex01))
```
[Snippet](https://play.haskell.org/saved/rGLvoUcQ)

Even with as much as we've looked at, we're still exploring the very tippy tip of the iceberg of the Haskell language. We've only looked at lists, but I've hinted that functions like map and fold can work over other data structures as well, like trees for example. Exploring how a function like map or fold might work over different data structures will require exploring _type classes_ - a topic we'll have to tackle in another (possibly series of) posts!

But don't be discouraged - with the information we've covered we can create most data structures, and most any function to manipulate these data structures, only missing some conveniences and abstractions that would make our code more concise and reusable, but no more correct. You've got the basic building blocks right now!