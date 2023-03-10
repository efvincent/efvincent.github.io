---
title: Haskell Basics - Lists & Functions 03
id: haskell-basics-lists-&-funcs-03
tags:
	- haskell
	- basics
	- functions
categories:
	- haskell
date: 2023-02-17 01:00:00
---
In the {% post_link 230210-Haskell-Basics-03 'last post' %} we explored the definition of functions by defining a function that returns `Yep` (our own boolean definition, `Booly`) if we pass it an empty list (using our own list definition: `Lst`).  Here's the code with which we were working, and that we'll expand upon in this post:

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

Dare we do the same with - numbers?

## Natural Numbers
Why on Earth would we want to implement our own type for numbers? To answer that we'll need to take a bit of a detour; prepare yourself for a very condensed history lesson.

### Church and Turing
The difference between imperative languages and functional languages is more than just anonymous functions being passed to a map or fold. In 1936 Alan Turing and Alonzo Church (who was Turing's PhD advisor at Princeton) were both working on the idea of formalizing the idea of computability.

Turing's approach led him to describe an imaginary machine he called the universal machine that could read and write ones and zeros to cells on an infinitely long tape. This machine could theoretically run algorithms that were encoded as ones and zeros on that infinite tape. In 1945 Turing's work inspired John Von Neumann to create the Van Neumann architecture which introduced the ideas of a CPU, memory, and some other clever bits, and computers started to look vaguely like what we think of today. Then Grace Hopper came along and created the first compiler because people were sick of programming with ones and zeros (forgive me). Imperative programming languages evolved from the need to push ones and zeros around the circuits of a computer in a less annoying way.

Alonzo went a different way - he created the Lambda Calculus, which is a very small set of rules that form a system of  variables and functions. No imaginary machine, just a pencil and paper. It turns out that Church's Lambda Calculus was just as expressive - meaning any computation a Turing machine could calculate could be calculated in lambda calculus, and vice versa. This is known as the Church Turing Thesis. Church's Lambda Calculus is the beating heart of functional programming. Haskell for example is an evolution of that first Lambda Calculus.

Church came up with Lambda Calculus at pretty much the same time Turing came up with his machine, and yet Turing gets _two_ movies (the one you know and the more obscure _Codebreaker_, a biopic from 2011), and no one has ever heard of Church. Sure Turing also cracked the German Enigma machine which may have tipped the balance in World War II, and then was horribly persecuted and prosecuted because of his sexual preferences, while Church lived in New Jersey and taught math. Fair enough. 

I joke of course, Turing and his accomplishments in describing both computation and a machine to do the computation is as fundamental as anything that Newton, Euler, or Einstein contributed to humanity, and Turing more than deserves all the praise and celebration we now heap upon him.

### Lambda Calculus & Natural Numbers
In order to be able to prove things about computability, the lambda calculus must be able to describe natural numbers, which play a key role in inductive proofs, which \[waves hands\] were needed to prove the facts of computability. Understanding how natural numbers work in Lambda Calculus, or for us in Haskell, will provide a great foundation for later explorations into inductive proofs and proof assistants. **Now** we'll get down to business.

### Natural Numbers in Haskell
This is the type of natural numbers we'll be using:
```haskell Natural Numbers in Haskell
-- | Data type for natural numbers
data Nat
  = Z         -- represents zero
  | S Nat     -- represents the successor of any natural number
```

That's all it takes to define the natural numbers, two data constructors. `Z` represents zero, and `S n`  where `n` is another `Nat`, is the _successor_ of `n`. For example:
```haskell Examples of natural numbers as Nat
Z                -- zero
S Z              -- one
S(S Z)           -- two
S(S(S Z))        -- three
```

Now we can write some functions on `Nat`. Let's start with a simple one - `isZero`, which should look familiar; it's logically equivalent to the `isEmpty` we wrote for our `Lst` type.
```haskell is a natural number zero?
-- | returns Yep (aka true) if a @Nat@ is zero, and Nope otherwise
isZero :: Nat -> Booly
isZero Z = Yep
isZero _ = Nope
```

How about adding two `Nat`s ?
```haskell adding two Nats
-- | Adds two natural numbers
add :: Nat -> Nat -> Nat
add  Z    n = n
add (S m) n = add m (S n)
```

The `add` function is (finally) an interesting one - it's a recursive function. It says:

* _if you add zero to any Nat `n`, you get back `n`_
* _if you add the successor `S` of any Nat `m` to a Nat `n`, that's equal to adding `m` to the successor `S` of `n`_

If the first number being added is zero, we're done. This is the base case of the recursive function. Every recursive function needs to have a base case to allow it to escape the recursion. When that first number is not zero, it must be the successor of some number. Strip the `S` off that first number and, to keep the equality, we move that `S` to the second number. That first number heads towards zero, and the second number increases. If we use this approach with normal digits we'd see this:
```text
   4    +  4
= (4-1) + (4+1)
=  3    +  5
= (3-1) + (5+1)
=  2    +  6
= (2-1) + (6+1)
=  1    +  7
= (1-1) + (7+1)
=  0    +  8
=          8
```

This is obviously not an efficient way to compute addition over natural numbers, but it is valid, and importantly, it **is** an effective way to _reason_ about numbers. To create a system of natural numbers from effectively nothing you must think about the symbols we use represent. The _digit_ 7 is just a shape, a glyph. There's no inherent meaning in that glyph. This representation is just as valid: `S(S(S(S(S(S(S Z))))))`.

Multiplication is similarly interesting:
```haskell multiplication of Nats
mul :: Nat -> Nat -> Nat
mul  Z    _ = Z
mul  _    Z = Z
mul (S Z) n = n
mul (S m) n = add n (mul m n)
```