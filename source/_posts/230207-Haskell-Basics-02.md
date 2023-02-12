---
title: Haskell Basics - Lists 01
id: haskell-basics-lists-01
tags:
  - haskell
  - basics
  - induction
  - data-structures
categories:
  - haskell
date: 2023-02-07 02:00:00
---

Lists are one of the fundamental data structures in Haskell and many other programming languages. [LISP](https://en.wikipedia.org/wiki/Lisp_(programming_language)) for example is a portmanteau of "LISt" and "Processing". The implementation of lists in Haskell is also a great way to look at a few different key features of the language that we'll discuss.

## No Implicit Prelude
Let's do some "first principals" type experimentation. We'll create a module and tell Haskell not to import `Prelude`, which is the standard library. This will allow us to create our own list types and functions without conflicting with the ones from the standard library. 

If you want to follow along, I suggest following these {% post_link 230207-GHCup 'installation instructions' %}. Then you can create a file with the `*.hs` extension.

```haskell
{-# LANGAUGE NoImplicitPrelude #-}

module List230207 where
```

The first line is a compiler directive that excludes Prelude from being automatically imported. The other line defines a module. You can name your module any legal module name; alpha-numeric starting with a capital letter.

# A List Type
We can define a list in Haskell like this (normally we'd use Prelude's list, this is illustrative only):

```haskell The Lst Type
{-# LANGUAGE NoImplicitPrelude #-}

module List230207 where

import Prelude (Show)

data Lst a
  = NIL
  | Cons a (Lst a)
  deriving (Show)
```

After excluding `Prelude` with the language extension, we add back in something called `Show`, which will make it easier to display values we're working with in the REPL; don't worry about the details for now.

## Algebraic Data Types
Let's break down the definition of the `Lst` type by first examining the `data` keyword, which indicates the beginning of a definition of a new _algebraic data type_. There are two kinds of algebraic data types - sum types and product types. What we've described above is a _sum type_. It's called sum because the total number of possible values is the sum of the number of values that each _data constructor_ can produce. Let's look at a simple example:

```haskell Example Algebraic data type
data Answer 
  = Yes 
  | No 
  | LeaningTowards Bool
  deriving (Show)
```

In this strange example, we have a data type `Answer` that has three data constructors. Two of them, `Yes` and `No`, take no parameters. The last one, `LeaningTowards` has a `Bool` parameter. This means the values, or terms, that inhabit the `Answer` data type are:
`Yes`,  `No`,  `LeaningTowards True`, and `LeaningTowards False`

The total number of values inhabiting `Answer` is four, which is the _sum_ of the number of terms that each data constructor can produce. `Yes` and `No` can only produce one value each since they have no parameters. Intuitively they behave like values and you'll soon see how in code they're used in places where values are used, but `LeaningTowards` has a parameter of type `Bool`, which defines two data constructors (`True` and `False`), bringing to total possible values of type `Answer` to four. Algebra! Sum!

The other type of algebraic data type is the _product_ type. An example is:
```haskell
data Quiz = Quiz Answer Answer
```

The `data` keyword is the same, indicating we're introducing a new (algebraic) type. The data type is `Quiz`, and it has one data constructor, also `Quiz`. Don't be confused by the fact that the type and the one data constructor are both defined as `Quiz`, they're used in different contexts, so once the idea of types vs type constructors clicks for you, this doesn't present a problem. These are 16 possible values for the type `Quiz`:

| | | | |
|-|-|-|-|
| `Quiz Yes Yes` | `Quiz Yes No` | `Quiz Yes (LeaningTowards True)` | `Quiz Yes (LeaningTowards False)` |
| `Quiz No Yes`  | `Quiz No No`  | `Quiz No (LeaningTowards True)`  | `Quiz No (LeaningTowards False)`  |
| `Quiz (LeaningTowards True) Yes`  | `Quiz (LeaningTowards True) No`  | `Quiz (LeaningTowards True) (LeaningTowards True)`  | `Quiz (LeaningTowards True) (LeaningTowards False)`  |
| `Quiz (LeaningTowards False) Yes`  | `Quiz (LeaningTowards False) No`  | `Quiz (LeaningTowards False) (LeaningTowards True)`  | `Quiz (LeaningTowards False) (LeaningTowards False)`  |

The one and only data constructor for `Quiz` has two parameters of type `Answer`, and we've seen that `Answer` has four possible values, therefore the product type `Quiz` has `4 * 4` or sixteen possible values. Algebra! Product!

## Kinds and Polymorphic (aka Generic) Types
The `Quiz` and `Answer` are monomorphic types because they contain no type parameters. We say the **kind** of `Quiz` and `Answer` is `Type`. This is best illustrated with an example of a simple _polymorphic_ data type:
```haskell the unbiquitous Maybe type
data Maybe a
  = Nothing
  | Just a
```

The `Maybe` type is omnipresent in functional programming languages, and some form of it is increasingly found in imperative languages as well. It's typically used to represent a value that may or may not exist. The type itself is parameterized - the `a` in the example is a **type parameter**. If the **kind** of `Quiz` is `Type`, what is the **kind** of `Maybe`? 

it's `Type -> Type`. 

Knowing what we know about Haskell functions, this seems like a function that takes a type and returns a type. That's pretty much what it is. We cannot specify terms to have a type `Maybe`, because `Maybe` isn't a `Type`! it's a `Type -> Type`, or a _higher kinded type_. To get a `Type` from `Maybe`, you must pass it a `Type`. For example:
```haskell student quiz results
s1 :: Maybe Quiz
s1 = Just (Quiz Yes No)

s2 :: Maybe Quiz
s2 = Nothing       -- didn't take the quiz

s3 :: Maybe Quiz
s3 = Just (Quiz No (LeaningTowards Yes))
```

The kind of `Maybe Quiz` is `Type` which means it can be used to specify the type of the terms `s1`, `s2`, and `s3` above.

We see `Maybe` data constructors `Nothing` and `Just` being used here. Either a student took the quiz, represented by `Just ...` or they didn't, represented by `Nothing`. 

## Back to Our List
```haskell
data Lst a
  = NIL
  | Cons a (Lst a)
deriving (Show)
```

Now that we understand algebraic data types and higher kinded types, we can see that `Lst`  is kind `Type -> Type`,  and `NIL` and `Cons Lst` are data constructors. So far so good.  `Lst` adds one more concept - a recursively defined, or _inductive_ construct. The first data constructor is `NIL`, which represents an empty list. The other data constructor, `Cons a (Lst a)` is recursive; it has two parameters, the first being `a` which represents the type of values the list can contain, and it the second parameter is a value of type `Lst a`, another list. Lets look at an example of using our `Lst` type.
```haskell Lst Example
module List230207 where

import Prelude (Show, Int)

data Lst a
  = NIL
  | Cons a (Lst a)
  deriving (Show)

nums :: Lst Int
nums = Cons 1 (Cons 2 (Cons 3 NIL))
```

In this example `nums` is a `Lst Int` , or a list of integers. I've included `Int` in the list of identifiers being imported from Prelude so we could specify the type parameter for `Lst`  to be specifically `Int`.  Noticed how we declare constants in a similar way that we declare functions, the type specification comes before the assignment (see this post on {% post_link 230206-Haskell-Basics-01 'functions' for more info%}). This is effectively how Haskell's standard library implements the list type, only since lists are so common and heavily used, there's some syntactic sugar in Haskell syntax that makes working with lists much more bearable and intuitive. We'll see these later, but our definition is definitely a valid way to represent lists.

So now that we've got a list defined ... we need to do something with it. Next time we'll work with our list type in creating some functions you typically use with lists - things like getting the length, appending one list to another, and mapping values in a list using a function to create a new list.

Happy Hacking!
