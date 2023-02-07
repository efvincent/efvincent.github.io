---
title: Haskell Basics - Functions 01
categories:
  - haskell
  - functional
  - basics
tags:
date: 2023-02-06
---
The syntax for function definition in Haskell is different from most other programming languages, especially mainstream programming languages. Let's take a look at a simple function.

```haskell
-- | Perform math operation on two integers
mathy :: Int -> Int -> Int
mathy m n = (m * 10) + n
```

If you're new to Haskell this will look very strange. The first thing to notice is that the first line (after the comment) is the type declaration for the function. A variable `mathy` is going to be bound to the definition of this function. In Haskell once a variable is bound it cannot be changed. In Haskell values are immutable and once a variable is bound to a variable, including functions, they cannot be altered.

The double colon `::` is how we define _type signatures_, so the first line says that `mathy` has the type `Int -> Int -> Int`, which we can think of as the type signature of a function. in this case there are 3 parts separated by right arrows `->`. Using conventional terminology, you might say that each part is a parameter of the function except the last, which is the return value of the function. Therefore we can say that `mathy` takes two `Int` parameters and returns an `Int` value.

The next line is the definition. The variable bound to the function is separated by its parameters by whitespace. `m` and `n` are the parameters of the function. The "body" of the function is the expression after the equal sign, `(m * 10) + n`.

## Calling Functions
Once the function is defined it can be called with parameters to return a value. 
```haskell
> mathy 21 5
215

> mathy 8 8
88
```

## Partial Application
Aside from the different syntax we've just seen, the first real big significant difference between Haskell and most other languages is that the arguments are simply listed after the function name; no parentheses or commas.

More significantly, we can call a function without passing _all the arguments_ to the function. Let's take a look at the function, it's type, and the types of values that come from calling the function in different ways. Note the command `:type` at the REPL returns the type (it can be abbreviated `:t`.

```haskell
> -- the type of the function bound to mathy
> :t mathy
mathy :: Int -> Int -> Int

> -- callng with one parameter
> :t mathy 8
mathy 8 :: Int -> Int

> -- calling the all parameters
> :t mathy 8 4
mathy 8 4 :: Int
```

You should be seeing the pattern here. It's clear if we add one bit of knowledge to our burgeoning knowledge of Haskell... the arrow type `->`. You may have thought the arrow was just a bit of syntax, but in Haskell it's a type just like `Boolean` or `Int`.  It's very similar conceptually to C#'s `Func<T,TResult>` type. It's a polymorphic type that takes two type arguments, and describes a computation that has one input type and one output type.

But wait ... we just said `mathy` takes two arguments, no? Well, not really. In the above example, we see that `mathy` has type signature `Int -> Int -> Int`. But remember, the arrow type describes a function from one value to another value. We can add parenthesis to make it clearer: `mathy :: Int -> (Int -> Int)`.  The parenthesis show how the arrow type is an _infix type constructor_ that binds to the right. Just like the C# type `Func<T,TResult>` that describes a function delegate has two type parameters `T` and `TResult`, we now see that the arrow type in Haskell `->` is a polymorphic type in two type parameters, the first is the input to the function, and the second is the output. 

Now we can see that `mathy` is actually a function that takes an integer and produces a function that takes an integer and produces an integer. We can do this:

```Haskell
> mathyOfEight = mathy 8
mathyOfEight :: Int -> Int

> mathyOfEight 5
85

> mathy 8 5 == mathyOfEight 5
True
```

We've seen that in Haskell a function is defined using a type signature, and a function body. We've seen the Haskell arrow `->` type, which is the type of function. The next post will get into a bit more of the syntax of functions in Haskell.