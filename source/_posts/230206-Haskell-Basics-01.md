---
title: Haskell Basics - Functions 01
id: haskell-basics-functions-01
categories:
  - haskell
tags:
  - basics
  - functions
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

## Arrow - The Type of Functions
Aside from the different syntax we've just seen, the first real big significant difference between Haskell and most other languages is that the arguments are simply listed after the function name; no parentheses or commas.

### Partial Application
More significantly, we can call a function without passing _all the arguments_ to the function. Let's take a look at the function, it's type, and the types of values that come from calling the function in different ways. Note the command `:type` at the REPL returns the type (it can be abbreviated `:t`).

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

You should be seeing the pattern here. Arrow `->` is actually a polymorphic type constructor, in some other languages called a generic type. Specifically it's a generic type in two type variables, the input (on the left of the arrow) and the output (on the right of the arrow). In C#, the type of anonymous functions is `Func<T,TReturn>`, a polymorphic type in two type variables, the first `T` is the type of input, and the second `TReturn` is the type of output. Java has a similar generic type for anonymous functions.

### Infix
Haskell has the notion of _infix_ operators, and that's what `->` is, an _**infix**_ *polymorphic type constructor*. Functions and type constructors are typically post fix; the arguments go after the function name. Infix works like the plus sign, which represents the addition function. The plus sits between the two numbers being added. Haskell lets us define functions, and type constructors, as infix.

We could define our `mathy` function that's infix:
```haskell
-- | An infix version of mathy
(++++) :: Int -> Int -> Int
(++++) m n = (m * 10) + n
```

Then it could be called infix style, like you would addition:
```haskell
> 8 ++++ 5
85
```

You can also call non symbolic functions in an infix style by surrounding it with back tick characters.
```haskell
-- infix calling of mathy
> 8 `mathy` 5
85
```

We can see that `->` is a type constructor by assigning an alias that's not symbolic. We'll use the same name as the C# type for anonymous functions. 
```haskell
-- | create a type that's an alias for arrow
type Func = (->)

-- | using Func to define a function. It has the same type
--   as mathy, so we can assign it directly
mathy2 :: Func Int (Func Int Int)
mathy2 = mathy
```

It's now even more clear that `mathy2`, like `mathy` is actually a function that takes an integer and produces a function that takes an integer and produces an integer. We can do this:

```Haskell
> mathyOfEight = mathy 8
mathyOfEight :: Int -> Int

> mathyOfEight 5
85

> mathy 8 5 == mathyOfEight 5
True
```

We've seen that in Haskell a function is defined using a type signature, and a function body. We've seen the Haskell arrow `->` type, which is the type of functions, and we've seen how we can partially apply functions to create new functions with fewer arguments. A future post will get into a bit more of the syntax of functions in Haskell.