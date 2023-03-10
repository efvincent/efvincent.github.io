---
title : Haskell Basics - Lists & Functions 02
id: haskel-basics-lsts-&-funcs
tags:
	- haskell
	- basics
	- functions
categories:
	- haskell
date: 2023-02-11 05:00:00
---

Previously:

- {% post_link 230206-Haskell-Basics-01 %}
- {% post_link 230207-Haskell-Basics-02 %}

In recent posts, we've discussed {% post_link 230207-Haskell-Basics-02 'a list data type' %} and the {% post_link 230206-Haskell-Basics-01 'basics of function types' %}. Here's the list that we came up with last time (I've updated the module name for this post, but otherwise it's the same as last time).
```haskell Lst Example
{-# LANGUAGE NoImplicitPrelude #-}
module List230210 where

import Prelude (Show)

data Lst a
  = NIL
  | Cons a (Lst a)
  deriving (Show)
```

# Functions on our List Type
Now we'll combine topics of those two posts and write some of the common functions found that operate on lists, and in doing so, we'll come across several other important concepts in Haskell and in functional programming in general.

## Is the List Empty?
Determining whether or not  a list is empty is perhaps the simplest possible list function. When programming in Haskell one approach is to write the signature of the function. Believe it or not, in many simple cases, there's only one way to write a function for a given signature. 
``` haskell Signature of isEmpty
isEmpty :: Lst a -> Bool
```

We're stating that the variable `isEmpty` has the type: "a function from of  `Lst a`  to `Bool`". In Haskell, functions are **pure**. This means that functions behave more like mathematical functions than the functions we're used to in other programming languages. The function `isEmpty` can only use/refer to values passed to it as parameters and in its _context_, which means any variables or modules that are in scope. In this case, there's nothing in our module but the `isEmpty` function, so it has no other information other than the parameter of type `Lst a`. No other information is available to the function. Let's look at an implementation.
```haskell isEmpty implementation
{-# LANGUAGE NoImplicitPrelude #-}
module List230210 where

import Prelude (Show)

-- | A datatype for lists of values
data Lst a
  = NIL
  | Cons a (Lst a)
  deriving (Show)

-- | A custom implementation of a boolean like type
data Booly
  = Yep
  | Nope
  deriving (Show)

-- | Return @Yep@ if a @Lst@ is empty, and @Nope@ otherwise
isEmpty :: Lst a -> Booly
isEmpty NIL = Yep
isEmpty _   = Nope
```

There are several new elements here to look at.  Normally everything in `Prelude` is imported automatically, but if you recall we've explicitly excluded `Prelude` with our `LANGUAGE` directive at the top of the module, so we can redefine types and functions related to lists in this exercise. So we need to implement our own boolean type. The "official" boolean data type in Haskell is `Bool`, and our `Booly` data type is defined the same way. We've named ours differently to make it obvious we've defined our own two valued type, but that's all a boolean type is, a data type with two values, and we intuitively assign _semantics_ of truth-hood and false-hood to the two data constructors `Yep` or `True`, and `Nope` or `False`. 

### Pattern Matching

Finally we get to the definition of the `isEmpty`, and we see one of the key concepts in Haskell that looks very strange to those familiar with mainstream languages: pattern matching. There are two definitions of `isEmpty`. In the first, in the position of the parameter, we have the _data constructor_ `NIL` from the definition of `Lst`, not a variable like we saw in the first function post, `mathy`. This causes Haskell to _pattern match_ against the incoming value. If the incoming parameter has the value `NIL`, the expression evaluates to `Yep`. 

In the second definition, there's an underscore in the parameter position. This indicates that Haskell should ignore the value in that position, any value will match the underscore. This means that if the first pattern doesn't match a `NIL`, the second pattern will match any value. For the `isEmpty` function, this is what we need. If we match `NIL`, then `Yep` the list is empty. If we match _anything else_, then `Nope` the list is not empty. That's the complete definition of `isEmpty`!

### The Type (almost) Defines the Function
Earlier it was stated that often, especially for simple functions, there's only one way to write a function given its type. `isEmpty` is such a case. What else can we possible do with a function of type `Lst a -> Booly` ? There are only two possible outputs - `Yep` and `Nope`. Since the signature specifies a `Lst` of `a` values, but it doesn't say what `a` should be, it could be anything. This is a polymorphic function with no constraints on the type variable `a`. Since we don't know what `a`  is, we can't know any function that works on `a` or any of the data constructors that might be part of the definition of `a`. So in effect, the `a` tells us nothing. All we know about the parameter comes from `Lst`, and it says the value might be `NIL`, or it might be `Cons a (Lst a)`. 

There are only four ways to define this function:

```haskell Four possible ways to define iEmpty
-- | the right way
isEmpty :: Lst a -> Booly
isEmpty NIL = Yep
isEmpty _   = Nope

-- | not the right way
isEmpty2 :: Lst a -> Booly
isEmpty NIL = Nope
isEmpty _   = Yep

-- | cmon now
isEmpty3 :: Lst a -> Booly
isEmpty3 _  = Yep 

-- | ok we've stopped trying
isEmpty4 :: Lst a -> Booly
isEmpty4 _  = Nope

```

The first has the semantics we're looking for - it's consistent with the human language we're using. The second is syntactically correct, but has the wrong semantics; it has what we sometimes call a logic error. The last two are _constant functions_. They ignore their argument and always return the same value, clearly not what we're looking for. So there's only really one way to write this function! 

🤔 ... if you're thinking there's something to the fact that there are only 4 ways to write this function, congratulations, you're paying attention. This should remind you of the discussion of algebraic types from the {% post_link 230207-Haskell-Basics-02 'post on lists' %}, where we said that for product types, the number of possible values for a type is the product of the number of values in each of the parameterized types. Recall that the type `Quiz Answer Answer` has 16 different possible values, since `Answer` had four possible values.

Our `Booly` type has two possible values. But our `Lst` type is recursive, and you can see how it might have an infinite number of values. So `2 * Infinity == Infinity`, not four. **But**, since we conceptually only care about the two different data constructors for `Lst`, `NIL` and `Cons...`, conceptually there are only two values to `Lst`, giving `2 * 2 == 4`, but where's the product algebraic type?  

Recall again, this time from the {% post_link 230206-Haskell-Basics-01 'post on types' %}, that the arrow `->` we've been using for function definitions is a polymorphic type constructor over two types. This is easier to see when we alias the arrow and use it in a prefix way (rather than infix). That gives us this:
```haskell alias the arrow type
-- @type@ keyword creates an alias for a data type
type Func = (->)

-- | Return @Yep@ if a @Lst@ is empty, and @Nope@ otherwise, now
--   with a type signature using the @Func@ alias for @(->)@
isEmpty :: Func (Lst a) Booly 
isEmpty NIL = Yep
isEmpty _   = Nope
```

Ah - now we see that the arrow is a type, and the two type parameters that make it a product type are the input type and the return type.If we borrow the notation `|T|` from set theory to mean the cardinality (number of values) that inhabit a type, then we see that:

```
|T a b| == |a| * |b|
```

Where `T` is a polymorphic type over `a` and `b` which are type variables, and in the case of `isEmpty` we have
```
|Func (Lst a) Booly| == |Lst a| * |Booly| == 2 * 2 == 4 
```

If we allow that we're only considering two semantically relevant values populating `Lst`.

#### So what?
This is not going to help you configure your Kubernetes cluster or get all the text boxes in your div to line up correctly. Understanding things at this level is the starting point, the tip of the iceberg, to understanding the incredible relationship between computer science, logic, and abstract mathematics. Haskell is a great tool for this exploration, an exploration on which I myself have taken only a few steps. My writing these posts helps me continue to look closely at these topics, continue to have new insights, and solidify concepts.

Have you ever heard someone say that functional programs are "easier to reason about"? Most of the people who I've heard say this love functional programming, and know that there's _something_ to it that is intuitive, that stimulates their need for things to be ordered and logical. But then there are those who have really studied and gained an understanding of abstract computer science, type theory, programming language theory, set theory, category theory, etc. or some combination of those. They think of reasoning differently. 

{% blockquote Oxford English Dictionary %}
**reasoning (noun)**: ​
the process of thinking about things in a logical way; opinions and ideas that are based on logical thinking
{% endblockquote %}

_...based on logical thinking._ That's the key. That's what's out there to be learned.

Happy Hacking!

