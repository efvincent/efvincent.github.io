---
date: 2009-08-25 03:32
title: "C# Nuts and Bolts: Delegates and Functions, Part I"
categories:
  - Imperative
  - C#
tags:
  - c-sharp
  - intPtr
  - multicastDelegate
  - reflector
  - delegate
  - disassemble
---

Two years ago I took up the [OCaml](http://en.wikipedia.org/wiki/OCaml) programming language, purely out of intellectual curiosity. For me, [OCaml](http://en.wikipedia.org/wiki/OCaml) was a gateway drug to functional programming in general. I’ve since spent a good bit of time with [F#](http://msdn.microsoft.com/en-us/fsharp/default.aspx) and [Haskell](<http://en.wikipedia.org/wiki/Haskell_(programming_language)>). With the consulting work I do I don’t have the opportunity to use these languages in production, but I have found that using these languages has unlocked a new worlds of problem solving techniques.

This series lays down a groundwork of C# understanding upon which we can build a library of modern problem solving techniques, where we take the most useful functional approaches, and see how to apply them to C# programming in .NET. Even if you’ve not any interest in functional programming, this series can be of service helping you understand some of the less obvious aspects of C# programming.

<!-- more -->

## Delegates

C++ has pointers to functions. C# has delegates. A [delegate](<http://msdn.microsoft.com/en-us/library/ms173171(VS.80).aspx>) is a .NET type that can be thought of as a type safe function pointer. A function pointer is a variable that instead of pointing to or containing data, points to a function. This variable can be used to invoke, or call, the function it is pointing at.

_Type safe_ refers to the fact that a delegate variable cannot point to just any function, it can only point to a function that matches the delegate's _signature_. Signature refers to the parameters and return value of the function. Together, the parameters and return type form a fingerprint for a function which must match that the delegate.

In this example, a delegate is created at line 5 called `GetStatsDelegate `where the signature specifies a single `string` parameter and an `integer` return value. Judging from the name, the delegate should point to a function that gets some stats about a string.

```C#
class Program
{
    // define a delegate type for a function that takes a
    // string and returns an int
    delegate int GetStatsDelegate(string str);

    public static void main() {

        // Define a variable of our new delegate type, and
        // assign it a value. Any method with the same signature
        // can be assigned to this variable.
        GetStatsDelegate myFunc = GetLengthWithoutSpaces;

        // Both these print 0
        Console.WriteLine(myFunc("Hello"));
        Console.WriteLine(GetLengthWithoutSpaces("Hello"));
    }

    // This is a function that we can point to, because it has
    // the right signature for our delegate
    public static int GetLengthWithoutSpaces(string s) {
        return 0;
    }
}
```

At line 12 the delegate we created (which is a type) is used to define a variable, `myFunc`. We then set `myFunc’s` value equal to `GetLengthWithoutSpaces`, which is a function. Lines 15 and 16 call the function, once using the delegate, and once the traditional way.[![image](http://blog.efvincent.com/wp-content/uploads/2009/08/image_thumb.png)](http://blog.efvincent.com/wp-content/uploads/2009/08/image.png)

Here’s where it starts to get interesting. This is a snip from [Reflector](http://www.red-gate.com/products/reflector/), looking at how `GetStatsDelegate` is defined.  We defined it as a delegate, but the compiler has built what looks like a standard class which inherits from the MulticastDelegate framework class.

This is good to know. C# treats delegates in a special way. They’ve been granted their own keyword and a special unique way of declaration. The declaration declares a subclass for us, saving us the trouble of building out a subclass ourselves. If we use Reflector to dig into the base class (System.Delegate), we see these properties and fields:

```C#
// *** From Delegate ***

// Properties
public MethodInfo Method { get; }
public object Target { get; }

// Fields
internal MethodBase _methodBase;
internal IntPtr _methodPtr;
internal IntPtr _methodPtrAux;
internal object _target;

// *** From MulticastDelegate ***

// Fields
private IntPtr _invocationCount;
private object _invocationList;
```

These fields tell us a few things. An instance of a delegate has reflection information about the method it points to, this is what the `MethodInfo` public property. The pointer to the function itself is of type `IntPtr`, which we can think about as a pointer to memory.

The `_target` field is important too. In the case of a delegate that points to an instance method (ie a non-static method on a non-null object), the `_target` field points to the object with the method. This is important because it is not obvious from looking at code that a delegate holds a reference to the object with the method. It’s possible for this to be the source of memory leaks, where objects cannot be garbage collected because a delegate is holding a reference. You see this most often in events. Being delegates, events hold references that need to be _unregistered_ in order to release these references.

Also we have something called `_invocationList`, which if we dig through the disassembly we come to find is an array of Delegates implemented as a linked list using pointers (`IntPtr`) directly for performance reasons. This means that `MulticastDelegates` are delegates that have a chain of additional delegates. This is used heavily in .NET’s event mechanism, where an _event_ can have multiple _handlers_. The handlers are simply functions wrapped in delegates added to the linked list of a the event `MulticastDelegate`.

Next post we’ll look at using delegates, not as events (which anyone who has ever dropped a button on a form knows about), but as function pointers and objects, and start looking at some novel approaches to solving common problems.

_As always if you’ve got questions or topics you’d like me to follow up on, please leave a not in the comments!_
