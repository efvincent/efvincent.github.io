---
date: 2010-02-01 19:19:41
title: "C# Nuts and Bolts: Lambdas"
categories:
  - Imperative
  - C#
tags:
  - c-sharp
  - delegate
  - functional
  - lambda
---

You don’t need to use Lambdas to make a living writing code in .NET. You can probably get away with not even knowing what Linq is. You can also still find work coding in .NET 2.0. But that’s painfully BORING. So to avoid the stinging monotony of working in a 5 year reverse time-shift, let’s take a look at lambdas.

<!-- more -->

The (arguably) simplest lambda.

```C#
string message = "Hello World";
Action simpleLambda = () => Console.WriteLine(message);

Console.WriteLine(“Get ready for a message!”);
simpleLambda();
Console.WriteLine(“That was the message!”);
```

The the type “Action” is a delegate, which can be thought of as a function pointer. The variable “simpleLambda” is now a function. The same way that “message” is a string. In reality, there are delegates and pointers involved, but for the day to day, just think of “simpleLambda” as a function. It can be called just like you would any function, which is what we’ve done here.

There are two different Types that are generally used for these functions. They are “Action” and “Func”. Action, which we just saw, is a function that doesn’t return anything. It’s a void function. When we want to define a function that returns something, we use the “Func” type. Remember back to your early days of Visual Basic, Pascal, etc., when there were different keywords that you’d use to specify if there was a return value or not? Same kinda thing here. Func returns, Action doesn’t. So here’s a Func example.

```C#
// Define a func that returns something

Func<string> minutesAndSeconds = () => string.Format("{0:mm:ss.ffff}", DateTime.Now);

Console.Write("Current Minutes and seconds are: ");
Console.WriteLine(minutesAndSeconds());
```

Here’s a function that returns a string. It’s type is Func<string>, which you can read as a function that returns a string. Let’s break down the syntax of the lambda itself. It centers around the lambda operator “=>”, which some people read as “goes to”. In both our examples, simpleLambda and minutesAndSeconds are functions that take no parameters. So on the left of the lambda operator are empty parenthesis. Means no parameters to the function. Easy. On the right is the body of the function.

There are two ways of expressing the body of the function. If the body is a single statement, you can just put that statement. This is called an Expression Lambda. If you need more than one statement on the right of the Lambda operator, you end up with something like this:

```C#
Func<string, string> hasher =
    s => {
        var sha = new System.Security.Cryptography.SHA1CryptoServiceProvider();
        return Convert.ToBase64String(sha.ComputeHash(ASCIIEncoding.UTF8.GetBytes(s)));
    };

Console.WriteLine("The base64 encoded SHA1 hash of \"Hello World\" is {0}", hasher("Hello World"));
```

A couple of differences here… first there’s some added whitespace to make stuff more readable. Don’t let it throw you. Whitespace == good. Second, the type is now Func<string, string>. The key to reading the Func type is that the last generic type specification is the return type. So it reads “a function that takes a string and returns a string”. Since this lambda takes a string as a parameter, to the left of the lambda operator goes the parameter. You can name this parameter anything you want, just like when you’re defining a regular function. This parameter is bound to the input to the function.

The next thing that’s different is there are open and close curly brackets, just like a function definition, after the lambda operator. This is the body, it’s just like a function body. You declare local variables in here, and when you’re done, you return something. Or if this is an Action instead of a Func, you don’t need to return anything (remember, Action is like a void function). This is the same function except as a statically defined method on a class:

```C#
public static string Hasher(string s) {
    var sha = new System.Security.Cryptography.SHA1CryptoServiceProvider();
    return Convert.ToBase64String(sha.ComputeHash(ASCIIEncoding.UTF8.GetBytes(s)));
}
```

Thaaaaatts pretty much it. There’re plenty of implementation details under the covers, and there’s a whole world of design patterns and techniques to explore with regards to _using_ lambdas. But as far as what they are, you’re looking at it. I’ll push it a little further in the next blog post. If you’ve got any questions or want to dive deeper into this or any other subject I write about, let me know in the comments. In the mean time, have fun!
