---
date: 2010-02-01 03:12:13
title: "Applied Functional: Getting User Input"
categories:
  - Imperative
  - C#
tag:
  - c-sharp
  - functional
---

The thing with spending time on functional programming is that the typical .NET programmer will not likely be able to write production code in F#, and almost certainly not in Haskell. But what you are able to do is apply a new set of problem solving approaches to your every day work.

<!-- more -->

So here’s a simple problem that we see all the time; you’re writing a console application to test a component. Here’s the pseudo code:

```C#
/// While still processing
///     Get a string from the user
///     Process the string
/// End While
```

Super duper simple. So here’s a straight forward way to code it:

```C#
static void Main(string[] args) {
    NonReusableLoop();
}

static void NonReusableLoop() {
    string input = string.Empty;
    bool done = false;
    do {
        Console.Write("Enter a web site to process. Press enter to end: ");
        input = Console.ReadLine();
        if (string.IsNullOrEmpty(input)) {
            done = true;
        } else {
            ProcessWebSite(input, true);    // don't really care about second parameter
        }
    } while (!done);
}

static void ProcessWebSite(string webSite, bool someOtherParameter) {
    // Do some fake work
    if (!webSite.StartsWith("http://", StringComparison.OrdinalIgnoreCase)) {
        throw new InvalidOperationException("Poorly formed web site name");
    }
    Console.WriteLine("Processing web site {0}", webSite);
    var rnd = new Random(DateTime.Now.Millisecond);
    Console.WriteLine("Web site has {0:#,##0} pages!", rnd.Next(1, 1000));
}
```

So lets say we want to extract something reusable out of this. Lets say we regularly churn up little console apps and we’re tired of writing this over and over again.

There are a couple of approaches that seem obvious you if you’ve got your functional thinking cap on. For example, this first pass looks like a function that does the interesting work (calls ProcessWebSite()) in the middle of a bunch of other stuff (getting user input, looping, checking for the quit condition). Let’s go with that observation:

```C#
static void Main(string[] args) {
    ProcessUserInput("Enter a web site url", input => ProcessWebSite(input, true));
}

static void ProcessUserInput(string prompt, Action<string> fn) {
    if (fn == null) return;
    string input = string.Empty;
    bool done = false;
    do {
        Console.Write("{0}. Press enter to end: ", prompt);
        input = Console.ReadLine();
        if (string.IsNullOrEmpty(input)) {
            done = true;
        } else {
            try {
                fn(input);
            } catch (Exception ex) {
                Console.WriteLine("Exception thrown: {0}", ex.Message);
                Console.Write("Show exception stack? (y/N):");
                var exInput = Console.ReadLine();
                if (exInput.Equals("y", StringComparison.OrdinalIgnoreCase)) {
                    Console.WriteLine(ex.ToString());
                }
            }
        }
    } while (!done);
}
```

Now we’ve got a function that does the collection of the user strings, checking for the quit condition, and even adds some simple error handling to catch any exceptions that the “work” might throw. The parameter is an “Action<string>”. This is the .NET way of describing a function that returns void and has a single string parameter. This is a functional way of looking at the problem, because in functional programming passing functions around and working on functions is a fundamental technique. Getting used to doing this is a powerful tool.

But there’s a better way of looking at it. What if we think of the strings the user is entering as a set of strings. We end up with something like this:

```C#
static void Main(string[] args) {
    ProcessStrings(
        SetOfUserInput("Enter a web site url"),
        str => ProcessWebSite(str, true));
}

static void ProcessStrings(IEnumerable<string> setOfStrings, Action<string> fn) {
    foreach (var item in setOfStrings) fn(item);
}

static IEnumerable<string> SetOfUserInput(string prompt) {
    string input = string.Empty;
    bool done = false;
    do {
        Console.Write("{0}. Press enter to end: ", prompt);
        input = Console.ReadLine();
        if (string.IsNullOrEmpty(input)) {
            done = true;
        } else {
            yield return input;
        }
    } while (!done);
}
```

Now we’ve got a function that returns a set of strings (IEnumerable<string> is the .NET way of saying a set of strings), and a function that takes a set of strings and applies a function to each member. The yield keyword makes the SetOfUserInput() method into a set. Each time the yield return keyword is hit, the value is passed out to the caller. When the caller attempts to retrieve the next value from the set, control is passed to the line after the yield return. This construct is called a continuation, and it’s a powerful concept.

These two simple concepts, that functions can be passed around and worked on, and that you can generate sets of data using continuations, are simple and fundamental to functional programming. But as these examples show you can use these techniques to your advantage in C#, solving real, every day problems.
