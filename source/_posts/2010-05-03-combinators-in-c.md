---
date: 2010-05-03 05:15:43
title: Combinators in C#
categories:
  - Imperative
  - C#
tag:
  - c-sharp
  - closure
  - Combinator
  - delegate
  - functional
  - lambda
---

As C# has evolved it has acquired more and more of what some people refer to has functional programming features and constructs. One such concept is the idea that a function is a "first class” value. This is a fancy way of saying that functions are values that can be passed to and returned from other functions. A function that operates on other functions is called a high-order function.

<!-- more -->

Combinators are high order functions that compose, combine, or otherwise modify functions in useful and interesting ways. These types of operations are not typically seen in C#, but expanding your problem solving toolkit to include these concepts is not only fun and interesting, but can result in new, efficient, robust solutions.

## The Timer

Imagine you’ve got a method that you want to put a stopwatch on. For example, in the following code, you need to see how long it’s taking each download to complete, for debugging information.

```java
class Program {

    static string url;

    static void Main(string[] args) {

        url = "http://microsoft.com";
        RetrieveFromWeb();

        url = "http://amazon.com";
        RetrieveFromWeb();

        url = "http://dpreview.com";
        RetrieveFromWeb();

        Console.Write("Press any key...");
        Console.ReadKey(true);
    }

    static void RetrieveFromWeb() {
        System.Net.WebClient wc = new System.Net.WebClient();
        var s = wc.DownloadString(url);
        Console.WriteLine("URL: {0} - string length is {1}", url, s.Length);
    }
}
```

An ugly solution would be to do this.

```java
static void Main(string[] args) {
    var sw = new System.Diagnostics.Stopwatch();

    sw.Start();
    url = "http://microsoft.com";
    RetrieveFromWeb();
    Console.WriteLine("{0:#,##0}ms", sw.ElapsedMilliseconds);

    sw.Restart();
    url = "http://amazon.com";
    RetrieveFromWeb();
    Console.WriteLine("{0:#,##0}ms", sw.ElapsedMilliseconds);

    sw.Restart();
    url = "http://dpreview.com";
    RetrieveFromWeb();
    Console.WriteLine("{0:#,##0}ms", sw.ElapsedMilliseconds);

    Console.Write("Press any key...");
    Console.ReadKey(true);
}
```

Try not to get physically ill. You and I both know there’s plenty of code running around in the wild that looks a lot like this. Red flags – you see repeated code – starting, stopping, printing the time. Should we go into the RetrieveFromWeb() method and modify it for timing? Let’s not. Lets try this instead. Step one, let’s define a variable for the method we’re calling. Leaving out the timing code for now, we make this change:

```java
static void Main(string[] args) {</p><p>    // Create a delegate, set it to the method of interest</p><p>    Action retrieveFromWeb = RetrieveFromWeb;

    url = "http://microsoft.com";
    retrieveFromWeb();

    url = "http://amazon.com";
    retrieveFromWeb();

    url = "http://dpreview.com";
    retrieveFromWeb();

    Console.Write("Press any key...");
    Console.ReadKey(true);
}
```

I’ve added a local variable of type _Action, \_and set it equal to the call to the method \_RetrieveFromWeb(),_ and now we’re calling that method indirectly. It has the same exact effect, the method gets called three times. Only we’ve added a layer of indirection. We do this all the time in OO programming; for example, you might have a Person object, but rather than coding directly to that object, you create an IPerson interface, and code to that, opening up the possibility of mocking the object, decorating it, etc. Similar thing here. Rather than “binding” the call sites directly to the method, we’re binding to a variable that points to the method.

We’re doing this because now we’ve got a value that can be altered or augmented to add additional functionality. This is where a combinator comes in. This is a simple timer combinator:

```java
class Combinators {

    public static Action Time(Action a) {
        return () => {
            var sw = new System.Diagnostics.Stopwatch();
            sw.Start();
            try {
                a();
            } finally {
                Console.WriteLine("{0:#,##0}ms", sw.ElapsedMilliseconds);
            }
        };
    }
}
```

This combinator takes an _Action_ and returns a new action (aka a new function), that has timing included. At line 8 the parameter action is being called, the timing is what’s added. The only change we have to make to the main method is where the delegate is being defined:

```java
Action retrieveFromWeb = Combinators.Time(RetrieveFromWeb);</p>
```

The rest of the method stays the same, but now, there’s timing added. This is a super-simple, contrived example, but you should be starting to see what’s possible. Let’s take this a step further. Here’s an example simulating retrieval of information from the database:

```java
static void Main(string[] args) {

    Func<string, Guid> lookupUser = LookupUser;

    var emails = new[] {
        "eric@work.com", "joel@office.com", "cole@school.com",
        "karin@job.com", "haley@home.com" };

    foreach (var em in emails) {
        var id = lookupUser(em);
        Console.WriteLine("user {0} id = {1}", em, id);
    }

    Console.Write("Press any key...");
    Console.ReadKey(true);
}

static Guid LookupUser(string email) {
    // Fake looking up a user in the database
    var rnd = new Random(System.DateTime.Now.Millisecond);
    Thread.Sleep(rnd.Next(250, 2500));
    return Guid.NewGuid();
}
```

Memoization is the idea that a function can remember the results for given parameters. It’s like caching, but the term is specific to caching function results based on input. Here’s a simple Memoization combinator for C#:

```java
public static Func<A, B> Memoize<A, B>(Func<A, B> fn) {
    var dict = new Dictionary<A, B>();
    return a => {
        B b = default(B);
        if (!dict.TryGetValue(a, out b)) {
            b = fn(a);
            dict.Add(a, b);
        }
        return b;
    };
}
```

It’s simple, but it demonstrates some very useful and interesting functional programming concepts. First, it’s takes and returns Func<A,B>. This is a delegate with a parameter of type A that returns a type B. This will work for effectively any method with that signature. Next point of interest, a dictionary is created, then the lambda is created and returned. The lambda refers to the dictionary defined outside the lambda. It is said that the dictionary is _captured_ in a _closure_. It’s not important that you remember the terms, but look over the code and see if the concept is clicking for you. This function will return (effectively) a function, the dictionary is _captured_ by that function. Even when the call to Memoize() goes out of scope, the dict variable will still exist in the returned function. Enough talk. We modify the main program just slightly:

```java
Func<string, Guid> lookupUser = Combinators.Memoize(LookupUser);
```

The Memoize function will create a new function, one that caches results of the LookupUser function _automatically._ Nothing else has to change in the program to take advantage of this. Want to be sure that it’s actually working? Time it! The non-memoized LookupUser() has a built in Thread.Sleep(2500), and so takes 2.5sec \* number of lookups to run. The memoized version will run almost instantly, so we can prove the memoizer is working by timing. The time combinator we created earlier was for timing Actions. I’ve created a overload of the time combinator that has the signature we need - Func<A,B>:

```java
public static Func<A, B> Time<A, B>(Func<A, B> fn) {
    return a => {
        var sw = new System.Diagnostics.Stopwatch();
        sw.Start();
        try {
            return fn(a);
        } finally {
            Console.WriteLine("{0:#,##0}ms", sw.ElapsedMilliseconds);
        }
    };
}
```

and added it to the definition of lookUpUser in the main function

```java
Func<string, Guid> lookupUser =
  Combinators.Time(
      Combinators.Memoize<string,Guid>(LookupUser));</p>
```

Again without modifying the main code, we’ve augmented the method. Throw in some duplicate email addresses to test it out. The real power of these techniques becomes more evident the more you use them. Functional programming developers have been using these are similar techniques for years, and now with the added ability and flexibility of C#, we can employ these patters as well.
