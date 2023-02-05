---
date: 2010-02-03 04:24:18
title: Roll Your Own Language Constructs
categories:
  - Imperative
  - C#
tag:
  - c-sharp
  - delegate
  - functional
  - lambda
---

My last couple of blog entries were ostensibly about lambda functions ([last one](http://blog.efvincent.com/c-nuts-and-bolts-lambdas/), [one before](http://blog.efvincent.com/c-nuts-and-bolts-lambdas02)). With these ideas, or a general comfort level with lambdas and the Action<> and Func<> delegate types in the back of your mind, consider this.

<!-- more -->

I was working on a project where the development team was instructed to implement a retry logic into our data access layer. In other words, if we experience database connectivity problems, retry the operation as seems reasonable.

Here’s some code we’re going to use as a basis for the conversation:

```C#
class Program {
    static void Main(string[] args) {

        try {
            Retry01();
        } catch (Exception ex) {
            Console.WriteLine(ex.Message);
        }
        Console.Write("Press any key...");
        Console.ReadKey(true);
    }

    static void Retry01() {
        // Iterate through the directories on the C:\ drive
        foreach (var dir in Directory.EnumerateDirectories("C:\\")) {
            OccasionallyThrow(0.1d);
            Console.WriteLine("dir: {0}", dir);
        }
    }

    static Random _rnd = new Random(DateTime.Now.Millisecond);
    /// <summary>
    /// Occasionally throw an exception
    /// </summary>
    /// <param name="freq">Percentage (as a fraction) of the time to
    /// throw an exception. Uses a random number generator, so
    /// this is not an exact measure</param>
    static void OccasionallyThrow(double freq) {
        if (_rnd.NextDouble() <= freq)
            throw new InvalidOperationException("An occasional exception");
    }
}
```

We’ve got a method, `OccasionallyThrow(freq)` that, occasionally throws an exception (duh). We’ve got a method that iterates through directories on the C:\ drive and pretty much can’t get through it without throwing.

So first pass, let’s fix `Retry01()` so it actually retries.

```C#
static void Retry01() {
    // Iterate through the directories on the C:\ drive
    foreach (var dir in Directory.EnumerateDirectories("C:\\")) {

        int retryCount = 0;
        bool done = false;
        do {
            try {

                OccasionallyThrow(0.25d);
                Console.WriteLine("dir: {0}", dir);

                done = true;

            } catch (Exception ex) {
                if (ex is InvalidOperationException) {
                    if (retryCount++ < 3) {
                        // can try again. Under the limit
                        Console.WriteLine("Caught exception, retry #{0}", retryCount);
                        done = false;
                    } else {
                        // retried too many times already
                        Console.WriteLine("Exceeded retry count. Throwing.");
                        throw;
                    }
                }
            }
        } while (!done);
    }
}
```

Yikes. That’s a bunch of code. Assuming we want to make this construct generally available, some refactoring is in order. Worse, the boilerplate code that manages retries _surrounds_ the code we want to apply it to, at lines 10 and 11 of a 30 line block of code. This is a similar problem to the one described in [my last post](http://blog.efvincent.com/c-nuts-and-bolts-lambdas02), so you may see where this is going. Here’s one approach to solving the problem.

```C#
static void Retry(Action action, int maxRetries = 3) {
    int retryCount = 0;
    bool done = false;
    do {
        try {
            action();
            done = true;
        } catch (Exception ex) {
            if (ex is InvalidOperationException) {
                if (retryCount++ < maxRetries) {
                    // can try again. Under the limit
                    Console.WriteLine("Caught exception, retry #{0}", retryCount);
                    done = false;
                } else {
                    // retried too many times already
                    Console.WriteLine("Exceeded retry count. Throwing.");
                    throw;
                }
            }
        }
    } while (!done);
}

static void Retry02() {
    foreach (var dir in Directory.EnumerateDirectories("C:\\")) {
        Retry(() => {
            OccasionallyThrow(0.25d);
            Console.WriteLine("dir: {0}", dir);
        }, 3);
    }
}
```

This approach solves many of the problems we've identified. The retry logic is encapsulated in a function that’s trivial to reuse. One point of interest here… if you’re not familiar with the syntax and techniques, it may look a bit strange. But it also should look a little familiar; consider the “using” block:

```C#
byte someByte = 100;
using (var stream = File.Open("C:\\data.dat", FileMode.CreateNew)) {
    for (byte i = 0; i < someByte; i++)
        stream.WriteByte(i);
}
```

The C# using block has a similar pattern. There’s some construct (using), and then a block of code to which the construct applies. It has a similar feel to our Retry() solution. Hmmm… could we use the Retry() approach to write our own using?

```C#
static void Using<T>(T disposableVar, Action<T> usingBlock) where T : IDisposable {
    try {
        usingBlock(disposableVar);
    } finally {
        disposableVar.Dispose();
    }
}

static void UsingMyUsing() {
    byte someByte = 100;

    Using(File.Open("C:\\data.dat", FileMode.CreateNew), stream => {
        for (byte i = 0; i < someByte; i++)
            stream.WriteByte(i);
    });
}
```

See that’s why I like coding so much… that’s definitely cool. It’s not exactly character for character like the using statement, but it’s pretty darn close (actually, mine is a couple of characters shorter). And it’s equivalent (at least it is superficially, I haven’t checked thoroughly, it’s almost 1am after all). It had to get a little fancier with the generics, but we’ve recreated a C# language construct.

This article is long enough, and I’m sure you get the picture. To me, this is a great example of how functional thinking can lead to interesting, robust solutions to problems. Let me know if you’ve got any questions or comments. Until then, have fun!
