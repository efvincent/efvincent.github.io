---
date: 2010-02-02 18:51:56
layout: post
title: Lambdas – Exposing Disposable Resources in your API
categories:
  - Imperative
  - C#
tags:
  - c-sharp
  - delegate
  - functional
  - lambda
---

The [last article](http://blog.efvincent.com/c-nuts-and-bolts-lambdas/) described the absolute (and I mean really absolute) basics of lambdas in C#. Assuming you’re continuing from there, where to next? Let’s use some lambdas. There are two approaches we can use. First is using lambdas in places you traditionally used other approaches. Second is using them in new and interesting ways. Hmmm. Which to choose.

<!-- more -->

### New and Interesting Code Constructs using Lambdas

Yea that’ll do. Let’s take an example situation. You’re tasked with writing part of a data access layer. It’s a low level data access layer, we’re not using Entity Framework, no Linq to SQL. No NHibernate. Just you and the keyboard. Sweet.

The team decides they're comfortable with the forward only read only data reader. Ok. So you start sketching out your code and end up with this:

```C#
public static IDataReader GetSomeData() {
    using (var con = new SqlConnection(conString))
    using (var cmd = new SqlCommand(cmdString, con)) {
        con.Open();
        IDataReader rdr = cmd.ExecuteReader();
        return rdr;
    }
}
```

Well, this won’t work. Trying to return the reader, but you’ve got a command and a connection both in using blocks. On returning, those blocks will terminate, disposing of the connection and command (which is good). What now? Briefly consider _not_ using using blocks, not disposing of anything and just returning the reader. Nah – just can’t do that. Ok, so bail on data reader and go with a data set or some other statefull representation of the data I guess.

But WAIT! Don’t punch out just yet. What’s happening here is we’ve got a disposable resource we want to use in a spot that’s surrounded if you will by other important code. Let’s take a look at this:

```C#
public static void UsingReaderGetSomeData(Action<IDataReader> processor) {
    using (var con = new SqlConnection(conString))
    using (var cmd = new SqlCommand(cmdString, con)) {
        con.Open();
        using (IDataReader rdr = cmd.ExecuteReader()) {
            processor(rdr);
        }
    }
}
```

Now we’ve got a method that does all the right things with the data reader. The reader, its connection, and its command are all in using blocks so they’ll be properly disposed of. We ask the caller to supply an `Action<IDataReader>`, and we’ll apply that action to our well protected data reader.

This looks reasonable, but since it’s a data reader, won’t the caller always be doing the `while(rdr.Read())` thing? Let’s take care of that for them.

```C#
public static void UsingReaderGetSomeData(Action<IDataRecord> processor) {
    using (var con = new SqlConnection(conString))
    using (var cmd = new SqlCommand(cmdString, con)) {
        con.Open();
        using (IDataReader rdr = cmd.ExecuteReader()) {
            while(rdr.Read()) {
                processor(rdr);
            }
        }
    }
}
```

Now instead of asking for an `Action<IDataReader>`, we’re asking for an `Action<IDataRecord>` that will be called once for each record. Our function can iterate the data reader and call the processor once for each result. Here’s a sample caller that builds an HTML ordered list and writes the results to the console:

```C#
static void Main(string[] args) {

    StringBuilder sb = new StringBuilder();
    sb.AppendLine("<ol>");
    Dal.UsingReaderGetSomeData(dr => {
        sb.Append(" <li>");
        sb.Append(dr[0]);
        sb.AppendLine("</li>");
    });
    sb.AppendLine("</ol>");
    Console.WriteLine(sb);

    Console.Write("Press any key...");
    Console.ReadKey(true);
}
```

A client uses our function like a loop. Everything it puts in the curly braces from line 5 – 9 is like the the body of the loop. With this approach, we’re getting the benefits of the data reader, it’s fast and forward only. The construct is similar to a loop, so the consumers (our team mates) should be able to understand how to use this. And finally, we’re taking proper care to dispose of the resources.

To summarize, this approach allows you define functions that accept functions as parameters, and build more complex behavior. This technique is called _functional composition_. It’s one of the core principal concepts of Functional Programming, but as we’ve seen here, it can be applied to solutions in imperative programming languages like C# to great effect. Using techniques like this help you break your code down in smaller and smaller chunks, with some functions doing little bits of work, and other functions serving as glue or framework code. All this makes your code easier to test and less prone to errors if done correctly.

### Taking it Further, or, Something More like Real Production Code

Alright, that was interesting. And as far as blog posts go, that’s all I was really trying to get across. But if you’re curious about what production code might look like that uses an approach like this, here’s an example:

```C#
private static void UsingReader(
    string command,
    Action<SqlCommand> preAction = null,
    IEnumerable<Action<IDataRecord>> recordProcessors = null,
    Action<SqlCommand> postAction = null) {

    // Instantiate connection and command, open the connection
    using (var con = new SqlConnection(conString))
    using (var cmd = new SqlCommand(command, con)) {
        con.Open();

        // Execute a pre-action if specified
        if (preAction != null) preAction(cmd);

        // If the reader processing blocks are supplied
        if (recordProcessors != null) {

            // Get an enumerator for the processing blocks & move
            // to the first processor
            var processorEnumerator = recordProcessors.GetEnumerator();
            if (!processorEnumerator.MoveNext())
                throw new InvalidOperationException(
                    "Supply at least one record processor");

            // Open the reader
            using (var reader = cmd.ExecuteReader(CommandBehavior.CloseConnection)) {

                do {
                    // Get the current record processing block
                    var recordProcessor = processorEnumerator.Current;

                    // Iterate the records of the IDataReader using the current block
                    while (reader.Read())
                        recordProcessor(reader);

                    // The loop condition advances both IDataReader and enumerator
                    // in sync continuing as long as there's a next resultSet and a
                    // processor to process it
                } while (reader.NextResult() && processorEnumerator.MoveNext());
            }

        } else
            // If there are no actions to execute using data reader, just execute query
            cmd.ExecuteNonQuery();

        // Do any clean up that might be needed
        if (postAction != null)
            postAction(cmd);
    }
}
```

Here we pass optional pre- and post- actions that allow the caller to optionally manipulate the command before and after it’s executed. We also accept a set of actions we’re calling record processors, to handle the case when a stored procedure or command is returning multiple result sets. In the case of multiple result sets, we’d expect the caller to send a different record processor for each result set it expects.

So this seems like a lot of code. Why bother? The way I approached it was that this code is a private method of the data access layer, and the public methods would leverage this method to avoid repeating the gory details of working with data readers. Leveraging the above method, our original `GetSomeData()` now looks like:

```C#
public static void UsingReaderGetSomeData(Action<IDataRecord> recordProcessor) {
    UsingReader(cmdString, null, new Action<IDataRecord>[] { recordProcessor });
}
```

And if we had a data access method that took a couple of parameters, it would look like this:

```C#
public static void UsingReaderGetSearch(string criteria, Action<IDataRecord> resultProcessor) {
    UsingReader(cmdString,
        cmd => cmd.Parameters.Add(new SqlParameter("@searchCriteria", criteria)),
        new Action<IDataRecord>[] { resultProcessor });
}
```

I hope this has proved helpful. I’ll keep throwing more of these posts out there, let me know if there’s something you’d like to see.
