---
title: Adding Concurrency Optimization in Silverlight 3
categories:
  - Imperative
  - C#
tag:
  - c-sharp
  - concurrency
  - silverlight
  - writableBitmap
date: 2009-08-18 05:33:22
---
  
About a week ago Joa Ebert posted [Flirting with Silverlight](http://blog.joa-ebert.com/2009/08/10/flirting-with-silverlight/), in which he discussed his experiences with a cool little Lorenz attractor application. The _strange attractor_ app (which you can check at his [blog](http://blog.joa-ebert.com/2009/08/10/flirting-with-silverlight/)) was his way of playing around with the newly released Silverlight 3 (Props to Joa for some very cool work BTW). It calculates a shape in 3D defined by some 300,000 particles, and animates the spinning and rotating of this shape as the user moves the mouse across the display surface.

His point was to relate his first Silverlight experience, and how the non-optimized Silverlight application compared very favorably with the significantly optimized AS3 version. The HTML5 Javascript version was a non-starter, barely able to achieve a few frames per second compared to 30 for AS3 and SL3.

<!-- more -->

Joa made [his code](http://blog.joa-ebert.com/2009/08/10/flirting-with-silverlight/) available. After looking at the SL3 source code I thought it would be trivial to add some basic concurrency that would _significantly_ increase performance. Here’s a comparison of different implementations:

- [Flash AS3 Version](http://www.joa-ebert.com/swf/index.php?swf=alchemy/Example03), Optimized
- [Original Silverlight Version](http://www.joa-ebert.com/files/xpa/attractor/), naive implementation
- [Silverlight Version](http://efvincent.com/poc/strangeattractor/), optimized to leverage Concurrency
- [HTML 5 Version](http://mediaerror.com/lab/javascript/strangeattractor)

A point of interest, as far as I know there is [no way to do concurrency in Action Script 3](http://blogs.adobe.com/aharui/2008/01/threads_in_actionscript_3.html). In Joa’s application, the bulk of the hard work is done in a loop that executes every frame:

```C#
void OnStoryboardCompleted(object sender, EventArgs eventArgs)
{
    targetX += (mouseX - targetX) * 0.1;
    targetY += (mouseY - targetY) * 0.1;

    var index = MaxScreen;
    var maxIndex = MaxScreen;

    while (--index > -1)
        bitmap.Pixels[index] = 0x000000;

    var particle = particles;
    var matrix = Matrix4x4.RotationY(targetX * 0.05) * Matrix4x4.RotationX(targetY * 0.05) * translationMatrix;

    var cx = 275.0;
    var cy = 200.0;

    var w = 0.0;
    var xi = 0;
    var yi = 0;
    var x = 0.0;
    var y = 0.0;
    var z = 0.0;
    var pz = 0.0;

    while (null != particle)
    {
        x = particle.X;
        y = particle.Y;
        z = particle.Z;

        pz = focalLength + x * matrix.I02 + y * matrix.I12 + z * matrix.I22 + matrix.I32;

        if (0.0 < pz)
        {
            xi = (int)( ( w = focalLength / pz ) * ( x * matrix.I00 + y * matrix.I10 + z * matrix.I20 ) + cx );
            yi = (int)( w * ( x * matrix.I01 + y * matrix.I11 + z * matrix.I21 ) + cy );

            index = xi + yi * ScreenWidth;

            if (index > -1 && index < maxIndex)
            {
                var color = bitmap.Pixels[index] + 0x202020;
                bitmap.Pixels[index] = color > 0xffffff ? 0xffffff : color;
           }
        }

        particle = particle.Next;
    }

    bitmap.Invalidate();

    storyboard.Begin();
  }
}
```

What he’s doing here is writing pixels into a bitmap. At line 9, he sets all the pixels to zero (black), erasing the previous frame. At line 23 he starts a loop that will calculate each pixel that should be turned on for the upcoming frame. In both these cases he’s dealing with a one dimensional array of pixels. This is the perfect case for data parallelism. The work of setting pixels can be divided between processors.

In general, concurrency is not a simple technique to implement correctly. The number and type of problems that one can run into are **way** beyond the scope of anything short of a very thick book. But this perhaps the simplest case; simple _static composition data parallelism_. It’s static because you can pre-assign the amount of work each thread will do. It’s data parallelism because each thread will be doing the same kind of work (calculating and flipping pixels), as opposed to a solution where one thread does one thing (like talk on the network) while another thread does something else (like shred xml files).

We get this done by building a function that behaves like a `For()` loop, but executes the body in parallel between several threads. If you’ve played with the recently released Visual Studio 2010 beta that has the .NET 4.0 framework, you’ll recognize this function which is in the next version of the framework (but in a far more sophisticated and powerful form).

```C#
void ParallelFor(int lo, int hi, Action<int> body, int p)
{
    int chunk = (hi - lo) / p;      // Iterations per thread

    AutoResetEvent[] latch = new AutoResetEvent[p];
    for (int i = 0; i < p; i++) latch[i] = new AutoResetEvent(false);

    // Schedule the events to run in parallel
    for (int i = 0; i < p; i++)
    {
        ThreadPool.QueueUserWorkItem((ob) =>
        {
            int pid = (int)ob;
            int start = lo + pid * chunk;
            int end = pid == p - 1 ? hi : start + chunk;
            for (int j = start; j < end; j++)
            {
                body(j);
            }
            latch[pid].Set();
        }, i);
    }
    WaitHandle.WaitAll(latch);
}
```

ParallelFor() takes lo and hi values, an `Action<int>`, and the degree of desired parallelism. The `Action<int> `is a delegate, which is like a pointer to a function. In this case, it’s a pointer to a function that has a single int parameter, and returns void. This function is the work that happens every iteration. Silverlight, .NET, and C# make it possible (I think it's easy, but I've been doing this a while) to use anonymous methods and what amount to functional techniques to make Joa’s code multithreaded.  It may be best to illustrate by example.

```C#
while (--index > -1)
    bitmap.Pixels[index] = 0x000000;
```

Joa’s code above, which initializes the pixels in the bitmap, becomes:

```C#
ParallelFor(0, buffer.Pixels.Length, (i) => buffer.Pixels[i] = 0x0, numComputationThreads);
```

The parameters of ParallelFor are zero for where to start the for loop, Pixels.Length for where to end the loop, the lambda `function (i) => buffer.Pixels[i] = 0x0`, and the number of threads to use to do the work. The lambda function is anonymous (i.e. we don’t have to create a function with a name). The type of “i” is _inferred_ from context to be an int. The body of the lambda (the part after the “=>”), is the work that is done each time through the loop.

This makes the clearing of last frame’s bitmap concurrent. To make the rendering of the next frame concurrent is a bit more invasive, but still not bad. Here’s the concurrent version.

```C#
ParallelFor(0, particles.Length, (i) =>
{
    Particle part = particles[i];
    var x = part.X;
    var y = part.Y;
    var z = part.Z;

    var w = 0.0;
    var xi = 0;
    var yi = 0;

    var pz = focalLength + x * matrix.I02 + y * matrix.I12 + z * matrix.I22 + matrix.I32;

    if (0.0 < pz)
    {
        xi = (int)((w = focalLength / pz) * (x * matrix.I00 + y * matrix.I10 + z * matrix.I20) + cx);
        yi = (int)(w * (x * matrix.I01 + y * matrix.I11 + z * matrix.I21) + cy);

        var index = xi + yi * ScreenWidth;

        if (index > -1 && index < maxIndex)
        {
            var color = buffer.Pixels[index] + 0x202020;
            buffer.Pixels[index] = color > 0xffffff ? 0xffffff : color;
        }
    }

}, numComputationThreads);

buffer.Invalidate();
lastRender = elapsed;

}
```

There are a few changes that I made to make the optimization a bit easier. Joa used a linked list of particles, I captured an array, just because it was a bit easier to make concurrent. But the significant difference is the body of Joa’s loop is now in an anonymous function. Values that will change in the body of the loop are now defined in the loop (x,y,z, etc). This is because if the loop used the variables defined outside the lambda, then the different threads would try to use the same variables as one another, which is obviously a problem. Other than that, the code is pretty much the same.

So how much of an improvement is there? Yea, a lot. I put a frame counter on Joa’s original code and on my MBP dual Core Duo 2.4Ghz 4Gig ram running Windows 7 release, it was getting about 30 frames per second. On OS X it was about the same. With the concurrency, and set to use 4 threads, it got up to 45 fps. On my quad core Dell M6400 Core 2 Exreme Q9300 at 2.54 Ghz with 8gig ram, it went up to 65 fps. If found it was best to use more threads then you have cores. So 4 threads was good on the dual core, 8 on the quad core.

Of course, this means the app will absolutely peg the processor, which is good if you want max speed. But you can also throttle the frame rate back, say hold it at 30, and use less processor to get the same fps as the original.

Silverlight 3 has caused a bit of a splash in the RIA community if the Twitter chatter is any indication. There is some venomous talk out there; you know the such and such tech sucks, such and such tech rules, and of course Microsoft is evil. I choose to stay out of that fray, perhaps it’s hitting 40 that did it. I don’t care to argue with Fanbois. I will say that the the community will discover that the .NET framework and C# (even VB.NET) make for a serious development platform that will benefit the community at large. It’s a very good time to be a developer!

Here’s a link to a zip file containing my version of the project: [Strange Attractor Project](https://skydrive.live.com/?cid=db16741b8ce498b0#)

Would you like to see the concepts in this post expanded? Want to know more about concurrency, functional patterns, generics? Leave me a note in the comments or send me a Twitter reply at @efvincent. Oh, and thanks for reading!
