---
title: A Fresh Start
categories:
  - misc
tags:
date: 2023-02-05 13:00
---
I'm restarting this blog after several years of inattention. I've rebooted a few times over the years, but failed in the past to keep it up. Now I feel the need to capture my experiences, interests, and path of further education, for my own sake. I look to this as a purpose where before I can't say I had one. So with this in mind, off I go.

## Parting with the Past
I have a few posts from the past, most of which are more than 12 years old. In previous reboots I incorporated those post in new versions of this blog. Some of them are still interesting, possibly even relevant. But I've changed significantly in the last dozen years. Connecting my thoughts of 12+ years ago with my current thinking seems feels forced. I'm no longer intrigued by the possibility or writing multi-threaded algorithms for the browser in Silverlight! 

So with that I'm putting all those articles into an archive. This with be the new oldest post. The new, Fresh Start.

# Areas of Interest
There are many. Too many. It's a wonderful problem to have. I sit in my den on a Sunday morning, put a playlist from the [Total Baroque](https://www.youtube.com/channel/UCS7CucNrX3f6JCuIj_ugZpA)  channel on the entertainment system, and have to choose what to spend the next few hours studying, practicing, writing. Today I'm getting this blog back on its feet; consider that area of interest #1. 

I'm using [Hexo](https://hexo.io) to generate the blog because I wanted a simple static site generator that allows the authoring of posts using straight markdown. My posts won't be locked into a too-proprietary system. I use [Obsidian ](https:/obsidian.md) heavily for notes, and I can set up the source for my blog to also be an Obsidian "vault", allowing me to work on blog posts with the same tools that I use all the time for other purposes. In the past I've used blog systems that were also tied to my current interest... but as interests change, so did my desire to keep up with whatever esoteric blogging approach I was using. Hexo is a node based system, simple to set up and use and, importantly, simple to come back to and be able to continue using without having to remember a series of bespoke incantations. Let's see how long this lasts.

## Functional Programming
What a broad topic. I've got to take this back to about 2006. Around this time (I'm not going to look up specific dates) I was working as an independent contractor / consultant sub-contracting under Microsoft Consulting, building a system for the Pinellas county tax collector in Clearwater Florida. It was Microsoft stack, C# specifically, a distributed system, "thick" client (what we called native apps at the time) using [DCOM](https://en.wikipedia.org/wiki/Distributed_Component_Object_Model), a now obsolete distributed networking stack.

### C# gets Linq
Around this time Microsoft added [Linq](https://en.wikipedia.org/wiki/Language_Integrated_Query) to C# and the .NET ecosystem. Linq is marketing speak for a combination of runtime, language, and library extensions that "_adds native data querying capabilities to .NET languages_". This amounted to adding anonymous functions to C#, [extension methods](https://en.wikipedia.org/wiki/Extension_method#), and an abstraction called `IEnumerable` that make it reasonably intuitive to write and compose functions we'd now recognize as building on maps and folds.

This was a bit shocking and revolutionary at a time when "functional" concepts had not yet started to become ubiquitous in the most popular OO ecosystems, Java and .NET. I remember specifically working with [Jerry Maresca](https://www.linkedin.com/in/maresca/), one of the senior engineers from Microsoft (with whom I greatly enjoyed working), hungrily consuming these new capabilities and libraries, and experimenting with the new approaches to both low level algorithm implementations, but also what this meant for the structure and organization of entire programs. Object Oriented programming had been the norm for "line of business" programming since the late 80s / early 90s.

Linq and the associated technologies and libraries squeezed functional concepts (which interestingly enough is not a term that has a single, well accepted defintion), into what remains to this day a _very_ object oriented ecosystem. As we began to see the elegance and utility of thinking of problems in terms of functions, functional composition, and data structures uncoupled from the functions by which they are manipulated (ie _methods_). This is directly at odds with object oriented thinking, and we did some _strange_ experiments combining these paradigms using these early tools.

But the real benefit for me was to open my eyes to the possibility of significantly different ways to think about computing at all levels. In short, I thought I had fairly complete knowledge of the landscape of production software engineering and computer programming, and to realize how wrong I was about that was captivating and exciting!

## A Pivotal Discussion over Lunch
A couple of years later, working again with Microsoft Consulting with [Stephen Cohen](https://www.linkedin.com/in/stcohen/) (Chief Architect at Microsoft), this time for the Department of State in DC, I was at lunch with Stephen and [Randy Miller](https://www.linkedin.com/in/ggmiller12345/). The topic of Lisp came up, at the time i was aware of lisp, but had failed to have any interest in a language that was outside of the ecosystem in which I was immersed. Randy made a compelling argument supporting his admiration and appreciation for lisps which piqued my curiosity.

Over the next weeks during frequent visits to the local Borders where several of us on that gig would hang out in evenings (we were "on the road" in DC from all over the country for this gig), I began seeking out and reading about Lisps and other languages. I wandered the field and eventually came across OCaml, and after playing with it a bit and gaining interest, fell upon a research project being worked on by [Don Syme](https://en.wikipedia.org/wiki/Don_Syme) of [Microsoft Research](https://en.wikipedia.org/wiki/Microsoft_Research), a discovery that would radically alter the course of my journey.

## F\# 
Don Syme was developing [F#](https://en.wikipedia.org/wiki/F_Sharp_(programming_language)), a descendant of OCaml for the .NET framework at around the time I was having that lunch with Stephen and Randy. F# was still a couple of years away from production ready release, but you could get your hands on the compiler from the Microsoft Research site, and with some fiddling you could actually use it in an ecosystem (.NET) which was rich enough to make the endeavor worth while.

This was a major tipping point for me. I was now obsessed with this new way of thinking about writing software. I'd eventually come to understand that functional programming wasn't new at all, and importantly, function programming opens the door to a universe of cross disciplinary thought. I spent several years of using F# on my own time, and also building an appreciation for [Clojure](https://en.wikipedia.org/wiki/Clojure) (a JVM lisp), but never having the opportunity to use either professionally. That would change when I was cold contacted by [Aimee Gerzofsky](https://www.linkedin.com/in/aimeegerzofsky/) a recruiter at [Jet.com](https://en.wikipedia.org/wiki/Jet.com).

Jet.com was an e-commerce startup, pre-launch in 2015. The interesting part - they were an F# shop! This was 90% of the reason I would eventually join Jet and move to the NY area. I was working **full time in a functional programming language**. This was then and remains now far and away the best job I've ever had. I was being paid to spend thousands of hours over the next few years writing code that I would have been writing on my own time. F# remains a favorite of mine, though my current interests have moved on to find and probe the more exotic regions of the computer science landscape.

# Haskell
A few years later I found myself with a deep interest in Haskell. I had taken a few shots at learning Haskell (which happens to many I've since learned), giving up and returning to the familiar, comfortable lands of F#, Clojure, and OCaml, before it finally started to click for me. This corresponded to a push toward management in my career.

This "push" was not driven by my interests, but rather that of the companies for whom I worked. This is common for software engineers as they hit the 10, 15, 20 year marks in their career. Companies will place a hand firmly in the small of your back and shove you into management, where your skills will immediately start to fade as thoughts of budgets, timelines, recruiting, career planning, and most disruptively, the authoring and delivery of endless power point presentations take the time you'd rather spend writing beautiful code. Sigh.

Most of my spare time was spent diligently building my intuition for the pure functional world of Haskell. This is an ongoing endeavor to this day. Haskell is my favorite general purpose programming language, and once again I find myself fixated on technologies that I will very likely never have the opportunity to work on professionally; this serves only to motivate me further. 


# Gödel, Escher, Bach
In the last several years, starting in approximately 2018, I crossed paths with several topics that were not obviously connected with my interest in functional programming. I received a copy of  [Gödel, Escher, Back](https://en.wikipedia.org/wiki/G%C3%B6del,_Escher,_Bach) (Hofstadter, 1979) (G.E.B.) from my family as a birthday or Christmas gift (I forget which), it had been on my wish list for some time. I knew nothing about it other than it appeared on the must read lists of several people I admired or followed.

Around the same time, I had come across [Phil Wadler's](https://en.wikipedia.org/wiki/Philip_Wadler) [Propositions as Types](https://www.youtube.com/watch?v=IOiZatlZtGU) talk from Strange Loop 2015, which broke my brain. Wadler was talking about something that was obviously profound and fascinating, and I didn't understand 25% of it. But the fuse was once again lit for me. There was a connection between logic (which I knew nothing about), and computer science, and it was fascinating... more to come.

Also around the same time, my daughter was at Kenyon College studying philosophy and the classics. She had taken a course on Propositional Logic, and in chatting about it I came to realize that the topic had much overlap with the boolean logic that's a deep and integral aspect of computer programming. Add propositional logic to the list of topics in which I suddenly found myself deeply interested.

### All Things Connected

Between the beginning of my studying G.E.B., discovering the idea of Proposition as Types, opening a line of studying of propositional logic, and all of the studying of functional programming I've done over several years, I came to understand the underpinnings of all these topics are related. Abstract Mathematics. Another shocking discovery for me - mathematics, a topic for which I would regularly declare an unabashed disdain for my entire life until this point, had stealthily worked it's way into my academic life to suddenly spring forth and become the center of my universe. I had to, and still have to suppress the feeling that had I come to this realization 30 years sooner, I would be in a very different place indeed. Such, as they say, is life.

### The Academics

Which brings me, finally, to the remainder of the list of my interests. They include, but are not limited to:

- Type Theory
  - Dependent type theory
  - Linear types
- Category Theory
- Set Theory
- Formal Verification
  - Calculus of Constructors
  - Logic
  - Coq, Agda, Idris, and most recently Lean4
  - TLA+
- Compiler Design
  - The various flavors of Lambda Calculus & Combinators
- Programming Language Theory

This challenge I now face regularly; spending enough time with each of these topics to retain what I've learned, while pushing forward the boundaries of my own knowledge one area at a time. Again, this is a really nice problem to have.

My journey continues.