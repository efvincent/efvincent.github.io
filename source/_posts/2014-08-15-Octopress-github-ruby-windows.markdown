---
layout: post
title: "GitHubify(Blog)"
date: 2014-08-15 12:00:11 -0400
comments: true
categories: [Blog, Windows, Octopress]
---
For my shiney new blog I decided to go full hipster and host on GitHub. This means that
the entire site, source code as well as the blog website itself, can be seen at
[my blog's repository](https://github.com/efvincent/efvincent.github.io). Of course all of this will be painfully
uncool by this time next year. But for the time being it's fun and the tech is relevant.

Dig in if you want to see the ins and outs of setting all this up on a Windows workstation,
because Windows is so far out of fashion stuff is starting to not work on it by default. Karma.
<!--more -->
## Static Site Generation
The way GitHub hosted web sites work is that the entire web site is pure static HTML,
JavaScript, and CSS. If you work in the industry you realize that all the web site
frameworks (server side) like JSP, ASP.NET, Rails, PHP, etc. all *generate* pages on
the fly as the requests come in. This requires a server be running somewhere that can
run .NET, Java, Ruby, or whatever.

A static site everything is pre-generated, so any server that can respond to requests
by serving up a file can host a static site. But no one want's to hand craft HTML pages
for every blog post. So what you have is a generator; some kind of program that can take
blog posts written in plain text and **poof!** Generate a static website.

The basic idea is that you have a *source* directory that has your blog posts written
in (typically) [markdown](https://en.wikipedia.org/wiki/Markdown), CSS, templates for HTML,
and the Ruby / JavaScript / whatever that the generator is written in.

There's typically a command line interface; so you issue the proper command the
generator takes your markdown and creates a complete, static web site.

##GitHub
That's where [GitHub](www.github.com) comes in. If you don't know what GitHub is, or
if you're completely lost at this point, then you probably need to hit up this
[everything you need to know to become a modern developer](https://www.youtube.com/watch?v=oHg5SJYRHA0)
tutorial.

Think about it for two seconds, it makes perfect sense. GitHub hosts files in a repository and is a web site.
So if you put a static web site's worth of files *in a repository*, and GitHub does a tiny bit of magic, it
can serve those web pages up. And that's all a GitHub hosted static site is. You write posts in
markdown, you use a generator to create a static site, and you check it into GitHub.

## OctoPress
I admittedly don't have a ton of experience with static site generators. I played with
a couple before committing to [Octopress](www.octopress.org). At first I wanted to
use a [Node.js](http://nodejs.org) based static site generator. I googled around a bit
and ended up playing with [Wintersmith](https://github.com/jnordberg/wintersmith) for
a night. It worked, but GitHub makes it easiest to use a Ruby based generator called [Jekyll](http://jekyllrb.com).
A bit more poking around and I ended up with [Octopress](www.octopress.org), which is
built on top of Jekyll, offers additional features, and incredibly well written, well
documented, and easy to use.

When you go to their site you'll get walked through the installation step by step,
and there are some nice Ruby build tools available to get things moving, including
configuring your new Octopress generator to work with GitHub web sites. **Highly recommended**

## Ruby
Ok so [Ruby hates Windows](https://www.youtube.com/watch?v=gBkDvqIGSaE). It might be passive,
but the ill feelings are there. Which is fine, everyone can't love everyone, and Microsoft
has kinda earned it by being a dick until very recently. But whatever, I'm not with starting
flame war. It happens that I still use Windows for all kinds of reasons, so I needed to
get Ruby on my workstation.

Actually I had installed Ruby using [Chocolately](http://chocolatey.org/) because I
love playing with all the languages, but to run Jekyll you'll need the
[Ruby Development Kit](http://rubyinstaller.org/downloads/). The RDK is platform
specific, and is needed to build *gems* (Ruby packages) that are platform specific,
which Jekyll either is or depends on indirectly (I didn't check).

There are [very clear instructions](https://github.com/oneclick/rubyinstaller/wiki/Development-Kit)
that you can follow for getting this set up. Once that's done you can play along with
the [Octopress](www.octopress.org) setup instructions, which are great. Just follow the
track that has you deploy to GitHub.
