---
author: EFVincent
date: 2011-06-24 06:07
title: DI – Constructor Injection, Bootstrapping
categories:
  - Imperative
  - C#
tags:
  - AutoFac
  - bootstrap
  - c-sharp
  - Dependency Injection
---

## Constructor Injection

The idea of dependency injection is that classes are defined such that any dependencies on other classes or services, are _injected_ into the class by some external mechanism, as opposed to being “newed up” directly. The most common form of DI is constructor injection, where a class defines a constructor that has as its parameters the external dependencies required by the class.

<!-- more -->

There are several benefits to this particular method of injection; the most obvious is that in a well designed system the dependencies of a class are clearly visible in the constructor. In the [DI 101](http://blog.efvincent.com/practical-di-101) post a data provider was defined like this:

```java
public class DevDataProvider : IDataProvider {
    private readonly IIdentService _identService;
    private readonly ILogService _logSvc;
    private static readonly List<Employee> EmployeeStore = new List<Employee>();

    public DevDataProvider(IIdentService identService, ILogService logSvc) {
        if (identService == null) throw new ArgumentNullException("identService");
        if (logSvc == null) throw new ArgumentNullException("logSvc");
        _identService = identService;
        _logSvc = logSvc;
    }

    // Remaining implementation omitted for brevity
}
```

The constructor is on line 6. From this constructor we can see that the DevDataProvider has dependencies on an IIdentityService and an ILogService. There should be no other dependencies in the class other than to well known, stable libraries like the [BCL](http://msdn.microsoft.com/en-us/library/hfa3fa08.aspx).

There are other advantages to using constructor injection. Should the list of dependencies get too long, say longer than four parameters, you’ve got a code smell that perhaps the class is doing too much, violating the single responsibility principal.

## Bootstrapping

In order to be able to resolve dependencies, the DI container must be configured. This set up is done during the **bootstrapping** phase. Typically this only needs to be done once, but changes to the container make sense in some scenarios like when a DI container is being used to support extensions or plug-ins. In that case components might be added or removed from the DI container while the app is running. These scenarios are out of scope for this post.

The container may be configured in several ways – Auto configuring, configuration in code, and configuration files (typically XML / app or web.config files). My current favorite DI framework is AutoFac, and I typically configure in code, but different projects will have different demands, so familiarize yourself with the specifics of your selected framework and understand the tradeoffs involved in the different types of configuration. You can even configure the DI container using more than one method – perhaps Auto configuring for the bulk of the registrations, then code or XML for more specific configuration needs.

## Bootstrapping a Console Application

Depending on the type of application you’re working on, there are specific places for bootstrapping to take place. The _place_ to do configuration and bootstrapping is sometimes referred to as the **composition root** _(you can read about these concepts in more detail in [Mark Seeman’s Dependency Injection](http://www.manning.com/seemann/) book, published by Manning)_.

In a console application, the static Main() method is a typical place to configure the container. While we rarely write console apps in production (at least I rarely do), the simplicity makes it easy to see the implications of the bootstrapping procedure.

In the following sequence diagram, in step one [1] the Main() entry point is called on the console application. Main() is serving as the composition root. From there a private Bootstrap() methods is called [2] and the DI container is configured. The exact mechanism varies by framework.

[![Capture](http://blog.efvincent.com/wp-content/uploads/2011/06/Capture_thumb.png)](http://blog.efvincent.com/wp-content/uploads/2011/06/Capture.png)

Once the container is configured, the main entry point requests that the DI container resolve the App type [3]. The DI container creates whatever dependencies are required by the App [4]. This happens hierarchically; dependencies may themselves have dependencies and so on. The DI container sorts all this out and is also responsible for lifetimes of create objects etc. The DI container can create and return the instance off the App [5]. The Main() function can then pass control to the app [6] which will leverage the injected dependencies [7] to do the real work.

## Only Directly Reference the DI Container in the Bootstrapper

This is an important point, and if you get nothing else from this post, understand this.

- The DI container is configured in the composition root (Main() in this case)
- The DI container is used to resolve or build the App
- The app is then run to do the work

Once the app is instantiated, it should have all of its dependencies injected. **The app should not have a reference to the DI container!** If we allow the app or any of its dependencies to have access to the container, then several bad things happen:

#### We’ve taken on a dependency to the DI Container itself

Yes its true that the assembly has a dependency on the DI container. But for purposes of this discussion the assembly is not the application. The App class and the services (other classes) it depends on is the application. We don’t want to take a dependency on the DI container in those classes; rather, we should be able to switch to a different DI container if needed and not effect the App and the dependent services.

In any kind of a significant application the app’s classes would be in a different assembly, and services might be scattered across even more assemblies, and those should not have a dependency on a DI container. They should however be designed and built with the DI pattern in mind – with the dependencies specified in the constructor, with references to abstract types or interfaces, rather than to concrete implementations.

#### We’re hiding a dependency inside the App

Earlier I mentioned that a benefit of constructor injection is that the dependencies are clearly visible (even _documented_ if you will) in the signature of the constructor. We really don’t want to see lines like this buried in the methods of the classes:

```java
// Anti-pattern - don't use DI container except
// in composition root

var dal = Container.Resolve<IDataAccessService>();

// And defintely don't do this

var dal = new SqlDataAccessService(connectString);
```

A class that that has these lines buried inside somewhere has hidden dependencies on both the DI container and IDataAccessService (or worse, by using the new keyword directly, on the SqlDataAccessService). These hidden dependencies undermine the benefits of using DI containers at all.

### Bootstrapping in other Application Types

Other types of apps have different places for bootstrapping and application roots. Unlike a console app, an ASP.NET MVC 3 application isn’t top-down linear, the application must respond to web requests. It does so by creating instances of controllers, and calling methods on those controllers to respond to web requests.

A controller in MVC3 is like the app was in our console example above. It will be resolved, or created, by the DI container. Controllers are different in that there will likely be several different controllers in an MVC application. Also, we don’t get to resolve a controller and tell it to run right from the composition root, the ASP.NET MVC framework will be receiving web requests and will need to resolve controllers later, after bootstrapping.

In ASP.NET MVC 3 this is accomplished by providing a _hook_, or a place where we can supply a DI container for ASP.NET MVC 3 to use when creating controllers. The developer configures the DI container, and then wires that container into the MVC framework via an instance of IControllerActivator. In the case of AutoFac, there’s a [NuGet package called AutoFac.Mvc3](http://nuget.org/List/Packages/Autofac.Mvc3) that includes classes to integrate with MVC3. The implementation details are beyond the scope of this post – just [DuckDuckGo](http://duckduckgo.com/) AutoFac.Mvc and find a wealth of additional detail. Same goes for WCF, WPF, and Silverlight applications. There are best practices for configuring DI containers for each app type.

### DI Unfriendly Application Types

Some application types just do not lend themselves very easily to dependency injection patterns. Classic ASP.NET pops into mind immediately. It was written before Microsoft was as willing to accept OSS, community driven concepts such as DI Containers. A big red flag with ASP.NET is that all subclasses to the Page class (which is what all your ASP.NET pages are) must have a parameterless default constructor. Well there goes parameter injection!

There are other mechanisms for implementing DI patterns in this case, but they’re sub-optimal. Again I’d refer you to [Mark Seeman’s Dependency Injection](http://www.manning.com/seemann/) book, which is far and away the best DI book in the .NET space, for advice and examples in dealing with DI unfriendly application types.

### In Summary

Hopefully this was helpful in your understanding of a couple of key aspects of using DI containers. Practice a few console applications, and write some tests too. Once you get the idea, move on to more interesting application types. Before long you’ll be shocked you ever wrote applications _without_ some degree of dependency injection. Yea – it’s that good for you.
