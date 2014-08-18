---
author: EFVincent
comments: true
date: 2011-01-12 23:27:26+00:00
layout: post
slug: nuget-geeky-goodness
title: NuGet – Galleries of Geeky Goodness.
wordpress_id: 198
categories:
- C#
- EntityFramework
- NuGet
- basics
tag:
- basics
- C#
- ef
- entity framework
- NuGet
---

In a [previous post](http://blog.efvincent.com/open-source-nuget-and-more/) I described how Scott Hanselman’s [presentation](http://www.hanselman.com/blog/PDC10BuildingABlogWithMicrosoftUnnamedPackageOfWebLove.aspx) during PDC 2010 introduced me to several wonderful and interesting technologies that are either just now out or about to be. The first one I’ll discuss is [NuGet](http://nuget.codeplex.com). There are many excellent introductions to NuGet by [Hanselman](http://www.hanselman.com/blog/IntroducingNuPackPackageManagementForNETAnotherPieceOfTheWebStack.aspx) and others. I’ll see if I can make a meaningful contribution.

NuGet is an add-on to VS2010. It allows us to access _packages_ that have been published either locally or publicly on the web. The main NuGet gallery just went live today at [http://nuget.org](http://nuget.org). These packages make it easy to install, update, and remove libraries and tools in Visual Studio 2010.

<!-- more -->

## Installation


NuGet, like dozens of other useful add-ins, is available through the Visual Studio Extension Manager, reached under _Tools… Extension Manager…. _

[![image](http://blog.efvincent.com/wp-content/uploads/2011/01/image_thumb.png)](http://blog.efvincent.com/wp-content/uploads/2011/01/image.png)

From the Extension Manager choose (1) the online gallery, and then (2) search for NuGet. A restart of Visual Studio is required.


## The NuGet Package Manager Console


NuGet has two user interfaces. There’s a PowerShell powered console window. This is the UI I was first introduced to and have gravitated towards. The console gives you a more complete sense of control over the process. Open the console by selecting _View… Other Windows… Package Manager Console_.

[![image](http://blog.efvincent.com/wp-content/uploads/2011/01/image_thumb1.png)](http://blog.efvincent.com/wp-content/uploads/2011/01/image1.png)

The _Package Source_ drop down defaults to the official NuGet package gallery. There are already hundreds of packages out in the gallery, and you can expect it to grow rapidly. You can list the packages in the official library with:

List-Packages –remote

There are a lot of them, and since the console is PowerShell we can take advantage of a PowerShell grid display:

List-Packages –remote | out-GridView

[![image](http://blog.efvincent.com/wp-content/uploads/2011/01/image_thumb2.png)](http://blog.efvincent.com/wp-content/uploads/2011/01/image2.png)


## Installing and Removing Packages


Now comes the really cool part. The whole idea of NuGet is to be able to install / add external libraries to your project without having to find them on the web, download a zip or msi file, install it somewhere on your local machine, figure out what the instructions are for incorporating them into your project, and patching it in manually.

Let’s take for example, the latest CTP5 version of Entity Framework, that allows for _Code First_ use of the Entity Framework. EF4 will justify at least one future dedicated post, but we’ll use it here for a quick example. Normally, if you want to try out something like a new version of Entity Framework, there’s downloading, installing, shaving the chicken, bla bla bla that you’ve got to do. Not with NuGet. NuGet can effectively _“_xcopy deploy” it into our project.

Create a new console application in Visual Studio. From the Package Manager Console, type _Install-Package EF_ and press the tab key. An intellisense window will pop up showing packages that begin with EF.

[![image](http://blog.efvincent.com/wp-content/uploads/2011/01/image_thumb3.png)](http://blog.efvincent.com/wp-content/uploads/2011/01/image3.png)

At the time of this writing, there are five packages. The EFCTP4 ones are obsolete, from the last CTP. For this example, choose _EFCodeFirst_ and press enter. The console displays license info, downloads the EFCodeFirst package, and incorporates it into your project. In the solution explorer, we can see a reference  (1) and a file (2) added to the project.

[![image](http://blog.efvincent.com/wp-content/uploads/2011/01/image_thumb4.png)](http://blog.efvincent.com/wp-content/uploads/2011/01/image4.png)

Checking the properties of the reference to EntityFramework, you can see that it’s not installed to the GAC, it’s local to the solution. NuGet creates a solution level folder called _packages_. In that folder, in addition to components used as references, is all the data that NuGet needs to install and uninstall packages. All local, without changing any configuration on the machine. More details of the inner workings of NuGet and its packages will come at a later date, plus there’s information out there you can dig up if you’re so inclined.


## Using Entity Framework


Now that we’ve got it installed, let’s quickly use EFCodeFirst just to prove it’s working. For this example, you’ll need SQL Server installed (there are other options, but one thing at a time). I’ve got Express which works fine. In another post I’ll show the new SQL Server Compact Edition 4, which coincidentally you can add to a project using NuGet ![Smile](http://blog.efvincent.com/wp-content/uploads/2011/01/wlEmoticon-smile.png).

EFCodeFirst allows you to build POCO objects and use them to build a DB. Let’s build a quick model. There are a few examples floating around modeling blog posts and comments, so lets _not_ do that. Let’s do (super simple) prescriptions and medications. It’s on my mind because I’m recovering from knee surgery. This code is entered directly into the Program.cs source file for this example.

~~~java
public class Prescription {
    public int Id { get; set; }
    public string MedName { get; set; }
    public string Directions { get; set; }
    public int Quantity { get; set; }
    public int Refills { get; set; }
    public int RefillsRemaining { get; set; }
    public ICollection<FilledScript> FilledScripts { get; set; }
}

public class FilledScript {
    public int Id { get; set; }
    public Prescription Script { get; set; }
    public DateTime Filled { get; set; }
    public int Doses { get; set; }
}

public class MedContext : DbContext {
    public DbSet<Prescription> Prescriptions { get; set; }
    public DbSet<FilledScript> FilledScripts { get; set; }
}
```

We have a class for a Prescription, and one for a FilledScript (filled prescription). We give each one an integer Id, which EF will interpret (by convention) as the primary key and identity for each entity. A relation is created between them, again by convention. On the “one” side,  the Prescription has a collection of filled scripts, and on the “many” side, the filled script has a reference to its prescription.

The _MedContext_ class inherits from EF’s DbContext, and pulls the model together. Each DbSet<…> identifies an entity to EF. In this minimal case, that’s all that’s required to define a model using EFCodeFirst. Lets take a look at using it:

~~~java  
static void Main(string[] args) {

    DbDatabase.SetInitializer(new DropCreateDatabaseAlways<MedContext>());

    var ctx = new MedContext();
    var p = new Prescription() {
        MedName = "Vicoden",
        Directions = "One every 4 hours as needed for pain",
        Quantity = 60,
        Refills = 2,
        RefillsRemaining = 1,
        FilledScripts = new[] {
            new FilledScript() {
                Filled = DateTime.Parse("12/28/2010"),
                Doses = 0
            },
            new FilledScript() {
                Filled = DateTime.Parse("1/12/2011"),
                Doses = 48
            }
        }
    };

    ctx.Prescriptions.Add(p);
    ctx.SaveChanges();

    foreach (var script in ctx.Prescriptions) {
        Console.WriteLine("Script for {0}, filled {1} time(s), we have {2} doses on hand",
            script.MedName,
            script.FilledScripts.Count(),
            script.FilledScripts.Sum(fs => fs.Doses));
    }

    Console.Write("\nPress any key...");
    Console.ReadKey(true);
}
```

For this simple example I’m just adding some data and retrieving it right from the main() function of the console app. Line 3 probably the only line that needs explanation. It’s a call to the static method SetInitializer() on the static DbDatabase class of EF. The parameter is a new instance of DropCreateDatabaseAlways<MedContext>. Creating this initializer with it’s type parameter set to our MedContext tells the EF engine that it should always drop the database and recreate it, every time the app is run. This is part of EF’s new fluent interface. Under almost no circumstance would you actually use this initializer, but it’s handy for testing and playing around. This way you can iterate over your model getting a new database for each run.

The rest of the function adds a Prescription object with two FilledScript objects, and saves it in the context. It then loops through the prescriptions and writes some info to the console about each. No rocket science there.


## Where did EF Create the Database?


One cool thing about EFCodeFirst is that you can leave off pretty much all the configuration information. We didn’t tell EF anything about how or where to create the database. It came up with some reasonable defaults. If you open SSMS and connect to your instance of SQL Express, you can see the database it created. Here’s a DB diagram in SSMS from what EF created:

[![image](http://blog.efvincent.com/wp-content/uploads/2011/01/image_thumb5.png)](http://blog.efvincent.com/wp-content/uploads/2011/01/image5.png)

The database it created (1) is named according to the context that it stores, MedContext for us. It created tables (2 & 3) for the entities we created, and to implement the one to many relationship, it added a foreign key to the FilledScripts table linking it to the Prescriptions table. There’s always a DBA to disagree with an auto-generated schema, but in this simple case, you must admit there’s not a whole lot you could do differently. If we select the records out of the database after our run you get the expected results:

[![image](http://blog.efvincent.com/wp-content/uploads/2011/01/image_thumb6.png)](http://blog.efvincent.com/wp-content/uploads/2011/01/image6.png)

All of this was created by convention, and can also be overridden and specified explicitly using either attributes on the classes that make up the model, or by using EF’s fluent API. This barely scratches the surface, but hopefully gets you interested. I’ll cover EF in more depth in a future post, plus there’s plenty of info out there already.


## Back to NuGet – Removing a Package


Ok, we’ve seen how easy it is to add a package like EFCodeFirst (and how cool EF itself is). NuGet also allows you to remove a package easily. In the Package Manager Console window, issue the command:

~~~
PM> Uninstall-Package EFCodeFirst
Successfully removed 'EFCodeFirst 0.8' from ConsoleApplication2
Successfully uninstalled 'EFCodeFirst 0.8'
~~~

NuGet will remove all traces of the package from your project. For EFCodeFirst there’s not much to remove. But other packages are far more complicated, even pulling in prerequisite packages, and modifying the web or app.config. NuGet undoes all these changes when a package is removed.


## Try it yourself!


One great thing about NuGet is how painless it makes trying out new things. You’re not fouling up your precious work machine with a bunch of installations. You can create a quick test project, add a bunch of things you’ve always wanted to try, and just blow it all away when you’re done. No evidence you’ve been _learning_.

Ever want to try Fluent NHibernate but didn’t feel like dealing with all the crap that goes with tracking it down and installing it? How about Ninject, Castle, ELMAH, iTextSharp, Moq, or Prism? They’re all out there. Just NuGet them and start playing / learning.


## What else?


I didn’t even mention the wizardy, non-console approach. Right click on your project, and select Add Library Package Reference… and you get a pretty UI that lets you click instead of type. But you’re a coder. You can type.

Publishing your own package. You can create any package you want. Perhaps one that pulls in some of your favorite utility classes. Or perhaps you’ve got some corporate libraries and frameworks that you want to be able to incorporate using NuGet. You can create your own private or a corporate NuGet gallery. Or if you have something worth while, publish it up to the official NuGet gallery, it’s own to the public.

I hope you’ve found this ramble of mine useful. Search around for more NuGet goodness. There are plenty of bloggers way more interesting then me publishing good information.

Happy Coding!
