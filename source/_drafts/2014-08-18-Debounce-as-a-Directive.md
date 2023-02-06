---
title: Debounce as a Directive
categories:
  - Imperative
  - JavaScript
tags:
  - AngularJS
  - directives
  - $timeout
date: 2014-08-18 01:28:33
---

Some months ago a co-worker asked if there were a way to buffer the input on a text box so that the handler would not be called more than every X milliseconds. His app was doing the fairly typical chore of searching through a long list as the user types into a search box. Firing a `digest` (that's Angular's re-bind and re-render loop) for each keystroke causes a jumpy, jittery refresh storm.

What he was looking for is commonly referred to as to _debounce_ or _throttle_ a function. There are many implementations around, but his question was specifically about debounce in the context of Angular, preferably as a directive.

<!-- more -->

I found a snippet authored by [Tom Maitland](http://tommaitland.net) that does just what we need. I tweaked it slightly (also as a [Gist](https://gist.github.com/efvincent/9784923) and [JSFiddle](http://jsfiddle.net/efvincent/vkphp2fa/)):

```javascript
// Defines the module as "app", this is not best practice for module
// definition or project structure, focus on the directive
angular.module('app', []).directive('tlDebounce', function($timeout) {
  return {
    restrict: 'A',
    require: 'ngModel',
    priority: 99,
    link: function(scope, elm, attr, ngModelCtrl) {
      if (attr.type === 'radio' || attr.type === 'checkbox') return;

      elm.unbind('input');

      var debounce;
      elm.bind('input', function() {
        $timeout.cancel(debounce);
          ngModelCtrl.$setViewValue(elm.val());
        }, attr.tlDebounce || 1000);
      });
      elm.bind('blur', function() {
        scope.$apply(function() {
          ngModelCtrl.$setViewValue(elm.val());
        });
      });
    }
  }
});
```

## Examining the directive

The directive starts at line 3 by defining a module and calling the `directive` function which causes Angular to register a directive in that module. This directive is called `tlBounce`. To define a directive we pass a function and our function returns a directive definition object.

### Injecting Angular's Timer service

The debounce directive will use a timer to assure that when attached to a text box, the underlying model will only be updated every X milliseconds. We'll see the algorithm in a bit.

When Angular needs an instance of our directive, it will call the function we've provided. Angular will inspect the function and detect that it has a dependency; to call the function Angular must provide, or _inject_ something called `$timeout`, which is one of Angular's services. Angular offers many _services_ to you as the application developer to use in creating a directive (or factory, filter, controller, or the other Angular things). These services are objects or functions provided by the framework.

The tip-off that `$timeout` is a service is the leading dollar sign. Angular will use its [Injector](https://docs.angularjs.org/api/auto/service/$injector#!) service to find a `$timeout` and pass it to us. We'll then use the `$timeout` in our _link function_.

### The link function

Without getting into the guts of directive development, suffice it to say that in most cases when writing a directive you'll want to focus on the `link` function. For more information on the link function, there's the [Angular Docs](http://angularjs.org/), and another good source of information is [AngularJS Hub](http://www.angularjshub.com/examples/customdirectives/compilelinkfunctions/).

The `link` function sets up each instance of the directive. You supply the function that can have up to four parameters. `link: function(scope, elm, attr, ngModelCtrl)`.

#### Link function parameters

The first is the directive's local scope, which is usually used like you use scope in a controller to maintain and bind to the internal state of the directive. We'll see how `scope` is used here in a second.

Second is the `elm` or _element_ parameter. This is the DOM element that the directive to which the directive is attached. For debounce, the directive is attached to an input, usually a text box. You can do the usual DOM-stuff to the element, attach event handlers, change content, add children, etc. AAMOF you actually have an Angular wrapper around the element, so you get some additional JQuery like functions on the element.

Third is `attr`, the attributes. This is a map of the attributes on the element to which our directive is attached. In our case, we're using the `attr` to detect if we're attached to a radio button or check box; our deBounce doesn't have a meaning for those controllers, so we bail if we see that we're attached to one (line 9).

Lastly is the `ngModelCtrl`, which is the least intuitive. This is a controller requirement of our directive, it says that any directive we're attached to needs to have an `ngModelCtrl` controller. This sort of limits our deBouncer, but the target use case is to put this directive on a text box that's using AngularJS's binding.

### DeBounce Algorithm

The strategy is as follows:

1. Detach the input handler (line 11) that usually updates the model
2. Bind a new input handler
   1. Cancel any pending debounce timer (line 15)
   2. Set up a new debounce timer
      1. It should go off in the time specified by the `tl-debounce` attribute, or 1,000ms if the attribute is
         not specified (line 18).
   3. When it goes off, it should tell the model controller `ngModelCtrl` to set it's value `.$setViewValue(elm.val())`
3. Bind a new blur handler, so when th user leaves the field the model is always updated.
   1. Put the call to `$setNewValue()` inside a `scope.apply()` so the change causes a digest (Angular rebinds everything).

### The Digest

One important point - you may be thinking that you could have used the standard JavaScript timer. Why use Angular's `$timer` service? It's because Angular needs to know when the model changes so that it can perform it's two way binding / model-view synchronization. By using Angular's timer, you can be assured that Angular will know when the timer goes off and will do all the right Angular binding stuff at that time.

### But Eric, you're wrong and oh so stupid!

> You **could** use the normal timer, because when you call `.$setViewValue()` you're letting Angular know something needs to change, right? I mean, it starts in a dollar sign, so it's all Angulary, right?

Heh. You'd think so, but you'd be wrong. This is the kind of thing that makes you scratch your head, then waste fifteen minutes, then look up the docs, then unleash a stream of profanities. It happens that `.$setViewValue()` does **not** cause a digest, probably because of performance or some other really good reason. Doesn't make it fun though, and it's the kind of undiscoverable crap that qualifies as a legit complaint about AngularJS. Take your medicine.

So that's DeBounce - it actually works pretty well for things like text boxes that do searches and stuff like that. I use it in production, but there's no warrentee so YMMV. Have a good one...

-e
