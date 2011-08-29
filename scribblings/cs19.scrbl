#lang scribble/manual
@(require planet/scribble
          planet/version
          planet/resolver
          scribble/eval
          racket/sandbox
          racket/port
          (only-in racket/contract any/c)
          racket/runtime-path
          "scribble-helpers.rkt"
          "../js-assembler/get-js-vm-implemented-primitives.rkt")

@(require (for-label (this-package-in resource))
          (for-label (this-package-in web-world)))

@(define-runtime-path whalesong-path "..")


@title{CS19 instructions for Whalesong}
@author+email["Danny Yoo" "dyoo@hashcollision.org"]


@section{Installation}

We'll install a local development copy of Whalesong in a @filepath{whalesong} subdirectory.
On the very first time we install Whalesong:
@verbatim|{
    $ git clone git://github.com/dyoo/whalesong.git
    $ cd whalesong
    $ make
}|
The @filepath{make} step make take a minute or two, and creates a command-line program called
@filepath{whalesong} that we'll use to build Whalesong programs.


Whenever we need to update whalesong, we should do the following
@verbatim|{
    $ git pull
    $ make
}|



@section{Usage}
The @filepath{whalesong} launcher in the subdirectory will compile
programs to standalone @filepath{.xhtml} files.


Example usage: using @litchar{whalesong build} to compile a whalesong program.
@verbatim|{
fermi ~/work/whalesong $ cd examples
fermi ~/work/whalesong/examples $ cat hello.rkt
#lang planet dyoo/whalesong

(display "hello world")
(newline)
fermi ~/work/whalesong/examples $ ../whalesong build hello.rkt 
fermi ~/work/whalesong/examples $ google-chrome hello.xhtml
Created new window in existing browser session.
fermi ~/work/whalesong/examples $ 
}|


There are examples in the @link["https://github.com/dyoo/whalesong/tree/master/examples"]{@filepath{whalesong/examples}} and
@link["https://github.com/dyoo/whalesong/tree/master/web-world/examples"]{@filepath{whalesong/web-world/examples}}.




@section{API}

@defmodule/this-package[web-world]

For the purposes of tour-guide, we'll be focusing on the
@racketmodname/this-package[web-world] library in Whalesong.

Like the big-bang in regular world, the callbacks are world-to-world
functions.  One difference introduced by the web is the web page
itself: because the page itself is a source of state, it too will be
passed to callbacks.  This library presents a
functional version of the DOM in the form of a @tech{view}.

The world-updating callbacks may optionally take an @tech{event} object, which 
provides additional information about the event that triggered the callback.



@defproc[(big-bang [w world]
                   [h big-bang-handler] ...) world]{
Start a big bang computation.
}
@defproc[(initial-view [x any]) big-bang-handler]{
Provide an initial view for the big-bang.}
@defproc[(stop-when [stop? ([w world] [dom view] ->  boolean)]) big-bang-handler]{
Tells @racket[big-bang] the predicate for termination.
}
@defproc[(on-tick [tick-f ([w world] [v view] [e event]? -> world)]) big-bang-handler]{
Tells @racket[big-bang] to update the world during clock ticks.
}


@defproc[(on-mock-location-change [location-f ([w world] [v view] [e event]? -> world)]) big-bang-handler]{
Tells @racket[big-bang] to update the world during simulated movement.

During the extent of a big-bang, a form widget will appear in the
@tt{document.body} to allow you to manually send location-changing
events.

The optional @tech{event} argument will contain numbers for
@racket["latitude"] and @racket["longitude"].

}


@defproc[(on-location-change [location-f ([w world] [v view] [e event]? -> world)]) big-bang-handler]{
Tells @racket[big-bang] to update when the location changes, as
received by the
@link["http://dev.w3.org/geo/api/spec-source.html"]{Geolocation API}.

The optional @tech{event} argument will contain numbers for
@racket["latitude"] and @racket["longitude"].
}




@defproc[(to-draw [draw-f ([w world] [v view] -> view)]) big-bang-handler]{
Tells @racket[big-bang] how to update the rendering of the world.
}



@subsection{Views}

A @deftech{view} is a functional representation of the browser DOM
tree.  A view is always focused on an element, and the functions in
this subsection show how to traverse and manipulate the view.



@defproc[(->view [x any]) view]{

Coerse a value into a view whose focus is on the topmost element.
Common values for @racket[x] include @tech{resource}s.
}


@defproc[(view-focus [v view] [selector String]) view]{
Focuses the view on an element, given the @racket[selector].  The view
will be searched starting from the toplevelmost node.

Selectors are currently restricted to @litchar{#id} selectors for the
moment.
}


@defproc[(view-left [v view]) view]{
Move the focus to the previous sibling.
}
@defproc[(view-right [v view]) view]{
Move the focus to the next sibling.}

@defproc[(view-up [v view]) view]{
Move the focus to the parent.}

@defproc[(view-down [v view]) view]{
Move the view to the first child.}

@defproc[(view-text [v view]) string]{
Get the textual content at the focus.
}
@defproc[(update-view-text [v view] [s string]) view]{
Update the textual content at the focus.}

@defproc[(view-bind [v view] [type string] [world-updater ([w world] [v view]  [e event]? -> world)]) view]{
Attach a world-updating event to the focus.

Common event types include @racket["click"], @racket["trigger"],
@racket["hover"].}

@defproc[(view-show [v view]) view]{
Show the element at the focus.
}
@defproc[(view-hide [v view]) view]{
Hide the element at the focus.
}

@defproc[(view-attr [v view] [name String]) view]{
Get the attribute @racket[name] at the focus.
}

@defproc[(update-view-attr [v view] [name String] [value String]) view]{
Update the attribute @racket[n] with the value @racket[v] at the focus.
}

@defproc[(view-id [v view]) world]{
Get the unique identifier of the node at the focus.
}

@defproc[(view-form-value [v view]) view]{
Get the form value of the node at the focus.}

@defproc[(update-view-form-value [v view] [value String]) view]{
Update the form value of the node at the focus.}

@defproc[(view-append-child [d dom]) view]{
Add the dom node @racket[d] as the last child of the focused node.}
                    


@subsection{Events}
An @deftech{event} is a structure that holds name-value pairs.

@defstruct[event ([kvpairs (listof (list symbol value))])]{}

@defproc[(event-ref [evt event?] [name (or/c symbol string)]) value]{
Get an value from the event, given its @racket[name].
}

@defproc[(event-keys [evt event?]) (listof symbol)]{
Get an list of the event's keys.
}






@section{Including external resources}
@defmodule/this-package[resource]

Programs may need to use an external file @deftech{resource} that isn't
itself a Racket program, but instead some other kind of data.
Graphical programs will often use @filepath{.png}s, and web-related
programs @filepath{.html}s, for example.  Whalesong provides the
@racketmodname/this-package[resource] library to refer and use these
external resources.  When Whalesong compiles a program into a package,
these resources will be bundled alongside the JavaScript-compiled
output.

@defform[(define-resource id [path-string])]{
Defines a resource with the given path name.

For example,
@codeblock|{
#lang planet dyoo/whalesong
(require (planet dyoo/whalesong/resource))
(define-resource my-whale-image-resource "humpback.png")
}|
}
As a convenience, you can also write
@codeblock|{
#lang planet dyoo/whalesong
(require (planet dyoo/whalesong/resource))
(define-resource humpback.png)
}|
which defines a variable named @racket[humpback.png] whose
resource is @filepath{humpback.png}.



@defproc[(resource->url [a-resource resource?]) string?]{
Given a resource, gets a URL.

For example,
@codeblock|{
#lang planet dyoo/whalesong
(require (planet dyoo/whalesong/resource)
         (planet dyoo/whalesong/image))

(define-resource my-whale-image-resource "humpback.png")

(define WHALE-IMAGE
  (bitmap/url (resource->url my-whale-image-resource)))
}|

}


