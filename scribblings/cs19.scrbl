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
fermi ~/whalesong $ cd examples

fermi ~/whalesong/examples $ cat hello.rkt
#lang planet dyoo/whalesong

(display "hello world")
(newline)

fermi ~/whalesong/examples $ ../whalesong build hello.rkt 

fermi ~/whalesong/examples $ google-chrome hello.xhtml
Created new window in existing browser session.

fermi ~/whalesong/examples $ 
}|


@section{Examples}

There are examples in the
@link["https://github.com/dyoo/whalesong/tree/master/examples"]{@filepath{whalesong/examples}}
and
@link["https://github.com/dyoo/whalesong/tree/master/web-world/examples"]{@filepath{whalesong/web-world/examples}}.
Let's look at a few of them.



@subsection{Hello world}

Let's try making a simple, standalone executable.  At the moment, the
program must be written in the base language of @racket[(planet
dyoo/whalesong)].  This restriction unfortunately  prevents arbitrary
@racketmodname[racket/base] programs from compiling at the moment;
the developers (namely, dyoo) will be working to remove this
restriction as quickly as possible.


Write a @filepath{hello.rkt} with the following content
@filebox["hello.rkt"]{
@codeblock{
    #lang planet dyoo/whalesong
    (display "hello world")
    (newline)
}}
This program is a regular Racket program, and can be executed normally,
@verbatim|{
$ racket hello.rkt 
hello world
$
}|
However, it can also be packaged with @filepath{whalesong}.
@verbatim|{
    $ whalesong build hello.rkt

    $ ls -l hello.xhtml
    -rw-rw-r-- 1 dyoo nogroup 692213 Jun  7 18:00 hello.xhtml
}|
Running @tt{whalesong build} on a Racket program will produce a self-contained
@filepath{.xhtml} file.  If you open this file in your favorite web browser,
you should see a triumphant message show on screen.




@subsection{Tick tock}

Let's do something a little more interesting, and create a ticker that
counts on the screen.

The first thing we can do is mock up a web page with a user interface, like this.
@filebox["index.html"]{
@verbatim|{
<html>
  <head><title>My simple program</title></head>
  <body>
    <p>The current counter is: <span id="counter">fill-me-in</span></p>
  </body>
</html>
}|
}
We can even look at this in a standard web browser.

Once we're happy with the statics of our program, we can inject dynamic behavior.
Write a file called @filepath{tick-tock.rkt} with the following content.
@filebox["tick-tock.rkt"]{
@codeblock|{
#lang planet dyoo/whalesong
(require (planet dyoo/whalesong/web-world)
         (planet dyoo/whalesong/resource))

(define-resource index.html)

;; draw: world view -> view
(define (draw world dom)
  (update-view-text (view-focus dom "#counter") world))


;; tick: world view -> world
(define (tick world dom)
  (add1 world))


;; stop?: world view -> boolean
(define (stop? world dom)
  (> world 10))

(big-bang 0
          (initial-view index.html)
          (to-draw draw)
          (on-tick tick 1)
          (stop-when stop?))
}|
}

Several things are happening here.
@itemize[

@item{We @racket[require] a few libraries to get us some additional
behavior; in particular, @racketmodname/this-package[web-world] to let
us write event-driven web-based programs, and @racketmodname/this-package[resource]
to give us access to external @tech{resource}s.}

@item{We use @racket[define-resource] to refer to external files, like @filepath{index.html} that
we'd like to include in our program.}

@item{We use @racket[big-bang] to start up a computation that
responses to events.  In this example, that's clock ticks introduced
by @racket[on-tick], though because we're on the web, we can
bind to many other kinds of web events (by using @racket[view-bind]).}
]

The rest of this document describes the API.






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
Provide an initial view for the big-bang.  Normally, @racket[x] will be a @tech{resource}
to a web page.
@codeblock|{
...
(define-resource page1.html)
...
(big-bang ...
          (initial-view page1.html))
}|
}


@defproc[(stop-when [stop? ([w world] [dom view] ->  boolean)]) big-bang-handler]{
Tells @racket[big-bang] when to stop.
@codeblock|{
...
(define-struct world (given expected))
...

;; stop?: world view -> boolean
(define (stop? world dom)
  (string=? (world-given world) (world-expected world)))

(big-bang ...
          (stop-when stop?))
}|
}


@defproc*[(((on-tick [tick-f ([w world] [v view] [e event]? -> world)] [delay real]) big-bang-handler)
           ((on-tick [tick-f ([w world] [v view] [e event]? -> world)]) big-bang-handler))]{
Tells @racket[big-bang] to update the world during clock ticks.

By default, this will send a clock tick 28 times a second, but if
given @racket[delay], it will use that instead.
@codeblock|{
...
;; tick: world dom -> world
(define (tick world view)
  (add1 world))

(big-bang ...
          (on-tick tick 5)) ;; tick every five seconds
}|
}


@defproc[(on-mock-location-change [location-f ([w world] [v view] [e event]? -> world)]) big-bang-handler]{
Tells @racket[big-bang] to update the world during simulated movement.

During the extent of a big-bang, a form widget will appear in the
@tt{document.body} to allow you to manually send location-changing
events.

The optional @tech{event} argument will contain numbers for
@racket["latitude"] and @racket["longitude"].
@codeblock|{
...
;; move: world view event -> world
(define (move world dom event)
  (list (event-ref event "latitude")
        (event-ref event "longitude")))
...
(big-bang ...
          (on-mock-location-change move))
}|
}


@defproc[(on-location-change [location-f ([w world] [v view] [e event]? -> world)]) big-bang-handler]{
Tells @racket[big-bang] to update when the location changes, as
received by the
@link["http://dev.w3.org/geo/api/spec-source.html"]{Geolocation API}.

The optional @tech{event} argument will contain numbers for
@racket["latitude"] and @racket["longitude"].
@codeblock|{
...
;; move: world view event -> world
(define (move world dom event)
  (list (event-ref event "latitude")
        (event-ref event "longitude")))
...
(big-bang ...
          (on-location-change move))
}|
}




@defproc[(to-draw [draw-f ([w world] [v view] -> view)]) big-bang-handler]{
Tells @racket[big-bang] how to update the rendering of the world.  The draw
function will be called every time an event occurs.

@codeblock|{
...
(define-struct world (name age))

;; draw: world view -> view
(define (draw world dom)
  (update-view-text (view-focus dom "#name-span")
                    (world-name world)))
...
(big-bang ...
          (to-draw draw))
}|
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

Attach a world-updating event to the focus.  When the world-updater is
called, the view will be focused on the element that triggered the
event.

Common event types include @racket["click"], @racket["mouseenter"], @racket["change"].}

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
Whenever an event occurs in web-world, it may include some auxiliary
information about the event.  As a concrete example, location events
from @racket[on-location-change] and @racket[on-mock-location-change]
can send latitude and longitude values, as long as the world callback
can accept the event as an argument.


@defstruct[event ([kvpairs (listof (list symbol (or/c string number)))])]{}

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




@section{Tips and Tricks}
@subsection{Hiding standard output or directing it to an element}

@declare-exporting/this-package[web-world]

For a web-world program, output written by normal side effects such as
@racket[printf] or @racket[display] is still written to the current
output port, whose default behavior appends to the end of
@tt{document.body}.  You may want to either disable such printing or
direct the output to a particular element on the page.  For such
purposes, use a combination of @racket[current-output-port] and
@racket[open-output-element].

For example, in
@codeblock|{
...
(current-output-port (open-output-element "stdout-div"))
...
(big-bang ...
          (on-tick (lambda (world dom)
                     (printf "Tick!\n")
                     (add1 world)))
          ...)
}|

All subsequent I/O side effects after the call to
@racket[current-output-port] will be written out to the
@tt{stdout-div}, which can be easily styled with @tt{display: none} to
hide it from normal browser display.


@defproc[(open-output-element [id string]) output-port]{
Opens an output port that will be directed to write to the DOM element
whose id is @racket[id].  Note: writing to this port shouldn't fail,
even if the id does not currently exist on the page.
}