#lang scribble/manual
@(require planet/scribble
          planet/version
          planet/resolver
          scribble/eval
          scribble/bnf
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

Racket 5.1.2 or greater is a prerequisite for Whalesong.  Brown CS
maintains its own installation of Racket 5.1.3 in
@filepath{/local/projects/racket/releases/5.1.3}.
To access it, you can add the following to your @filepath{.environment}:
@filebox[".environment"]{
@verbatim|{
pathprependifdir PATH "/local/projects/racket/releases/5.1.3/bin"    
}|}
Hopefully, this should already be configured to be the default for the @tt{cs19} group
by the time you read this.



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
@filepath{.xhtml} file.  If we open this file in our favorite web browser,
we should see a triumphant message show on screen.




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
@tt{document.body} to allow us to manually send location-changing
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



@defproc[(view-focus? [v view] [selector String]) boolean]{
Return true if the view can be focused using the given selector.

Selectors are currently restricted to @litchar{#id} selectors for the
moment.
}

@defproc[(view-focus [v view] [selector String]) view]{
Focuses the view on an element, given the @racket[selector].  The view
will be searched starting from the toplevelmost node.

Selectors are currently restricted to @litchar{#id} selectors for the
moment.
}


Once we have a view, we can refocus the view to different elements by using
@racket[view-left], @racket[view-right], @racket[view-up], and @racket[view-down].


@defproc[(view-left? [v view]) boolean]{
See if the view can be moved to the previous sibling.
}
@defproc[(view-left [v view]) view]{
Move the focus to the previous sibling.
}
@defproc[(view-right? [v view]) boolean]{
See if the view can be moved to the next sibling.
}
@defproc[(view-right [v view]) view]{
Move the focus to the next sibling.}

@defproc[(view-up? [v view]) boolean]{
See if the view can be moved to the parent.
}
@defproc[(view-up [v view]) view]{
Move the focus to the parent.}

@defproc[(view-down? [v view]) boolean]{
See if the view can be moved to the first child.
}
@defproc[(view-down [v view]) view]{
Move the view to the first child.}



Once we focus the view on an element, we can bind a world handler to it that
responds to events.
@defproc[(view-bind [v view] [type string] [world-updater ([w world] [v view]  [e event]? -> world)]) view]{
Attach a world-updating event to the focus.  When the world-updater is
called, the view will be focused on the element that triggered the
event.  Common event types include @racket["click"],
@racket["mouseenter"], @racket["change"].}



When the view is on an element that we'd like to query or update, we can use
several functions:

@defproc[(view-text [v view]) string]{
Get the textual content at the focus.
}

@defproc[(update-view-text [v view] [s string]) view]{
Update the textual content at the focus.}


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


@defproc[(view-css [v view] [name String]) view]{
Get the css value @racket[name] at the focus.
}

@defproc[(update-view-css [v view] [name String] [value String]) view]{
Update the css value @racket[n] with the value @racket[v] at the focus.
}


@defproc[(view-id [v view]) world]{
Get the unique identifier of the node at the focus.
}

@defproc[(view-form-value [v view]) view]{
Get the form value of the node at the focus.}

@defproc[(update-view-form-value [v view] [value String]) view]{
Update the form value of the node at the focus.}



@defproc[(view-append-child [d dom]) view]{
Add the dom node @racket[d] as the last child of the focused node.
Focus moves to the appended child.

Dom nodes can be created by using @racket[xexp->dom], which converts a
@tech{xexp} to a node.
}

@defproc[(view-remove [v view]) view]{
Remove the dom node at the focus from the view @racket[v].  Focus tries to move
to the right, if there's a next sibling.  If that fails, focus then
moves to the left, if there's a previous sibling.  If that fails too,
then focus moves to the parent.}





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



@section{Dynamic DOM generation with xexps} 
@declare-exporting/this-package[web-world]
We often need to dynamically inject new dom nodes into an existing
view.  As an example where the UI is entirely in code:
@codeblock|{
#lang planet dyoo/whalesong
(require (planet dyoo/whalesong/web-world))

;; tick: world view -> world
(define (tick world view)
  (add1 world))

;; draw: world view -> view
(define (draw world view)
  (view-append-child view
                     (xexp->dom `(p "hello, can you see this? "
                                    ,(number->string world)))))

(big-bang 0 (initial-view (xexp->dom '(html (head) (body (@ (id "body"))))))
            (on-tick tick 1)
            (to-draw draw))
}|

Normally, we'll want to do as much of the statics as possible with
@filepath{.html} resources, but when nothing else will do, we can
generate DOM nodes programmatically.



We can create new DOMs from an @tech{xexp}, which is a s-expression
representation for a DOM node.  Here are examples of expressions that
evaluate to xexps:

@racketblock["hello world"]

@racketblock['(p "hello, this" "is an item")]

@racketblock[
(local [(define name "josh")]
   `(p "hello" (i ,name)))]

@racketblock[
  '(div (\@ (id "my-div-0"))
        (span "This is a span in a div"))]

@racketblock[
  `(div (\@ ,(fresh-id))
        (span "This is another span in a div whose id is dynamically generated"))]


More formally, a @deftech{xexp} is:
@(let ([open @litchar{(}]
       [close @litchar{)}]
       [at @litchar[(symbol->string '\@)]])
@BNF[(list @nonterm{xexp}
           @nonterm{string}
           @BNF-seq[open @nonterm{id} @kleenestar[@nonterm{xexp}] close]
           @BNF-seq[open @nonterm{id} open at @kleenestar[@nonterm{key-value}] close @kleenestar[@nonterm{xexp}] close])
     (list @nonterm{key-value}
           @BNF-seq[open @nonterm{symbol} @nonterm{string} close])
])


To check to see if something is a xexp, use @racket[xexp?]:
@defproc[(xexp? [x any]) boolean]{
Return true if @racket[x] is a xexp.
}


@defproc[(xexp->dom [an-xexp xexp]) dom]{
Return a dom from the xexp.
}


When creating xexps, we may need to create unique ids for the nodes.
The web-world library provides a @racket[fresh-id] form to create these.
@defproc[(fresh-id) string]{
Return a string that can be used as a DOM node id.
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
As a convenience, we can also write
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

For a web-world program, output is normally done by using
@racket[to-draw].  However, side effecting functions, such as
@racket[printf] or @racket[display], are still available, and will
append to @tt{document.body}.

We may want to disable such printing or redirect it to a particular
element on the page.  For such purposes, use a combination of
@racket[current-output-port] and @racket[open-output-element] to
redirect the output of these side effect functions to somewhere else.

For example:
@codeblock|{
...
;; Redirect standard output to a div called "stdout-div".
(current-output-port (open-output-element "stdout-div"))
...
(big-bang ...
          (on-tick (lambda (world dom)
                     (begin
                       (printf "Tick!\n")
                       (add1 world))))
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
