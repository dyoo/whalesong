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
          "scribble-helpers.rkt")

@(require (for-label (except-in (this-package-in cs019/cs019)
          ;; I am not documenting these: SK already documents them.
                                define:
                                Sig:
                                Number$ Boolean$
                                local
                                shared
                                printf display
                                define-struct define-struct:
                                define
                                if
                                format
                                string=? string?
                                image?
                                e
                                string
                                number->string
                                quasiquote
                                bitmap/url
                                symbol? symbol=?
                                current-output-port
                                lambda
                                true false
                                check-expect
                                ...)))

@(define-runtime-path whalesong-path "..")


@title{CS019 instructions for Whalesong}
@author+email["Danny Yoo" "dyoo@hashcollision.org"]



@section{Installation}

Racket 5.1.3 or greater is a prerequisite for Whalesong.  Brown CS
maintains its own installation of Racket 5.1.3 in
@filepath{/local/projects/racket/releases/5.1.3}.  This should already be in
your @litchar{PATH}.


If it isn't, you can add the following to your @filepath{.environment}:
@filebox[".environment"]{
@verbatim|{
pathprependifdir PATH "/local/projects/racket/releases/5.1.3/bin"    
}|}
But hopefully, this should already be configured to be the default for the @tt{cs019} group
by the time you read this.



Run the following to create the @filepath{whalesong} launcher program in
your current directory.
@codeblock|{
#lang racket/base
(require (planet dyoo/whalesong:1:8/make-launcher))
}|
This may take a few minutes, as Racket is compiling Whalesong, its
dependencies, and its documentation.  When it finally finishes,
you should see a @filepath{whalesong} launcher in the current
directory.

If you see the following error message during installation:
@verbatim|{
raco setup: error: during Building docs for ...scribblings/manual.scrbl
raco setup:   require: unknown module: 'program
}|
please ignore it: it is due to a bug in Racket's documentation
generator.


Finally, try executing the launcher.  If it is in the current directory, 
@verbatim|{
$ ./whalesong 
Usage: whalesong <subcommand> [option ...] <arg ...>
  where any unambiguous prefix can be used for a subcommand

The Whalesong command-line tool for compiling Racket to JavaScript

For help on a particular subcommand, use 'whalesong <subcommand> --help'
  whalesong version           Print the current version
  whalesong build             build a standalone html and javascript package
  whalesong get-runtime       print the runtime library to standard output
  whalesong get-javascript    Gets just the JavaScript code and prints it to standard output
}|
It should only take a few seconds to execute.


@subsection{Troubleshooting a slow launcher} 

A few students have been reporting that it takes minutes for Whalesong
to run.  This is unusually slow: it should only take a few seconds
(5-10 seconds) to perform any command.

There are two things that might factor into this kind of performance:
@itemize[
 @item{The network file system might be performing sluggishly.}

 @item{The Whalesong installation may not have completed successfully,
which would mean the launcher has to recompile Whalesong on each run.}
]

It's difficult to control the network file system.  The second
possibility, though, should never happen in an ideal world.  Still, if
installation was cancelled halfway in the process (by pressing
Control-C, for example), or if one of Whalesong's dependencies did not
finish compiling, it may account for Whalesong's slow performance.

If you are observing unusual slowness, please do the following at the
command line:

@verbatim|{
$ raco setup
}|

You will see a lot of stuff fly by the screen. The command forces a
re-installation of Whalesong and its dependencies, as well as any
other libraries in your Racket installation.  After this completes,
try using Whalesong again.



@section{Examples}

There are examples in the
@link["https://github.com/dyoo/whalesong/tree/master/examples"]{@filepath{whalesong/examples}}
and
@link["https://github.com/dyoo/whalesong/tree/master/web-world/examples"]{@filepath{whalesong/web-world/examples}}.
Let's look at a few of them.



@subsection{Hello world}

Let's try making a simple, standalone executable.  At the moment, the
program should be written in the base language of @racket[(planet
dyoo/whalesong/cs019)], as it provides the language features that
you've been using in cs019 (@racket[local], @racket[shared], etc...),
as well as support for the @racketmodname/this-package[web-world]
package described later in this document.


Write a @filepath{hello.rkt} with the following content
@filebox["hello.rkt"]{
@codeblock{
    #lang planet dyoo/whalesong/cs019
    "hello world"
}}
This program is a regular Racket program, and can be executed normally,
@verbatim|{
$ racket hello.rkt 
"hello world"
$
}|
However, it can also be packaged with @filepath{whalesong}.
@verbatim|{
    $ whalesong build hello.rkt
    Writing program #<path:/home/dyoo/work/whalesong/examples/hello.js>
    Writing resource #<path:/gpfs/main/home/dyoo/work/whalesong/examples/excanvas.js>
    Writing resource #<path:/gpfs/main/home/dyoo/work/whalesong/examples/canvas.text.js>
    Writing resource #<path:/gpfs/main/home/dyoo/work/whalesong/examples/optimer-normal-normal.js>
    Writing html #<path:/gpfs/main/home/dyoo/work/whalesong/examples/hello.html>
    Writing manifest #<path:/gpfs/main/home/dyoo/work/whalesong/examples/hello.appcache>

    $ ls -l hello.html
    -rw-r--r-- 1 dyoo dyoo 3817 2011-09-10 15:02 hello.html
    $ ls -l hello.js
    -rw-r--r-- 1 dyoo dyoo 1146428 2011-09-10 15:02 hello.js
}|

Running @tt{whalesong build} will produce @filepath{.html} and
@filepath{.js} files.  If we open this file in our favorite web
browser, we should see a triumphant message show on screen.


There are several other files generated as part of the application
besides the main @filepath{.html} and the @filepath{.js}.  Several of
these files provide Internet Explorer compatibility and should be
included during distribution.  The @filepath{.appcache} file, too,
should be included, as it catalogs the files in the application, and is
used to enable offline
@link["http://www.w3.org/TR/html5/offline.html"]{HTML application
support}.



@subsection{Using the appcache}

The applications generated with Whalesong are intended to work even
when you're not connected to a network.  When uploading your app with
the @tt{cs019-upload} script, don't forget to upload the
@filepath{appcache} file along with the other files.  Once you visit
the application's page on your phone, the application files will be
cached locally.  Bookmark the page, turn off Wi-Fi, and revisit the
page again: the application should continue to work.




@subsection{Tick tock}

Let's do something a little more interesting and create a ticker that
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
#lang planet dyoo/whalesong/cs019

(define-resource index.html)


(define: (draw [world : Number$] [dom : View$]) -> View$
  (update-view-text (view-focus dom "counter") world))

(define: (tick [world : Number$] [dom : View$]) -> Number$
  (add1 world))

(define: (stop? [world : Number$] [dom : View$]) -> Boolean$
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
@item{We're using the signature support in the cs019 language as discussed in
@url{http://www.cs.brown.edu/courses/cs019/2011/software/doc}.}

@item{We use @racket[define-resource] to refer to external
@tech{resource} files, like @filepath{index.html} that we'd like to
include in our program.}

@item{
Whalesong includes a world library for doing event-driven programs.
As you may have seen earlier, we use @racket[big-bang] to start up a
computation that responses to events.  In this example, that's clock
ticks introduced by @racket[on-tick].

However, because we're on the web, we can bind to many other kinds of
web events (by using @racket[view-bind]).  Each of our callbacks also
consumes the DOM as an argument, since the DOM, too, is a source of
external state in a program.}]


This program cannot be executed directly in Racket/DrRacket,
unfortunately, but it can be compiled through Whalesong and @link["http://hashcollision.org/whalesong/examples/cs019/tick-tock/tick-tock.html"]{run in the
browser}.



@subsection{Where am I?}

@margin-note{The resource used is: @link["http://hashcollision.org/whalesong/examples/cs019/where-am-i/index.html"]{@filepath{index.html}}.}
Finally, let's look at a program that displays our current geolocation.

@filebox["where-am-i.rkt"]{
@codeblock|{
#lang planet dyoo/whalesong/cs019

(define-resource index.html)

(define-struct: coord ([lat : Number$]
                       [lng : Number$]))

;; coord/unknown?: any -> boolean
;; Returns true if x is a coord or the symbol 'unknown.
(define (coord/unknown? x)
  (or (coord? x)
      (and (symbol? x)
           (symbol=? x 'unknown))))

(define Coord/Unknown$ (Sig: coord/unknown?))
  

;; The world stores both the real location, as well as a mocked-up
;; one.
(define-struct: world ([real : Coord/Unknown$]
                       [mock : Coord/Unknown$]))
(define World$ (Sig: world?))



(define: (location-change [world : World$]
                          [dom : View$]
                          [evt : Event$]) -> World$
  (make-world (make-coord (event-ref evt "latitude")
                          (event-ref evt "longitude"))
              (world-mock world)))


(define: (mock-location-change [world : World$]
                               [dom : View$]
                               [evt : Event$]) -> World$
  (make-world (world-real world)
              (make-coord (event-ref evt "latitude")
                          (event-ref evt "longitude"))))


(define: (draw [world : World$] [dom : View$]) -> View$
  (local [(define v1 
            (if (coord? (world-real world))
                (update-view-text
                 (view-focus dom "real-location")
                 (format "lat=~a, lng=~a"
                         (coord-lat (world-real world))
                         (coord-lng (world-real world))))
                dom))
          (define v2
            (if (coord? (world-mock world))
                (update-view-text
                 (view-focus v1 "mock-location")
                 (format "lat=~a, lng=~a"
                         (coord-lat (world-mock world))
                         (coord-lng (world-mock world))))
                v1))]
         v2))


(big-bang (make-world 'unknown 'unknown)
          (initial-view index.html)
          (to-draw draw)
          (on-location-change location-change)
          (on-mock-location-change mock-location-change))
}|
}

@link["http://hashcollision.org/whalesong/examples/cs019/where-am-i/where-am-i.html"]{This program} uses @racket[on-location-change], which uses HTML5's
Geolocation support to provide latitude and longitude information.  We
receive a change to our location in the form of an @tech{event}.  To
make it easier to test programs that depend on Geolocation, a
@racket[on-mock-location-change] provides the same interface, but
the location can be entered from a form in the browser window.



@subsection{More web-world examples}
Here are more examples of web-world demos, to get a feel for the library:
@itemize[
@item{@link["http://hashcollision.org/whalesong/examples/attr-animation/attr-animation.html"]{attr-animation.html} [@link["http://hashcollision.org/whalesong/examples/attr-animation/attr-animation.rkt"]{src}]  Uses @racket[update-view-attr] and @racket[on-tick] to perform a simple color animation.}

@item{@link["http://hashcollision.org/whalesong/examples/boid/boid.html"]{boid.html} [@link["http://hashcollision.org/whalesong/examples/boid/boid.rkt"]{src}]  Uses @racket[update-view-css] and @racket[on-tick] to perform an animation of a flock of @link["http://en.wikipedia.org/wiki/Boids"]{boids}.}


@item{@link["http://hashcollision.org/whalesong/examples/dwarves/dwarves.html"]{dwarves.html}
[@link["http://hashcollision.org/whalesong/examples/dwarves/dwarves.rkt"]{src}]
Uses @racket[view-show] and @racket[view-hide] to manipulate a view.  Click on a dwarf to make them hide.
  }

@item{@link["http://hashcollision.org/whalesong/examples/dwarves-with-remove/dwarves-with-remove.html"]{dwarves-with-remove.html}
[@link["http://hashcollision.org/whalesong/examples/dwarves-with-remove/dwarves-with-remove.rkt"]{src}]
Uses @racket[view-focus?] and @racket[view-remove] to see if a dwarf should be removed from the view.
}

@item{@link["http://hashcollision.org/whalesong/examples/field/field.html"]{field.html}
[@link["http://hashcollision.org/whalesong/examples/field/field.rkt"]{src}]
Uses @racket[view-bind] to read a text field, and @racket[update-view-text] to change
the text content of an element.
}

@item{@link["http://hashcollision.org/whalesong/examples/phases/phases.html"]{phases.html}
[@link["http://hashcollision.org/whalesong/examples/phases/phases.rkt"]{src}]
Switches out one view entirely in place of another.  Different views can correspond to phases in a program.
}


@item{@link["http://hashcollision.org/whalesong/examples/tick-tock/tick-tock.html"]{tick-tock.html}
[@link["http://hashcollision.org/whalesong/examples/tick-tock/tick-tock.rkt"]{src}]
Uses @racket[on-tick] to show a timer counting up.
}

@item{@link["http://hashcollision.org/whalesong/examples/redirected/redirected.html"]{redirected.html}
[@link["http://hashcollision.org/whalesong/examples/redirected/redirected.rkt"]{src}]
Uses @racket[on-tick] to show a timer counting up, and also uses @racket[open-output-element] to
pipe side-effecting @racket[printf]s to a hidden @tt{div}.
}

@item{@link["http://hashcollision.org/whalesong/examples/todo/todo.html"]{todo.html}
[@link["http://hashcollision.org/whalesong/examples/todo/todo.rkt"]{src}]
A simple TODO list manager.
}

@item{@link["http://hashcollision.org/whalesong/examples/where-am-i/where-am-i.html"]{where-am-i.html}
[@link["http://hashcollision.org/whalesong/examples/where-am-i/where-am-i.rkt"]{src}]
Uses @racket[on-location-change] and @racket[on-mock-location-change] to demonstrate location services.
}


@item{@link["http://hashcollision.org/whalesong/examples/hot-cross-buns/hot-cross-buns.html"]{hot-cross-buns.html}
[@link["http://hashcollision.org/whalesong/examples/hot-cross-buns/hot-cross-buns.rkt"]{src}]
Demonstrates use of checkboxes.  Uses @racket[view-has-attr?] to see if a checkbox has been
checked, and @racket[remove-view-attr] to change the @emph{checked} attribute when the user
wants to reset the page.
}
]

These examples are written in a less featureful language level
(@litchar{#lang planet dyoo/whalesong}), which is why it uses explicit
@racket[require] statements to pull in support for
@racketmodname/this-package[web-world] and
@racketmodname/this-package[resource].  As long as you use
@litchar{#lang planet dyoo/whalesong/cs019}, you shouldn't need to
require those particular libraries.


@section{API}

@declare-exporting/this-package[cs019/cs019]

The majority of the functions and forms provided in the cs019 language
should be those of the official @link["http://www.cs.brown.edu/courses/csci0190/2011/software/doc"]{cs019} language.  There are a few deviations documented in @secref["bugs"].


For the purposes of tour-guide, we'll be focusing on the
@racketmodname/this-package[web-world] library in Whalesong.

Like the big-bang in regular world, the callbacks in web-world are
world-to-world functions.  One difference introduced by the web is the
web page itself: because the page itself is a source of state, it too
will be passed to callbacks.  This library presents a functional
version of the DOM in the form of a @tech{view}.

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


If both the @racket[initial-view] and @racket[to-draw] are omitted
from a @racket[big-bang], then @racket[big-bang] will render the world
value itself.
}


@defproc[(stop-when [stop? ([w world] [dom view] ->  boolean)]) big-bang-handler]{
Tells @racket[big-bang] when to stop.

For example,
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

will stop the computation as soon as @racket[stop?] returns @racket[true].
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
  (update-view-text (view-focus dom "name-span")
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


@defproc[(view? [x any]) boolean]{
Produces true if x is a @tech{view}.}

@defthing[View$ Sig]{The signature of a view.}

@defproc[(->view [x any]) view]{

Coerse a value into a view whose focus is on the topmost element.
Common values for @racket[x] include @tech{resource}s.
}



@defproc[(view-focus? [v view] [id String]) boolean]{
Return true if the view can be focused onto an element in the view
with the given id.}

@defproc[(view-focus [v view] [id String]) view]{
Focuses the view on an element, given the @racket[id].
}


Once we have a view, we can refocus the view using
@racket[view-focus], or traverse the view locally by using
@racket[view-left], @racket[view-right], @racket[view-up], and
@racket[view-down].


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
Hide the element at the focus.  The element will continue to exist
in the tree, but not be shown.
}


@defproc[(view-attr [v view] [name String]) view]{
Get the attribute @racket[name] at the focus.
}

@defproc[(view-has-attr? [v view] [name String]) boolean]{
Returns true if the element at the focus has an attribute @racket[name].
}

@defproc[(update-view-attr [v view] [name String] [value String]) view]{
Update the attribute @racket[name] with the value @racket[value] at the focus.
}

@defproc[(remove-view-attr [v view] [name String]) view]{
Remove the attribute @racket[name] at the focus.
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
@tech{xexp} to a node.  Furthermore, values can be treated as DOM
nodes whose DOM representation will be the way they print.  This
including images.}



@defproc[(view-insert-left [v view] [d dom]) view]{
Add the dom node @racket[d] as the previous sibling of the focused node.
Focus moves to the inserted node.}

@defproc[(view-insert-right [v view] [d dom]) view]{
Add the dom node @racket[d] as the next sibling of the focused node.
Focus moves to the inserted node.}



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

You can construct events for testing purposes by using @racket[make-event].
@codeblock|{
#lang planet dyoo/whalesong/cs019
;; Synthesizing a location event
(define my-event (make-event '((latitude 41) 
                               (longitude -71))))
(check-expect (event-ref my-event "latitude") 41)
(check-expect (event-ref my-event "longitude") -71)
}|


@defthing[Event$ sig]{The signature for an event.}

@defproc[(event-ref [evt event?] [name (or/c symbol string)]) value]{
Get an value from the event, given its @racket[name].
}

@defproc[(event-keys [evt event?]) (listof symbol)]{
Get an list of the event's keys.
}



@section{Dynamic DOM generation with xexps} 
@declare-exporting/this-package[cs019/cs019]
We often need to dynamically inject new dom nodes into an existing
view.  As an example where the UI is entirely in code:
@codeblock|{
#lang planet dyoo/whalesong/cs019

;; tick: world view -> world
(define (tick world view)
  (add1 world))

;; draw: world view -> view
(define (draw world view)
  (view-append-child view
                     (xexp->dom `(p "hello, can you see this? "
                                    ,(number->string world)))))

(big-bang 0 (initial-view
             (xexp->dom '(html (head) (body))))
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
@declare-exporting/this-package[cs019/cs019]

Programs may need to use an external file @deftech{resource} that isn't
itself a Racket program, but instead some other kind of data.
Graphical programs will often use @filepath{.png}s, and web-related
programs @filepath{.html}s, for example.  Whalesong provides the
@racketmodname/this-package[resource] library to refer and use these
external resources.  When Whalesong compiles a program into a package,
these resources will be bundled alongside the JavaScript-compiled
output.

@defform[(define-resource id [path-string])]{
Defines a resource with the given path name.  The path is relative
to the program.

For example,
@codeblock|{
#lang planet dyoo/whalesong/cs019
(define-resource my-whale-image-resource "humpback.png")
}|
}
Since the name we're using will often match the filename itself,
as a convenience, we can also write the following:
@codeblock|{
#lang planet dyoo/whalesong/cs019
(define-resource humpback.png)
}|
which defines a variable named @racket[humpback.png] whose
resource is @filepath{humpback.png}.


If the resource given has an extension one of the following:
@itemize[
@item{@filepath{.png}}
@item{@filepath{.gif}}
@item{@filepath{.jpg}}
@item{@filepath{.jpeg}}]
then the resource is also an image for which @racket[image?] will be true.

If the resource has the extension @filepath{.html}, then it will be
run through an HTML purifying process to make sure the HTML is
well-formed.

@defproc[(resource? [x any]) boolean]{
Produces true if @racket[x] is a resource.
}

@defthing[Resource$ sig]{
The signature of a resource.
}



@defproc[(resource->url [a-resource resource?]) string?]{
Given a resource, gets its URL.

For example,
@codeblock|{
#lang planet dyoo/whalesong/cs019

(define-resource my-whale-image-resource "humpback.png")

(define WHALE-IMAGE
  (bitmap/url (resource->url my-whale-image-resource)))
}|

This particular example is somewhat redundant, because image resources
behave already as images.

}




@section{Tips and tricks}
@subsection{Hiding standard output or directing it to an element}

@declare-exporting/this-package[cs019/cs019]

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


@subsection{Shrink final versions of programs}
The JavaScript files generated by Whalesong can be compressed by using
 the @tt{@"-"}@tt{@"-"compress@"-"javascript} option during @tt{build}.

For example:
@verbatim|{
$ whalesong build pacman.rkt
[omitting output]

$ ls -l pacman.js
-rw-rw-r-- 1 dyoo nogroup 1181979 Nov  9 15:19 pacman.js

$ whalesong build --compress-javascript pacman.rkt
[omitting output]
$ ls -l pacman.js
-rw-rw-r-- 1 dyoo nogroup 782199 Nov  9 15:19 pacman.js
}|

Compression takes some time, so it is not turned on by default.  It
may be useful when deploying a finished program.



@section[#:tag "bugs"]{Known bugs and deviations}

@itemize[

@item{Whalesong's compiler doesn't know how to compile programs with
embedded image snips.  You can work around this by using
@racket[define-resource], and save an image file in the same directory
as your source.}


@item{The printing of @racket[shared] structures currently doesn't use
the same syntax as in the standard cs019 language.  For example,
@racket[(shared ([a (cons 1 a)]) a)] prints in Whalesong as @litchar{#0=(cons 1 #0#)}.}

]



@subsection{Deviations from the regular cs019 language}

Most of the basic cs019 forms have been implemented in Whalesong's
implementation, but some functions and syntax have not yet been
implemented.  Here is the complete list of deviations:
@verbatim|{
Misc
===
exit
sleep
delay
promise
force
time
identity
state
match
Char
name
module-begin
define-datatype
recur


Testing
===
check-range
check-error
check-within
check-member-of
check-property
check-with
equal~?
cons-of
=~

Lists
===
caaar
caadr
cdar
cddr
cadar
cdaar
cdadr
cddar
cadddr
caddr
cdddr
sort
quicksort

IO
===
with-output-to-file
with-input-from-file
with-output-from-string
with-output-to-string
read
print
pretty-print


Hashes
===
hash-iterate-first
hash-iterate-key
hash-iterate-next
hash-iterate-value
hash-update
hash-ref!
hash-update!


Images & Universe
===
register
universe
animate
run-simulation
run-movie
LOCALHOST
on-receive
on-mouse
on-new
on-msg
on-disconnect
on-key
on-release
stop-with
on-draw
make-bundle
iworld-name
iworld?
make-package
package?
bundle?
mail?
iworld=?
make-mail
iworld1
iworld2
iworld3
sexp?
launch-many-worlds
mouse-event?
record?
bitmap
key-event?
key=?
Key$
image=?
pen
pen-color
pen-style
pen-cap
pen-join
make-pen
pen-width
color-list->bitmap
pinhole-x
pinhole-y
center-pinhole
put-pinthole
clear-pinhole
freeze
save-image
overlay/pinhole
underlay/pinhole
overlay/offset
overlay/align/offset
underlay/align/offset
underlay/offset
pen-cap?
real-valued-posn?
pen-join?
pen-style?
scene+curve
triangle/asa
triangle/sas
triangle/aas
triangle/ssa
triangle/ass
triangle/sss
triangle/saa
polygon
empty-image
add-curve
to-string
}|