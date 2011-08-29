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

There are a few examples in the @filepath{whalesong/examples} directory.




@section{API}

@defmodule/this-package[web-world]
For the purposes of tour-guide, we'll be focusing on the
@racketmodname/this-package[web-world] library in Whalesong.





@defproc[(big-bang) world]{
Start a big bang computation.
}
@defproc[(initial-view) big-bang-handler]{
Provide an initial view for the big-bang.}
@defproc[(stop-when) big-bang-handler]{
Tells @racket[big-bang] the predicate for terminating.
}
@defproc[(on-tick) big-bang-handler]{
Tells @racket[big-bang] to update the world during clock ticks.
}
@defproc[(to-draw) big-bang-handler]{
Tells @racket[big-bang] how to update the rendering of the world.
}


@defproc[(->view [x any]) view]{
Coerse a value into a view.  Common values for @racket[x] include resources.
}

@defproc[(view-focus [v view] [selector String]) view]{
Focuses the view on an element, given the @racket[selector].

Selectors are currently restricted to @litchar{#id} selectors for now.
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
Get the string content at the focus.
}
@defproc[(update-view-text) view]{
Set the string content at the focus.}

@defproc[(view-bind [v view] [type string] [world-updater world-updater]) view]{
Attach a world-updating event.

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
                    












@section{Including external resources}
@defmodule/this-package[resource]

Programs may need to use external file resources that aren't
themselves Racket programs, but instead some other kind of data.
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

