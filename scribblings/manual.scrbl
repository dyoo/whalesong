#lang scribble/manual
@(require planet/scribble
          planet/version
          planet/resolver
          scribble/eval
          scribble/bnf
          racket/sandbox
          racket/port
          racket/list
          (only-in racket/contract any/c)
          racket/runtime-path
          "scribble-helpers.rkt")


@(require racket/runtime-path)
@(define-runtime-path git-head-path "../.git/refs/heads/master")


@(require (for-label (this-package-in js)
                     (this-package-in js/world))
          (for-label (except-in (this-package-in lang/base)
                                string?
                                printf
                                number->string
                                void
                                quasiquote
                                string=?
                                string
                                e
                                number?
                                newline
                                current-output-port
                                display))
          (for-label (this-package-in resource)
          (for-label (this-package-in web-world))))



@inject-javascript-inline|{
  var _gaq = _gaq || [];
  _gaq.push(['_setAccount', 'UA-24146890-1']);
  _gaq.push(['_trackPageview']);
 
  (function() {
    var ga = document.createElement('script'); ga.type = 'text/javascript'; ga.async = true;
    ga.src = ('https:' == document.location.protocol ? 'https://ssl' : 'http://www') + '.google-analytics.com/ga.js';
    var s = document.getElementsByTagName('script')[0]; s.parentNode.insertBefore(ga, s);
  })();      
}|


@inject-javascript-src{http://hashcollision.org/whalesong/examples/runtime.js}


@(define-runtime-path whalesong-path "..")


@;; I may need an evaluator for some small examples.
@(define my-evaluator
   (call-with-trusted-sandbox-configuration 
    (lambda ()
      (parameterize ([sandbox-output 'string]
                     [sandbox-error-output 'string])
        (make-evaluator 'racket)))))



@title{Whalesong: a Racket to JavaScript compiler}
@author+email["Danny Yoo" "dyoo@hashcollision.org"]



@centered{@smaller{Source code can be found at:
@url{https://github.com/dyoo/whalesong}.  The latest version of this
document lives in @url{http://hashcollision.org/whalesong}.}}

@(if (file-exists? git-head-path)
     (let ([git-head (call-with-input-file git-head-path port->string)])
       @centered{@smaller{Current commit head is @tt{@git-head}.}})
     "")



@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@section{Introduction}
@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

Whalesong is a compiler from Racket to JavaScript; it takes Racket
programs and translates them so that they can run stand-alone on a
user's web browser.  It should allow Racket programs to run with
(hopefully!) little modification, and provide access through the
foreign-function interface to native JavaScript APIs.  The included
runtime library supports the numeric tower, an image library, and a
framework to program the web in functional event-driven style.


The GitHub source repository to Whalesong can be found at
@url{https://github.com/dyoo/whalesong}.  If you have any questions or
comments, please feel free to use the
@link["http://lists.racket-lang.org/users/"]{Racket-users} mailing
list.


Prerequisites: at least @link["http://racket-lang.org/"]{Racket
5.1.1}.  If you wish to use the JavaScript compression option,
 you will need @link["http://www.java.com"]{Java 1.6} SDK.
      @; (This might be superfluous information, so commented out
      @;  for the moment...)
      @;The majority of the project is written
      @;@link["http://docs.racket-lang.org/ts-guide/index.html"]{Typed
      @;Racket}, and Racket 5.1.1 and above provides the support necessary to
      @;compile Whalesong; otherwise, compilation may take an unusual amount
      @;of time.



@subsection{Examples}
Here are a collection of programs that use the @emph{web-world} library described
later in this document:
@itemize[

@item{@link["http://hashcollision.org/whalesong/examples/raphael-demo/raphael-demo.html"]{raphael-demo.html}
[@link["http://hashcollision.org/whalesong/examples/raphael-demo/raphael-demo.rkt"]{src}]
Uses features of the JavaScript FFI to access the @link["http://raphaeljs.com/"]{RaphaelJS} vector graphics library.
}

@item{@link["http://hashcollision.org/whalesong/examples/google-maps/google-maps.html"]{google-maps.html}
[@link["http://hashcollision.org/whalesong/examples/google-maps/google-maps.rkt"]{src}
@link["http://hashcollision.org/whalesong/examples/google-maps/maps-lib.rkt"]{library src}]
Uses features of the JavaScript FFI (@racket[js-function->procedure],
@racket[js-async-function->procedure],
@racket[make-world-event-handler]) to treat a Google Maps instance as a
source of events for a World program.  Click on the map, and the coordinate stored in the world
should synchronize with the clicked location.}


@item{@link["http://hashcollision.org/whalesong/examples/attr-animation/attr-animation.html"]{attr-animation.html} 
[@link["http://hashcollision.org/whalesong/examples/attr-animation/attr-animation.rkt"]{src} 
 @link["http://hashcollision.org/whalesong/examples/attr-animation/view.html"]{view.html}
 @link["http://hashcollision.org/whalesong/examples/attr-animation/style.css"]{style.css}]
Uses @racket[update-view-attr] and @racket[on-tick] to perform a simple color animation.}


@item{
@link["http://hashcollision.org/whalesong/examples/color-buttons/color-buttons.html"]{color-buttons.html}
[@link["http://hashcollision.org/whalesong/examples/color-buttons/color-buttons.rkt"]{src}
 @link["http://hashcollision.org/whalesong/examples/color-buttons/view.html"]{view.html}]
Uses @racket[view-bind-many] to bind several events at once.  Clicking on a button should
change the color of the header by adjusting its CSS @tt{color} attribute.
}


@item{@link["http://hashcollision.org/whalesong/examples/boid/boid.html"]{boid.html} 
[@link["http://hashcollision.org/whalesong/examples/boid/boid.rkt"]{src}
 @link["http://hashcollision.org/whalesong/examples/boid/view.html"]{view.html}]  Uses @racket[update-view-css] and @racket[on-tick] to perform an animation of a flock of @link["http://en.wikipedia.org/wiki/Boids"]{boids}.}


@item{@link["http://hashcollision.org/whalesong/examples/dwarves/dwarves.html"]{dwarves.html}
[@link["http://hashcollision.org/whalesong/examples/dwarves/dwarves.rkt"]{src}
 @link["http://hashcollision.org/whalesong/examples/dwarves/view.html"]{view.html}]
Uses @racket[view-show] and @racket[view-hide] to manipulate a view.  Click on a dwarf to make them hide.
  }

@item{@link["http://hashcollision.org/whalesong/examples/dwarves-with-remove/dwarves-with-remove.html"]{dwarves-with-remove.html}
[@link["http://hashcollision.org/whalesong/examples/dwarves-with-remove/dwarves-with-remove.rkt"]{src}
 @link["http://hashcollision.org/whalesong/examples/dwarves-with-remove/view.html"]{view.html}]
Uses @racket[view-focus?] and @racket[view-remove] to see if a dwarf should be removed from the view.
}

@item{@link["http://hashcollision.org/whalesong/examples/field/field.html"]{field.html}
[@link["http://hashcollision.org/whalesong/examples/field/field.rkt"]{src}
 @link["http://hashcollision.org/whalesong/examples/field/view.html"]{view.html}]
Uses @racket[view-bind] to read a text field, and @racket[update-view-text] to change
the text content of an element.
}

@item{@link["http://hashcollision.org/whalesong/examples/phases/phases.html"]{phases.html}
[@link["http://hashcollision.org/whalesong/examples/phases/phases.rkt"]{src}
@link["http://hashcollision.org/whalesong/examples/phases/view1.html"]{view1.html}
@link["http://hashcollision.org/whalesong/examples/phases/view2.html"]{view2.html}]
Switches out one view entirely in place of another.  Different views can correspond to phases in a program.
}


@item{@link["http://hashcollision.org/whalesong/examples/tick-tock/tick-tock.html"]{tick-tock.html}
[@link["http://hashcollision.org/whalesong/examples/tick-tock/tick-tock.rkt"]{src}
 @link["http://hashcollision.org/whalesong/examples/tick-tock/view.html"]{view.html}]
Uses @racket[on-tick] to show a timer counting up.
}

@item{@link["http://hashcollision.org/whalesong/examples/redirected/redirected.html"]{redirected.html}
[@link["http://hashcollision.org/whalesong/examples/redirected/redirected.rkt"]{src}
 @link["http://hashcollision.org/whalesong/examples/redirected/view.html"]{view.html}]
Uses @racket[on-tick] to show a timer counting up, and also uses @racket[open-output-element] to
pipe side-effecting @racket[printf]s to a hidden @tt{div}.
}

@item{@link["http://hashcollision.org/whalesong/examples/todo/todo.html"]{todo.html}
[@link["http://hashcollision.org/whalesong/examples/todo/todo.rkt"]{src}
 @link["http://hashcollision.org/whalesong/examples/todo/view.html"]{view.html}]
A simple TODO list manager.
}

@item{@link["http://hashcollision.org/whalesong/examples/where-am-i/where-am-i.html"]{where-am-i.html}
[@link["http://hashcollision.org/whalesong/examples/where-am-i/where-am-i.rkt"]{src}
 @link["http://hashcollision.org/whalesong/examples/where-am-i/view.html"]{view.html}]
Uses @racket[on-location-change] and @racket[on-mock-location-change] to demonstrate location services.
}


@item{@link["http://hashcollision.org/whalesong/examples/hot-cross-buns/hot-cross-buns.html"]{hot-cross-buns.html}
[@link["http://hashcollision.org/whalesong/examples/hot-cross-buns/hot-cross-buns.rkt"]{src}
 @link["http://hashcollision.org/whalesong/examples/hot-cross-buns/view.html"]{view.html}]
Demonstrates use of checkboxes.  Uses @racket[view-has-attr?] to see if a checkbox has been
checked, and @racket[remove-view-attr] to change the @emph{checked} attribute when the user
wants to reset the page.
}
]


I also gave a 
@link["http://hashcollision.org/whalesong/racketcon"]{presentation}
of Whalesong at RacketCon 2011, including examples like:
@itemize[

@item{@link["http://hashcollision.org/whalesong/racketcon/rain.html"]{rain.html}
[@link["http://hashcollision.org/whalesong/racketcon/rain.rkt"]{src}]
Uses the image libraries to show droplets of water falling down.}

@item{@link["http://hashcollision.org/whalesong/racketcon/pacman.html"]{pacman.html}
[@link["http://hashcollision.org/whalesong/racketcon/pacman.rkt"]{src}]
Pacman.}
]






@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@section{Getting started}
@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


@subsection{Installing Whalesong}



Before you begin, if you are using DrRacket,
@itemize[#:style 'ordered
        @item{Please go to the Racket submenu.}
        @item{Select the Limit Memory item.}
        @item{Change the setting to Unlimited.}]
This is to avoid an installation-time issue that prevents
Whalesong from fully compiling.


If you want to use Whalesong, run the following to create
the @filepath{whalesong} launcher:
@codeblock|{
#lang racket/base
(require (planet dyoo/whalesong:1:21/make-launcher))
}|

This may take a few minutes, as Racket is compiling Whalesong, its
dependencies, and its documentation.  When it finally finishes,
you should see a @filepath{whalesong} launcher in the current
directory.

You should also see a @filepath{whalesong-gui} launcher that includes
a minimal graphical user interface.






At this point, you should be able to run the @filepath{whalesong} executable from the command line.
@verbatim|{
$ ./whalesong
Usage: whalesong <subcommand> [option ...] <arg ...>
  where any unambiguous prefix can be used for a subcommand

The Whalesong command-line tool for compiling Racket to JavaScript

For help on a particular subcommand, use 'whalesong <subcommand> --help'
  whalesong build             build a standalone html and javascript package
  whalesong get-runtime       print the runtime library to standard output
  whalesong get-javascript    Gets just the JavaScript code and prints it to standard output
           }|
and if this does appear, then Whalesong should be installed successfully.


To repeat: whenever Whalesong's source code is updated from Github,
please re-run the @tt{raco setup} step.  Otherwise, Racket will try to
recompile Whalesong on every single use, which can be very expensive.





@subsection{Making @tt{.html} files with Whalesong}

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
    Writing program #<path:/home/dyoo/work/whalesong/examples/hello.js>
    Writing html #<path:/home/dyoo/work/whalesong/examples/hello.html>

    $ ls -l hello.html
    -rw-r--r-- 1 dyoo dyoo 3817 2011-09-10 15:02 hello.html
    $ ls -l hello.js
    -rw-r--r-- 1 dyoo dyoo 841948 2011-09-10 15:02 hello.js

}|

@margin-note{Visit @link["http://hashcollision.org/whalesong/examples/hello/hello.html"]{hello.html} to execute this program.}
Running @tt{whalesong build} on a Racket program will produce a
@filepath{.html} and @filepath{.js} file.  If you open the
@filepath{.html} in your favorite web browser, you should see a
triumphant message show on screen.


We can do something slightly more interesting.  Let's write a Whalesong program
that accesses the JavaScript DOM.  Call this file @filepath{dom-play.rkt}.
@margin-note{
Visit @link["http://hashcollision.org/whalesong/examples/dom-play/dom-play.html"]{dom-play.html} to execute this program.}

@filebox["dom-play.rkt"]{
@codeblock|{
#lang planet dyoo/whalesong

;; Uses the JavaScript FFI, which provides bindings for:
;; $ and call-method
(require (planet dyoo/whalesong/js))

;; insert-break: -> void
(define (insert-break)
  (call-method ($ "<br/>") "appendTo" body)
  (void))

;; write-message: any -> void
(define (write-message msg)
  (void (call-method (call-method (call-method ($ "<span/>") "text" msg)
                    "css" "white-space" "pre")
              "appendTo"
              body)))

;; Set the background green, and show some content
;; on the browser.
(void (call-method body "css" "background-color" "lightgreen"))
(void (call-method ($ "<h1>Hello World</h1>") "appendTo" body))
(write-message "Hello, this is a test!")
(insert-break)
(let loop ([i 0])
  (cond
   [(= i 10)
    (void)]
   [else
    (write-message "iteration ") (write-message i)
    (insert-break)
    (loop (add1 i))]))
}|}
This program uses the @link["http:/jquery.com"]{JQuery} API provided by @racketmodname[(planet dyoo/whalesong/js)],
as well as the native JavaScript FFI to produce output on the browser.
If we run Whalesong on this program, and view the resulting @filepath{dom-play.html} in our
web browser, we should see a pale, green page with some output.





@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@subsection{Using Whalesong functions from JavaScript}
@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

Whalesong also allows functions defined from Racket to be used from
JavaScript.  As an example, we can take the boring @emph{factorial}
function and define it in a module called @filepath{fact.rkt}:

@margin-note{
The files can also be downloaded here: 
@itemlist[@item{@link["http://hashcollision.org/whalesong/fact-example/fact.rkt"]{fact.rkt}} 
@item{@link["http://hashcollision.org/whalesong/fact-example/index.html"]{index.html}}]
with generated JavaScript binaries here:
@itemlist[
@item{@link["http://hashcollision.org/whalesong/fact-example/fact.js"]{fact.js}}
@item{@link["http://hashcollision.org/whalesong/fact-example/runtime.js"]{runtime.js}}
]
}


@filebox["fact.rkt"]{
@codeblock|{
#lang planet dyoo/whalesong
(provide fact)
(define (fact x)
  (cond
   [(= x 0)
    1]
   [else
    (* x (fact (sub1 x)))]))
}|}

Instead of creating a standalone @tt{.html}, we can use @tt{whalesong} to
get us the module's code.  From the command-line:
@verbatim|{
    $ whalesong get-javascript fact.rkt > fact.js
    $ ls -l fact.js
    -rw-r--r-- 1 dyoo dyoo 27421 2011-07-11 22:02 fact.js
}|

This file does require some runtime support not included in
@filepath{fact.js}; let's generate the @tt{runtime.js} and save
it as well.  At the command-line:
@verbatim|{
    $ whalesong get-runtime > runtime.js
    $ ls -l runtime.js
    -rw-r--r-- 1 dyoo dyoo 544322 2011-07-11 22:12 runtime.js
}|
Now that we have these, let's write an @filepath{index.html} that uses
the @racket[fact] function that we @racket[provide]ed from
@filepath{fact.rkt}.
@filebox["index.html"]{
@verbatim|{
<!DOCTYPE html>
<html>
<head>
<script src="runtime.js"></script>
<script src="fact.js"></script>

<script>
// Each module compiled with 'whalesong get-runtime' is treated as a
// main module.  invokeMains() will invoke them.
plt.runtime.invokeMains();  

plt.runtime.ready(function() {

   // Grab the definition of 'fact'...
   var myFactClosure = plt.runtime.lookupInMains('fact');

   // Make it available as a JavaScript function...
   var myFact = plt.baselib.functions.asJavaScriptFunction(
        myFactClosure);

   // And call it!
   myFact(function(v) {
              $('#answer').text(v.toString());
          },
          function(err) {
              $('#answer').text(err.message).css("color", "red");
          },
          10000
          // "one-billion-dollars"
          );
});
</script>
</head>

<body>
The factorial of 10000 is <span id="answer">being computed</span>.
</body>
</html>
}|
}

@margin-note{See: @link["http://hashcollision.org/whalesong/fact-example/bad-index.html"]{bad-index.html}.}
Replacing the @racket[10000] with @racket["one-billion-dollars"] should
reliably produce a proper error message.




@section{Using @tt{whalesong}}

Whalesong provides a command-line utility called @tt{whalesong} for
translating Racket to JavaScript.  It can be run in several modes:

@itemize[
@item{To create HTML + js documents}
@item{To output the compiled JavaScript as a single @filepath{.js} file}
]

Using @tt{whalesong} to generate HTML+js documents is
relatively straightforward with the @tt{build} command.  To use it,
pass the name of the file to it:
@verbatim|{
    $ whalesong build [name-of-racket-file]
}|
A @filepath{.html} and @filepath{.js} will be written to the current directory, as will any external resources that the program uses.



The @tt{whalesong} commands support these command line options:

@itemize[

@item{@verbatim{--compress-javascript} Use Google Closure's JavaScript
compiler to significantly compress the JavaScript.  Using this
currently requires a Java 1.6 JDK.}

@item{@verbatim{--verbose} Write verbose debugging information to standard error.}

@item{@verbatim{--dest-dir}  Write files to a separate directory, rather than the current directory.}

@item{@verbatim{--split-modules} Write each dependent module as a
separate file, rather than in one large @filepath{.js}.  This may be
necessary if your browser environment prohibits large @filepath{.js}
files.  The files will be numbered starting from @racket[1].}

]




For more advanced users, @tt{whalesong} can be used to generate
JavaScript in non-standalone mode.  This gives the web developer more
fine-grained control over how to control and deploy the outputted
program.



@subsection{@tt{build}}

Given the name of a program, this builds 
@filepath{.html} and @filepath{.js} files into the current working directory.

The @filepath{.html} and @filepath{.js} should be self-contained, with an exception: if
the file uses any external @tech{resource}s by using
@racket[define-resource], those resources are written into the current
working directory, if they do not already exist there.


@subsection{@tt{get-javascript}}

Given the name of a program, writes the JavaScript to standard output,
as well as its dependent modules.  The outputted file is meant to be
used as a @tt{SCRIPT} source.

By default, the given program will be treated as a @emph{main} module.
All main modules will be executed when the JavaScript function
@tt{plt.runtime.invokeMains()} is called.


@subsection{@tt{get-runtime}}

Prints out the core runtime library that the files generated by
get-javascript depend on.





@section{Including external resources}
@defmodule/this-package[resource]

Programs may need to use external file @deftech{resource}s that aren't
themselves Racket programs, but instead some other kind of data.
Graphical programs will often use @filepath{.png}s, and web-related
programs @filepath{.html}s, for example.  Whalesong provides the
@racketmodname/this-package[resource] library to refer and use these
external resources.  When Whalesong compiles a program into a package,
these resources will be bundled alongside the JavaScript-compiled
output.

@defform[(define-resource id [path-string])]{
Defines a @tech{resource} with the given path name.

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
@tech{resource} is @filepath{humpback.png}.


If the resource given has an extension one of the following:
@itemize[
@item{@filepath{.png}}
@item{@filepath{.gif}}
@item{@filepath{.jpg}}
@item{@filepath{.jpeg}}]
then it can be treated as an image for which @racket[image?] will be true.

If the resource has the extension @filepath{.html}, then it will be
run through an HTML purifying process to make sure the HTML is
well-formed.



@defproc[(resource? [x any]) boolean]{
Returns @racket[#t] if @racket[x] is a @tech{resource}.}

@defproc[(resource->url [a-resource resource?]) string?]{
Given a @tech{resource}, gets a URL.

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



@; Not done yet!
@;@defproc[(resource->input-port [a-resource resource?]) string?]{
@;Given a resource, gets an input-port of its contents.
@;}

@;@defform[(define-remote-resource id url-string])]{
@;Given a url, creates a remote resource.  At the time of Whalesong compilation,
@;Whalesong will freeze a static copy of the file.
@;}








@section{The web-world API}

@defmodule/this-package[web-world]

The @tt{web-world} library allows you to write functional event-driven
@link["http://world.cs.brown.edu"]{World} programs for the web; the
user defines functional callbacks to handle events, and receive and
consume a world argument.

One difference introduced by the web is the web page itself: because
the page itself is a source of state, it too will be passed to
callbacks.  This library presents a functional version of the DOM in
the form of a @tech{view}.

@margin-note{Visit @link["http://hashcollision.org/whalesong/examples/tick-tock/tick-tock.html"]{tick-tock.html} to execute this program.}
Let's demonstrate this by creating a basic ticker that counts on the
screen every second.

The first thing we can do is mock up a web page with a user interface, like this.
@filebox["view.html"]{
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

(define-resource view.html)

;; draw: world view -> view
(define (draw world dom)
  (update-view-text (view-focus dom "counter") world))


;; tick: world view -> world
(define (tick world dom)
  (add1 world))


;; stop?: world view -> boolean
(define (stop? world dom)
  (> world 10))

(big-bang 0
          (initial-view view.html)
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

@item{We use @racket[define-resource] to refer to external files, like @filepath{view.html} that
we'd like to include in our program.}

@item{We use @racket[big-bang] to start up a computation that
responses to events.  In this example, that's clock ticks introduced
by @racket[on-tick], though because we're on the web, we can
bind to many other kinds of web events (by using @racket[view-bind]).}
]



@subsection{@racket[big-bang] and its options}
@declare-exporting/this-package[web-world]
@defproc[(big-bang [w world]
                   [h big-bang-handler] ...) world]{
Start a big bang computation.  The @racket[big-bang] consumes an initial world,
as well as several handlers to configure it, described next:
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
@racket[initial-view] should only be used in the lexical context of a @racket[big-bang].
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
@racket[stop-when] should only be used in the lexical context of a @racket[big-bang].
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
@racket[on-tick] should only be used in the lexical context of a @racket[big-bang].
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
@racket[on-mock-location-change] should only be used in the lexical context of a @racket[big-bang].
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
@racket[on-location-change] should only be used in the lexical context of a @racket[big-bang].
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
@racket[to-draw] should only be used in the lexical context of a @racket[big-bang].
}



@subsection{Views}
@declare-exporting/this-package[web-world]
A @deftech{view} is a functional representation of the browser DOM
tree.  A view is always focused on an element, and the functions in
this subsection show how to traverse and manipulate the view.



@defproc[(->view [x any]) view]{

Coerse a value into a view whose focus is on the topmost element.
Common values for @racket[x] include @tech{resource}s.
}


@defproc[(view-focus? [v view] [id String]) boolean]{
Return true if the view can be focused using the given id.
}


@defproc[(view-focus [v view] [id String]) view]{
Focuses the view on an element, given the @racket[id].  The view
will be searched starting from the toplevelmost node.
}


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



@defproc[(view-forward? [v view]) boolean]{
See if the view can be moved forward.}

@defproc[(view-forward [v view]) view]{
Move the view forward, assuming a pre-order traversal.
}

@defproc[(view-backward? [v view]) boolean]{
See if the view can be moved backward.}

@defproc[(view-backward [v view]) view]{
Move the view backward, assuming a pre-order traversal.
}




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

Common event types include @racket["click"], @racket["mouseenter"],
@racket["change"].}  Note that the name of the event should not
include an @racket["on"] prefix.


A view may have many elements to bind, and it's a common pattern to
focus and view.  As a convenience the API provides some syntactic support to
bind multiple handlers at once:
@defform[(view-bind-many a-view [id type world-updater] ...)]{
Composes the use of @racket[view-focus] and @racket[view-bind] to conveniently bind
multiple handlers at once.

Common event types include @racket["click"], @racket["mouseenter"], or
@racket["change"].}  Note that the name of each event should not
include an @racket["on"] prefix.

As an example:
@codeblock|{
(define (click-handler w v) ...)

(define (change-handler w v) ...)

(define-resource view.html)

(define my-static-view (->view view.html))

(define connected-view
  (view-bind-many my-static-view 
                  ["id1" "click" click-handler]
                  ["id2" "click" click-handler]
                  ["id3" "change" change-handler]))
...
}|



If the collection of ids, types, and handlers can't be represented as a static list, then
@racket[view-bind-many*] is an alternate helper function that may be helpful to bind
a bulk number of handlers to a view.


@defproc[(view-bind-many* [v view] [id+type+updater-list (listof (list string string world-updater))]) view]{
A functional version of @racket[view-bind-many].  Composes the use of
@racket[view-focus] and @racket[view-bind] to conveniently bind
multiple handlers at once.

Common event types include @racket["click"], @racket["mouseenter"], or
@racket["change"].}  Note that the name of each event should not
include an @racket["on"] prefix.


As an example:
@codeblock|{
(define (click-handler w v) ...)

(define (change-handler w v) ...)

(define-resource view.html)

(define my-static-view (->view view.html))

(define connected-view
  (view-bind-many* my-static-view 
                  `(["id1" "click" ,click-handler]
                    ["id2" "click" ,click-handler]
                    ["id3" "change" ,change-handler])))
...
}|





@defproc[(view-show [v view]) view]{
Show the element at the focus.
}
@defproc[(view-hide [v view]) view]{
Hide the element at the focus.
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

@defproc[(view-width [v view]) number]{
Get the width at the focus.
}

@defproc[(view-height [v view]) number]{
Get the height at the focus.
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



Dom nodes can be created by using @racket[xexp->dom], which converts a
@tech{xexp} to a node, and attached to the view by using
@racket[view-append-child], @racket[view-insert-left], and
@racket[view-insert-right].


@defproc[(view-append-child [v view] [d dom]) view]{
Add the dom node @racket[d] as the last child of the focused node.
Focus moves to the inserted node.}


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
@declare-exporting/this-package[web-world]
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



@subsection{Dynamic DOM generation with xexps} 
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
           @nonterm{symbol}
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


We may also want to take a view and turn it back into an @tech{xexp}.
@defproc[(view->xexp [a-view view]) xexp]{
Coerses a view into a @tech{xexp}.
}







@subsection{Tips and tricks: Hiding standard output or directing it to an element}

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












@section{The JavaScript Foreign Function Interface}

@defmodule/this-package[js]{


[[This needs to describe what hooks we've got from the JavaScript side
of things.

In particular, we need to talk about the plt namespace constructed by
the runtime, and the major, external bindings, like
@tt{plt.runtime.invokeMains}.

The contracts here are not quite right either.  I want to use JQuery
as the type in several of the bindings here, but don't quite know how
to teach Scribble about them yet.]]

@defproc[(js-string? [x any]) boolean]{
Returns @racket[#t] if the value is a primitive JavaScript string value.
}

@defproc[(string->js-string [str string]) js-string]{
Coerses @racket[str] to a JavaScript string value.
}

@defproc[(js-string->string [js-str js-string]) string]{
Coerses @racket[js-str] to a string value.
}


@defproc[(js-number? [x any]) boolean]{
Returns @racket[#t] if @racket[x] is a primitive JavaScript number.
}

@defproc[(number->js-number [num number]) js-number]{
Coerses @racket[num] to a JavaScript number.
}

@defproc[(js-number->number [js-num js-number]) number]{
Coerses @racket[js-num] to a number.
}

@defproc[(js-null? [x any]) boolean]{
Returns @racket[#t] if @racket[x] is @tt{null}.
}

@defthing[js-null js-value]{
JavaScript's @tt{null} value.
}




@defproc[(get-attr [obj js-object] [key (or/c string symbol)]) js-value]{
Looks up the attribute of a JavaScript object.
}

@defproc[(set-attr! [obj js-object] [key (or/c string symbol)] [value js-value]) js-value]{
Sets the attribute of a JavaScript object.
}


@defproc[(load-script [url string?]) void]{
Dynamically loads a script from the URL.
}


@defproc[(js-function->procedure [f (U string js-function)]) procedure]{
Given either a string representation of a JavaScript function, or a javascript function
object, returns a procedure that can be called.

For example, the following shows how to lift a simple function:
@racketblock[
(define primitive-* 
  (js-function->procedure
   "function(x) { return x * x; }"))
(primitive-* 42)
]

Caveats: the lifted function does no auto-coersion of values.
}


@defproc[(js-async-function->procedure [f (or/c string js-function)])
procedure]{ Given either a string representation of a JavaScript
function, or a javascript function object, returns a procedure that
can be called.  The first two arguments to the function are special,
representing the success and fail continuations that continue the rest
of the computation.

For example, the following shows how to lift a simple function:
@racketblock[
(define primitive-* 
  (js-async-function->procedure
   "function(success, fail, x) { success(x * x); }"))
(primitive-* 42)
]

Note that, unlike @racket[js-function->procedure], the
JavaScript implementation is expected to call the success or fail
continuation explicitly.

As another example that takes advantage of the explicit continuation style:
@racketblock[
(define js-sleep 
  (js-async-function->procedure 
   "function(success, fail, x) { setTimeout(success, x); }"))
(js-sleep 1000)
]

Caveats: the lifted function does no auto-coersion of values.
}




@defproc[(alert [msg string?]) void]{

Displays an alert.  Currently implemented using JavaScript's
@litchar{alert} function.}



@defthing[body any/c]{
A JQuery-wrapped value representing the body of the DOM.
}


@defproc[(call-method [object any/c] 
                      [method-name string?]
                      [arg any/c] ...) any/c]{

Calls the method of the given object, assuming @racket[object] is a
JavaScript value that supports that method call.  The raw return
value is passed back.

For example,
@racketblock[(call-method body "css" "background-color")]
should return the css color of the body.
}



@defproc[($ [locator any/c]) any/c]{

Uses JQuery to construct or collect a set of DOM elements, as
described in the @link["http://api.jquery.com/jQuery/"]{JQuery
documentation}.

For example,
@racketblock[(call-method ($ "<h1>Hello World</h1>")
                          "appendTo"
                          body)]
will construct a @tt{h1} header, and append it to
the document body.
}


@defproc[(in-javascript-context?) boolean]{Returns true if the running context
supports JavaScript-specific functions.}

@defproc[(viewport-width) number?]{
Can only be called in a JavaScript context.

Returns the width of the viewport.
}

@defproc[(viewport-height) number?]{
Can only be called in a JavaScript context.

Returns the height of the viewport.
}

}



@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@subsection{Adding new event handlers to world programs with the FFI}
@defmodule/this-package[js/world]
@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

The callback-driven asynchronous APIs in JavaScript can act as sources
of World events.  The following function allows web-world programs to
bind to these sources.

@defproc[(make-world-event-handler [setup procedure] [shutdown procedure]) event-handler]{
Creates a new handler type that can be used with @racket[big-bang].
@racket[big-bang] calls the first argument at the beginning of the
event-loop, and calls the second right before the event loop
terminates.

The @racket[setup] and @racket[shutdown] functions are usually
constructed with @racket[js-function->procedure] in order to bind to
native JavaScript APIs.

The @racket[setup] function is called with an JavaScript function
value that, when called, emits a new event into the world's event
loop.  The return value of the @racket[setup] function will be saved,
and when the @racket[shutdown] procedure calls, that value is passed
to it, with the intent that shutting down a service will likely
require information that's produced at setup-time.


For example, we can reimplement some of the behavior of
@racket[on-location-change] with the following:
@codeblock|{
#lang planet dyoo/whalesong
(require (planet dyoo/whalesong/js)
         (planet dyoo/whalesong/js/world))

(define setup-geo
  (js-function->procedure
   "function (locationCallback) {
        return navigator.geolocation.watchPosition(
            function(evt) {
                var coords = evt.coords;
                locationCallback(plt.runtime.makeFloat(coords.latitude),
                                 plt.runtime.makeFloat(coords.longitude)); })}"))

(define shutdown-geo
  (js-function->procedure
   "function (watchId) {
        navigator.geolocation.clearWatch(watchId); }"))

;; We create a new event handler type here.
(define on-geo (make-world-event-handler setup-geo shutdown-geo))


;; Once defined, we can use on-geo as just another world-handler type.

(define (move world view lat lng)
  (list lat lng))

(big-bang (list 'undefined 'undefined)
          (on-geo move))
}|
}










@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@section{Simple world programming}
@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

Whalesong provides a library to support writing functional I/O
programs
(@link["http://www.ccs.neu.edu/scheme/pubs/icfp09-fffk.pdf"]{A
Functional I/O System}).  Here's an example of such a world program:

@inject-empty-span-with-id{simple-world-program}
[FIXME: embed a world program here.]















@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@section{Acknowledgements}
@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@;; shriram, kathi, emmanuel, everyone who helped with moby and wescheme
@;;
@;; also need to list out all the external libraries we're using
@;; and the license.


Whalesong uses code and utilities from the following external projects:
@itemlist[
@item{jshashtable (@url{http://www.timdown.co.uk/jshashtable/})}
@item{js-numbers (@url{http://github.com/dyoo/js-numbers/})}
@item{JSON (@url{http://www.json.org/js.html})}
@item{jquery (@url{http://jquery.com/})}
@item{Google Closure Compiler (@url{http://code.google.com/p/closure-compiler/})}
@item{Base64 encode (@url{http://www.webtoolkit.info/})}

@item{excanvas (@url{http://excanvas.sourceforge.net/})}
@item{canvas-text (@url{http://code.google.com/p/canvas-text/source/browse/trunk})} 
]

The following folks have helped tremendously in the implementation of
Whalesong by implementing libraries, giving guidence, reporting bugs,
and suggesting improvements.

@;;;;
@; in alphabetical order
@;;;;
@(apply itemlist
   (map item (sort (list
   "Ethan Cecchetti"
   "Scott Newman"
   "Zhe Zhang"
   "Jens Axel Søgaard"
   "Jay McCarthy"
   "Sam Tobin-Hochstadt"
   "Doug Orleans"
   "Richard Cleis"
   "Asumu Takikawa"
   "Eric Hanchrow"
   "Greg Hendershott"
   "Shriram Krishnamurthi"
   "Emmanuel Schanzer"
   "Robby Findler"
   "Gregor Kiczales"
   "Cristina Teodoropol"
   "Matthew Flatt"
   "Keith Decker"
) string<?))
)