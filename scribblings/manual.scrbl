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

@(require racket/runtime-path)
@(define-runtime-path git-head-path "../.git/refs/heads/master")


@(require (for-label (this-package-in js))
          (for-label (this-package-in lang/base))
          (for-label (this-package-in resource)))



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
@; Warning Will Robinson, Warning!
@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@centered{@larger{@bold{@italic{Warning: this is work in progress!}}}}







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
@url{https://github.com/dyoo/whalesong}.



Prerequisites: at least @link["http://racket-lang.org/"]{Racket
5.1.1}, and a @link["http://www.java.com"]{Java 1.6} SDK.
      @; (This might be superfluous information, so commented out
      @;  for the moment...)
      @;The majority of the project is written
      @;@link["http://docs.racket-lang.org/ts-guide/index.html"]{Typed
      @;Racket}, and Racket 5.1.1 and above provides the support necessary to
      @;compile Whalesong; otherwise, compilation may take an unusual amount
      @;of time.




@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@section{Getting started}
@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


@subsection{Installing Whalesong}

At the time of this writing, although Whalesong has been deployed to
@link["http://planet.racket-lang.org"]{PLaneT}, the version on PLaneT
is out of date.  I'll be updating the PLaneT package as soon as
Whalesong starts to stabilize, but the system as a whole is still in
some flux.

You may want to get the latest sources instead of using the version on
PLaneT.  Doing so requires doing a little bit of manual work.  The
steps are:

@itemlist[
@item{Check Whalesong out of Github.}
@item{Set up the PLaneT development link to your local Whalesong instance.}
@item{Run @link["http://docs.racket-lang.org/raco/setup.html"]{@tt{raco setup}} over Whalesong to finish the installation}]

We can check it out of the source repository in
@link["https://github.com/"]{GitHub}; the repository can be checked out by
using @tt{git clone}.  At the command-line, clone the tree
with: @verbatim|{ $ git clone git://github.com/dyoo/whalesong.git }|
This should check the repository in the current directory.



Next, let's set up a @link["http://docs.racket-lang.org/planet/Developing_Packages_for_PLaneT.html#(part._devlinks)"]{PLaneT development link}.  Make sure you are in the
parent directory that contains the @filepath{whalesong} repository, and
then run this on your command line:
@verbatim|{
$ planet link dyoo whalesong.plt 1 0 whalesong
}|
(You may need to adjust the @tt{1} and @tt{0} major/minor numbers a bit to be larger
than the latest version that's on PLaneT at the time.)



Finally, we need to set up Whalesong with @tt{raco setup}.
Here's how to do this at the command
line:
@verbatim|{
$ raco setup -P dyoo whalesong.plt 1 0
}|
This should compile Whalesong, as well as set up the @filepath{whalesong} executable.
Any time the source code in @filepath{whalesong} changes, we should repeat
this @tt{raco setup} step again.


At this point, you should be able to rung @filepath{whalesong} from the command line.
@verbatim|{
$ ./whalesong
Expected one of the following: [build, get-runtime, get-javascript].                    
           }|
and if this does appear, then Whalesong should be installed successfully.


Note: whenever Whalesong's source code is updated from Github, please
re-run the @tt{raco setup}.  Otherwise, Racket will try to recompile
Whalesong on every single use of Whalesong, which can be very
expensive.






@subsection{Making Standalone @tt{.xhtml} files with Whalesong}

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


We can do something slightly more interesting.  Let's write a Whalesong program
that accesses the JavaScript DOM.  Call this file @filepath{dom-play.rkt}.
@margin-note{
The generated program can be downloaded here: @link["http://hashcollision.org/whalesong/examples/dom-play.xhtml"]{dom-play.xhtml}
}

@filebox["dom-play.rkt"]{
@codeblock|{
#lang planet dyoo/whalesong

;; Uses the JavaScript FFI, which provides bindings for:
;; $ and call
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
If w run Whalesong on this program, and view the resulting @filepath{dom-play.xhtml} in your
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

Instead of creating a standalone @tt{.xhtml}, we can use @tt{whalesong} to
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

Replacing the @racket[10000] with @racket["one-billion-dollars"] should
reliably produce a proper error message.




@section{Using @tt{whalesong}}

Whalesong provides a command-line utility called @tt{whalesong} for
translating Racket to JavaScript.  It can be run in several modes:

@itemize[
@item{To create standalone XHTML documents}
@item{To output the compiled JavaScript as a single @filepath{.js} file}
@item{To output the compiled JavaScript as several @filepath{.js} files, one per module.  (this isn't done yet...)}
]

Using @tt{whalesong} to generate standalone XHTML documents is
relatively straightforward with the @tt{build} command.  To use it,
pass the name of the file to it:
@verbatim|{
    $ whalesong build [name-of-racket-file]
}|
An @filepath{.xhtml} will be written to the current directory.

Almost all of the @tt{whalesong} commands support two command line options:

@itemize{

@item{@tt{--compress-javascript}: Use Google Closure's JavaScript
compiler to significantly compress the JavaScript.  Using this
currently requires a Java 1.6 JDK.}

@item{@tt{--verbose}: write verbose debugging information to standard error.}
}




For more advanced users, @tt{whalesong} can be used to generate
JavaScript in non-standalone mode.  This gives the web developer more
fine-grained control over how to control and deploy the outputted
program.



@subsection{@tt{build}}

Given the name of a program, this builds a standalone
@filepath{.xhtml} file into the current working directory that
executes the program in a web browser.

The @filepath{.xhtml} should be self-contained, with an exception: if
the file uses any external resources by using
@racket[define-resource], those resources are written into a
subdirectory called @filepath{res} under the current working
directory.


@subsection{@tt{get-javascript}}

Given the name of a program, writes the JavaScript to standard output,
as well as its dependent modules.  The outputted file is meant to be
used as a @tt{SCRIPT} source.

By default, the given program will be treated as a @emph{main} module.
All main modules will be executed when the JavaScript function
@tt{plt.runtime.invokeMains()} is called.


@subsection{@tt{write-javascript-files}}
[NOT DONE YET]
(needs to write a MANIFEST file?)
(this almost seems like we need some concept of a JAR... )


@subsection{@tt{write-resources}}
[NOT DONE YET]



@subsection{@tt{get-runtime}}

Prints out the core runtime library that the files generated by
get-javascript depend on.





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

@defform[(define-resource id path-string)]{
Defines a resource with the given path name.

For example,
@codeblock|{
#lang planet dyoo/whalesong
(require (planet dyoo/whalesong/resource))
(define-resource my-whale-image-resource "humpback.png")
}|
}


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



@; Not done yet!
@;@defproc[(resource->input-port [a-resource resource?]) string?]{
@;Given a resource, gets an input-port of its contents.
@;}

@;@defform[(define-remote-resource id url-string])]{
@;Given a url, creates a remote resource.  At the time of Whalesong compilation,
@;Whalesong will freeze a static copy of the file.
@;}





@section{The JavaScript API}

@defmodule/this-package[js]{


This needs to describe what hooks we've got from the JavaScript side
of things.

In particular, we need to talk about the plt namespace constructed by
the runtime, and the major, external bindings, like
@tt{plt.runtime.invokeMains}.

The contracts here are not quite right either.  I want to use JQuery
as the type in several of the bindings here, but don't quite know how
to teach Scribble about them yet.



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
@section{World programming}
@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

Whalesong provides a library to support writing functional I/O
programs
(@link["http://www.ccs.neu.edu/scheme/pubs/icfp09-fffk.pdf"]{A
Functional I/O System}).  Here's an example of such a world program:

@inject-empty-span-with-id{simple-world-program}
[FIXME: embed a world program here.]






@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@section{Internals}
@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

Please skip this section if you're a regular user: this is really
notes internal to Whalesong development, and is not relevant to most
people.


These are notes that describe the internal details of the
implementation, including the type map from Racket values to
JavaScript values.  It should also describe how to write FFI
bindings, eventually.

@subsection{Architecture}

The basic idea is to reuse most of the Racket compiler infrastructure.
We use the underlying Racket compiler to produce bytecode from Racket
source; it also performs macro expansion and module-level
optimizations for us.  We parse that bytecode using the
@racketmodname[compiler/zo-parse] collection to get an AST,
compile that to an
intermediate language, and finally assemble JavaScript.

@verbatim|{
                     AST                 IL                 
 parse-bytecode.rkt -----> compiler.rkt ----> assembler.rkt 

}|

The IL is intended to be translated straightforwardly.  We currently
have an assembler to JavaScript @filepath{js-assembler/assemble.rkt},
as well as a simulator in @filepath{simulator/simulator.rkt}.  The
simulator allows us to test the compiler in a controlled environment.


@subsection{parser/parse-bytecode.rkt}

(We try to insulate against changes in the bytecode structure by
using the version-case library to choose a bytecode parser based on
the Racket version number.  Add more content here as necessary...)

@subsection{compiler/compiler.rkt}

This translates the AST to the intermediate language.  The compiler has its
origins in the register compiler in @link[    "http://mitpress.mit.edu/sicp/full-text/book/book-Z-H-35.html#%_sec_5.5"
]{Structure and Interpretation of
Computer Programs}  with some significant modifications.
                  
                  Since this is a stack machine,
we don't need any of the register-saving infrastructure in the
original compiler.  We also need to support slightly different linkage
structures, since we want to support multiple value contexts.  We're
trying to generate code that works effectively on a machine like the
one described in @url{http://plt.eecs.northwestern.edu/racket-machine/}.


The intermediate language is defined in @filepath{il-structs.rkt}, and a
simulator for the IL in @filepath{simulator/simulator.rkt}.  See
@filepath{tests/test-simulator.rkt} to see the simulator in action, and
@filepath{tests/test-compiler.rkt} to see how the output of the compiler can be fed
into the simulator.

The assumed machine is a stack machine with the following atomic
registers:
@itemlist[
    @item{val: value}
    @item{proc: procedure}
    @item{argcount: number of arguments}
]
and two stack registers:
@itemlist[
          @item{env: environment stack}
          @item{control: control stack}
]

@subsection{js-assembler/assemble.rkt}
The intent is to potentially support different back end generators 
for the IL.  @filepath{js-assembler/assemble.rkt} provides a backend
for JavaScript.

The JavaScript assembler plays a few tricks to make things like tail
calls work:

@itemlist[
          @item{Each basic block is translated to a function taking a MACHINE
            argument.}

           @item{ Every GOTO becomes a function call.}

           @item{ The head of each basic-blocked function checks to see if we
     should trampoline
     (@url{http://en.wikipedia.org/wiki/Trampoline_(computers)})}

           @item{We support a limited form of computed jump by assigning an
     attribute to the function corresponding to a return point.  See
     the code related to the LinkedLabel structure for details.}
]

Otherwise, the assembler is fairly straightforward.  It depends on
library functions defined in @filepath{runtime-src/runtime.js}.  As soon as the compiler
stabilizes, we will be pulling in the runtime library in Moby Scheme
into this project.  We are right in the middle of doing this, so expect
a lot of flux here.


The assembled output distinguishes between Primitives and Closures.
Primitives are only allowed to return single values back, and are not
allowed to do any higher-order procedure calls.  Closures, on the
other hand, have full access to the machine, but they are responsible
for calling the continuation and popping off their arguments when
they're finished.




@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@subsection{Values}
@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

All values should support the following functions

@itemlist[
   @item{plt.runtime.toDomNode(x, mode): produces a dom representation.  mode can be either 'write', 'display', or 'print'}

   @item{plt.runtime.equals(x, y): tests if two values are equal to each other}

]





@subsubsection{Numbers}

Numbers are represented with the
@link["https://github.com/dyoo/js-numbers"]{js-numbers} JavaScript
library.  We re-exports it as a @tt{plt.baselib.numbers} namespace
which provides the numeric tower API.

Example uses of the @tt{plt.baselib.numbers} library include:

@itemlist[
@item{Creating integers: @verbatim{42}  @verbatim{16}}

@item{Creating big integers: @verbatim{plt.baselib.numbers.makeBignum("29837419826")}}

@item{Creating floats: @verbatim{plt.baselib.numbers.makeFloat(3.1415)}}

@item{Predicate for numbers: @verbatim{plt.baselib.numbers.isSchemeNumber(42)}}

@item{Adding two numbers together: @verbatim{plt.baselib.numbers.add(42, plt.baselib.numbers.makeFloat(3.1415))}}

@item{Converting a plt.baselib.numbers number back into native JavaScript floats: @verbatim{plt.baselib.numbers.toFixnum(...)}}
]

Do all arithmetic using the functions in the @tt{plt.baselib.numbers} namespace.
One thing to also remember to do is apply @tt{plt.baselib.numbers.toFixnum} to any
native JavaScript function that expects numbers.




@subsubsection{Pairs, @tt{NULL}, and lists}

Pairs can be constructed with @tt{plt.runtime.makePair(f, r)}.  A pair
is an object with @tt{first} and @tt{rest} attributes.  They can be
tested with @tt{plt.runtime.isPair()};

The empty list value, @tt{plt.runtime.NULL}, is a single,
distinguished value, so compare it with @tt{===}.

Lists can be constructed with @tt{plt.runtime.makeList}, which can take in
multiple arguments.  For example,
@verbatim|{ var aList = plt.runtime.makeList(3, 4, 5); }|
constructs a list of three numbers.

The predicate @tt{plt.runtime.isList(x)} takes an argument x and
reports if the value is chain of pairs, terminates by NULL.  At the
time of this writing, it does NOT check for cycles.




@subsection{Vectors}
Vectors can be constructed with @tt{plt.runtime.makeVector(x ...)}, which takes
in any number of arguments.  They can be tested with @tt{plt.runtime.isVector}, 
and support the following methods and attributes:
@itemlist[
    @item{ref(n): get the nth element}
    @item{set(n, v): set the nth element with value v}
    @item{length: the length of the vector }]




@subsection{Strings}

Immutable strings are represented as regular JavaScript strings.

Mutable strings haven't been mapped yet.



@subsection{VOID}

The distinguished void value is @tt{plt.runtime.VOID}; functions
implemented in JavaScript that don't have a useful return value should
return @tt{plt.runtime.VOID}.


@subsection{Undefined}
The undefined value is JavaScript's @tt{undefined}.


@subsection{EOF}
The eof object is @tt{plt.runtime.EOF}


@subsubsection{Boxes}
Boxes can be constructed with @tt{plt.runtime.makeBox(x)}.  They can be
tested with @tt{plt.runtime.isBox()}, and they support two methods:
@verbatim|{
   box.get(): returns the value in the box
   box.set(v): replaces the value in the box with v 
}|





@subsubsection{Structures}

structure types can be made with plt.runtime.makeStructureType.  For example,
@verbatim|{
    var Color = plt.runtime.makeStructureType(
        'color',    // name
        false,      // parent structure type
        3,          // required number of arguments
        0,          // number of automatically-filled fields
        false,      // OPTIONAL: the auto-v value
        false       // OPTIONAL: a guard procedure
        );
}|

@tt{makeStructuretype} is meant to mimic the @racket[make-struct-type]
function in Racket.  It produces a structure type value with the
following methods:
@itemlist[

        @item{@tt{constructor}: create an instance of a structure type.

For example,
@verbatim|{
    var aColor = Color.constructor(3, 4, 5);
}|
creates an instance of the Color structure type.
}


        @item{@tt{predicate}: test if a value is of the given structure type.

For example,
@verbatim|{
     Color.predicate(aColor) --> true
     Color.predicate("red") --> false
}|
}


        @item{@tt{accessor}: access a field of a structure.

For example,
@verbatim|{
    var colorRed = function(x) { return Color.accessor(x, 0); };
    var colorGreen = function(x) { return Color.accessor(x, 1); };
    var colorBlue = function(x) { return Color.accessor(x, 2); };
}|
}
        @item{@tt{mutator}: mutate a field of a structure.

For example,
@verbatim|{
    var setColorRed = function(x, v) { return Color.mutator(x, 0, v); };
}|

}
]
In addition, it has a @tt{type} whose @tt{prototype} can be changed in order
to add methods to an instance of a structure type.  For example,
@verbatim|{
    Color.type.prototype.toString = function() {
        return "rgb(" + colorRed(this) + ", "
                      + colorGreen(this) + ", "
                      + colorBlue(this) + ")";
    };
}|
should add a toString method for instances of the @tt{Color} structure.



@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@subsection{Tests}
@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

The test suite in @filepath{tests/test-all.rkt} runs the test suite.
You'll need to
run this on a system with a web browser, as the suite will evaluate
JavaScript and make sure it is producing values.  A bridge module
in @filepath{tests/browser-evaluate.rkt} brings up a temporary web server
that allows us
to pass values between Racket and the JavaScript evaluator on the
browser for testing output.




@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@subsection{What's in @tt{js-vm} that's missing from Whalesong?}
@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(This section should describe what needs to get done next.)

The only types that are mapped so far are
@itemlist[
@item{immutable strings}
@item{numbers}
@item{pairs}
@item{null}
@item{void}
@item{vectors}
]
We need to bring around the following types previously defined in @tt{js-vm}:
(This list will shrink as I get to the work!)
@itemlist[
@item{immutable vectors}
@item{regexp}
@item{byteRegexp}
@item{character}
@item{placeholder}
@item{path}
@item{bytes}
@item{immutable bytes}
@item{keywords}
@item{hash}
@item{hasheq}
@item{struct types}
@item{exceptions}
@item{thread cells}

@item{big bang info}
@item{worldConfig}
@item{effectType}
@item{renderEffectType}
@item{readerGraph}
]



@(define missing-primitives
   (let ([in-whalesong-ht (make-hash)])
     (for ([name whalesong-primitive-names])
          (hash-set! in-whalesong-ht name #t))
     (filter (lambda (name)
               (not (hash-has-key? in-whalesong-ht name)))
             js-vm-primitive-names))))


What are the list of primitives in @filepath{js-vm-primitives.js} that we
haven't yet exposed in whalesong?  We're missing @(number->string (length missing-primitives)):
   @(apply itemlist (map (lambda (name)
                           (item (symbol->string name)))
                         missing-primitives))
        


(I should catalog the bug list in GitHub, as well as the feature list,
so I have a better idea of what's needed to complete the project.)


(We also need a list of the primitives missing that prevent us from
running @racketmodname[racket/base]; it's actually a short list that
I'll be attacking once things stabilize.)















@section{The Whalesong language}

@defmodule/this-package[lang/base]

This needs to at least show all the bindings available from the base
language.

@defthing[true boolean]{The boolean value @racket[#t].}
@defthing[false boolean]{The boolean value @racket[#f].}
@defthing[pi number]{The math constant @racket[pi].}
@defthing[e number]{The math constant @racket[pi].}
@defthing[null null]{The empty list value @racket[null].}

@defproc[(boolean? [v any/c]) boolean?]{Returns true if v is @racket[#t] or @racket[#f]}



@defform[(let/cc id body ...)]{}
@defform[(null? ...)]{}
@defform[(not ...)]{}
@defform[(eq? ...)]{}
@defform[(equal? ...)]{}
@defform[(void ...)]{}



@subsection{IO}
@defform[(current-output-port ...)]{}
@defform[(current-print ...)]{}
@defform[(write ...)]{}
@defform[(write-byte ...)]{}
@defform[(display ...)]{}
@defform[(newline ...)]{}
@defform[(format ...)]{}
@defform[(printf ...)]{}
@defform[(fprintf ...)]{}
@defform[(displayln ...)]{}



@subsection{Numeric operations}
@defform[(number? ...)]{}
@defform[(+ ...)]{}
@defform[(- ...)]{}
@defform[(* ...)]{}
@defform[(/ ...)]{}
@defform[(= ...)]{}
@defform[(add1 ...)]{}
@defform[(sub1 ...)]{}
@defform[(< ...)]{}
@defform[(<= ...)]{}
@defform[(> ...)]{}
@defform[(>= ...)]{}
@defform[(abs ...)]{}
@defform[(quotient ...)]{}
@defform[(remainder ...)]{}
@defform[(modulo ...)]{}
@defform[(gcd ...)]{}
@defform[(lcm ...)]{}
@defform[(floor ...)]{}
@defform[(ceiling ...)]{}
@defform[(round ...)]{}
@defform[(truncate ...)]{}
@defform[(numerator ...)]{}
@defform[(denominator ...)]{}
@defform[(expt ...)]{}
@defform[(exp ...)]{}
@defform[(log ...)]{}
@defform[(sin ...)]{}
@defform[(sinh ...)]{}
@defform[(cos ...)]{}
@defform[(cosh ...)]{}
@defform[(tan ...)]{}
@defform[(asin ...)]{}
@defform[(acos ...)]{}
@defform[(atan ...)]{}
@defform[(sqr ...)]{}
@defform[(sqrt ...)]{}
@defform[(integer-sqrt ...)]{}
@defform[(sgn ...)]{}
@defform[(make-rectangular ...)]{}
@defform[(make-polar ...)]{}
@defform[(real-part ...)]{}
@defform[(imag-part ...)]{}
@defform[(angle ...)]{}
@defform[(magnitude ...)]{}
@defform[(conjugate ...)]{}
@defform[(string->number ...)]{}
@defform[(number->string ...)]{}
@defform[(random ...)]{}
@defform[(exact? ...)]{}
@defform[(integer? ...)]{}
@defform[(zero? ...)]{}

@subsection{String operations}
@defform[(string? s)]{}
@defform[(string ...)]{}
@defform[(string=? ...)]{}
@defform[(string->symbol ...)]{}
@defform[(string-length ...)] {}
@defform[(string-ref ...)] {}
@defform[(string-append ...)] {}
@defform[(string->list ...)] {}
@defform[(list->string ...)] {}



@subsection{Character operations}
@defform[(char? ch)]{}
@defform[(char=? ...)]{}




@subsection{Symbol operations}
@defform[(symbol? ...)]{}
@defform[(symbol->string? ...)]{}



@subsection{List operations}
@defform[(pair? ...)]{}
@defform[(cons ...)]{}
@defform[(car ...)]{}
@defform[(cdr ...)]{}
@defform[(list ...)]{}
@defform[(length ...)]{}
@defform[(append ...)]{}
@defform[(reverse ...)]{}
@defform[(map ...)]{}
@defform[(for-each ...)]{}
@defform[(member ...)]{}
@defform[(list-ref ...)]{}
@defform[(memq ...)]{}
@defform[(assq ...)]{}



@subsection{Vector operations}
@defform[(vector? ...)]{}
@defform[(make-vector ...)]{}
@defform[(vector ...)]{}
@defform[(vector-length ...)]{}
@defform[(vector-ref ...)]{}
@defform[(vector-set! ...)]{}
@defform[(vector->list ...)]{}
@defform[(list->vector ...)]{}










@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@section{Acknowledgements}
@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@;; shriram, kathi, emmanuel, everyone who helped with moby and wescheme
@;;
@;; also need to list out all the external libraries we're using
@;; and the license.


Whalesong uses code and utilities from the following external projects:
@itemlist[
@item{   jshashtable (@url{http://www.timdown.co.uk/jshashtable/})}
@item{   js-numbers (@url{http://github.com/dyoo/js-numbers/})}
@item{   JSON (@url{http://www.json.org/js.html})}
@item{   jquery (@url{http://jquery.com/})}
@item{   Google Closure Compiler (@url{http://code.google.com/p/closure-compiler/})}
]

The following folks have helped tremendously in the implementation of
Whalesong by implementing libraries, giving guidence, and suggesting
improvements:

@itemlist[
   @item{Ethan Cecchetti}
   @item{Scott Newman}
   @item{Zhe Zhang}
   @item{Jens Axel Søgaard}
   @item{Shriram Krishnamurthi}
   @item{Emmanuel Schanzer}
]