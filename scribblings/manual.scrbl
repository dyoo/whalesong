#lang scribble/manual
@(require planet/scribble
          planet/version
          planet/resolver
          scribble/eval
          racket/sandbox
          (for-label racket/base)
          racket/runtime-path
          "scribble-helpers.rkt"
          "../js-assembler/get-js-vm-implemented-primitives.rkt")


@inject-javascript|{
  var _gaq = _gaq || [];
  _gaq.push(['_setAccount', 'UA-24146890-1']);
  _gaq.push(['_trackPageview']);
 
  (function() {
    var ga = document.createElement('script'); ga.type = 'text/javascript'; ga.async = true;
    ga.src = ('https:' == document.location.protocol ? 'https://ssl' : 'http://www') + '.google-analytics.com/ga.js';
    var s = document.getElementsByTagName('script')[0]; s.parentNode.insertBefore(ga, s);
  })();      
}|


@(define-runtime-path whalesong-path "..")


@;; I may need an evaluator for some small examples.
@(define my-evaluator
   (call-with-trusted-sandbox-configuration 
    (lambda ()
      (parameterize ([sandbox-output 'string]
                     [sandbox-error-output 'string])
        (make-evaluator 'racket)))))



@title{Whalesong: a Racket to JavaScript compiler}
@author+email["Danny Yoo" "dyoo@cs.wpi.edu"]



@centered{@smaller{Source code can be found at:
@url{https://github.com/dyoo/whalesong}.  The latest version of this
document lives in @url{http://hashcollision.org/whalesong}.}}




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
(hopefully!) little modification, and provide access through the foreign-function
interface to native JavaScript APIs.  The included runtime library
also includes a framework to programming the web in functional
event-driven style.


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
@link["http://planet.racket-lang.org"]{PLaneT}, what's up there is probably
already out of date!  You may want to get the latest sources instead
of using the version on PLaneT.  Doing so
requires doing a little bit of manual work.  The steps are:

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




@subsection{Running Whalesong}

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
@filebox["dom-play.rkt"]{
@codeblock|{
#lang planet dyoo/whalesong

;; Uses the JavaScript FFI, which provides bindings for:
;; $ and call
(require (planet dyoo/whalesong/js))

;; insert-break: -> void
(define (insert-break)
  (call ($ "<br/>") "appendTo" body)
  (void))

;; write-message: any -> void
(define (write-message msg)
  (void (call (call (call ($ "<span/>") "text" msg)
                    "css" "white-space" "pre")
              "appendTo"
              body)))

;; Set the background green, and show some content
;; on the browser.
(void (call body "css" "background-color" "lightgreen"))
(void (call ($ "<h1>Hello World</h1>") "appendTo" body))
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
@section{Extended example}
@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(This example needs to use modules.  It should also show how we can use the
other command-line options to compress the javascript, and how to
use @tt{get-javascript} and @tt{get-runtime}, to allow the user to 
build a customized html file.)




@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@section{Reference}
@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(This section should describe the whalesong language.)



@subsection{The @filepath{whalesong} command-line}

(This section should describe the whalesong launcher and the options
we can use.)

(We want to add JavaScript compression here as an option.)

(We also need an example that shows how to use the get-javascript and get-runtime
commands to do something interesting...)

@subsection{@tt{build}}

@subsection{@tt{get-runtime}}

@subsection{@tt{get-javascript}}



@section{The JavaScript API}

(This needs to describe what hooks we've got from the JavaScript side of things.

In particular, we need to talk about the plt namespace constructed by the runtime,
and the major, external bindings, like @tt{plt.runtime.invokeMains})





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
library, which introduces a @tt{jsnums} namespace which provides the
numeric tower API.

Example uses of the @tt{js-numbers} library include:

@itemlist[
@item{Creating integers: @verbatim{42}  @verbatim{16}}

@item{Creating big integers: @verbatim{jsnums.makeBignum("29837419826")}}

@item{Creating floats: @verbatim{jsnums.makeFloat(3.1415)}}

@item{Predicate for numbers: @verbatim{jsnums.isSchemeNumber(42)}}

@item{Adding two numbers together: @verbatim{jsnums.add(42, jsnums.makeFloat(3.1415))}}

@item{Converting a jsnums number back into native JavaScript floats: @verbatim{jsnums.toFixnum(...)}}
]

Do all arithmetic using the functions in the @tt{jsnums} namespace.
One thing to also remember to do is apply @tt{jsnums.toFixnum} to any
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
The undefined values is 



@subsubsection{Boxes}
Boxes can be constructed with @tt{plt.runtime.makeBox(x)}.  They can be
tested with @tt{plt.runtime.isBox()}, and they support two methods:
@verbatim|{
   box.get(): returns the value in the box
   box.set(v): replaces the value in the box with v 
}|






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
@subsection{Incomplete features}
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
@item{box}
@item{placeholder}
@item{path}
@item{bytes}
@item{immutable bytes}
@item{keywords}
@item{hash}
@item{hasheq}
@item{color}
@item{structs}
@item{struct types}
@item{exceptions}
@item{thread cells}

@item{big bang info}
@item{worldConfig}
@item{effectType}
@item{renderEffectType}
@item{readerGraph}
]

What are the list of primitives in @filepath{js-vm-primitives.js}?  They are:
@(apply itemlist (map (lambda (name)
                        (item (symbol->string name)))
                      js-vm-primitive-names))



(I should catalog the bug list in GitHub, as well as the feature list,
so I have a better idea of what's needed to complete the project.)


(We also need a list of the primitives missing that prevent us from
running @racketmodname[racket/base]; it's actually a short list that
I'll be attacking once things stabilize.)





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
