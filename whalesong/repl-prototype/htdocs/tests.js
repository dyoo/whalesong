jQuery(document).ready(function() {
    "use strict";    

    var repl;                   // Will be initialized at the bottom
                                // of this file.
    var outputSpan = $("<span/>");

    var testsRunCount = 0;
    var failureCount = 0;

    var noteRedFailure = function(e) {
        failureCount++;
        $("#failure-index").css("display", "inline");
        $("#failure-index").append($("<a/>").attr("href", "#fail" + failureCount)
                                   .text("" + failureCount)).append(' ');
        var failMsgText = " FAIL" + ((e.message || e || '') ? 
                                     ": " + (e.message || e || '') : "");
        $(document.body).append($("<span/>").text(failMsgText)
                                .css("color", "red")
                                .css("white-space", "pre")
                                .append($("<a/>").attr("name", "fail" + failureCount)));
        $(document.body).css("background-color", "#eeaaaa");
    };


    var runTests = function(after) {
        runAsyncTests(0, after);
    };

    var asyncTests = [];
    var queueAsyncTest = function(name, f) {
        asyncTests.push({name: name, f: f});
    };

    var runAsyncTests = function(i, k) {
        if (i < asyncTests.length) {
            runAsyncTest(asyncTests[i].name,
                         asyncTests[i].f,
                         function() {
                             setTimeout(function() {
                                 runAsyncTests(i+1, k);
                             }, 10);
                         });
        } else {
            k();
        }
    };

    var runAsyncTest = function(name, f, k) {
        $(document.body).append("running " + name + "... ");
        var success = function() {
            $(document.body).append(" ok").append($("<br/>"));
            testsRunCount++;
            k();
        };
        var fail = function(e) {
            noteRedFailure(e);
            testsRunCount++;
	    $(document.body).append($("<br/>"));
            //	    $(document.body).append(e + '');
	    //$(document.body).append($("<br/>"));
            k();
        };
        try {
            f(success, fail);
        } catch(e) {
            fail(e);
        }
    };


    var queueTest = function(name, code, expectedText) {
        queueAsyncTest(name, function(success, fail) {
            var checkOutput = function(err) {
                var observedText = outputSpan.text().replace(/\n$/, "");
                if (observedText === expectedText) {
                    success();
                } else {
                    fail("not the same: " + observedText + 
                         ", " +
                         expectedText);
                }
            };
            // Reinitialize outputSpan.
            outputSpan = jQuery("<span/>");
            repl.reset(function() {
                repl.compileAndExecuteProgram(name, code, checkOutput,
                                              checkOutput);
                });
        });
    };

    var queueErrorTest = function(name, code, expectedErrorText) {
        queueAsyncTest(name, function(success, fail) {
            var checkOutput = function(err) {
                var errText = ((err && err.message) || err) + '';
                if (errText === expectedErrorText) {
                    success();
                } else {
                    fail("not the same: " + errText + 
                         ", " + expectedErrorText);
                }
            };
            repl.reset(function() {
                repl.compileAndExecuteProgram(name, code, checkOutput,  checkOutput);
            });
        });
    };



    //////////////////////////////////////////////////////////////////////


    queueTest("local",
              "(local [(define x 42)] x)",
              "42");
    queueTest("local",
              "(local [(define x 42) (define y 3)] (+ x y))",
              "45");
    queueTest("local",
              "(local [] 1)",
              "1");

    queueTest("test simple function application program",
              "(define (double x) (+ x x)) (double 25)",
              "50");

    queueTest("test simple function application program 2",
              "(define (double x) (+ x x)) (double (double 25))",
              "100");

    queueTest("simple structures 1",
              "(define-struct foo (x y)) (foo-x (make-foo 3 4))",
              "3");

    queueTest("simple structures 2",
              "(define-struct foo (x y)) (foo-y (make-foo 3 4))",
              "4");

    queueTest("simple structures predicate 1",
              "(define-struct foo (x y)) (foo? (make-foo 3 4))",
              "true");

    queueTest("simple structures predicate 2",
              "(define-struct foo (x y)) (foo? 'foo)",
              "false");

    queueTest("division",
              "(/ 18 2)",
              "9");
    

    queueTest("symbol string",
              "(symbol->string 'hello)",
              "\"hello\"");

    queueTest("string->symbol",
              "(string->symbol \"hello\")",
              "'hello");

    // queueTest("formatting lists",
    //           "(format \"~a\" '(1 2))",
    //           "\"(list 1 2)\"");


    queueTest("symbols that should not leak js implementation",
              "'constructor",
              "'constructor");

    queueTest("symbols that should not leak js implementation 2",
              "'hasOwnProperty",
              "'hasOwnProperty");

    queueTest("symbols that should not leak js implementation 3",
              "'__proto__",
              "'__proto__");


    queueTest("simple set!",
              "(define x 42) (set! x 16) x",
              "16")


    queueErrorTest("test mis-application 1",
                   "(define (double x) (+ x x)) (double double)",
                   "+: expects a number as 1st argument, but given: #<function:double>; other arguments were: #<function:double>");

    queueErrorTest("test mis-application 2",
                   "(define (double x) (+ x x)) (double double 25)",
                   "double: expects 1 argument, but given 2: #<function:double> 25");

    queueErrorTest("test a error in map",
                   '(map add1 (list "1"))',
                   'add1: expects a number as 1st argument, but given: "1"');
    

    queueErrorTest("test non-boolean in if test position",
                   "(if 3 'four 'five)",
                   'if: expected a boolean value, but found: 3');

    queueErrorTest("test if not enough args",
                   "(if)",
                   "if: expected a test, a consequence, and an alternative, but all three were not found");

    queueErrorTest("test if not enough args",
                   "(if true)",
                   "if: expected a test, a consequence, and an alternative, but all three were not found");

    queueErrorTest("test if not enough args",
                   "(if true false)",
                   "if: expected a test, a consequence, and an alternative, but all three were not found");

    queueErrorTest("test if too many args",
                   "(if true false true false)",
                   "if: expected only a test, a consequence, and an alternative, but found more than three of these");


    queueErrorTest("test non-boolean in 'or'",
                   "(or 42 4)",
                   'or: expected a boolean value, but found: 42');

    queueErrorTest("test non-boolean in 'or', second position",
                   "(or #f 4)",
                   'or: expected a boolean value, but found: 4');

    queueErrorTest("test and empty",
                   "(and)",
                   "and: expected at least 2 arguments, but given 0");

    queueErrorTest("test and empty",
                   "(and 1)",
                   "and: expected at least 2 arguments, but given 1");

    queueErrorTest("test and empty",
                   "(and 'foo)",
                   "and: expected at least 2 arguments, but given 1");

    queueErrorTest("test or empty",
                   "(or)",
                   "or: expected at least 2 arguments, but given 0");

    queueErrorTest("test or only one arg",
                   "(or 1)",
                   "or: expected at least 2 arguments, but given 1");

    queueErrorTest("test and empty",
                   "(or 'noo)",
                   "or: expected at least 2 arguments, but given 1");

    queueErrorTest("test non-boolean in 'and'",
                   "(and 'blah 4)",
                   "and: expected a boolean value, but found: blah");

    queueErrorTest("test non-boolean in 'and', second position",
                   "(and #t 'not-bool)",
                   "and: expected a boolean value, but found: not-bool");
    
    queueErrorTest("beside given 1 arg",
		   "(beside 5)",
		   "beside: expects at least 2 arguments, but given 1: 5");
    
    queueErrorTest("beside given 3 non-image args",
		   "(beside 2 3 3)",
		   "beside: expects an image as 1st argument, but given: 2; other arguments were: 3 3");
    
    queueErrorTest("beside given bad args, correct number",
		   "(beside 1 1)",
		   "beside: expects an image as 1st argument, but given: 1; other arguments were: 1");

    queueErrorTest("beside given 0 args",
		   "(beside)",
		   "beside: expects at least 2 arguments, but given 0");

    queueErrorTest("beside/align give too few args",
		   "(beside/align 3 3)",
		   "beside/align: expects at least 3 arguments, but given 2: 3 3");
    queueErrorTest("beside/align given bad args",
		   "(beside/align 3 3 3 3 3 3)",
		   "beside/align: expects a y-place as 1st argument, but given: 3; other arguments were: 3 3 3 3 3");

    queueErrorTest("beside/align given bad args, correct number",
		   "(beside/align 1 1 1)",
		   "beside/align: expects a y-place as 1st argument, but given: 1; other arguments were: 1 1");

    queueErrorTest("above given 0 args",
		   "(above)",
		   "above: expects at least 2 arguments, but given 0");
    
    queueErrorTest("above given bad args",
		   "(above 2 1 1)",
		   "above: expects an image as 1st argument, but given: 2; other arguments were: 1 1");
    
    queueErrorTest("above given bad args, correct number",
		   "(above 1 1)",
		   "above: expects an image as 1st argument, but given: 1; other arguments were: 1");

    queueErrorTest("above/align given no args",
		   "(above/align)",
		   "above/align: expects at least 3 arguments, but given 0");

    queueErrorTest("above/align given bad args, correct number",
		   "(above/align 1 1 1)",
		   "above/align: expects a x-place as 1st argument, but given: 1; other arguments were: 1 1");

    queueErrorTest("above/align given too many args, all bad",
		   "(above/align 1 1 1 1 1)",
		   "above/align: expects a x-place as 1st argument, but given: 1; other arguments were: 1 1 1 1"); 

    queueErrorTest("rotate given no args",
		   "(rotate)",
		   "rotate: expects 2 arguments, but given 0");
    
    queueErrorTest("rotate given correct number of bad args",
		   "(rotate 1 1)",
		   "rotate: expects an image as 2nd argument, but given: 1; other arguments were: 1");

    queueErrorTest("rotate given too many args",
		   "(rotate 1 1 1 1 1 1)",
		   "rotate: expects 2 arguments, but given 6: 1 1 1 1 1 1");

    queueErrorTest("crop given no args",
		   "(crop)",
		   "crop: expects 5 arguments, but given 0");
    
    queueErrorTest("crop given correct # args, but bad args",
		   "(crop 1 1 1 1 1)",
		   "crop: expects an image as 5th argument, but given: 1; other arguments were: 1 1 1 1");
    
    queueErrorTest("frame given no args",
		   "(frame)",
		   "frame: expects 1 argument, but given 0");

    queueErrorTest("frame given correct number args, but bad args",
		   "(frame 1)",
		   "frame: expects an image as 1st argument, but given: 1");

    queueErrorTest("frame given too many args",
		   "(frame 1 1)",
		   "frame: expects 1 argument, but given 2: 1 1");
    
    queueErrorTest("flip-vertical given no args",
		   "(flip-vertical)",
		   "flip-vertical: expects 1 argument, but given 0");

    queueErrorTest("flip-vertical given correct amount of args, but bad args",
		   "(flip-vertical 1)",
		   "flip-vertical: expects an image as 1st argument, but given: 1");

    queueErrorTest("flip-vertical given too many args",
		   "(flip-vertical 1 1)",
		   "flip-vertical: expects 1 argument, but given 2: 1 1");
    
    queueErrorTest("flip-horizontal given no args",
		   "(flip-horizontal)",
		   "flip-horizontal: expects 1 argument, but given 0");

    queueErrorTest("flip-horizontal given correct amount of args, but bad args",
		   "(flip-horizontal 1)",
		   "flip-horizontal: expects an image as 1st argument, but given: 1");

    queueErrorTest("flip-horizontal given too many args",
		   "(flip-horizontal 1 1)",
		   "flip-horizontal: expects 1 argument, but given 2: 1 1");

    queueErrorTest("text given no args",
		   "(text)",
		   "text: expects 3 arguments, but given 0");

    queueErrorTest("text given correct # args, but bad args",
		   "(text 1 1 1)",
		   "text: expects a string as 1st argument, but given: 1; other arguments were: 1 1");

    queueErrorTest("text given too many args",
		   "(text 1 1 1 1)",
		   "text: expects 3 arguments, but given 4: 1 1 1 1");

    queueErrorTest("text/font given bad number of args",
		   "(text/font 1)",
		   "text/font: expects 8 arguments, but given 1: 1");

    queueErrorTest("text/font given bad but correct amount of args",
		   "(text/font 1 1 1 1 1 1 1 1)",
		   "text/font: expects a string as 1st argument, but given: 1; other arguments were: 1 1 1 1 1 1 1");

    queueErrorTest("bitmap/url given bad amount of args",
		   "(bitmap/url)",
		   "image-url: expects 1 argument, but given 0");

    queueErrorTest("bitmap/url given correct amount of args, but bad as",
		   "(bitmap/url 1)",
		   "image-url: expects a string as 1st argument, but given: 1");

    queueErrorTest("video-url given bad amount of args",
		   "(video-url)",
		   "video-url: expects 1 argument, but given 0");
    
    queueErrorTest("video-url given correct arg amount, but bad as",
		   "(video-url 1)",
		   "video-url: expects a string as 1st argument, but given: 1");

    queueErrorTest("image-width given no args",
		   "(image-width)",
		   "image-width: expects 1 argument, but given 0");

    queueErrorTest("image-width given correct arg amount, but bad as",
		   "(image-width 1)",
		   "image-width: expects an image as 1st argument, but given: 1");

    queueErrorTest("image-height given no args",
		   "(image-height)",
		   "image-height: expects 1 argument, but given 0");

    queueErrorTest("image-height given correct arg amount, but bad as",
		   "(image-height 1)",
		   "image-height: expects an image as 1st argument, but given: 1");

    queueErrorTest("image-baseline given no args",
		   "(image-baseline)",
		   "image-baseline: expects 1 argument, but given 0");

    queueErrorTest("image-baseline given correct arg amount, but bad as",
		   "(image-baseline 1)",
		   "image-baseline: expects an image as 1st argument, but given: 1");
    
    queueErrorTest("color-list->image bad arg amount",
		   "(color-list->image)",
		   "color-list->image: expects 5 arguments, but given 0");

    queueErrorTest("color-list->bitmap bad arg a",
		   "(color-list->bitmap 1 1 1 1 1)",
		   "color-list->bitmap: expects 3 arguments, but given 5: 1 1 1 1 1");

    queueErrorTest("color-list->bitmap bad arg amount",
		   "(color-list->bitmap)",
		   "color-list->bitmap: expects 3 arguments, but given 0");

    queueErrorTest("color-list->image bad arg a",
		   "(color-list->image 1 1 1 1 1)",
		   "color-list->image: expects a list of image as 1st argument, but given: 1");
    
    queueErrorTest("mode? no args",
		   "(mode?)",
		   "mode?: expects 1 argument, but given 0");
    
    queueErrorTest("mode? too many args",
		   "(mode? 1 1)",
		   "mode?: expects 1 argument, but given 2: 1 1");
    
    queueErrorTest("image-color? no args",
		   "(image-color?)",
		   "image-color?: expects 1 argument, but given 0");
    
    queueErrorTest("image-color? too many args",
		   "(image-color? 1 1)",
		   "image-color?: expects 1 argument, but given 2: 1 1");

    queueErrorTest("name->color no args",
		   "(name->color)",
		   "name->color: expects 1 argument, but given 0");
    
    queueErrorTest("name->color too many args",
		   "(name->color 1 1)",
		   "name->color: expects 1 argument, but given 2: 1 1");

    queueErrorTest("x-place? no args",
		   "(x-place?)",
		   "x-place?: expects 1 argument, but given 0");
    
    queueErrorTest("x-place? too many args",
		   "(x-place? 1 1)",
		   "x-place?: expects 1 argument, but given 2: 1 1");

    queueErrorTest("y-place? no args",
		   "(y-place?)",
		   "y-place?: expects 1 argument, but given 0");
    
    queueErrorTest("y-place? too many args",
		   "(y-place? 1 1)",
		   "y-place?: expects 1 argument, but given 2: 1 1");

    queueErrorTest("angle? no args",
		   "(angle?)",
		   "angle?: expects 1 argument, but given 0");

    queueErrorTest("angle? too many args",
		   "(angle? 1 1)",
		   "angle?: expects 1 argument, but given 2: 1 1");

    queueErrorTest("side-count? no args",
		   "(side-count?)",
		   "side-count?: expects 1 argument, but given 0");

    queueErrorTest("side-count? too many args",
		   "(side-count? 1 1)",
		   "side-count?: expects 1 argument, but given 2: 1 1");
    
    queueErrorTest("step-count? no args",
		   "(step-count?)",
		   "step-count?: expects 1 argument, but given 0");

    queueErrorTest("step-count? too many args",
		   "(step-count? 1 1)",
		   "step-count?: expects 1 argument, but given 2: 1 1");

    queueErrorTest("on-tick bad args amount",
		   "(on-tick)",
		   "on-tick: expects 1 or 2 arguments, but given 0");

    queueErrorTest("on-tick 1 arg",
		   "(on-tick 1)", 
		   "on-tick: expects a function name as 1st argument, but given: 1");

    queueErrorTest("on-tick 2 args",
		   "(on-tick 1 2)",
		   "on-tick: expects a function name as 1st argument, but given: 1; other arguments were: 2");
    

    queueErrorTest("on-tap bad arg amount",
		   "(on-tap)",
		   "on-tap: expects 1 argument, but given 0");

    queueErrorTest("on-tap bad arg a",
		   "(on-tap 1)",
		   "on-tap: expects a function name as 1st argument, but given: 1");

    queueErrorTest("on-tilt bad arg amount",
		   "(on-tilt)",
		   "on-tilt: expects 1 argument, but given 0");

    queueErrorTest("on-tilt bad arg a",
		   "(on-tilt 1)",
		   "on-tilt: expects a function name as 1st argument, but given: 1");

    queueErrorTest("on-key bad arg amount",
		   "(on-key)",
		   "on-key: expects 1 argument, but given 0");
    
    queueErrorTest("on-key bad arg a",
		   "(on-key 1)",
		   "on-key: expects a function name as 1st argument, but given: 1");


    queueErrorTest("stop-when bad arg amount",
		   "(stop-when)",
		   "stop-when: expects 1 argument, but given 0");
    
    queueErrorTest("stop-when bad arg a",
		   "(stop-when 1)",
		   "stop-when: expects a function name as 1st argument, but given: 1");
    /*
      queueErrorTest("stop-when! bad arg amount",
      "(stop-when!)",
      "stop-when!: expects 2 arguments, but given 0");
      
      queueErrorTest("stop-when! bad arg a",
      "(stop-when! 1 1)",
      "stop-when!: expects a function name as 1st argument, but given: 1; other arguments were: 1");
    */
    queueErrorTest("on-redraw bad arg amount",
		   "(on-redraw)",
		   "on-redraw: expects 1 argument, but given 0");

    queueErrorTest("on-redraw bad arg a",
		   "(on-redraw 1)",
		   "on-redraw: expects a function name as 1st argument, but given: 1");



    queueErrorTest("on-draw bad arg amount",
		   "(on-draw)",
		   "on-draw: expects 1 or 2 arguments, but given 0");
    
    queueErrorTest("on-draw bad arg a",
		   "(on-draw 1)",
		   "on-draw: expects a function name as 1st argument, but given: 1");

    queueErrorTest("on-draw bad arg as",
		   "(on-draw 1 2)",
		   "on-draw: expects a function name as 1st argument, but given: 1; other arguments were: 2");

    queueErrorTest("initial-effect bad arg amount (no args)",
		   "(initial-effect)",
		   "initial-effect: expects 1 argument, but given 0");

    queueErrorTest("initial-effect too many args",
		   "(initial-effect 1 2)",
		   "initial-effect: expects 1 argument, but given 2: 1 2");

    queueErrorTest("big-bang given no arguments",
		   "(big-bang)",
		   "big-bang: expects at least 1 argument, but given 0");

    queueErrorTest("big-bang given wrong type of arg as 2nd arg",
		   "(big-bang 1 1)",
		   "big-bang: expects a handler as 2nd argument, but given: 1; other arguments were: 1");

    queueErrorTest("make-struct-type wrong number of args",
		   "(make-struct-type)",
		   "make-struct-type: expects 4 or 5 or 6 or 7 or 8 or 9 or 10 arguments, but given 0");
    
    queueErrorTest("make-struct-type give wrong arg type",
		   "(make-struct-type 1 1 1 1)",
		   "make-struct-type: expects a symbol as 1st argument, but given: 1; other arguments were: 1 1 1");

    queueErrorTest("make-struct-field-accessor wrong number of args",
		   "(make-struct-field-accessor)",
		   "make-struct-field-accessor: expects 2 or 3 arguments, but given 0");
    
    queueErrorTest("make-struct-field-accessor given wrong arg type",
		   "(make-struct-field-accessor 1 1 1)",
		   "make-struct-field-accessor: expects an accessor procedure that requires a field index as 1st argument, but given: 1; other arguments were: 1 1");

    queueErrorTest("make-struct-field-accessor given 2 wrong args",
		   "(make-struct-field-accessor 1 1)", 
		   "make-struct-field-accessor: expects an accessor procedure that requires a field index as 1st argument, but given: 1; other arguments were: 1");

    queueErrorTest("make-struct-field-mutator given 2 args, bad a",
		   "(make-struct-field-mutator 1 1)",
		   "make-struct-field-mutator: expects a mutator procedure that requires a field index as 1st argument, but given: 1; other arguments were: 1");

    queueErrorTest("make-struct-field-mutator given no args",
		   "(make-struct-field-mutator)",
		   "make-struct-field-mutator: expects 2 or 3 arguments, but given 0");

    queueErrorTest("make-struct-field-mutator given 3 args, bad a",
		   "(make-struct-field-mutator 1 1 1)",
		   "make-struct-field-mutator: expects a mutator procedure that requires a field index as 1st argument, but given: 1; other arguments were: 1 1");

    queueErrorTest("procedure-arity given no args",
		   "(procedure-arity)",
		   "procedure-arity: expects 1 argument, but given 0");

    queueErrorTest("procedure-arity given bad arg a",
		   "(procedure-arity 1)",
		   "procedure-arity: expects a function name as 1st argument, but given: 1");
    
    queueErrorTest("procedure-arity given way too many args",
		   "(procedure-arity 1 1 1 1 1 1 1 1 1 1)",
		   "procedure-arity: expects 1 argument, but given 10: 1 1 1 1 1 1 1 1 1 1");

    queueErrorTest("apply given no args",
		   "(apply)",
		   "apply: expects at least 2 arguments, but given 0");

    queueErrorTest("apply given bad arg as",
		   "(apply 1 1)",
		   "apply: expects a function name as 1st argument, but given: 1; other arguments were: 1");
    
    queueErrorTest("apply given bad arg a for second arg",
		   "(apply add1 1)",
		   "apply: expects a list as 2nd argument, but given: 1; other arguments were: #<function:add1>");

    queueErrorTest("compose given bad arg a",
		   "(compose 1)",
		   "compose: expects a function name as 1st argument, but given: 1");
    
    queueErrorTest("compose given bad 2nd arg a",
		   "(compose add1 1)",
		   "compose: expects a function name as 2nd argument, but given: 1; other arguments were: #<function:add1>");
    
    queueErrorTest("current-inexact-milliseconds given too many args",
		   "(current-inexact-milliseconds 1)",
		   "current-inexact-milliseconds: expects 0 arguments, but given 1: 1");

    queueErrorTest("current-seconds given too many args",
		   "(current-seconds 1)",
		   "current-seconds: expects 0 arguments, but given 1: 1");	

    queueErrorTest("not given no args",
		   "(not)",
		   "not: expects 1 argument, but given 0");
    
    queueErrorTest("not given too many args",
		   "(not 1 1)",
		   "not: expects 1 argument, but given 2: 1 1");

    queueErrorTest("random given too many args",
		   "(random 1 1)",
		   "random: expects 0 or 1 arguments, but given 2: 1 1");
    
    queueErrorTest("random given bad arg a",
		   "(random add1)",
		   "random: expects a non-negative exact integer as 1st argument, but given: #<function:add1>");

    queueErrorTest("sleep given bad arg a",
		   "(sleep add1)",
		   "sleep: expects a non-negative real number as 1st argument, but given: #<function:add1>");

    queueErrorTest("sleep given too many args",
		   "(sleep 1 2)",
		   "sleep: expects 0 or 1 arguments, but given 2: 1 2");
    
    queueErrorTest("identity given no args",
		   "(identity)",
		   "identity: expects 1 argument, but given 0");
    
    queueErrorTest("identity given too many args",
		   "(identity 2 2)",
		   "identity: expects 1 argument, but given 2: 2 2");
    
    queueErrorTest("raise given no args",
		   "(raise)",
		   "raise: expects 1 argument, but given 0");
    
    queueErrorTest("raise given too many args",
		   "(raise 1 1)",
		   "raise: expects 1 argument, but given 2: 1 1");
    
    queueErrorTest("error given no args",
		   "(error)",
		   "error: expects at least 1 argument, but given 0");
    
    queueErrorTest("error given bad a",
		   "(error 1)",
		   "error: expects a symbol or string as 1st argument, but given: 1");
    
    queueErrorTest("make-exn given no args",
		   "(make-exn)",
		   "make-exn: expects 2 arguments, but given 0");
    
    queueErrorTest("make-exn given bad arg as",
		   "(make-exn 1 1)",
		   "exn: expects argument of type continuation mark set, but given: 1");
    
    queueErrorTest("* given bad arg",
		   "(* add1)",
		   "*: expects a number as 1st argument, but given: #<function:add1>");
    
    queueErrorTest("/ given insufficient number of arguments",
		   "(/ 1)",
		   "/: expects at least 2 arguments, but given 1: 1");

    queueErrorTest("/ given bad arg 1st position",
		   "(/ add1 1)",
		   "/: expects a number as 1st argument, but given: #<function:add1>; other arguments were: 1");

    queueErrorTest("/ given bad arg 2nd position",
		   "(/ 1 add1)",
		   "/: expects a number as 2nd argument, but given: #<function:add1>; other arguments were: 1");

    queueErrorTest("/ given bad arg 3rd position",
		   "(/ 1 2 add1)",
		   "/: expects a number as 3rd argument, but given: #<function:add1>; other arguments were: 1 2");

    queueErrorTest("- given bad arg",
		   "(- add1)",
		   "-: expects a number as 1st argument, but given: #<function:add1>");

    queueErrorTest("+ given bad arg",
		   "(+ add1)",
		   "+: expects a number as 1st argument, but given: #<function:add1>");

    queueErrorTest("/ given no arguments",
		   "(/)",
		   "/: expects at least 2 arguments, but given 0");

    queueErrorTest("/ given only one argument",
                   "(/ 5)",
                   "/: expects at least 2 arguments, but given 1: 5");

    queueErrorTest("- given no args",
		   "(-)",
		   "-: expects at least 1 argument, but given 0");
    
    queueErrorTest("= given no args",
		   "(=)",
		   "=: expects at least 2 arguments, but given 0");

    queueErrorTest("= given bad 2nd arg",
		   "(= 1 sub1)",
		   "=: expects a number as 2nd argument, but given: #<function:sub1>; other arguments were: 1");
    
    queueErrorTest("= given bad first arg",
		   "(= sub1 1)",
		   "=: expects a number as 1st argument, but given: #<function:sub1>; other arguments were: 1");
    
    queueErrorTest("=~ given no args",
		   "(=~)",
		   "=~: expects 3 arguments, but given 0");
    
    queueErrorTest("=~ given bad as",
		   "(=~ add1 sub1 add1)",
		   "=~: expects a real as 1st argument, but given: #<function:add1>; other arguments were: #<function:sub1> #<function:add1>");
    
    queueErrorTest("sub1 given no args",
		   "(sub1)",
		   "sub1: expects 1 argument, but given 0");
    
    queueErrorTest("sub1 given a bad arg",
		   "(sub1 add1)",
		   "sub1: expects a number as 1st argument, but given: #<function:add1>");

    queueErrorTest("sub1 given too many args",
		   "(sub1 1 1 1)",
		   "sub1: expects 1 argument, but given 3: 1 1 1");
    
    queueErrorTest("add1 given too many args",
		   "(add1 1 1 1)",
		   "add1: expects 1 argument, but given 3: 1 1 1");

    queueErrorTest("add1 given no args",
		   "(add1)",
		   "add1: expects 1 argument, but given 0");
    
    queueErrorTest("add1 given a bad arg",
		   "(add1 add1)",
		   "add1: expects a number as 1st argument, but given: #<function:add1>");
    

    queueErrorTest("< given no args",
		   "(<)",
		   "<: expects at least 2 arguments, but given 0");

    queueErrorTest("< given 1 bad arg",
		   "(< add1 1)",
		   "<: expects a number as 1st argument, but given: #<function:add1>; other arguments were: 1");
    
    queueErrorTest("< given 1 bad arg (2nd arg)",
		   "(< 1 add1)",
		   "<: expects a number as 2nd argument, but given: #<function:add1>; other arguments were: 1");
    
    queueErrorTest("< given too few args",
		   "(< 1)",
		   "<: expects at least 2 arguments, but given 1: 1");

    queueErrorTest("> given no args",
		   "(>)",
		   ">: expects at least 2 arguments, but given 0");

    queueErrorTest("> given 1 bad arg",
		   "(> add1 1)",
		   ">: expects a number as 1st argument, but given: #<function:add1>; other arguments were: 1");
    
    queueErrorTest("> given 1 bad arg (2nd arg)",
		   "(> 1 add1)",
		   ">: expects a number as 2nd argument, but given: #<function:add1>; other arguments were: 1");
    
    queueErrorTest("> given too few args",
		   "(> 1)",
		   ">: expects at least 2 arguments, but given 1: 1");


    queueErrorTest("<= given no args",
		   "(<=)",
		   "<=: expects at least 2 arguments, but given 0");
    
    queueErrorTest("<= given bad arg (first arg))",
		   "(<= add1 1)",
		   "<=: expects a number as 1st argument, but given: #<function:add1>; other arguments were: 1");
    
    queueErrorTest("<= given 1 bad arg (2nd arg)",
		   "(<= 1 add1)",
		   "<=: expects a number as 2nd argument, but given: #<function:add1>; other arguments were: 1");
    
    queueErrorTest("<= given too few args",
		   "(<= 1)",
		   "<=: expects at least 2 arguments, but given 1: 1");

    queueErrorTest(">= given no args",
		   "(>=)",
		   ">=: expects at least 2 arguments, but given 0");
    
    queueErrorTest(">= given bad arg (first arg))",
		   "(>= add1 1)",
		   ">=: expects a number as 1st argument, but given: #<function:add1>; other arguments were: 1");
    
    queueErrorTest(">= given 1 bad arg (2nd arg)",
		   "(>= 1 add1)",
		   ">=: expects a number as 2nd argument, but given: #<function:add1>; other arguments were: 1");
    
    queueErrorTest(">= given too few args",
		   "(>= 1)",
		   ">=: expects at least 2 arguments, but given 1: 1");
    
    queueErrorTest("abs given no args",
		   "(abs)",
		   "abs: expects 1 argument, but given 0");
    
    queueErrorTest("abs given 1 bad arg",
		   "(abs add1)",
		   "abs: expects a real as 1st argument, but given: #<function:add1>");

    queueErrorTest("abs given too many args",
		   "(abs 1 1)",
		   "abs: expects 1 argument, but given 2: 1 1");
    
    queueErrorTest("quotient given no args",
		   "(quotient)",
		   "quotient: expects 2 arguments, but given 0");
    
    queueErrorTest("quotient given 1 bad arg (first arg))",
		   "(quotient add1 1)",
		   "quotient: expects an integer as 1st argument, but given: #<function:add1>; other arguments were: 1");
    
    queueErrorTest("quotient given 1 bad arg (2nd arg)",
		   "(quotient 1 add1)",
		   "quotient: expects an integer as 2nd argument, but given: #<function:add1>; other arguments were: 1");
    
    queueErrorTest("quotient given too many args",
		   "(quotient 1 1 1)",
		   "quotient: expects 2 arguments, but given 3: 1 1 1");

    queueErrorTest("remainder given no args",
		   "(remainder)",
		   "remainder: expects 2 arguments, but given 0");
    
    queueErrorTest("remainder given 1 bad arg (first arg))",
		   "(remainder add1 1)",
		   "remainder: expects an integer as 1st argument, but given: #<function:add1>; other arguments were: 1");
    
    queueErrorTest("remainder given 1 bad arg (2nd arg)",
		   "(remainder 1 add1)",
		   "remainder: expects an integer as 2nd argument, but given: #<function:add1>; other arguments were: 1");
    
    queueErrorTest("remainder given too many args",
		   "(remainder 1 1 1)",
		   "remainder: expects 2 arguments, but given 3: 1 1 1");

    queueErrorTest("modulo given no args",
		   "(modulo)",
		   "modulo: expects 2 arguments, but given 0");
    
    queueErrorTest("modulo given 1 bad arg (first arg))",
		   "(modulo add1 1)",
		   "modulo: expects an integer as 1st argument, but given: #<function:add1>; other arguments were: 1");
    
    queueErrorTest("modulo given 1 bad arg (2nd arg)",
		   "(modulo 1 add1)",
		   "modulo: expects an integer as 2nd argument, but given: #<function:add1>; other arguments were: 1");
    
    queueErrorTest("modulo given too many args",
		   "(modulo 1 1 1)",
		   "modulo: expects 2 arguments, but given 3: 1 1 1");
    
    queueErrorTest("max given no args",
		   "(max)",
		   "max: expects at least 1 argument, but given 0");
    
    queueErrorTest("max given 1 bad arg",
		   "(max add1)",
		   "max: expects a real as 1st argument, but given: #<function:add1>");
    
    queueErrorTest("max given 1 good 1 bad arg",
		   "(max 1 add1)",
		   "max: expects a real as 2nd argument, but given: #<function:add1>; other arguments were: 1");
    
    queueErrorTest("min given no args",
		   "(min)",
		   "min: expects at least 1 argument, but given 0");
    
    queueErrorTest("min given 1 bad arg",
		   "(min add1)",
		   "min: expects a real as 1st argument, but given: #<function:add1>");
    
    queueErrorTest("min given 1 good 1 bad arg",
		   "(min 1 add1)",
		   "min: expects a real as 2nd argument, but given: #<function:add1>; other arguments were: 1");
    
    queueErrorTest("gcd given no args",
		   "(gcd)",
		   "gcd: expects at least 1 argument, but given 0");
    
    queueErrorTest("gcd given 1 bad arg",
		   "(gcd add1)",
		   "gcd: expects an integer as 1st argument, but given: #<function:add1>");
    
    queueErrorTest("gcd given 1 good 1 bad arg",
		   "(gcd 1 add1)",
		   "gcd: expects an integer as 2nd argument, but given: #<function:add1>; other arguments were: 1");

    queueErrorTest("lcm given no args",
		   "(lcm)",
		   "lcm: expects at least 1 argument, but given 0");
    
    queueErrorTest("lcm given 1 bad arg",
		   "(lcm add1)",
		   "lcm: expects an integer as 1st argument, but given: #<function:add1>");
    
    queueErrorTest("lcm given 1 good 1 bad arg",
		   "(lcm 1 add1)",
		   "lcm: expects an integer as 2nd argument, but given: #<function:add1>; other arguments were: 1");
    
    queueErrorTest("floor given no args",
		   "(floor)",
		   "floor: expects 1 argument, but given 0");
    
    queueErrorTest("floor given 1 bad arg",
		   "(floor add1)",
		   "floor: expects a real as 1st argument, but given: #<function:add1>");
    
    queueErrorTest("floor given too many args",
		   "(floor 1 1 1)",
		   "floor: expects 1 argument, but given 3: 1 1 1");
    
    queueErrorTest("ceiling given no args",
		   "(ceiling)",
		   "ceiling: expects 1 argument, but given 0");
    
    queueErrorTest("ceiling given 1 bad arg",
		   "(ceiling add1)",
		   "ceiling: expects a real as 1st argument, but given: #<function:add1>");
    
    queueErrorTest("ceiling given too many args",
		   "(ceiling 1 1 1)",
		   "ceiling: expects 1 argument, but given 3: 1 1 1");

    queueErrorTest("round given no args",
		   "(round)",
		   "round: expects 1 argument, but given 0");
    
    queueErrorTest("round given 1 bad arg",
		   "(round add1)",
		   "round: expects a real as 1st argument, but given: #<function:add1>");
    
    queueErrorTest("round given too many args",
		   "(round 1 1 1)",
		   "round: expects 1 argument, but given 3: 1 1 1");

    queueErrorTest("numerator given no args",
		   "(numerator)",
		   "numerator: expects 1 argument, but given 0");
    
    queueErrorTest("numerator given 1 bad arg",
		   "(numerator add1)",
		   "numerator: expects a rational number as 1st argument, but given: #<function:add1>");
    
    queueErrorTest("numerator given too many args",
		   "(numerator 1 1 1)",
		   "numerator: expects 1 argument, but given 3: 1 1 1");

    queueErrorTest("denominator given no args",
		   "(denominator)",
		   "denominator: expects 1 argument, but given 0");
    
    queueErrorTest("denominator given 1 bad arg",
		   "(denominator add1)",
		   "denominator: expects a rational number as 1st argument, but given: #<function:add1>");
    
    queueErrorTest("denominator given too many args",
		   "(denominator 1 1 1)",
		   "denominator: expects 1 argument, but given 3: 1 1 1");

    queueErrorTest("expt given no args",
		   "(expt)",
		   "expt: expects 2 arguments, but given 0");
    
    queueErrorTest("expt given bad first arg, valid 2nd arg",
		   "(expt add1 1)",
		   "expt: expects a number as 1st argument, but given: #<function:add1>; other arguments were: 1");
    
    queueErrorTest("expt given valid 1st arg, bad 2nd arg",
		   "(expt 1 add1)",
		   "expt: expects a number as 2nd argument, but given: #<function:add1>; other arguments were: 1");

    queueErrorTest("expt given too many args",
		   "(expt 1 1 1)",
		   "expt: expects 2 arguments, but given 3: 1 1 1");
    
    queueErrorTest("exp given no args",
		   "(exp)",
		   "exp: expects 1 argument, but given 0");
    
    queueErrorTest("exp given 1 bad arg",
		   "(exp add1)",
		   "exp: expects a number as 1st argument, but given: #<function:add1>");

    queueErrorTest("exp given too many bad args",
		   "(exp 1 1)",
		   "exp: expects 1 argument, but given 2: 1 1");

    queueErrorTest("log given no args",
		   "(log)",
		   "log: expects 1 argument, but given 0");

    queueErrorTest("log given 1 bad arg",
		   "(log add1)",
		   "log: expects a number as 1st argument, but given: #<function:add1>");

    queueErrorTest("log given too many args",
		   "(log 1 1)",
		   "log: expects 1 argument, but given 2: 1 1");
    
    queueErrorTest("sin given no args",
		   "(sin)",
		   "sin: expects 1 argument, but given 0");
    
    queueErrorTest("sin given 1 bad arg",
		   "(sin add1)",
		   "sin: expects a number as 1st argument, but given: #<function:add1>");
    
    queueErrorTest("sin given too many args",
		   "(sin 1 1)",
		   "sin: expects 1 argument, but given 2: 1 1");
    //next trig func
    
    queueErrorTest("cos given no args",
		   "(cos)",
		   "cos: expects 1 argument, but given 0");
    
    queueErrorTest("cos given 1 bad arg",
		   "(cos add1)",
		   "cos: expects a number as 1st argument, but given: #<function:add1>");
    
    queueErrorTest("cos given too many args",
		   "(cos 1 1)",
		   "cos: expects 1 argument, but given 2: 1 1");
    //next trig func

    queueErrorTest("tan given no args",
		   "(tan)",
		   "tan: expects 1 argument, but given 0");
    
    queueErrorTest("tan given 1 bad arg",
		   "(tan add1)",
		   "tan: expects a number as 1st argument, but given: #<function:add1>");
    
    queueErrorTest("tan given too many args",
		   "(tan 1 1)",
		   "tan: expects 1 argument, but given 2: 1 1");
    //next trig func

    queueErrorTest("asin given no args",
		   "(asin)",
		   "asin: expects 1 argument, but given 0");
    
    queueErrorTest("asin given 1 bad arg",
		   "(asin add1)",
		   "asin: expects a number as 1st argument, but given: #<function:add1>");
    
    queueErrorTest("asin given too many args",
		   "(asin 1 1)",
		   "asin: expects 1 argument, but given 2: 1 1");
    //next trig func

    queueErrorTest("acos given no args",
		   "(acos)",
		   "acos: expects 1 argument, but given 0");
    
    queueErrorTest("acos given 1 bad arg",
		   "(acos add1)",
		   "acos: expects a number as 1st argument, but given: #<function:add1>");
    
    queueErrorTest("acos given too many args",
		   "(acos 1 1)",
		   "acos: expects 1 argument, but given 2: 1 1");
    //next trig func

    queueErrorTest("atan given no args",
		   "(atan)",
		   "atan: expects 1 argument, but given 0");
    
    queueErrorTest("atan given 1 bad arg",
		   "(atan add1)",
		   "atan: expects a number as 1st argument, but given: #<function:add1>");
    
    queueErrorTest("atan given too many args",
		   "(atan 1 1)",
		   "atan: expects 1 argument, but given 2: 1 1");
    //next trig func
    
    
    queueErrorTest("sinh given no args",
		   "(sinh)",
		   "sinh: expects 1 argument, but given 0");
    
    queueErrorTest("sinh given 1 bad arg",
		   "(sinh add1)",
		   "sinh: expects a number as 1st argument, but given: #<function:add1>");
    
    queueErrorTest("sinh given too many args",
		   "(sinh 1 1)",
		   "sinh: expects 1 argument, but given 2: 1 1");
    //next trig func
    
    queueErrorTest("cosh given no args",
		   "(cosh)",
		   "cosh: expects 1 argument, but given 0");
    
    queueErrorTest("cosh given 1 bad arg",
		   "(cosh add1)",
		   "cosh: expects a number as 1st argument, but given: #<function:add1>");
    
    queueErrorTest("cosh given too many args",
		   "(cosh 1 1)",
		   "cosh: expects 1 argument, but given 2: 1 1");
    
    queueErrorTest("sqr given no args",
		   "(sqr)",
		   "sqr: expects 1 argument, but given 0");
    
    queueErrorTest("sqr given 1 bad arg",
		   "(sqr add1)",
		   "sqr: expects a number as 1st argument, but given: #<function:add1>");
    
    queueErrorTest("sqr given too many args",
		   "(sqr 1 1)",
		   "sqr: expects 1 argument, but given 2: 1 1");
    
    queueErrorTest("sqrt given no args",
		   "(sqrt)",
		   "sqrt: expects 1 argument, but given 0");

    queueErrorTest("sqrt given 1 bad arg",
		   "(sqrt add1)",
		   "sqrt: expects a number as 1st argument, but given: #<function:add1>");
    
    queueErrorTest("sqrt given too many args",
		   "(sqrt 1 1)",
		   "sqrt: expects 1 argument, but given 2: 1 1");

    queueErrorTest("integer-sqrt given no args",
		   "(integer-sqrt)",
		   "integer-sqrt: expects 1 argument, but given 0");

    queueErrorTest("integer-sqrt given 1 bad arg",
		   "(integer-sqrt add1)",
		   "integer-sqrt: expects an integer as 1st argument, but given: #<function:add1>");
    
    queueErrorTest("integer-sqrt given too many args",
		   "(integer-sqrt 1 1)",
		   "integer-sqrt: expects 1 argument, but given 2: 1 1");

    queueErrorTest("make-rectangular given no args",
		   "(make-rectangular)",
		   "make-rectangular: expects 2 arguments, but given 0");
    
    queueErrorTest("make-rectangular given bad first arg, good 2nd",
		   "(make-rectangular add1 1)",
		   "make-rectangular: expects a real as 1st argument, but given: #<function:add1>; other arguments were: 1");
    
    
    queueErrorTest("make-rectangular given good first, bad second arg",
		   "(make-rectangular 1 add1)",
		   "make-rectangular: expects a real as 2nd argument, but given: #<function:add1>; other arguments were: 1");

    queueErrorTest("make-rectangular given too many args",
		   "(make-rectangular 1 1 1 1 1)",
		   "make-rectangular: expects 2 arguments, but given 5: 1 1 1 1 1");
    
    queueErrorTest("make-polar given no args",
		   "(make-polar)",
		   "make-polar: expects 2 arguments, but given 0");
    
    queueErrorTest("make-polar given bad first arg, good 2nd",
		   "(make-polar add1 1)",
		   "make-polar: expects a real as 1st argument, but given: #<function:add1>; other arguments were: 1");
    
    
    queueErrorTest("make-polar given good first, bad second arg",
		   "(make-polar 1 add1)",
		   "make-polar: expects a real as 2nd argument, but given: #<function:add1>; other arguments were: 1");

    queueErrorTest("make-polar given too many args",
		   "(make-polar 1 1 1 1 1)",
		   "make-polar: expects 2 arguments, but given 5: 1 1 1 1 1");
    
    queueErrorTest("real-part given no args",
		   "(real-part)",
		   "real-part: expects 1 argument, but given 0");
    
    queueErrorTest("real-part given bad arg",
		   "(real-part add1)",
		   "real-part: expects a number as 1st argument, but given: #<function:add1>");

    queueErrorTest("real-part given too many args",
		   "(real-part 1 1)",
		   "real-part: expects 1 argument, but given 2: 1 1");
    
    queueErrorTest("imag-part given no args",
		   "(imag-part)",
		   "imag-part: expects 1 argument, but given 0");
    
    queueErrorTest("imag-part given bad arg",
		   "(imag-part add1)",
		   "imag-part: expects a number as 1st argument, but given: #<function:add1>");

    queueErrorTest("imag-part given too many args",
		   "(imag-part 1 1)",
		   "imag-part: expects 1 argument, but given 2: 1 1");

    queueErrorTest("angle given no args",
		   "(angle)",
		   "angle: expects 1 argument, but given 0");
    
    queueErrorTest("angle given bad arg",
		   "(angle add1)",
		   "angle: expects a number as 1st argument, but given: #<function:add1>");

    queueErrorTest("angle given too many args",
		   "(angle 1 1)",
		   "angle: expects 1 argument, but given 2: 1 1");
    
    queueErrorTest("magnitude given no args",
		   "(magnitude)",
		   "magnitude: expects 1 argument, but given 0");
    
    queueErrorTest("magnitude given bad arg",
		   "(magnitude add1)",
		   "magnitude: expects a number as 1st argument, but given: #<function:add1>");

    queueErrorTest("magnitude given too many args",
		   "(magnitude 1 1)",
		   "magnitude: expects 1 argument, but given 2: 1 1");	

    queueErrorTest("conjugate given no args",
		   "(conjugate)",
		   "conjugate: expects 1 argument, but given 0");
    
    queueErrorTest("conjugate given bad arg",
		   "(conjugate add1)",
		   "conjugate: expects a number as 1st argument, but given: #<function:add1>");

    queueErrorTest("conjugate given too many args",
		   "(conjugate 1 1)",
		   "conjugate: expects 1 argument, but given 2: 1 1");

    queueErrorTest("sgn given no args",
		   "(sgn)",
		   "sgn: expects 1 argument, but given 0");
    
    queueErrorTest("sgn given bad arg",
		   "(sgn add1)",
		   "sgn: expects a real number as 1st argument, but given: #<function:add1>");

    queueErrorTest("sgn given too many args",
		   "(sgn 1 1)",
		   "sgn: expects 1 argument, but given 2: 1 1");	

    queueErrorTest("inexact->exact given no args",
		   "(inexact->exact)",
		   "inexact->exact: expects 1 argument, but given 0");
    
    queueErrorTest("inexact->exact given bad arg",
		   "(inexact->exact add1)",
		   "inexact->exact: expects a number as 1st argument, but given: #<function:add1>");

    queueErrorTest("inexact->exact given too many args",
		   "(inexact->exact 1 1)",
		   "inexact->exact: expects 1 argument, but given 2: 1 1");

    queueErrorTest("exact->inexact given no args",
		   "(exact->inexact)",
		   "exact->inexact: expects 1 argument, but given 0");
    
    queueErrorTest("exact->inexact given bad arg",
		   "(exact->inexact add1)",
		   "exact->inexact: expects a number as 1st argument, but given: #<function:add1>");

    queueErrorTest("exact->inexact given too many args",
		   "(exact->inexact 1 1)",
		   "exact->inexact: expects 1 argument, but given 2: 1 1");

    queueErrorTest("number->string given no args",
		   "(number->string)",
		   "number->string: expects 1 argument, but given 0");
    
    queueErrorTest("number->string given bad arg",
		   "(number->string add1)",
		   "number->string: expects a number as 1st argument, but given: #<function:add1>");

    queueErrorTest("number->string given too many args",
		   "(number->string 1 1)",
		   "number->string: expects 1 argument, but given 2: 1 1");

    queueErrorTest("string->number given no args",
		   "(string->number)",
		   "string->number: expects 1 argument, but given 0");
    
    queueErrorTest("string->number given bad arg",
		   "(string->number add1)",
		   "string->number: expects a string as 1st argument, but given: #<function:add1>");

    queueErrorTest("string->number given too many args",
		   "(string->number 1 1)",
		   "string->number: expects 1 argument, but given 2: 1 1");


    queueErrorTest("xml->s-exp given no args",
		   "(xml->s-exp)",
		   "xml->s-exp: expects 1 argument, but given 0");
    
    queueErrorTest("xml->s-exp given bad arg",
		   "(xml->s-exp add1)",
		   "xml->s-exp: expects a string as 1st argument, but given: #<function:add1>");

    queueErrorTest("xml->s-exp given too many args",
		   "(xml->s-exp 1 1)",
		   "xml->s-exp: expects 1 argument, but given 2: 1 1");

    queueErrorTest("procedure? given no args",
		   "(procedure?)",
		   "procedure?: expects 1 argument, but given 0");
    
    queueErrorTest("procedure? too many args",
		   "(procedure? 1 1)",
		   "procedure?: expects 1 argument, but given 2: 1 1");

    //next predicate

    queueErrorTest("pair? given no args",
		   "(pair?)",
		   "pair?: expects 1 argument, but given 0");
    
    queueErrorTest("pair? too many args",
		   "(pair? 1 1)",
		   "pair?: expects 1 argument, but given 2: 1 1");

    //next predicate

    queueErrorTest("cons? given no args",
		   "(cons?)",
		   "cons?: expects 1 argument, but given 0");
    
    queueErrorTest("cons? too many args",
		   "(cons? 1 1)",
		   "cons?: expects 1 argument, but given 2: 1 1");

    //next predicate

    queueErrorTest("empty? given no args",
		   "(empty?)",
		   "empty?: expects 1 argument, but given 0");
    
    queueErrorTest("empty? too many args",
		   "(empty? 1 1)",
		   "empty?: expects 1 argument, but given 2: 1 1");

    //next predicate
    queueErrorTest("null? given no args",
		   "(null?)",
		   "null?: expects 1 argument, but given 0");
    
    queueErrorTest("null? too many args",
		   "(null? 1 1)",
		   "null?: expects 1 argument, but given 2: 1 1");

    //next predicate
    queueErrorTest("undefined? given no args",
		   "(undefined?)",
		   "undefined?: expects 1 argument, but given 0");
    
    queueErrorTest("undefined? too many args",
		   "(undefined? 1 1)",
		   "undefined?: expects 1 argument, but given 2: 1 1");

    //next predicate
    /*
      queueErrorTest("void? given no args",
      "(void?)",
      "void?: expects 1 argument, but given 0");
      
      queueErrorTest("void? too many args",
      "(void? 1 1)",
      "void?: expects 1 argument, but given 2: 1 1");
    */
    //next predicate
    queueErrorTest("symbol? given no args",
		   "(symbol?)",
		   "symbol?: expects 1 argument, but given 0");
    
    queueErrorTest("symbol? too many args",
		   "(symbol? 1 1)",
		   "symbol?: expects 1 argument, but given 2: 1 1");

    //next predicate
    queueErrorTest("string? given no args",
		   "(string?)",
		   "string?: expects 1 argument, but given 0");
    
    queueErrorTest("string? too many args",
		   "(string? 1 1)",
		   "string?: expects 1 argument, but given 2: 1 1");

    //next predicate
    queueErrorTest("char? given no args",
		   "(char?)",
		   "char?: expects 1 argument, but given 0");
    
    queueErrorTest("char? too many args",
		   "(char? 1 1)",
		   "char?: expects 1 argument, but given 2: 1 1");

    //next predicate
    queueErrorTest("boolean? given no args",
		   "(boolean?)",
		   "boolean?: expects 1 argument, but given 0");
    
    queueErrorTest("boolean? too many args",
		   "(boolean? 1 1)",
		   "boolean?: expects 1 argument, but given 2: 1 1");

    //next predicate
    queueErrorTest("vector? given no args",
		   "(vector?)",
		   "vector?: expects 1 argument, but given 0");
    
    queueErrorTest("vector? too many args",
		   "(vector? 1 1)",
		   "vector?: expects 1 argument, but given 2: 1 1");

    //next predicate
    queueErrorTest("struct? given no args",
		   "(struct?)",
		   "struct?: expects 1 argument, but given 0");
    
    queueErrorTest("struct? too many args",
		   "(struct? 1 1)",
		   "struct?: expects 1 argument, but given 2: 1 1");

    //next predicate
    queueErrorTest("eof-object? given no args",
		   "(eof-object?)",
		   "eof-object?: expects 1 argument, but given 0");
    
    queueErrorTest("eof-object? too many args",
		   "(eof-object? 1 1)",
		   "eof-object?: expects 1 argument, but given 2: 1 1");

    //next predicate
    queueErrorTest("posn? given no args",
		   "(posn?)",
		   "posn?: expects 1 argument, but given 0");
    
    queueErrorTest("posn? too many args",
		   "(posn? 1 1)",
		   "posn?: expects 1 argument, but given 2: 1 1");
    /*
    //next predicate
    queueErrorTest("bytes? given no args",
    "(bytes?)",
    "bytes?: expects 1 argument, but given 0");
    
    queueErrorTest("bytes? too many args",
    "(bytes? 1 1)",
    "bytes?: expects 1 argument, but given 2: 1 1");

    //next predicate
    queueErrorTest("byte? given no args",
    "(byte?)",
    "byte?: expects 1 argument, but given 0");
    
    queueErrorTest("byte? too many args",
    "(byte? 1 1)",
    "byte?: expects 1 argument, but given 2: 1 1");
    */
    //next predicate
    queueErrorTest("number? given no args",
		   "(number?)",
		   "number?: expects 1 argument, but given 0");
    
    queueErrorTest("number? too many args",
		   "(number? 1 1)",
		   "number?: expects 1 argument, but given 2: 1 1");

    //next predicate
    queueErrorTest("complex? given no args",
		   "(complex?)",
		   "complex?: expects 1 argument, but given 0");
    
    queueErrorTest("complex? too many args",
		   "(complex? 1 1)",
		   "complex?: expects 1 argument, but given 2: 1 1");

    //next predicate
    queueErrorTest("real? given no args",
		   "(real?)",
		   "real?: expects 1 argument, but given 0");
    
    queueErrorTest("rational? too many args",
		   "(rational? 1 1)",
		   "rational?: expects 1 argument, but given 2: 1 1");

    //next predicate


    queueErrorTest("integer? given no args",
		   "(integer?)",
		   "integer?: expects 1 argument, but given 0");
    

    queueErrorTest("integer? too many args",
		   "(integer? 1 1)",
		   "integer?: expects 1 argument, but given 2: 1 1");


    queueErrorTest("exact? given no args",
		   "(exact?)",
		   "exact?: expects 1 argument, but given 0");
    
    queueErrorTest("exact? given a bad arg",
		   "(exact? add1)",
		   "exact?: expects a number as 1st argument, but given: #<function:add1>");
    
    queueErrorTest("exact? given too many args",
		   "(exact? 1 1)",
		   "exact?: expects 1 argument, but given 2: 1 1");

    queueErrorTest("inexact? given no args",
		   "(inexact?)",
		   "inexact?: expects 1 argument, but given 0");
    
    queueErrorTest("inexact? given a bad arg",
		   "(inexact? add1)",
		   "inexact?: expects a number as 1st argument, but given: #<function:add1>");
    
    queueErrorTest("inexact? given too many args",
		   "(inexact? 1 1)",
		   "inexact?: expects 1 argument, but given 2: 1 1");

    queueErrorTest("odd? given no args",
		   "(odd?)",
		   "odd?: expects 1 argument, but given 0");
    
    queueErrorTest("odd? given a bad arg",
		   "(odd? add1)",
		   "odd?: expects an integer as 1st argument, but given: #<function:add1>");
    
    queueErrorTest("odd? given too many args",
		   "(odd? 1 1)",
		   "odd?: expects 1 argument, but given 2: 1 1");

    queueErrorTest("even? given no args",
		   "(even?)",
		   "even?: expects 1 argument, but given 0");
    
    queueErrorTest("even? given a bad arg",
		   "(even? add1)",
		   "even?: expects an integer as 1st argument, but given: #<function:add1>");
    
    queueErrorTest("even? given too many args",
		   "(even? 1 1)",
		   "even?: expects 1 argument, but given 2: 1 1");

    queueErrorTest("zero? given no args",
		   "(zero?)",
		   "zero?: expects 1 argument, but given 0");
    
    queueErrorTest("zero? given a bad arg",
		   "(zero? add1)",
		   "zero?: expects a number as 1st argument, but given: #<function:add1>");
    
    queueErrorTest("zero? given too many args",
		   "(zero? 1 1)",
		   "zero?: expects 1 argument, but given 2: 1 1");

    queueErrorTest("positive? given no args",
		   "(positive?)",
		   "positive?: expects 1 argument, but given 0");
    
    queueErrorTest("positive? given a bad arg",
		   "(positive? add1)",
		   "positive?: expects a real as 1st argument, but given: #<function:add1>");
    
    queueErrorTest("positive? given too many args",
		   "(positive? 1 1)",
		   "positive?: expects 1 argument, but given 2: 1 1");

    queueErrorTest("negative? given no args",
		   "(negative?)",
		   "negative?: expects 1 argument, but given 0");
    
    queueErrorTest("negative? given a bad arg",
		   "(negative? add1)",
		   "negative?: expects a real as 1st argument, but given: #<function:add1>");
    
    queueErrorTest("negative? given too many args",
		   "(negative? 1 1)",
		   "negative?: expects 1 argument, but given 2: 1 1");

    queueErrorTest("box? given no args",
		   "(box?)",
		   "box?: expects 1 argument, but given 0");
    
    queueErrorTest("box? given too many args",
		   "(box? 2 2)",
		   "box?: expects 1 argument, but given 2: 2 2");
    
    queueErrorTest("hash? given no args",
		   "(hash?)",
		   "hash?: expects 1 argument, but given 0");
    
    queueErrorTest("hash? given too many args",
		   "(hash? 2 2)",
		   "hash?: expects 1 argument, but given 2: 2 2");

    queueErrorTest("eq? given no args",
		   "(eq?)",
		   "eq?: expects 2 arguments, but given 0");
    
    queueErrorTest("eq? given too many args",
		   "(eq? 2 2 2)",
		   "eq?: expects 2 arguments, but given 3: 2 2 2");

    queueErrorTest("eq? given one arg",
		   "(eq? 1)",
		   "eq?: expects 2 arguments, but given 1: 1");

    queueErrorTest("eqv? given no args",
		   "(eqv?)",
		   "eqv?: expects 2 arguments, but given 0");
    
    queueErrorTest("eqv? given too many args",
		   "(eqv? 2 2 2)",
		   "eqv?: expects 2 arguments, but given 3: 2 2 2");

    queueErrorTest("eqv? given one arg",
		   "(eqv? 1)",
		   "eqv?: expects 2 arguments, but given 1: 1");

    queueErrorTest("equal? given no args",
		   "(equal?)",
		   "equal?: expects 2 arguments, but given 0");
    
    queueErrorTest("equal? given too many args",
		   "(equal? 2 2 2)",
		   "equal?: expects 2 arguments, but given 3: 2 2 2");

    queueErrorTest("equal? given one arg",
		   "(equal? 1)",
		   "equal?: expects 2 arguments, but given 1: 1");
    
    queueErrorTest("equal~? given no args",
		   "(equal~?)",
		   "equal~?: expects 3 arguments, but given 0");

    queueErrorTest("equal~? given too many args",
		   "(equal~? 1 1 1 1)",
		   "equal~?: expects 3 arguments, but given 4: 1 1 1 1");
    
    queueErrorTest("equal~? given bad arg a for 3rd arg",
		   "(equal~? add1 sub1 add1)",
		   "equal~?: expects a non-negative number as 3rd argument, but given: #<function:add1>; other arguments were: #<function:add1> #<function:sub1>");
    
    queueErrorTest("false? given no args",
		   "(false?)",
		   "false?: expects 1 argument, but given 0");
    
    queueErrorTest("false? given too many args",
		   "(false? 1 1)",
		   "false?: expects 1 argument, but given 2: 1 1");
    
    queueErrorTest("symbol=? given no args",
		   "(symbol=?)",
		   "symbol=?: expects 2 arguments, but given 0");
    
    queueErrorTest("symbol=? given too many args",
		   "(symbol=? 1 1 1)",
		   "symbol=?: expects 2 arguments, but given 3: 1 1 1");

    queueErrorTest("symbol=? given bad first arg",
		   "(symbol=? add1 sub1)",
		   "symbol=?: expects a symbol as 1st argument, but given: #<function:add1>; other arguments were: #<function:sub1>");

    queueErrorTest("symbol=? given bad 2nd arg",
		   "(symbol=? 'asdf sub1)",
		   "symbol=?: expects a symbol as 2nd argument, but given: #<function:sub1>; other arguments were: asdf");

    queueErrorTest("cons given no args",
		   "(cons)",
		   "cons: expects 2 arguments, but given 0");
    
    queueErrorTest("cons given too many args",
		   "(cons 1 1 1)",
		   "cons: expects 2 arguments, but given 3: 1 1 1");

    queueErrorTest("car given no args",
		   "(car)",
		   "car: expects 1 argument, but given 0");

    queueErrorTest("car given bad arg",
		   "(car add1)",
		   "car: expects a pair as 1st argument, but given: #<function:add1>");
    
    queueErrorTest("car given too many args",
		   "(car 1 1 1)",
		   "car: expects 1 argument, but given 3: 1 1 1");
    //next list primitive

    queueErrorTest("cdr given no args",
		   "(cdr)",
		   "cdr: expects 1 argument, but given 0");

    queueErrorTest("cdr given bad arg",
		   "(cdr add1)",
		   "cdr: expects a pair as 1st argument, but given: #<function:add1>");
    
    queueErrorTest("cdr given too many args",
		   "(cdr 1 1 1)",
		   "cdr: expects 1 argument, but given 3: 1 1 1");
    //next list primitive
    
    queueErrorTest("caar given no args",
		   "(caar)",
		   "caar: expects 1 argument, but given 0");

    queueErrorTest("caar given bad arg",
		   "(caar add1)",
		   "caar: expects a caarable value as 1st argument, but given: #<function:add1>");
    
    queueErrorTest("caar given too many args",
		   "(caar 1 1 1)",
		   "caar: expects 1 argument, but given 3: 1 1 1");
    //next list primitive
    
    queueErrorTest("cadr given no args",
		   "(cadr)",
		   "cadr: expects 1 argument, but given 0");

    queueErrorTest("cadr given bad arg",
		   "(cadr add1)",
		   "cadr: expects a cadrable value as 1st argument, but given: #<function:add1>");
    
    queueErrorTest("cadr given too many args",
		   "(cadr 1 1 1)",
		   "cadr: expects 1 argument, but given 3: 1 1 1");
    //next list primitive
    
    queueErrorTest("cdar given no args",
		   "(cdar)",
		   "cdar: expects 1 argument, but given 0");

    queueErrorTest("cdar given bad arg",
		   "(cdar add1)",
		   "cdar: expects a cdarable value as 1st argument, but given: #<function:add1>");
    
    queueErrorTest("cdar given too many args",
		   "(cdar 1 1 1)",
		   "cdar: expects 1 argument, but given 3: 1 1 1");
    //next list primitive
    
    queueErrorTest("cddr given no args",
		   "(cddr)",
		   "cddr: expects 1 argument, but given 0");

    queueErrorTest("cddr given bad arg",
		   "(cddr add1)",
		   "cddr: expects a cddrable value as 1st argument, but given: #<function:add1>");
    
    queueErrorTest("cddr given too many args",
		   "(cddr 1 1 1)",
		   "cddr: expects 1 argument, but given 3: 1 1 1");
    //next list primitive
    
    queueErrorTest("caaar given no args",
		   "(caaar)",
		   "caaar: expects 1 argument, but given 0");

    queueErrorTest("caaar given bad arg",
		   "(caaar add1)",
		   "caaar: expects a caaarable value as 1st argument, but given: #<function:add1>");
    
    queueErrorTest("caaar given too many args",
		   "(caaar 1 1 1)",
		   "caaar: expects 1 argument, but given 3: 1 1 1");
    //next list primitive
    
    queueErrorTest("caadr given no args",
		   "(caadr)",
		   "caadr: expects 1 argument, but given 0");

    queueErrorTest("caadr given bad arg",
		   "(caadr add1)",
		   "caadr: expects a caadrable value as 1st argument, but given: #<function:add1>");
    
    queueErrorTest("caadr given too many args",
		   "(caadr 1 1 1)",
		   "caadr: expects 1 argument, but given 3: 1 1 1");
    //next list primitive
    
    queueErrorTest("cadar given no args",
		   "(cadar)",
		   "cadar: expects 1 argument, but given 0");

    queueErrorTest("cadar given bad arg",
		   "(cadar add1)",
		   "cadar: expects a cadarable value as 1st argument, but given: #<function:add1>");
    
    queueErrorTest("cadar given too many args",
		   "(cadar 1 1 1)",
		   "cadar: expects 1 argument, but given 3: 1 1 1");
    //next list primitive
    
    queueErrorTest("cdaar given no args",
		   "(cdaar)",
		   "cdaar: expects 1 argument, but given 0");

    queueErrorTest("cdaar given bad arg",
		   "(cdaar add1)",
		   "cdaar: expects a cdaarable value as 1st argument, but given: #<function:add1>");
    
    queueErrorTest("cdaar given too many args",
		   "(cdaar 1 1 1)",
		   "cdaar: expects 1 argument, but given 3: 1 1 1");
    //next list primitive
    
    queueErrorTest("cdadr given no args",
		   "(cdadr)",
		   "cdadr: expects 1 argument, but given 0");

    queueErrorTest("cdadr given bad arg",
		   "(cdadr add1)",
		   "cdadr: expects a cdadrable value as 1st argument, but given: #<function:add1>");
    
    queueErrorTest("cdadr given too many args",
		   "(cdadr 1 1 1)",
		   "cdadr: expects 1 argument, but given 3: 1 1 1");
    //next list primitive
    
    queueErrorTest("cddar given no args",
		   "(cddar)",
		   "cddar: expects 1 argument, but given 0");

    queueErrorTest("cddar given bad arg",
		   "(cddar add1)",
		   "cddar: expects a cddarable value as 1st argument, but given: #<function:add1>");
    
    queueErrorTest("cddar given too many args",
		   "(cddar 1 1 1)",
		   "cddar: expects 1 argument, but given 3: 1 1 1");
    //next list primitive
    
    queueErrorTest("caddr given no args",
		   "(caddr)",
		   "caddr: expects 1 argument, but given 0");

    queueErrorTest("caddr given bad arg",
		   "(caddr add1)",
		   "caddr: expects a caddrable value as 1st argument, but given: #<function:add1>");
    
    queueErrorTest("caddr given too many args",
		   "(caddr 1 1 1)",
		   "caddr: expects 1 argument, but given 3: 1 1 1");
    //next list primitive
    
    queueErrorTest("cdddr given no args",
		   "(cdddr)",
		   "cdddr: expects 1 argument, but given 0");

    queueErrorTest("cdddr given bad arg",
		   "(cdddr add1)",
		   "cdddr: expects a cdddrable value as 1st argument, but given: #<function:add1>");
    
    queueErrorTest("cdddr given too many args",
		   "(cdddr 1 1 1)",
		   "cdddr: expects 1 argument, but given 3: 1 1 1");
    //next list primitive
    
    queueErrorTest("cadddr given no args",
		   "(cadddr)",
		   "cadddr: expects 1 argument, but given 0");

    queueErrorTest("cadddr given bad arg",
		   "(cadddr add1)",
		   "cadddr: expects a cadddrable value as 1st argument, but given: #<function:add1>");
    
    queueErrorTest("cadddr given too many args",
		   "(cadddr 1 1 1)",
		   "cadddr: expects 1 argument, but given 3: 1 1 1");
    //next list primitive
    
    queueErrorTest("rest given no args",
		   "(rest)",
		   "rest: expects 1 argument, but given 0");

    queueErrorTest("rest given bad arg",
		   "(rest add1)",
		   "rest: expects a non-empty list as 1st argument, but given: #<function:add1>");
    
    queueErrorTest("rest given too many args",
		   "(rest 1 1 1)",
		   "rest: expects 1 argument, but given 3: 1 1 1");
    //next list primitive
    
    queueErrorTest("first given no args",
		   "(first)",
		   "first: expects 1 argument, but given 0");

    queueErrorTest("first given bad arg",
		   "(first add1)",
		   "first: expects a non-empty list as 1st argument, but given: #<function:add1>");
    
    queueErrorTest("first given too many args",
		   "(first 1 1 1)",
		   "first: expects 1 argument, but given 3: 1 1 1");
    //next list primitive
    
    queueErrorTest("second given no args",
		   "(second)",
		   "second: expects 1 argument, but given 0");
    
    queueErrorTest("second given bad arg",
		   "(second add1)",
		   "second: expects a list with 2 or more elements as 1st argument, but given: #<function:add1>");
    
    queueErrorTest("second given too many args",
		   "(second 1 1 1)",
		   "second: expects 1 argument, but given 3: 1 1 1");
    
    queueErrorTest("third given no args",
		   "(third)",
		   "third: expects 1 argument, but given 0");
    
    queueErrorTest("third given bad arg",
		   "(third add1)",
		   "third: expects a list with 3 or more elements as 1st argument, but given: #<function:add1>");
    
    queueErrorTest("third given too many args",
		   "(third 1 1 1)",
		   "third: expects 1 argument, but given 3: 1 1 1");

    queueErrorTest("fourth given no args",
		   "(fourth)",
		   "fourth: expects 1 argument, but given 0");
    
    queueErrorTest("fourth given bad arg",
		   "(fourth add1)",
		   "fourth: expects a list with 4 or more elements as 1st argument, but given: #<function:add1>");
    
    queueErrorTest("fourth given too many args",
		   "(fourth 1 1 1)",
		   "fourth: expects 1 argument, but given 3: 1 1 1");

    queueErrorTest("fifth given no args",
		   "(fifth)",
		   "fifth: expects 1 argument, but given 0");
    
    queueErrorTest("fifth given bad arg",
		   "(fifth add1)",
		   "fifth: expects a list with 5 or more elements as 1st argument, but given: #<function:add1>");
    
    queueErrorTest("fifth given too many args",
		   "(fifth 1 1 1)",
		   "fifth: expects 1 argument, but given 3: 1 1 1");

    queueErrorTest("sixth given no args",
		   "(sixth)",
		   "sixth: expects 1 argument, but given 0");
    
    queueErrorTest("sixth given bad arg",
		   "(sixth add1)",
		   "sixth: expects a list with 6 or more elements as 1st argument, but given: #<function:add1>");
    
    queueErrorTest("sixth given too many args",
		   "(sixth 1 1 1)",
		   "sixth: expects 1 argument, but given 3: 1 1 1");

    queueErrorTest("seventh given no args",
		   "(seventh)",
		   "seventh: expects 1 argument, but given 0");
    
    queueErrorTest("seventh given bad arg",
		   "(seventh add1)",
		   "seventh: expects a list with 7 or more elements as 1st argument, but given: #<function:add1>");
    
    queueErrorTest("seventh given too many args",
		   "(seventh 1 1 1)",
		   "seventh: expects 1 argument, but given 3: 1 1 1");

    queueErrorTest("eighth given no args",
		   "(eighth)",
		   "eighth: expects 1 argument, but given 0");
    
    queueErrorTest("eighth given bad arg",
		   "(eighth add1)",
		   "eighth: expects a list with 8 or more elements as 1st argument, but given: #<function:add1>");
    
    queueErrorTest("eighth given too many args",
		   "(eighth 1 1 1)",
		   "eighth: expects 1 argument, but given 3: 1 1 1");

    queueErrorTest("length given no args",
		   "(length)",
		   "length: expects 1 argument, but given 0");
    
    queueErrorTest("length given a bad arg",
		   "(length add1)",
		   "length: expects a list as 1st argument, but given: #<function:add1>");
    
    queueErrorTest("length given too many args",
		   "(length 1 1)",
		   "length: expects 1 argument, but given 2: 1 1");
    
    queueErrorTest("list? given no args",
		   "(list?)",
		   "list?: expects 1 argument, but given 0");
    
    queueErrorTest("list? given too many args",
		   "(list? 1 1 1)",
		   "list?: expects 1 argument, but given 3: 1 1 1");
    
    queueErrorTest("list* given no args",
		   "(list*)",
		   "list*: expects at least 1 argument, but given 0");
    
    queueErrorTest("list* given too many args",
		   "(list* 1 1 1)",
		   "list*: expects a list as 3rd argument, but given: 1; other arguments were: 1 1");
    
    queueErrorTest("list-ref given no args",
		   "(list-ref)",
		   "list-ref: expects 2 arguments, but given 0");
    
    queueErrorTest("list-ref given too many args",
		   "(list-ref 1 1 1)",
		   "list-ref: expects 2 arguments, but given 3: 1 1 1");

    queueErrorTest("list-ref given bad arg for first arg",
		   "(list-ref add1 1)",
		   "list-ref: expects a list as 1st argument, but given: #<function:add1>; other arguments were: 1");

    queueErrorTest("list-ref given bad arg for second arg",
		   "(list-ref (list) add1)",
		   "list-ref: expects a non-negative exact integer as 2nd argument, but given: #<function:add1>; other arguments were: empty");
    
    queueErrorTest("append given bad args",
		   "(append sub1 add1)",
		   "append: expects a list as 1st argument, but given: #<function:sub1>; other arguments were: #<function:add1>")

    queueErrorTest("reverse given no args",
		   "(reverse)",
		   "reverse: expects 1 argument, but given 0");
    
    queueErrorTest("reverse given too many args",
		   "(reverse 1 1)",
		   "reverse: expects 1 argument, but given 2: 1 1");
    
    queueErrorTest("reverse given bad arg",
		   "(reverse add1)",
		   "reverse: expects a list as 1st argument, but given: #<function:add1>");


    queueErrorTest("underlay given no args",
		   "(underlay)",
		   "underlay: expects at least 2 arguments, but given 0");
    
    queueErrorTest("underlay given bad first arg",
		   "(underlay 1 1 1)",
		   "underlay: expects an image as 1st argument, but given: 1; other arguments were: 1 1");

    queueErrorTest("underlay given not enough args",
		   "(underlay 1)",
		   "underlay: expects at least 2 arguments, but given 1: 1");
    
    queueErrorTest("underlay/xy given no args",
		   "(underlay/xy)",
		   "underlay/xy: expects 4 arguments, but given 0");
    
    queueErrorTest("underlay/xy given bad first arg",
		   "(underlay/xy 1 1 1 1)",
		   "underlay/xy: expects an image as 1st argument, but given: 1; other arguments were: 1 1 1");

    queueErrorTest("underlay/xy given too many args",
		   "(underlay/xy 1 1 1 1 1)",
		   "underlay/xy: expects 4 arguments, but given 5: 1 1 1 1 1");

    queueErrorTest("underlay/align given no args",
		   "(underlay/align)",
		   "underlay/align: expects at least 4 arguments, but given 0");
    
    queueErrorTest("underlay/align given bad first arg",
		   "(underlay/align 1 1 1 1 1)",
		   "underlay/align: expects a x-place as 1st argument, but given: 1; other arguments were: 1 1 1 1");

    queueErrorTest("scale given no args",
		   "(scale)",
		   "scale: expects 2 arguments, but given 0");

    queueErrorTest("scale given too many args",
		   "(scale 1 1 1)",
		   "scale: expects 2 arguments, but given 3: 1 1 1");
    
    queueErrorTest("scale given bad second arg a",
		   "(scale 1 1)",
		   "scale: expects an image as 2nd argument, but given: 1; other arguments were: 1");
    

    queueErrorTest("scale given bad first arg a",
		   "(scale add1 1)",
		   "scale: expects a finite real number as 1st argument, but given: #<function:add1>; other arguments were: 1");

    queueErrorTest("scale/xy given no args",
		   "(scale/xy)",
		   "scale/xy: expects 3 arguments, but given 0");
    
    queueErrorTest("scale/xy given bad 3rd arg a",
		   "(scale/xy 1 1 1)",
		   "scale/xy: expects an image as 3rd argument, but given: 1; other arguments were: 1 1");
    
    queueErrorTest("scale/xy given too many args",
		   "(scale/xy 1 1 1 1)",
		   "scale/xy: expects 3 arguments, but given 4: 1 1 1 1");

    queueErrorTest("andmap given no args",
		   "(andmap)",
		   "andmap: expects at least 2 arguments, but given 0");
    
    queueErrorTest("andmap given bad first arg a",
		   "(andmap 1 1)",
		   "andmap: expects a function name as 1st argument, but given: 1; other arguments were: 1");

    queueErrorTest("andmap given bad 2nd arg a",
		   "(andmap add1 sub1)",
		   "andmap: expects a list as 2nd argument, but given: #<function:sub1>; other arguments were: #<function:add1>");
    
    queueErrorTest("ormap given no args",
		   "(ormap)",
		   "ormap: expects at least 2 arguments, but given 0");
    
    queueErrorTest("ormap given bad first arg a",
		   "(ormap 1 1)",
		   "ormap: expects a function name as 1st argument, but given: 1; other arguments were: 1");

    queueErrorTest("ormap given bad 2nd arg a",
		   "(ormap add1 sub1)",
		   "ormap: expects a list as 2nd argument, but given: #<function:sub1>; other arguments were: #<function:add1>");	


    queueErrorTest("memq given no args",
		   "(memq)",
		   "memq: expects 2 arguments, but given 0");
    
    queueErrorTest("memq given too many args",
		   "(memq 1 1 1)",
		   "memq: expects 2 arguments, but given 3: 1 1 1");
    
    queueErrorTest("memq given bad second arg a",
		   "(memq 1 1)",
		   "memq: expects a list as 2nd argument, but given: 1; other arguments were: 1");

    queueErrorTest("memv given no args",
		   "(memv)",
		   "memv: expects 2 arguments, but given 0");
    
    queueErrorTest("memv given too many args",
		   "(memv 1 1 1)",
		   "memv: expects 2 arguments, but given 3: 1 1 1");
    
    queueErrorTest("memv given bad second arg a",
		   "(memv 1 1)",
		   "memv: expects a list as 2nd argument, but given: 1; other arguments were: 1");	


    queueErrorTest("member given no args",
		   "(member)",
		   "member: expects 2 arguments, but given 0");
    
    queueErrorTest("member given too many args",
		   "(member 1 1 1)",
		   "member: expects 2 arguments, but given 3: 1 1 1");
    
    queueErrorTest("member given bad second arg a",
		   "(member 1 1)",
		   "member: expects a list as 2nd argument, but given: 1; other arguments were: 1");


    queueErrorTest("member? given no args",
		   "(member?)",
		   "member?: expects 2 arguments, but given 0");
    
    queueErrorTest("member? given too many args",
		   "(member? 1 1 1)",
		   "member?: expects 2 arguments, but given 3: 1 1 1");
    
    queueErrorTest("member? given bad second arg a",
		   "(member? 1 1)",
		   "member?: expects a list as 2nd argument, but given: 1; other arguments were: 1");


    queueErrorTest("memf given no args",
		   "(memf)",
		   "memf: expects 2 arguments, but given 0");
    
    queueErrorTest("memf given too many args",
		   "(memf 1 1 1)",
		   "memf: expects 2 arguments, but given 3: 1 1 1");
    
    queueErrorTest("memf given bad first arg a",
		   "(memf 1 1)",
		   "memf: expects a function name as 1st argument, but given: 1; other arguments were: 1");


    queueErrorTest("assq given no args",
		   "(assq)",
		   "assq: expects 2 arguments, but given 0");
    
    queueErrorTest("assq given too many args",
		   "(assq 1 1 1)",
		   "assq: expects 2 arguments, but given 3: 1 1 1");
    
    queueErrorTest("assq given bad second arg a",
		   "(assq 1 1)",
		   "assq: expects a list of pair as 2nd argument, but given: 1; other arguments were: 1");	

    queueErrorTest("assv given no args",
		   "(assv)",
		   "assv: expects 2 arguments, but given 0");
    
    queueErrorTest("assv given too many args",
		   "(assv 1 1 1)",
		   "assv: expects 2 arguments, but given 3: 1 1 1");
    
    queueErrorTest("assv given bad second arg a",
		   "(assv 1 1)",
		   "assv: expects a list of pair as 2nd argument, but given: 1; other arguments were: 1");


    queueErrorTest("assoc given no args",
		   "(assoc)",
		   "assoc: expects 2 arguments, but given 0");
    
    queueErrorTest("assoc given too many args",
		   "(assoc 1 1 1)",
		   "assoc: expects 2 arguments, but given 3: 1 1 1");
    
    queueErrorTest("assoc given bad second arg a",
		   "(assoc 1 1)",
		   "assoc: expects a list of pair as 2nd argument, but given: 1; other arguments were: 1");

    queueErrorTest("remove given no args",
		   "(remove)",
		   "remove: expects 2 arguments, but given 0");
    
    queueErrorTest("remove given too many args",
		   "(remove 1 1 1)",
		   "remove: expects 2 arguments, but given 3: 1 1 1");
    
    queueErrorTest("remove given bad second arg",
		   "(remove 1 1)",
		   "remove: expects a list as 2nd argument, but given: 1; other arguments were: 1");


    queueErrorTest("test check-expect functions",
                   "(check-expect + -)",
                   'check-expect cannot compare functions');

    queueErrorTest("test check-expect arity",
                   "(check-expect 1 2 3)",
                   'check-expect: expects 2 arguments, but given 3: 1 2 3');

    queueErrorTest("test check-expect arity",
                   "(check-expect )",
                   'check-expect: expects 2 arguments, but given 0');

    queueErrorTest("test check-within nonneg real",
                   "(check-within 2 1 -3)",
                   'check-within requires a non-negative real number for range, but given -3.');

    queueErrorTest("test check-within functions",
                   "(check-within + - 2)",
                   'check-within cannot compare functions');

    queueErrorTest("test check-within arity",
                   "(check-within 3 3 3 3 3 3 3 3 3 3 3)",
                   'check-within: expects 3 arguments, but given 11: 3 3 3 3 3 3 3 3 3 3 3');
    /*
      queueErrorTest("test print arity",
      '(print "hello" "world")',
      'print: expects 1 argument, but given 2: "hello" "world"');
    */
    queueErrorTest("test write arity",
                   "(write 1 2 3 4 5)",
                   "write: expects 1 or 2 arguments, but given 5: 1 2 3 4 5");

    queueErrorTest("test write arity",
                   "(write )",
                   "write: expects 1 or 2 arguments, but given 0");

    queueErrorTest("test display arity",
                   "(display 1 2 3 4 5)",
                   "display: expects 1 or 2 arguments, but given 5: 1 2 3 4 5");

    queueErrorTest("test display arity",
                   "(display )",
                   "display: expects 1 or 2 arguments, but given 0");

    queueErrorTest("test for-each arity",
                   "(for-each )",
                   "for-each: expects at least 2 arguments, but given 0");

    queueErrorTest("test for-each arity 2",
                   "(for-each 1 2 3 4 5)",
                   "for-each: expects a function name as 1st argument, but given: 1; other arguments were: 2 3 4 5");


    queueErrorTest("test for-each a",
                   "(for-each 1 2)",
                   "for-each: expects a function name as 1st argument, but given: 1; other arguments were: 2");



    //////////////////////////////////////////////////////////////////////


    //map
    queueErrorTest("test map with function that cannot be applied to elements in list",
                   "(map add1 (list \"hello\" \"world\"))",
                   'add1: expects a number as 1st argument, but given: "hello"');
    //filter
    queueErrorTest("test filter without procedure as 1st argument",
                   "(filter 2 (list 2 3 4))",
                   'filter: expects a function name (arity 1) as 1st argument, but given: 2; other arguments were: (list 2 3 4)');

    //foldl
    queueErrorTest("test foldl with arguments of wrong a",
                   "(foldl + \"1\" (list 2 3))",
                   '+: expects a number as 2nd argument, but given: "1"; other arguments were: 2');

    queueErrorTest("test foldl with arguments of wrong a",
                   "(foldl 1 7 (list 2 3))",
                   'foldl: expects a function name as 1st argument, but given: 1; other arguments were: 7 (list 2 3)');

    //foldr
    queueErrorTest("test foldr with arguments of wrong a",
                   "(foldr 1 2 (list 3))",
                   'foldr: expects a function name as 1st argument, but given: 1; other arguments were: 2 (list 3)');

    queueErrorTest("test foldr with arguments of wrong a",
                   "(foldr + \"hello\" (list 1 2 3))",
                   '+: expects a number as 2nd argument, but given: "hello"; other arguments were: 3');

    //argmax
    queueErrorTest("test argmax with empty list",
                   "(argmax 2 '())",
                   'argmax: expects a function name as 1st argument, but given: 2; other arguments were: empty');

    queueErrorTest("test argmax with first argument not a procedure",
                   "(argmax 2 (list 2))",
                   'argmax: expects a function name as 1st argument, but given: 2; other arguments were: (list 2)');

    queueErrorTest("test argmax with second argument not a list",
                   "(argmax car 2)",
                   'argmax: expects a non-empty list as 2nd argument, but given: 2; other arguments were: #<function:car>');

    //argmin
    queueErrorTest("test argmin with empty list",
                   "(argmin 2 '())",
                   'argmin: expects a function name as 1st argument, but given: 2; other arguments were: empty');

    queueErrorTest("test argmin with first argument not a procedure",
                   "(argmin 2 (list 2))",
                   'argmin: expects a function name as 1st argument, but given: 2; other arguments were: (list 2)');

    queueErrorTest("test argmin with second argument not a list",
                   "(argmin car 2)",
                   'argmin: expects a non-empty list as 2nd argument, but given: 2; other arguments were: #<function:car>');

    //build-list
    queueErrorTest("test build-list with first argument not a number",
                   "(build-list \"number\" add1)",
                   "build-list: expects a non-negative exact integer as 1st argument, but given: \"number\"; other arguments were: #<function:add1>");

    queueErrorTest("test build-list with second argument not a proc",
                   "(build-list 2 6)",
                   'build-list: expects a function name as 2nd argument, but given: 6; other arguments were: 2');

    //make-hash 

    queueErrorTest("test make-hash with listof listof pairs not first argument",
                   "(make-hash 1)",
                   'make-hash: expects a list of list of pairs as 1st argument, but given: 1');

    queueErrorTest("test make-hash with 2 arguments instead of 0 or 1",
                   "(make-hash (list (list 1 \"hello\") (list 2 \"world\") (list 3 \"today\")) 2)",
                   'make-hash: expects 0 or 1 arguments, but given 2: (list (list 1 "hello") (list 2 "world") (list 3 "today")) 2');


    //hash-set!

    queueErrorTest("test hash-set! with first argument not a hash",
                   "(hash-set! 5 2 \"bird\")",
                   'hash-set!: expects a hash as 1st argument, but given: 5; other arguments were: 2 "bird"');

    queueErrorTest("test hash-set! with wrong arity",
                   "(hash-set!)",
                   'hash-set!: expects 3 arguments, but given 0');

    //hash-ref 
    queueErrorTest("test hash-ref with wrong arity",
                   "(hash-ref \"thing\")",
                   'hash-ref: expects 2 or 3 arguments, but given 1: thing');

    queueErrorTest("test hash-ref with 1st argument not a hash",
                   "(hash-ref \"imma hash\" 2)",
                   'hash-ref: expects a hash as 1st argument, but given: "imma hash"; other arguments were: 2');

    //hash-remove!
    queueErrorTest("test hash-remove! with wrong arity",
                   "(hash-remove!)",
                   'hash-remove: expects 2 arguments, but given 0');

    queueErrorTest("test hash-remove! with wrong 1st argument not hash",
                   "(hash-remove! 2 2)",
                   'hash-remove!: expects a hash as 1st argument, but given: 2; other arguments were: 2');

    //hash-map

    queueErrorTest("test hash-map with wrong arity",
                   "(hash-map)",
                   'hash-map: expects 2 arguments, but given 0');

    queueErrorTest("test hash-map with wrong first argument not hash",
                   "(hash-map 2 cons)",
                   'hash-map: expects a hash as 1st argument, but given: 2; other arguments were: #<function:cons>');

    queueErrorTest("test hash-map with wrong 2nd argument not proc",
                   "(hash-map (make-hash (list (list 1 100) (list 2 200) (list 3 300))) 2)",
                   'hash-map: expects a function name as 2nd argument, but given: 2; other arguments were: #hash((1 . (list 100)) (2 . (list 200)) (3 . (list 300)))');

    //hash-for-each

    queueErrorTest("test hash-for-each with wrong arity",
                   "(hash-for-each)",
                   'hash-for-each: expects 2 arguments, but given 0');

    queueErrorTest("test hash-for-each with wrong 1st argument not proc",
                   "(hash-for-each \"imma hash brown\" cons)",
                   'hash-for-each: expects a hash as 1st argument, but given: "imma hash brown"; other arguments were: #<function:cons>');

    queueErrorTest("test hash-for-each with wrong 2nd argument not proc",
                   "(hash-for-each (make-hash) \"add\")",
                   'hash-for-each: expects a function name as 2nd argument, but given: "add"; other arguments were: #hash()');

    //make-string

    queueErrorTest("test make-string with wrong arity",
                   "(make-string)",
                   'make-string: expects 2 arguments, but given 0');

    queueErrorTest("test make-string with 1st argument not exact integer",
                   "(make-string 2.5 2)",
                   'make-string: expects a non-negative exact integer as 1st argument, but given: 5/2; other arguments were: 2');

    queueErrorTest("test make-string with 2nd argument not char",
                   "(make-string 3 2)",
                   'make-string: expects a character as 2nd argument, but given: 2; other arguments were: 3');

    //replicate

    queueErrorTest("test replicate with wrong arity",
                   "(replicate)",
                   'replicate: expects 2 arguments, but given 0');

    queueErrorTest("test replicate with 1st argument not exact integer",
                   "(replicate 2.1 \"World\")",
                   'replicate: expects a non-negative exact integer as 1st argument, but given: 21/10; other arguments were: "World"');

    queueErrorTest("test replicate with 2nd argument not string",
                   "(replicate 2 1)",
                   'replicate: expects a string as 2nd argument, but given: 1; other arguments were: 2');

    //string

    queueErrorTest("test string with 1st argument not char",
                   "(string 1)",
                   'string: expects a character as 1st argument, but given: 1');

    //string-length

    queueErrorTest("test string-length with wrong arity",
                   "(string-length)",
                   'string-length: expects 1 argument, but given 0');

    queueErrorTest("test string-length with 1st argument not string",
                   "(string-length 2)",
                   'string-length: expects a string as 1st argument, but given: 2');

    //string-ref

    queueErrorTest("test string-ref with wrong arity",
                   "(string-ref)",
                   'string-ref: expects 2 arguments, but given 0');

    queueErrorTest("test string-ref with 1st argument not string",
                   "(string-ref 2 2)",
                   'string-ref: expects a string as 1st argument, but given: 2; other arguments were: 2');

    queueErrorTest("test string-ref with 1st argument not string",
                   "(string-ref \"hello\" \"world\")",
                   'string-ref: expects a non-negative exact integer as 2nd argument, but given: "world"; other arguments were: "hello"');

    //out of bounds


    //string=?

    queueErrorTest("test string=? with 1st argument not string",
                   "(string=? 42 \"hello\")",
                   'string=?: expects a string as 1st argument, but given: 42; other arguments were: "hello"');

    queueErrorTest("test string=? with 2nd argument not string",
                   "(string=? \"thing\" 42)",
                   'string=?: expects a string as 2nd argument, but given: 42; other arguments were: "thing"');

    queueErrorTest("test string=? with wrong arity",
                   "(string=?)",
                   'string=?: expects at least 2 arguments, but given 0');

    //string-ci=?

    queueErrorTest("test string-ci=? with wrong arity",
                   "(string-ci=?)",
                   'string-ci=?: expects at least 2 arguments, but given 0');

    queueErrorTest("test string-ci=? with 1st argument not string",
                   "(string-ci=? 4 \"thing\")",
                   'string-ci=?: expects a string as 1st argument, but given: 4; other arguments were: "thing"');

    queueErrorTest("test string-ci=? with 1st argument not string",
                   "(string-ci=? \"thing\" 2)",
                   'string-ci=?: expects a string as 2nd argument, but given: 2; other arguments were: "thing"');

    //string<?

    queueErrorTest("test string<? with wrong arity",
                   "(string<?)",
                   'string<?: expects at least 2 arguments, but given 0');

    queueErrorTest("test string<? with 1st argument not string",
                   "(string<? \"thing\" 4)",
                   'string<?: expects a string as 2nd argument, but given: 4; other arguments were: "thing"');

    queueErrorTest("test string<? with 1st argument not string",
                   "(string<? 4 \"thing\")",
                   'string<?: expects a string as 1st argument, but given: 4; other arguments were: "thing"');

    //string>?

    queueErrorTest("test string>? with wrong arity",
                   "(string>?)",
                   'string>?: expects at least 2 arguments, but given 0');

    queueErrorTest("test string>? with 1st argument not string",
                   "(string>? 3 \"thing\")",
                   'string>?: expects a string as 1st argument, but given: 3; other arguments were: "thing"');

    queueErrorTest("test string>? with 1st argument not string",
                   "(string>? \"thing\" 3)",
                   'string>?: expects a string as 2nd argument, but given: 3; other arguments were: "thing"');


    //string<=?

    queueErrorTest("test string<=? with wrong arity",
                   "(string<=?)",
                   'string<=?: expects at least 2 arguments, but given 0');

    queueErrorTest("test string<=? with 1st argument not string",
                   "(string<=? 3 \"thing\")",
                   'string<=?: expects a string as 1st argument, but given: 3; other arguments were: "thing"');

    queueErrorTest("test string>? with 1st argument not string",
                   "(string>? \"thing\" 3)",
                   'string>?: expects a string as 2nd argument, but given: 3; other arguments were: "thing"');

    //string>=?

    queueErrorTest("test string>=? with wrong arity",
                   "(string>=?)",
                   'string>=?: expects at least 2 arguments, but given 0');

    queueErrorTest("test string>=? with 1st argument not string",
                   "(string>=? 2 \"thing\")",
                   'string>=?: expects a string as 1st argument, but given: 2; other arguments were: "thing"');

    queueErrorTest("test string>=? with 1st argument not string",
                   "(string>=? \"thing\" 2)",
                   'string>=?: expects a string as 2nd argument, but given: 2; other arguments were: "thing"');


    //string-ci<?

    queueErrorTest("test string-ci<? with wrong arity",
                   "(string-ci<?)",
                   'string-ci<?: expects at least 2 arguments, but given 0');

    queueErrorTest("test string-ci<? with 1st argument not string",
                   "(string-ci<? 1 \"thing\")",
                   'string-ci<?: expects a string as 1st argument, but given: 1; other arguments were: "thing"');

    queueErrorTest("test string-ci<? with 1st argument not string",
                   "(string-ci<? \"thing\" 2)",
                   'string-ci<?: expects a string as 2nd argument, but given: 2; other arguments were: "thing"');


    //string-ci>?

    queueErrorTest("test string-ci>? with wrong arity",
                   "(string-ci>?)",
                   'string-ci>?: expects at least 2 arguments, but given 0');

    queueErrorTest("test string-ci>? with 1st argument not string",
                   "(string-ci>? 1 \"thing\")",
                   'string-ci>?: expects a string as 1st argument, but given: 1; other arguments were: "thing"');

    queueErrorTest("test string-ci>? with 1st argument not string",
                   "(string-ci>? \"thing\" 2)",
                   'string-ci>?: expects a string as 2nd argument, but given: 2; other arguments were: "thing"');

    //string-ci<=?

    queueErrorTest("test string-ci<=? with wrong arity",
                   "(string-ci<=?)",
                   'string-ci<=?: expects at least 2 arguments, but given 0');

    queueErrorTest("test string-ci<=? with 1st argument not string",
                   "(string-ci<=? 1 \"thing\")",
                   'string-ci<=?: expects a string as 1st argument, but given: 1; other arguments were: "thing"');

    queueErrorTest("test string-ci<=? with 1st argument not string",
                   "(string-ci<=? \"thing\" 2)",
                   'string-ci<=?: expects a string as 2nd argument, but given: 2; other arguments were: "thing"');

    //string-ci>=?

    queueErrorTest("test string-ci>=? with wrong arity",
                   "(string-ci>=?)",
                   'string-ci>=?: expects at least 2 arguments, but given 0');

    queueErrorTest("test string-ci>=? with 1st argument not string",
                   "(string-ci>=? 1 \"thing\")",
                   'string-ci>=?: expects a string as 1st argument, but given: 1; other arguments were: "thing"');

    queueErrorTest("test string-ci>=? with 1st argument not string",
                   "(string-ci>=? \"thing\" 2)",
                   'string-ci>=?: expects a string as 2nd argument, but given: 2; other arguments were: "thing"');

    //substring

    queueErrorTest("test substring with wrong arity",
                   "(substring)",
                   'substring: expects 2 or 3 arguments, but given 0');

    queueErrorTest("test substring with 1st argument not string",
                   "(substring 1 1 2)",
                   'substring: expects a string as 1st argument, but given: 1; other arguments were: 1 2');

    queueErrorTest("test substring with 2nd argument not string",
                   "(substring \"hello\" \"world\" 2)",
                   'substring: expects a non-negative exact integer as 2nd argument, but given: "world"; other arguments were: "hello" 2');

    queueErrorTest("test substring with 3rd argument not string",
                   "(substring \"hello\" 2 \"world\")",
                   'substring: expects a non-negative exact integer as 3rd argument, but given: "world"; other arguments were: "hello" 2');                

    queueErrorTest("test substring with index out of bounds",
                   "(substring \"thing\" 0 53)",
                   'substring: ending index 53 out of range [0, 5] for string: "thing"');                


    //string-append

    queueErrorTest("test string-append with first argument not a string",
                   "(string-append 1 \"2\")",
                   'string-append: expects a string as 1st argument, but given: 1; other arguments were: "2"');

    queueErrorTest("test string-append with an argument not a string",
                   "(string-append \"1\" \"2\" \"3\" \"4\" 5)",
                   'string-append: expects a string as 5th argument, but given: 5; other arguments were: "1" "2" "3" "4"');

    //string->list

    queueErrorTest("test string->list with wrong arity",
                   "(string->list)",
                   'string->list: expects 1 argument, but given 0');

    queueErrorTest("test string->list with first argument not a string",
                   "(string->list 2)",
                   'string->list: expects a string as 1st argument, but given: 2');

    //list->string

    queueErrorTest("test list->string with first argument not a string",
                   "(list->string)",
                   'list->string: expects 1 argument, but given 0');

    queueErrorTest("test list->string with first argument not list of char",
                   "(list->string (list 1 2 3))",
                   'list->string: expects a list of character as 1st argument, but given: (list 1 2 3)');

    //string-copy

    queueErrorTest("test string-copy with wrong arity",
                   "(string-copy)",
                   'string-copy: expects 1 argument, but given 0');

    queueErrorTest("test string-copy with 1st argument not string",
                   "(string-copy 2)",
                   'string-copy: expects a string as 1st argument, but given: 2');

    //string->symbol

    queueErrorTest("test string->symbol with wrong arity",
                   "(string->symbol)",
                   'string->symbol: expects 1 argument, but given 0');

    queueErrorTest("test string->symbol with 1st argument not string",
                   "(string->symbol 2)",
                   'string->symbol: expects a string as 1st argument, but given: 2');

    //symbol->string

    queueErrorTest("test symbol->string with wrong arity",
                   "(symbol->string)",
                   'symbol->string: expects 1 argument, but given 0');

    queueErrorTest("test symbol->string with 1st argument not string",
                   "(symbol->string 2)",
                   'symbol->string: expects a symbol as 1st argument, but given: 2');

    //format

    queueErrorTest("test format with wrong arity",
                   "(format)",
                   'format: expects at least 1 argument, but given 0');

    queueErrorTest("test format with 1st argument not string",
                   "(format 1)",
                   'format: expects a string as 1st argument, but given: 1');

    //printf

    queueErrorTest("test bad inputs to big-bang",
                   "(big-bang 1 on-tick add1)",
                   "big-bang: expects a handler as 2nd argument, but given: #<function:on-tick>; other arguments were: 1 #<function:add1>");

    queueErrorTest("too many arguments",
                   "(define (f x) (* x x)) (f 3 4)",
                   "f: expects 1 argument, but given 2: 3 4");

    queueErrorTest("test printf with wrong arity",
                   "(printf)",
                   'printf: expects at least 1 argument, but given 0');

    queueErrorTest("test printf with 1st argument not string",
                   "(printf 1)",
                   'printf: expects a string as 1st argument, but given: 1');

    //string->int

    queueErrorTest("test string->int with wrong arity",
                   "(string->int)",
                   'string->int: expects 1 argument, but given 0');

    queueErrorTest("test string->int with 1st argument not string",
                   "(string->int 1)",
                   'string->int: expects a 1-letter string as 1st argument, but given: 1');

    queueErrorTest("test string->int with 1st argument not 1-letter string",
                   "(string->int \"23\")",
                   'string->int: expects a 1-letter string as 1st argument, but given: "23"');

    //int->string

    queueErrorTest("test int->string with wrong arity",
                   "(int->string)",
                   'int->string: expects 1 argument, but given 0');

    queueErrorTest("test int->string with 1st argument not exact integer",
                   "(int->string \"1\")",
                   'int->string: expects an exact integer in [0,55295] or [57344,1114111] as 1st argument, but given: "1"');

    //explode

    queueErrorTest("explode with wrong arity",
                   "(explode)",
                   'explode: expects 1 argument, but given 0');

    queueErrorTest("test explode with 1st argument not string",
                   "(explode 123)",
                   'explode: expects a string as 1st argument, but given: 123');

    //implode

    queueErrorTest("implode with wrong arity",
                   "(implode)",
                   'implode: expects 1 argument, but given 0');

    queueErrorTest("test implode with 1st argument not list of 1-letter strings",
                   "(implode \"h235s\")",
                   'implode: expects a list of 1-letter strings as 1st argument, but given: "h235s"');

    //string-alphabetic? 

    queueErrorTest("string-alphabetic? with wrong arity",
                   "(string-alphabetic?)",
                   'string-alphabetic?: expects 1 argument, but given 0');

    queueErrorTest("string-alphabetic? 1st argument not string",
                   "(string-alphabetic? 123)",
                   'string-alphabetic?: expects a string as 1st argument, but given: 123');

    //string-ith
    
    queueErrorTest("string-ith with wrong arity",
                   "(string-ith)",
                   'string-ith: expects 2 arguments, but given 0');

    queueErrorTest("string-ith 1st argument not string",
                   "(string-ith 1 2)",
                   'string-ith: expects a string as 1st argument, but given: 1; other arguments were: 2');

    queueErrorTest("string-ith 2nd argument not exact integer",
                   "(string-ith \"hello\" 1.5)",
                   'string-ith: expects an exact integer in [0, length of the given string minus 1 (4)] as 2nd argument, but given: 3/2; other arguments were: "hello"');

    queueErrorTest("string-ith index out of bounds",
                   "(string-ith \"hello\" 5)",
                   'string-ith: expects an exact integer in [0, length of the given string minus 1 (4)] as 2nd argument, but given: 5; other arguments were: "hello"');

    //string-lower-case?

    queueErrorTest("string-lower-case? with wrong arity",
                   "(string-lower-case?)",
                   'string-lower-case?: expects 1 argument, but given 0');

    queueErrorTest("string-lower-case? with 1st argument not string",
                   "(string-lower-case? 2)",
                   'string-lower-case?: expects a string as 1st argument, but given: 2');

    //string-numeric?

    queueErrorTest("string-numeric? with wrong arity",
                   "(string-numeric?)",
                   'string-numeric?: expects 1 argument, but given 0');

    queueErrorTest("tring-numeric? with 1st argument not string",
                   "(string-numeric? 2)",
                   'string-numeric?: expects a string as 1st argument, but given: 2');


    //string-upper-case?

    queueErrorTest("string-upper-case? with wrong arity",
                   "(string-upper-case?)",
                   'string-upper-case?: expects 1 argument, but given 0');

    queueErrorTest("string-upper-case? with 1st argument not string",
                   "(string-upper-case? 2)",
                   'string-upper-case?: expects a string as 1st argument, but given: 2');
    //string-whitespace?

    queueErrorTest("string-whitespace? with wrong arity",
                   "(string-whitespace?)",
                   'string-whitespace?: expects 1 argument, but given 0');

    queueErrorTest("string-whitespace? with 1st argument not string",
                   "(string-whitespace? 2)",
                   'string-whitespace?: expects a string as 1st argument, but given: 2');

    //build-string

    queueErrorTest("build-string with wrong arity",
                   "(build-string)",
                   'build-string: expects 2 arguments, but given 0');

    queueErrorTest("build-string? with 1st argument not non-negative exact integer",
                   "(build-string \"hello\" 2)",
                   'build-string: expects a non-negative exact integer as 1st argument, but given: "hello"; other arguments were: 2');

    queueErrorTest("build-string? with 2nd argument not a proc",
                   "(build-string 5 \"hello\")",
                   'build-string: expects a function name as 2nd argument, but given: "hello"; other arguments were: 5');

    //string->immutable-string DNE

    //string-set! DNE

    //string-fill! DNE

    /*PRIMITIVES['make-bytes'] =

      PRIMITIVES['bytes'] =

      PRIMITIVES['bytes->immutable-bytes'] =

      PRIMITIVES['bytes-length'] =

      PRIMITIVES['bytes-ref'] =

      PRIMITIVES['bytes-set!'] =

      PRIMITIVES['subbytes'] =

      PRIMITIVES['bytes-copy'] =

      PRIMITIVES['bytes-fill!'] =

      PRIMITIVES['bytes-append'] =

      PRIMITIVES['bytes->list'] =

      PRIMITIVES['list->bytes'] =

      PRIMITIVES['bytes=?'] =

      PRIMITIVES['bytes<?'] =

      PRIMITIVES['bytes>?'] = ALL DNE */

    //PRIMITIVES['make-vector']

    queueErrorTest("make-vector with wrong arity",
                   "(make-vector)",
                   'make-vector: expects 2 arguments, but given 0');

    queueErrorTest("make-vector with 1st argument not non-negative exact integer",
                   "(make-vector \"size\" 2)",
                   'make-vector: expects a non-negative exact integer as 1st argument, but given: "size"; other arguments were: 2');

    //PRIMITIVES['vector']

    //PRIMITIVES['vector-length']

    queueErrorTest("vector-length with wrong arity",
                   "(vector-length)",
                   'vector-length: expects 1 argument, but given 0');

    queueErrorTest("vector-length with 1st argument not vector",
                   "(vector-length 1)",
                   'vector-length: expects a vector as 1st argument, but given: 1');

    //PRIMITIVES['vector-ref']

    queueErrorTest("vector-ref with wrong arity",
                   "(vector-ref)",
                   'vector-ref: expects 2 arguments, but given 0');

    queueErrorTest("vector-ref with 1st argument not vector",
                   "(vector-ref 1 2)",
                   'vector-ref: expects a vector as 1st argument, but given: 1; other arguments were: 2');

    queueErrorTest("vector-ref with 2nd argument not non-negative ",
                   "(vector-ref (make-vector 1) 2.2)",
                   'make-vector: expects 2 arguments, but given 1: 1');

    queueErrorTest("vector-ref with index out of bounds",
                   "(vector-ref (vector 1) 7)",
                   'vector-ref: index 7 out of range [0, 0] for vector: #(1)');

    //PRIMITIVES['vector-set!'] 

    queueErrorTest("vector-set! with wrong arity",
                   "(vector-set! )",
                   'vector-set!: expects 3 arguments, but given 0');

    queueErrorTest("vector-length with 1st argument not vector",
                   "(vector-length 1)",
                   'vector-length: expects a vector as 1st argument, but given: 1');


    //PRIMITIVES['vector->list']

    queueErrorTest("vector->list with wrong arity",
                   "(vector->list)",
                   'vector->list: expects 1 argument, but given 0');

    queueErrorTest("vector->list with 1st argument not vector",
                   "(vector->list 1)",
                   'vector->list: expects a vector as 1st argument, but given: 1');

    //PRIMITIVES['list->vector']

    queueErrorTest("build-vector with wrong arity",
                   "(build-vector)",
                   'build-vector: expects 2 arguments, but given 0');

    queueErrorTest("list->vector with 1st argument not vector",
                   "(list->vector 1)",
                   'list->vector: expects a list as 1st argument, but given: 1');

    //PRIMITIVES['build-vector']

    queueErrorTest("build-vector with wrong arity",
                   "(build-vector)",
                   'build-vector: expects 2 arguments, but given 0');

    queueErrorTest("build-vector with 1st argument not non-negative exact integer",
                   "(build-vector \"hello\" 2)",
                   'build-vector: expects a non-negative exact integer as 1st argument, but given: "hello"; other arguments were: 2');

    queueErrorTest("build-vector with 2nd argument not a proc",
                   "(build-vector \"hello\" 2)",
                   'build-vector: expects a non-negative exact integer as 1st argument, but given: "hello"; other arguments were: 2');


    //PRIMITIVES['char=?']

    queueErrorTest("char=? with wrong arity",
                   "(char=?)",
                   'char=?: expects at least 2 arguments, but given 0');

    queueErrorTest("char=? with 1st argument not char",
                   '(char=? "hello")',
                   'char=?: expects at least 2 arguments, but given 1: hello');

    //PRIMITIVES['char<?'] 

    queueErrorTest("char<? with wrong arity",
                   "(char<?)",
                   'char<?: expects at least 2 arguments, but given 0');

    queueErrorTest("char<? with 1st argument not char",
                   "(char<? add1 #\\a)",
                   'char<?: expects a character as 1st argument, but given: #<function:add1>; other arguments were: #\\a');

    queueErrorTest("char<? with 2nd argument not char",
                   "(char<? #\\a add1)",
                   'char<?: expects a character as 2nd argument, but given: #<function:add1>; other arguments were: #\\a');

    //PRIMITIVES['char>?'] 

    queueErrorTest("char>? with wrong arity",
                   "(char>?)",
                   'char>?: expects at least 2 arguments, but given 0');

    queueErrorTest("char>? with 1st argument not char",
                   "(char>? add1 #\\a)",
                   'char>?: expects a character as 1st argument, but given: #<function:add1>; other arguments were: #\\a');

    queueErrorTest("char>? with 2nd argument not char",
                   "(char>? #\\a add1)",
                   'char>?: expects a character as 2nd argument, but given: #<function:add1>; other arguments were: #\\a');

    //PRIMITIVES['char<=?']

    queueErrorTest("char<=? with wrong arity",
                   "(char<=?)",
                   'char<=?: expects at least 2 arguments, but given 0');

    queueErrorTest("char<=? with 1st argument not char",
                   "(char<=? add1 #\\a)",
                   'char<=?: expects a character as 1st argument, but given: #<function:add1>; other arguments were: #\\a');

    queueErrorTest("char<=? with 2nd argument not char",
                   "(char<=? #\\a add1)",
                   'char<=?: expects a character as 2nd argument, but given: #<function:add1>; other arguments were: #\\a');

    //PRIMITIVES['char>=?']

    queueErrorTest("char>=? with wrong arity",
                   "(char>=?)",
                   'char>=?: expects at least 2 arguments, but given 0');

    queueErrorTest("char>=? with 1st argument not char",
                   "(char>=? add1 #\\a)",
                   'char>=?: expects a character as 1st argument, but given: #<function:add1>; other arguments were: #\\a');

    queueErrorTest("char>=? with 2nd argument not char",
                   "(char>=? #\\a add1)",
                   'char>=?: expects a character as 2nd argument, but given: #<function:add1>; other arguments were: #\\a');


    //PRIMITIVES['char-ci=?'] 

    queueErrorTest("char-ci=? with wrong arity",
                   "(char-ci=?)",
                   'char-ci=?: expects at least 2 arguments, but given 0');

    queueErrorTest("char-ci=? with 1st argument not char",
                   "(char-ci=? add1 #\\a)",
                   'char-ci=?: expects a character as 1st argument, but given: #<function:add1>; other arguments were: #\\a');

    queueErrorTest("char-ci=? with 2nd argument not char",
                   "(char-ci=? #\\a add1)",
                   'char-ci=?: expects a character as 2nd argument, but given: #<function:add1>; other arguments were: #\\a');


    //PRIMITIVES['char-ci<?'] 

    queueErrorTest("char-ci<? with wrong arity",
                   "(char-ci<?)",
                   'char-ci<?: expects at least 2 arguments, but given 0');

    queueErrorTest("char-ci<? with 1st argument not char",
                   "(char-ci<? add1 #\\a)",
                   'char-ci<?: expects a character as 1st argument, but given: #<function:add1>; other arguments were: #\\a');

    queueErrorTest("char-ci<? with 2nd argument not char",
                   "(char-ci<? #\\a add1)",
                   'char-ci<?: expects a character as 2nd argument, but given: #<function:add1>; other arguments were: #\\a');

    //PRIMITIVES['char-ci>?'] 

    queueErrorTest("char-ci>? with wrong arity",
                   "(char-ci>?)",
                   'char-ci>?: expects at least 2 arguments, but given 0');

    queueErrorTest("char-ci>? with 1st argument not char",
                   "(char-ci>? add1 #\\a)",
                   'char-ci>?: expects a character as 1st argument, but given: #<function:add1>; other arguments were: #\\a');

    queueErrorTest("char-ci>? with 2nd argument not char",
                   "(char-ci>? #\\a add1)",
                   'char-ci>?: expects a character as 2nd argument, but given: #<function:add1>; other arguments were: #\\a');

    //PRIMITIVES['char-ci<=?'] 

    queueErrorTest("char-ci<=? with wrong arity",
                   "(char-ci<=?)",
                   'char-ci<=?: expects at least 2 arguments, but given 0');

    queueErrorTest("char-ci<=? with 1st argument not char",
                   "(char-ci<=? add1 #\\a)",
                   'char-ci<=?: expects a character as 1st argument, but given: #<function:add1>; other arguments were: #\\a');

    queueErrorTest("char-ci<=? with 2nd argument not char",
                   "(char-ci<=? #\\a add1)",
                   'char-ci<=?: expects a character as 2nd argument, but given: #<function:add1>; other arguments were: #\\a');

    //PRIMITIVES['char-ci>=?'] 

    queueErrorTest("char-ci>=? with wrong arity",
                   "(char-ci>=?)",
                   'char-ci>=?: expects at least 2 arguments, but given 0');

    queueErrorTest("char-ci>=? with 1st argument not char",
                   "(char-ci>=? add1 #\\a)",
                   'char-ci>=?: expects a character as 1st argument, but given: #<function:add1>; other arguments were: #\\a');

    queueErrorTest("char-ci>=? with 2nd argument not char",
                   "(char-ci>=? #\\a add1)",
                   'char-ci>=?: expects a character as 2nd argument, but given: #<function:add1>; other arguments were: #\\a');

    //PRIMITIVES['char-alphabetic?'] 

    queueErrorTest("char-alphabetic? with wrong arity",
                   "(char-alphabetic?)",
                   'char-alphabetic?: expects 1 argument, but given 0');

    queueErrorTest("char-alphabetic? with 1st argument not char",
                   "(char-alphabetic? add1)",
                   'char-alphabetic?: expects a character as 1st argument, but given: #<function:add1>');

    //PRIMITIVES['char-numeric?'] 

    queueErrorTest("char-numeric? with wrong arity",
                   "(char-numeric?)",
                   'char-numeric?: expects 1 argument, but given 0');

    queueErrorTest("char-numeric? with 1st argument not char",
                   "(char-numeric? add1)",
                   'char-numeric?: expects a character as 1st argument, but given: #<function:add1>');

    //PRIMITIVES['char-whitespace?']

    queueErrorTest("char-whitespace? with wrong arity",
                   "(char-whitespace?)",
                   'char-whitespace?: expects 1 argument, but given 0');

    queueErrorTest("char-whitespace? with 1st argument not char",
                   "(char-whitespace? add1)",
                   'char-whitespace?: expects a character as 1st argument, but given: #<function:add1>');

    //PRIMITIVES['char-upper-case?'] 

    queueErrorTest("char-upper-case? with wrong arity",
                   "(char-upper-case?)",
                   'char-upper-case?: expects 1 argument, but given 0');

    queueErrorTest("char-upper-case? with 1st argument not char",
                   "(char-upper-case? add1)",
                   'char-upper-case?: expects a character as 1st argument, but given: #<function:add1>');

    //PRIMITIVES['char-lower-case?'] 

    queueErrorTest("char-lower-case? with wrong arity",
                   "(char-lower-case?)",
                   'char-lower-case?: expects 1 argument, but given 0');

    queueErrorTest("char-lower-case? with 1st argument not char",
                   "(char-lower-case? add1)",
                   'char-lower-case?: expects a character as 1st argument, but given: #<function:add1>');

    //PRIMITIVES['char->integer'] 

    queueErrorTest("char->integer with wrong arity",
                   "(char->integer)",
                   'char->integer: expects 1 argument, but given 0');

    queueErrorTest("char->integer with 1st argument not char",
                   "(char->integer add1)",
                   'char->integer: expects a character as 1st argument, but given: #<function:add1>');

    //PRIMITIVES['integer->char'] 

    queueErrorTest("integer->char with wrong arity",
                   "(integer->char)",
                   'integer->char: expects 1 argument, but given 0');

    queueErrorTest("integer->char with 1st argument not integer",
                   "(integer->char add1)",
                   'integer->char: expects an exact integer in [0,#x10FFFF], not in [#xD800,#xDFFF] as 1st argument, but given: #<function:add1>');
    //perhaps change the error message so hex does not show

    //PRIMITIVES['char-upcase']

    queueErrorTest("char-upcaser with wrong arity",
                   "(char-upcase)",
                   'char-upcase: expects 1 argument, but given 0');

    queueErrorTest("char-upcase with 1st argument not char",
                   "(char-upcase add1)",
                   'char-upcase: expects a character as 1st argument, but given: #<function:add1>');

    //PRIMITIVES['char-downcase'] 

    queueErrorTest("char-downcase with wrong arity",
                   "(char-downcase)",
                   'char-downcase: expects 1 argument, but given 0');

    queueErrorTest("char-downcase with 1st argument not char",
                   "(char-downcase add1)",
                   'char-downcase: expects a character as 1st argument, but given: #<function:add1>');

    //PRIMITIVES['make-posn'] 

    queueErrorTest("make-posn with wrong arity",
                   "(make-posn)",
                   'make-posn: expects 2 arguments, but given 0');

    //PRIMITIVES['posn-x']

    queueErrorTest("posn-x with wrong arity",
                   "(posn-x)",
                   'posn-x: expects 1 argument, but given 0');

    queueErrorTest("posn-x with 1st argument not posn",
                   "(posn-x add1)",
                   'posn-x: expects a posn as 1st argument, but given: #<function:add1>');

    //PRIMITIVES['posn-y'] 

    queueErrorTest("posn-y with wrong arity",
                   "(posn-y)",
                   'posn-y: expects 1 argument, but given 0');

    queueErrorTest("posn-y with 1st argument not posn",
                   "(posn-y add1)",
                   'posn-y: expects a posn as 1st argument, but given: #<function:add1>');

    //PRIMITIVES['key=?'] 

    queueErrorTest("key=? with wrong arity",
                   "(key=?)",
                   'key=?: expects 2 arguments, but given 0');

    //PRIMITIVES['image?'] 

    queueErrorTest("image? with wrong arity",
                   "(image?)",
                   'image?: expects 1 argument, but given 0');

    //PRIMITIVES['make-color']

    queueErrorTest("make-color with wrong arity",
                   "(make-color)",
                   'make-color: expects 3 or 4 arguments, but given 0');

    queueErrorTest("make-color with 1st argument not a number between 0 and 255",
                   "(make-color 256 1 2 3)",
                   'make-color: expects a number between 0 and 255 as 1st argument, but given: 256; other arguments were: 1 2 3');

    queueErrorTest("make-color with 2nd argument not a number between 0 and 255",
                   "(make-color 255 256 2 3)",
                   'make-color: expects a number between 0 and 255 as 2nd argument, but given: 256; other arguments were: 255 2 3');


    queueErrorTest("make-color with 3rd argument not a number between 0 and 255",
                   "(make-color 255 255 256 3)",
                   'make-color: expects a number between 0 and 255 as 3rd argument, but given: 256; other arguments were: 255 255 3');

    queueErrorTest("make-color with 4th argument not a number between 0 and 255",
                   "(make-color 255 255 255 256)",
                   'make-color: expects a number between 0 and 255 as 4th argument, but given: 256; other arguments were: 255 255 255');

    //PRIMITIVES['color-red']

    queueErrorTest("color-red with wrong arity",
                   "(color-red)",
                   'color-red: expects 1 argument, but given 0');

    queueErrorTest("color-red with 1st argument not color",
                   "(color-red add1)",
                   'color-red: expects a color as 1st argument, but given: #<function:add1>');

    //PRIMITIVES['color-green'] 

    queueErrorTest("color-green with wrong arity",
                   "(color-green)",
                   'color-green: expects 1 argument, but given 0');

    queueErrorTest("color-green with 1st argument not color",
                   "(color-green add1)",
                   'color-green: expects a color as 1st argument, but given: #<function:add1>');

    //PRIMITIVES['color-blue'] 

    queueErrorTest("color-blue with wrong arity",
                   "(color-blue)",
                   'color-blue: expects 1 argument, but given 0');

    queueErrorTest("color-blue with 1st argument not color",
                   "(color-blue add1)",
                   'color-blue: expects a color as 1st argument, but given: #<function:add1>');

    //PRIMITIVES['color-alpha']

    queueErrorTest("color-alpha with wrong arity",
                   "(color-alpha)",
                   'color-alpha: expects 1 argument, but given 0');

    queueErrorTest("color-alpha with 1st argument not color",
                   "(color-alpha add1)",
                   'color-alpha: expects a color as 1st argument, but given: #<function:add1>');


    //PRIMITIVES['empty-scene'] 

    queueErrorTest("empty-scene with wrong arity",
                   "(empty-scene)",
                   'empty-scene: expects 2 arguments, but given 0');

    queueErrorTest("empty-scene with 1st argument not non-negative number",
                   "(empty-scene add1 2)",
                   'empty-scene: expects a non-negative number as 1st argument, but given: #<function:add1>; other arguments were: 2');

    queueErrorTest("empty-scene with 2nd argument not non-negative number",
                   "(empty-scene 0 add1)",
                   'empty-scene: expects a non-negative number as 2nd argument, but given: #<function:add1>; other arguments were: 0');

    //PRIMITIVES['place-image']

    queueErrorTest("empty-scene with wrong arity",
                   "(empty-scene)",
                   'empty-scene: expects 2 arguments, but given 0');

    queueErrorTest("empty-scene with 1st argument not non-negative number",
                   "(empty-scene add1 2)",
                   'empty-scene: expects a non-negative number as 1st argument, but given: #<function:add1>; other arguments were: 2');

    queueErrorTest("empty-scene with 2nd argument not non-negative number",
                   "(empty-scene 0 add1)",
                   'empty-scene: expects a non-negative number as 2nd argument, but given: #<function:add1>; other arguments were: 0');

    //PRIMITIVES['place-image/align']

    queueErrorTest("place-image/align with wrong arity",
                   "(place-image/align)",
                   'place-image/align: expects 6 arguments, but given 0');

    queueErrorTest("place-image/align with 1st argument not image",
                   "(place-image/align add1 2 3 4 5 6)",
                   'place-image/align: expects an image as 1st argument, but given: #<function:add1>; other arguments were: 2 3 4 5 6');

    queueErrorTest("place-image/align with 2nd argument not real number",
                   "(place-image/align (circle 50 \"solid\" \"red\") \"hello\" 3 4 5 6)",
                   'place-image/align: expects a real number as 2nd argument, but given: "hello"; other arguments were: <image> 3 4 5 6');

    queueErrorTest("place-image/align with 3rd argument not real number",
                   "(place-image/align (circle 50 \"solid\" \"red\") 2 \"hello\" 4 5 6)",
                   'place-image/align: expects a real number as 3rd argument, but given: "hello"; other arguments were: <image> 2 4 5 6');

    queueErrorTest("place-image/align with 4th argument not x-place",
                   "(place-image/align (circle 50 \"solid\" \"red\") 2 3 (make-posn 2 3) 5 6)",
                   'place-image/align: expects a x-place as 4th argument, but given: (posn 2 3); other arguments were: <image> 2 3 5 6');

    queueErrorTest("place-image/alignwith 5th argument not y-place",
                   "(place-image/align (circle 50 \"solid\" \"red\") 2 3 \"left\" 5 6)",
                   'place-image/align: expects a y-place as 5th argument, but given: 5; other arguments were: <image> 2 3 "left" 6');

    queueErrorTest("place-image/align with 6th argument not image",
                   "(place-image/align (circle 50 \"solid\" \"red\") 2 3 \"left\" \"top\" add1)",
                   'place-image/align: expects an image as 6th argument, but given: #<function:add1>; other arguments were: <image> 2 3 "left" "top"');

    //PRIMITIVES['scene+line'] 

    queueErrorTest("scene+line with wrong arity",
                   "(scene+line)",
                   'scene+line: expects 6 arguments, but given 0');

    //PRIMITIVES['circle']

    queueErrorTest("circle with wrong arity",
                   "(circle)",
                   'circle: expects 3 arguments, but given 0');

    queueErrorTest("circle with 1st argument not non-negative number",
                   "(circle \"foo\" 2 3)",
                   'circle: expects a non-negative number as 1st argument, but given: "foo"; other arguments were: 2 3');

    queueErrorTest("circle with 2nd argument not style",
                   "(circle 1 true 3)",
                   'circle: expects a style ("solid" or "outline" or [0-255]) as 2nd argument, but given: true; other arguments were: 1 3');

    queueErrorTest("circle with 3rd argument not colour",
                   "(circle 1 \"solid\" add1)",
                   'circle: expects a color as 3rd argument, but given: #<function:add1>; other arguments were: 1 "solid"');

    //PRIMITIVES['star']

    queueErrorTest("star with wrong arity",
                   "(star)",
                   'star: expects 5 or 3 arguments, but given 0');

    queueErrorTest("star with 1st argument not non-negative number",
                   "(star \"hello\" 2 3)",
                   'star: expects a non-negative number as 1st argument, but given: "hello"; other arguments were: 2 3');

    queueErrorTest("star with 2nd argument not style",
                   "(star 1 \"moo\" 3)",
                   'star: expects a style ("solid" or "outline" or [0-255]) as 2nd argument, but given: "moo"; other arguments were: 1 3');

    queueErrorTest("star with 3rd argument not colour",
                   "(star 1 \"solid\" add1)",
                   'star: expects a color as 3rd argument, but given: #<function:add1>; other arguments were: 1 "solid"');

    //PRIMITIVES['radial-star'] 

    queueErrorTest("radial-star with wrong arity",
                   "(radial-star)",
                   'radial-star: expects 5 arguments, but given 0');

    queueErrorTest("radial-star with 1st argument not positive integer greater than or equal to 2",
                   "(radial-star 1 2 3 4 5)",
                   'radial-star: expects a positive integer greater than or equal to 2 as 1st argument, but given: 1; other arguments were: 2 3 4 5');

    queueErrorTest("star with 2nd argument not positive number",
                   "(radial-star 5 \"foo\" 3 4 5)",
                   'radial-star: expects a positive number as 2nd argument, but given: "foo"; other arguments were: 5 3 4 5');

    queueErrorTest("star with 3rd argument not positive number",
                   "(radial-star 5 4 \"foo\" 4 5)",
                   'radial-star: expects a positive number as 3rd argument, but given: "foo"; other arguments were: 5 4 4 5');

    queueErrorTest("star with 4th argument not style",
                   "(radial-star 5 4 2 \"foo\" 5)",
                   'radial-star: expects a style ("solid" or "outline" or [0-255]) as 4th argument, but given: "foo"; other arguments were: 5 4 2 5');

    queueErrorTest("star with 5th argument not colour",
                   "(radial-star 5 4 2 \"solid\" \"foo\")",
                   'radial-star: expects a color as 5th argument, but given: "foo"; other arguments were: 5 4 2 "solid"');

    // PRIMITIVES['rectangle'] 

    queueErrorTest("rectangle wrong arity",
                   "(rectangle)",
                   'rectangle: expects 4 arguments, but given 0');

    queueErrorTest("rectangle with 1st argument non-negative number",
                   "(rectangle \"foo\" 2 3 4)",
                   'rectangle: expects a non-negative number as 1st argument, but given: "foo"; other arguments were: 2 3 4');

    queueErrorTest("rectangle with 2nd argument not non-negative number",
                   "(rectangle 2 \"foo\" 3 4)",
                   'rectangle: expects a non-negative number as 2nd argument, but given: "foo"; other arguments were: 2 3 4');

    queueErrorTest("rectangle with 3rd argument not style",
                   "(rectangle 2 5 add1 4)",
                   'rectangle: expects a style ("solid" or "outline" or [0-255]) as 3rd argument, but given: #<function:add1>; other arguments were: 2 5 4');

    queueErrorTest("rectangle with 4th argument not colour",
                   "(rectangle 2 5 \"outline\" \"focus\")",
                   'rectangle: expects a color as 4th argument, but given: "focus"; other arguments were: 2 5 "outline"');

    // PRIMITIVES['regular-polygon'] 

    queueErrorTest("regular-polygon wrong arity",
                   "(regular-polygon)",
                   'regular-polygon: expects 4 arguments, but given 0');

    queueErrorTest("rectangle with 1st argument not non-negative number",
                   "(regular-polygon \"foo\" 2 3 4)",
                   'regular-polygon: expects a non-negative number as 1st argument, but given: "foo"; other arguments were: 2 3 4');

    queueErrorTest("rectangle with 2nd argument not positive integer greater than or equal to 3",
                   "(regular-polygon 2 \"foo\" 3 4)",
                   'regular-polygon: expects a positive integer greater than or equal to 3 as 2nd argument, but given: "foo"; other arguments were: 2 3 4');

    queueErrorTest("regular-polygon with 3rd argument not style",
                   "(regular-polygon 2 5 add1 4)",
                   'regular-polygon: expects a style ("solid" or "outline" or [0-255]) as 3rd argument, but given: #<function:add1>; other arguments were: 2 5 4');

    queueErrorTest("regular-polygonwith 4th argument not colour",
                   "(regular-polygon 2 5 \"outline\" \"focus\")",
                   'regular-polygon: expects a color as 4th argument, but given: "focus"; other arguments were: 2 5 "outline"');

    // PRIMITIVES['star-polygon'] 

    queueErrorTest("star-polygon wrong arity",
                   "(star-polygon)",
                   'star-polygon: expects 5 arguments, but given 0');

    queueErrorTest("star-polygon with 1st argument not non-negative number",
                   "(star-polygon \"foo\" 2 3 4 5)",
                   'star-polygon: expects a non-negative number as 1st argument, but given: "foo"; other arguments were: 2 3 4 5');

    queueErrorTest("star-polygon with 2nd argument not positive integer greater than or equal to 3",
                   "(star-polygon 2 \"foo\" 3 4 5)",
                   'star-polygon: expects a positive integer greater than or equal to 3 as 2nd argument, but given: "foo"; other arguments were: 2 3 4 5');

    queueErrorTest("star-polygon with 3rd argument not positive integer greater than or equal to 1",
                   "(star-polygon 2 5 add1 4 5)",
                   'star-polygon: expects a positive integer greater than or equal to 1 as 3rd argument, but given: #<function:add1>; other arguments were: 2 5 4 5');

    queueErrorTest("star-polygon with 4th argument not style",
                   "(star-polygon 2 5 5 add1 5)",
                   'star-polygon: expects a style ("solid" or "outline" or [0-255]) as 4th argument, but given: #<function:add1>; other arguments were: 2 5 5 5');

    queueErrorTest("star-polygon with 5th argument not colour",
                   "(star-polygon 2 5 5 \"outline\" \"focus\")",
                   'star-polygon: expects a color as 5th argument, but given: "focus"; other arguments were: 2 5 5 "outline"');

    // PRIMITIVES['rhombus'] 

    queueErrorTest("rhombus wrong arity",
                   "(rhombus)",
                   'rhombus: expects 4 arguments, but given 0');

    queueErrorTest("rhombus with 1st argument non-negative number",
                   "(rhombus \"foo\" 2 3 4)",
                   'rhombus: expects a non-negative number as 1st argument, but given: "foo"; other arguments were: 2 3 4');

    queueErrorTest("star with 2nd argument not non-negative number",
                   "(rhombus 2 \"foo\" 3 4)",
                   'rhombus: expects a non-negative number as 2nd argument, but given: "foo"; other arguments were: 2 3 4');

    queueErrorTest("rhombus with 3rd argument not style",
                   "(rhombus 2 5 add1 4)",
                   'rhombus: expects a style ("solid" or "outline" or [0-255]) as 3rd argument, but given: #<function:add1>; other arguments were: 2 5 4');

    queueErrorTest("rhombus with 4th argument not colour",
                   "(rhombus 2 5 \"outline\" \"focus\")",
                   'rhombus: expects a color as 4th argument, but given: "focus"; other arguments were: 2 5 "outline"');


    // PRIMITIVES['square'] 

    queueErrorTest("square wrong arity",
                   "(square)",
                   'square: expects 3 arguments, but given 0');

    queueErrorTest("square with 1st argument non-negative number",
                   "(square \"foo\" 3 4)",
                   'square: expects a non-negative number as 1st argument, but given: "foo"; other arguments were: 3 4');

    queueErrorTest("square with 2nd argument not style",
                   "(square 2 add1 4)",
                   'square: expects a style ("solid" or "outline" or [0-255]) as 2nd argument, but given: #<function:add1>; other arguments were: 2 4');

    queueErrorTest("square with 3rd argument not colour",
                   "(square 2 \"outline\" \"focus\")",
                   'square: expects a color as 3rd argument, but given: "focus"; other arguments were: 2 "outline"');

    // PRIMITIVES['triangle'] 

    queueErrorTest("triangle wrong arity",
                   "(triangle)",
                   'triangle: expects 3 arguments, but given 0');

    queueErrorTest("triangle with 1st argument non-negative number",
                   "(triangle \"foo\" 3 4)",
                   'triangle: expects a non-negative number as 1st argument, but given: "foo"; other arguments were: 3 4');

    queueErrorTest("triangle with 2nd argument not style",
                   "(triangle 2 add1 4)",
                   'triangle: expects a style ("solid" or "outline" or [0-255]) as 2nd argument, but given: #<function:add1>; other arguments were: 2 4');

    queueErrorTest("triangle with 3rd argument not colour",
                   "(triangle 2 \"outline\" \"focus\")",
                   'triangle: expects a color as 3rd argument, but given: "focus"; other arguments were: 2 "outline"');

    // PRIMITIVES['right-triangle'] 

    queueErrorTest("right-triangle wrong arity",
                   "(right-triangle)",
                   'right-triangle: expects 4 arguments, but given 0');

    queueErrorTest("right-triangle with 1st argument non-negative number",
                   "(right-triangle \"foo\" 3 4 3)",
                   'right-triangle: expects a non-negative number as 1st argument, but given: "foo"; other arguments were: 3 4 3');

    queueErrorTest("right-triangle with 2nd argument non-negative number",
                   "(right-triangle 5 \"foo\" 3 4)",
                   'right-triangle: expects a non-negative number as 2nd argument, but given: "foo"; other arguments were: 5 3 4');

    queueErrorTest("right-triangle with 3rd argument not style",
                   "(right-triangle 2 4 add1 4)",
                   'right-triangle: expects a style ("solid" or "outline" or [0-255]) as 3rd argument, but given: #<function:add1>; other arguments were: 2 4 4');

    queueErrorTest("right-triangle with 4th argument not colour",
                   "(right-triangle 5 2 \"outline\" \"focus\")",
                   'right-triangle: expects a color as 4th argument, but given: "focus"; other arguments were: 5 2 "outline"');

    // PRIMITIVES['isosceles-triangle'] 


    queueErrorTest("isosceles-triangle wrong arity",
                   "(isosceles-triangle)",
                   'isosceles-triangle: expects 4 arguments, but given 0');

    queueErrorTest("isosceles-triangle with 1st argument not non-negative number",
                   "(isosceles-triangle \"foo\" 3 4 3)",
                   'isosceles-triangle: expects a non-negative number as 1st argument, but given: "foo"; other arguments were: 3 4 3');

    queueErrorTest("isosceles-triangle with 2nd argument not finite real number between 0 and 360",
                   "(isosceles-triangle 5 \"foo\" 3 4)",
                   'isosceles-triangle: expects a finite real number between 0 and 360 as 2nd argument, but given: "foo"; other arguments were: 5 3 4');

    queueErrorTest("isosceles-triangle with 3rd argument not style",
                   "(isosceles-triangle 2 4 add1 4)",
                   'isosceles-triangle: expects a style ("solid" or "outline" or [0-255]) as 3rd argument, but given: #<function:add1>; other arguments were: 2 4 4');

    queueErrorTest("isosceles-triangle with 4th argument not colour",
                   "(isosceles-triangle 5 2 \"outline\" \"focus\")",
                   'isosceles-triangle: expects a color as 4th argument, but given: "focus"; other arguments were: 5 2 "outline"');

    // PRIMITIVES['ellipse'] 

    queueErrorTest("ellipse wrong arity",
                   "(ellipse)",
                   'ellipse: expects 4 arguments, but given 0');

    queueErrorTest("ellipse with 1st argument not non-negative number",
                   "(ellipse \"foo\" 3 4 3)",
                   'ellipse: expects a non-negative number as 1st argument, but given: "foo"; other arguments were: 3 4 3');

    queueErrorTest("ellipse with 2nd argument not non-negative number",
                   "(ellipse 5 \"foo\" 3 4)",
                   'ellipse: expects a non-negative number as 2nd argument, but given: "foo"; other arguments were: 5 3 4');

    queueErrorTest("ellipse with 3rd argument not string",
                   "(ellipse 2 4 add1 4)",
                   'ellipse: expects a style ("solid" or "outline" or [0-255]) as 3rd argument, but given: #<function:add1>; other arguments were: 2 4 4');

    queueErrorTest("ellipse with 4th argument not colour",
                   "(ellipse 5 2 \"outline\" \"focus\")",
                   'ellipse: expects a color as 4th argument, but given: "focus"; other arguments were: 5 2 "outline"');

    // PRIMITIVES['line'] 

    queueErrorTest("line wrong arity",
                   "(line)",
                   'line: expects 3 arguments, but given 0');

    queueErrorTest("line 1st argument not finite real",
                   "(line \"foo\" 3 4)",
                   'line: expects a finite real number as 1st argument, but given: "foo"; other arguments were: 3 4');

    queueErrorTest("line with 2nd argument not finite real",
                   "(line 5 \"foo\" 3)",
                   'line: expects a finite real number as 2nd argument, but given: "foo"; other arguments were: 5 3');

    queueErrorTest("line with 3rd argument not colour",
                   "(line 2 4 add1)",
                   'line: expects a color as 3rd argument, but given: #<function:add1>; other arguments were: 2 4');




    queueErrorTest("Redefinition",
                   "(define x 3) (define x 4)",
                   "x: this name has a previous definition and cannot be re-defined");



    queueErrorTest("define as a bare expression",
		   "define",
		   "define: expected an open parenthesis before define, but found none");

    queueErrorTest("if as a bare expression, but found none",
		   "if",
		   "if: expected an open parenthesis before if, but found none");

    queueErrorTest("cond as a bare expression",
		   "cond",
		   "cond: expected an open parenthesis before cond, but found none");

    queueErrorTest("case as a bare expression",
		   "case",
		   "case: expected an open parenthesis before case, but found none");

    queueErrorTest("let as a bare expression",
		   "let",
		   "let: expected an open parenthesis before let, but found none");

    queueErrorTest("let* as a bare expression",
		   "let*",
		   "let*: expected an open parenthesis before let*, but found none");

    queueErrorTest("letrec as a bare expression",
		   "letrec",
		   "letrec: expected an open parenthesis before letrec, but found none");

    queueErrorTest("quasiquote as a bare expression",
		   "quasiquote",
		   "quasiquote: expected an open parenthesis before quasiquote, but found none");
    queueErrorTest("unquote as a bare expression",
		   "unquote",
		   "unquote: expected an open parenthesis before unquote, but found none");

    queueErrorTest("unquote-splicing as a bare expression",
		   "unquote-splicing",
		   "unquote-splicing: expected an open parenthesis before unquote-splicing, but found none");

    // FIXME: more quasiquote/unquote/unquote-splicing tests needed.

    queueErrorTest("local as a bare expression",
		   "local",
		   "local: expected an open parenthesis before local, but found none");

    queueErrorTest("begin as a bare expression",
		   "begin",
		   "begin: expected an open parenthesis before begin, but found none");
    queueErrorTest("and as a bare expression",
		   "and",
		   "and: expected an open parenthesis before and, but found none");
    queueErrorTest("or as a bare expression",
		   "or",
		   "or: expected an open parenthesis before or, but found none");
    queueErrorTest("when as a bare expression",
		   "when",
		   "when: expected an open parenthesis before when, but found none");

    queueErrorTest("unless as a bare expression",
		   "unless",
		   "unless: expected an open parenthesis before unless, but found none");

    queueErrorTest("lambda as a bare expression",
		   "lambda",
		   "lambda: expected an open parenthesis before lambda, but found none");

    queueErrorTest("lambda as a bare expression",
		   "λ",
		   "λ: expected an open parenthesis before λ, but found none");

    queueErrorTest("quote as a bare expression",
		   "quote",
		   "quote: expected an open parenthesis before quote, but found none");








    queueErrorTest("define with no args",
		   "(define)",
		   "define: expected a variable, or a function name and its variables (in parentheses), after define, but nothing's there");
    
    queueErrorTest("define variable not given enough args",
		   "(define a)",
		   "define: expected an expression after the variable a but nothing's there");

    queueErrorTest("define function not given enough args",
		   "(define (f x))",
		   "define: expected an expression for the function body, but nothing's there");
    
    queueErrorTest("define given undefined variable",
		   "(define x y)",
		   "y: this variable is not defined");

    queueErrorTest("define given empty parenthesis",
		   "(define ())",
		   "define: expected a name for the function within the parentheses");

    queueErrorTest("define given empty parenthesis",
		   "(define 29)",
		   "define: expected a variable but found something else");



    queueErrorTest("define given too many args",
		   "(define (x) 3 4 5)",
		   "define: expected only one expression for the function body, but found 2 extra parts");

    queueErrorTest("define given bad function name",
		   "(define (\"x\") 3)",
		   "define: expected a function name after the open parenthesis but found something else");
    
    queueErrorTest("define given bad arg",
		   "(define (x y 7) 3)",
		   "define: expected a variable but found something else");
    
    queueErrorTest("define variable given too many args",
		   "(define x 1 2 3 4 5)",
		   "define: expected only one expression after the variable x, but found 4 extra parts");
    
    queueErrorTest("define-struct given no args",
		   "(define-struct)",
		   "define-struct: expected the structure name after define-struct, but nothing's there");
    
    queueErrorTest("define-struct given not enough args",
		   "(define-struct test)",
		   "define-struct: expected at least one field name (in parentheses) after the structure name, but nothing's there");

    queueErrorTest("define-struct given empty parenthesis",
		   "(define-struct ())",
		   "define-struct: expected the structure name after define-struct, but found something else");
    
    queueErrorTest("define-struct given too many args",
		   "(define-struct test (p1 p2) 2 3 4 5 6)",
		   "define-struct: expected nothing after the field names, but found 5 extra parts");

    queueErrorTest("define-struct given wrong kind of field",
		   "(define-struct test (4))",
		   "define-struct: expected a field name, but found something else");

    queueErrorTest("define-struct given too many args",
		   "(define-struct test (p1 p2) 2)",
		   "define-struct: expected nothing after the field names, but found 1 extra part");
    
    queueErrorTest("define-struct given bad structure name",
		   "(define-struct (name) (pr1 pr2))",
		   "define-struct: expected the structure name after define-struct, but found something else");
    
    queueErrorTest("define-struct not given proper field name",
		   "(define-struct test this func)",
		   "define-struct: expected at least one field name (in parentheses) after the structure name, but found something else");

    queueErrorTest("previous definition encountered",
		   "(define (fox a) 3) (define (fox b) 4)",
		   "fox: this name has a previous definition and cannot be re-defined");
    
    queueErrorTest("previous struct definition encountered",
		   "(define-struct fox (a)) (define-struct fox (b))",
		   "fox: this name has a previous definition and cannot be re-defined");
    
    queueErrorTest("dividing by zero",
		   "(/ 5 0)",
		   "/: cannot divide by zero");

    queueErrorTest("dividing by zero",
		   "(/ 5 4 0)",
		   "/: cannot divide by zero");

    queueErrorTest("dividing by zero",
		   "(/ 5 4 3 0)",
		   "/: cannot divide by zero");

    queueErrorTest("dividing by zero",
		   "(/ 5 0 4 3)",
		   "/: cannot divide by zero");

    queueErrorTest("dividing by zero",
		   "(/ 5 4 0 3)",
		   "/: cannot divide by zero");
    
    queueErrorTest("defining a variable with the same variable",
		   "(define c c)",
		   "c: this variable is not defined");

    queueErrorTest("define-values given bad first part",
		   "(define-values x (5 4))",
		   "define-values: expects a list of variables and a body, but found something else");
    
    queueErrorTest("define-values given only one part",
		   "(define-values (5 4))",
		   "define-values: expects a list of variables and a body, but found only one part");

    queueErrorTest("define-values given too many args",
		   "(define-values (x) (values 1) 1)",
		   "define-values: expects a list of variables and a body, but found an extra part");
    
    queueErrorTest("lambda given no args",
		   "(lambda)",
		   "lambda: expected at least one variable (in parentheses) after lambda, but nothing's there");
    
    queueErrorTest("lambda given extra arg no paren",
		   "(lambda 1)",
		   "lambda: expected at least one variable (in parentheses) after lambda, but found something else");
    
    // Note: we do not currently allow vararity functions in this language, so even though
    // this is ok in Scheme, it's not in WeScheme:
    queueErrorTest("Disallow vararity lambdas",
                   "(lambda args args)",
                   "lambda: expected at least one variable (in parentheses) after lambda, but found something else");

    queueErrorTest("lambda given 2 extra args no paren",
		   "(lambda 1 2)",
		   "lambda: expected at least one variable (in parentheses) after lambda, but found something else");
    
    queueErrorTest("lambda given empty paren",
		   "(lambda ())",
		   "lambda: expected an expression for the function body, but nothing's there");
    
    queueErrorTest("lambda given paren with bad arg type",
		   "(lambda (1))",
		   "lambda: expected a list of variables after lambda, but found something else");

    queueErrorTest("lambda given variable, but no function body",
		   "(lambda (x))",
		   "lambda: expected an expression for the function body, but nothing's there");
    
    queueErrorTest("lambda given two identical variables",
		   "(lambda (x x) 1)",
		   "lambda: found a variable that is already used here");
    
    queueErrorTest("lambda given an extra part",
		   "(lambda (x y) (+ y x) 1)",
		   "lambda: expected only one expression for the function body, but found 1 extra part");
    
    queueErrorTest("lambda given two extra part",
		   "(lambda (x y) (+ y x) 1 2)",
		   "lambda: expected only one expression for the function body, but found 2 extra parts");

    queueErrorTest("lambda given multiple variables not in paren",
		   "(lambda x y (+ 2 y))",
		   "lambda: expected at least one variable (in parentheses) after lambda, but found something else");
    
    queueErrorTest("lambda given lambda as variable, also no body for 2nd lambda",
		   "(lambda (x lambda) (lambda (u)))",
		   "lambda: this is a reserved keyword and cannot be used as a variable or function name");

    queueErrorTest("let given nothing",
		   "(let)",
		   "let: expected at least one binding (in parentheses) after let, but nothing's there");
    
    queueErrorTest("let given empty paren",
		   "(let ())",
		   "let: expected a single body, but found none");
    
    queueErrorTest("let given bad form",
		   "(let x 6)",
		   "let: expected sequence of key value pairs, but given something else");

    queueErrorTest("let given bad form",
		   "(let (x) 3)",
		   "let: expected a key/value pair, but given something else");
    
    queueErrorTest("let given bad form",
		   "(let (x 2) 3)",
		   "let: expected a key/value pair, but given something else");
    
    queueErrorTest("let given bad form",
		   "(let ((x 3) (x 6)) 5)",
		   "let: found a variable that is already used here");
    
    queueErrorTest("let given bad form",
		   "(let ((x x)) x)",
		   "x: this variable is not defined");
    
    queueErrorTest("let given bad form",
		   "(let ((x 3)))",
		   "let: expected a single body, but found none");
    
    queueErrorTest("let given bad form",
		   "(let ((x 5) (1)) 6)",
		   "let: expected a key/value pair, but given something else");

    queueErrorTest("let given bad form (too many values)",
		   "(let ((x 2 3)) 6)",
		   "let: expected a key/value pair, but given something else");
    
    queueErrorTest("let given bad form",
		   "(let 5 6)",
		   "let: expected sequence of key value pairs, but given something else");
    
    queueErrorTest("let given bad form",
		   "(let ((x 2)) ())",
		   "( ): expected a function, but nothing's there");

    queueErrorTest("let given too many parts",
		   "(let () 4 4 5 5)",
		   "let: expected a single body, but found 3 extra parts");

    queueErrorTest("local given too many parts",
		   "(local () 4 4 5 5)",
		   "local: expected a single body, but found 3 extra parts");

    queueErrorTest("local given no args",
		   "(local)",
		   "local: expected at least one definition (in square brackets) after local, but nothing's there");
    
    queueErrorTest("local given bad form",
		   "(local x 3)",
		   "local: expected a collection of definitions, but given something else");
    
    queueErrorTest("local given bad form",
		   "(local (x))",
		   "local: expected a definition, but given something else");
    
    queueErrorTest("local given bad form",
		   "(local (x) 7)",
		   "local: expected a definition, but given something else");
    
    queueErrorTest("local given bad form",
		   "(local (x 1) 7)",
		   "local: expected a definition, but given something else");
    
    queueErrorTest("local given bad form",
		   "(local ((x 6)))",
		   "local: expected a definition, but given something else");
    
    queueErrorTest("local given bad form",
		   "(local ((x 6) (2)) 4)",
		   "local: expected a definition, but given something else");
    
    queueErrorTest("local given bad form",
		   "(local 1 5)",
		   "local: expected a collection of definitions, but given something else");
    
    queueErrorTest("local given bad form",
		   "(local ())",
		   "local: expected a single body, but found none");
    
    queueErrorTest("local given bad form",
		   "(local ((x 1)) ())",
		   "local: expected a definition, but given something else");

    queueErrorTest("lambda used as name",
		   "(define (f lambda) 2)",
		   "lambda: this is a reserved keyword and cannot be used as a variable or function name");

    queueErrorTest("let used as name",
		   "(define (f let) 2)",
		   "let: this is a reserved keyword and cannot be used as a variable or function name");


    queueErrorTest("cond used as name",
		   "(define (f cond) 2)",
		   "cond: this is a reserved keyword and cannot be used as a variable or function name");

    queueErrorTest("else used as name",
		   "(define (f else) 2)",
		   "else: this is a reserved keyword and cannot be used as a variable or function name");

    queueErrorTest("if used as name",
		   "(define (f if) 2)",
		   "if: this is a reserved keyword and cannot be used as a variable or function name");

    queueErrorTest("define used as name",
		   "(define (f define) 2)",
		   "define: this is a reserved keyword and cannot be used as a variable or function name");

    queueErrorTest("cond: empty case",
                   "(cond)",
                   "cond: expected at least one clause after cond, but nothing's there");

    queueErrorTest("cond: non-clause",
                   "(cond 1)",
                   "cond: expected a clause with a question and an answer, but found something else");

    queueErrorTest("cond: non-clause, with more pieces",
                   "(cond 1 2 3)",
                   "cond: expected a clause with a question and an answer, but found something else");

    queueErrorTest("cond: clause with not enough pieces",
                   "(cond [])",
                   "cond: expected a clause with a question and an answer, but found an empty part");

    queueErrorTest("cond: clause with not enough pieces",
                   "(cond [true])",
                   "cond: expected a clause with a question and an answer, but found a clause with only one part");

    queueErrorTest("cond: clause with too many pieces",
                   "(cond [true false true])",
                   "cond: expected a clause with a question and an answer, but found a clause with 3 parts");

    
    queueErrorTest("cond error properly uses the cond keyword",
                   "(cond (empty? 3) (add1 4))",
                   "cond: expected a boolean value, but found: #<function:empty?>");

    queueErrorTest("cond boolean test again",
                   "(cond (3 4))",
                   "cond: expected a boolean value, but found: 3")

    queueErrorTest("cond error properly uses the cond keyword: make sure it checks inside too",
                   "(cond [(cond (number? 3) (add1 4)) 5])",
                   "cond: expected a boolean value, but found: #<function:number?>");

    queueErrorTest("cond error properly uses the cond keyword: check multiple parts",
                   "(cond (empty? 3 'ok))",
                   "cond: expected a clause with a question and an answer, but found a clause with 3 parts");

    queueErrorTest("cond fallthrough",
                   "(cond ((even? 43) 'huh?))",
                   "cond: all question results were false");
    

    queueErrorTest("cond/and test",
                   "(cond [(and 3 4 5) 6] [else 7])",
                   "and: expected a boolean value, but found: 3");


    queueErrorTest("cond/and/empty test",
                   "(cond [(and empty?) 3] [else 4])",
                   "and: expected at least 2 arguments, but given 1");
    

    queueErrorTest("cond else clause must be last",
                   "(cond [else 'ok] [true 'huh?])",
                   "cond: found an else clause that isn't the last clause in its cond expression; there is another clause after it");

    queueErrorTest("cond else clause must be last and unique.",
                   "(cond [else 'ok] [true 'huh?])",
                   "cond: found an else clause that isn't the last clause in its cond expression; there is another clause after it");

    queueErrorTest("cond else clause must be last and unique, even if it shows up twice",
                   "(cond [else 'ok] [else 'huh?])",
                   "cond: found an else clause that isn't the last clause in its cond expression; there is another clause after it");

    queueErrorTest("cond else clause must be last and unique; make sure the test isn't fragile",
                   "(cond [else 'ok] 42 [else 'huh?])",
                   "cond: expected a clause with a question and an answer, but found something else");

    queueErrorTest("else is a keyword only usable in context",
                   "else",
                   "else: not allowed here, because this is not a question in a clause");                       

    queueErrorTest("else is a keyword only usable in context",
                   "(else)",
                   "else: not allowed here, because this is not a question in a clause");                       

    queueErrorTest("double definitions",
                   "(define-struct x (y)) (define x-y 43)",
                   "x-y: this name has a previous definition and cannot be re-defined");

    queueErrorTest("define-values too few",
                   "(define-values (x y z) (values 1))",
                   "define-values: expected 3 values, but only received one: 1");

    queueErrorTest("define-values too few",
                   "(define-values (x y z) (values 1 2 3 4))",
                   "define-values: expected 3 values, but received 4");


    // FIXME: define-values looks wrong.  Here's what the code does:
    queueErrorTest("define-values too many parts",
                   "(define-values (x) a b c)",
                   "define-values: expects a list of variables and a body, but found a part");

    queueErrorTest("define-values too many parts",
                   "(define-values (x) (a) b c)",
                   "define-values: expected 1 part, but found 1 part");

    queueErrorTest('define one extra part',
                   '(define (f x) 1 2)',
                   'define: expected only one expression for the function body, but found 1 extra part');

    queueErrorTest('define duplicate identifier',
                   '(define (f x x) x)',
                   'define: found a variable that is already used here');







    var afterReplSetup = function(theRepl) {
        repl = theRepl;
        runTests(function() { $("#is-running").text("Tests finished.  " + testsRunCount + " tests executed."); });
    };

    $("#failure-index").css("display", "none");
    $("#is-running").text("Running...");
    plt.runtime.makeRepl({ write: function(dom) { outputSpan.append(dom); } },
                         afterReplSetup);
})

