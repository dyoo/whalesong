$(document).ready(function() {
    "use strict";    
    if (! console.log) { console.log = function() { }; }

    var repl = $("#repl");
    var output = $("#output");
    var breakButton = $("#break");
    var resetButton = $("#reset");
    breakButton.hide();
    breakButton.click(function() { interruptEvaluation(); });
    resetButton.click(function() { output.empty(); setupMachine(); });


    var M;

    var sendOutputToBottom = function() {
        output.get(0).scrollTop = output.get(0).scrollHeight;
    };

    
    var setupMachine = function() { 
        M = plt.runtime.currentMachine;
        M.reset();
        // We configure output to send it to the "output" DOM node.
        M.params.currentDisplayer = function(MACHINE, domNode) {
            $(domNode).appendTo(output);
            sendOutputToBottom();
        };
        M.params.currentErrorDisplayer = function(MACHINE, domNode) {
            $(domNode).css("color", "red").appendTo(output);
            sendOutputToBottom();
        };


        // We then want to initialize the language module.
        var initializeLanguage = function(afterLanguageInitialization) {
            // Load up the language.
            M.modules['whalesong/wescheme/lang/semantics.rkt'] =
                M.installedModules['whalesong/wescheme/lang/semantics.rkt']();
            
            var semanticsModule =
                M.modules['whalesong/wescheme/lang/semantics.rkt'];
            semanticsModule.invoke(
                M,
                function() {
                    M.params.currentNamespace = semanticsModule.getNamespace();
                    afterLanguageInitialization();
                },
                function(M, err) {
                    // Nothing should work if we can't get this to work.
                    console.log(M);
                    console.log(err);
                    console.log(err.stack);
                    alert("uh oh!: language could not be loaded.");
                });
        };
        repl.attr('disabled', 'true');
        repl.val('Please wait, initializing...');
        initializeLanguage(
            function() {
                repl.val('');
                repl.removeAttr('disabled');
                // Hook up a simple one-line REPL with enter triggering evaluation.
                repl.keypress(function(e) {
                    if (e.which == 13 && !repl.attr('disabled')) {
                        var src = repl.val();
                        $(this).val("");
                        repl.attr('disabled', 'true');
                        repl.val("... evaluating...");
                        breakButton.show();
                        compileAndEvaluate(src, 
                                           function() { repl.removeAttr('disabled');
                                                        repl.val("");
                                                        breakButton.hide();});
                    } 
                });
            });
    };


    setupMachine();


    // CPS'ed for-each.
    var forEachK = function(elts, f, after) {
        var n = elts.length;
        var loop = function(i) {
            if (i >= n) {
                return after();
            } else {
                return f(elts[i], function() { loop(i+1); });
            }
        }
        loop(0);
    };


    // writeErrorMessage: string -> void
    // Write out an error message.
    var writeErrorMessage = function(msg) {
        $("<span/>")
            .text(''+msg)
            .css("color", "red")
            .appendTo(output);
        $("<br/>").appendTo(output);
        sendOutputToBottom();
    };



    // Print: Racket value -> void
    // Prints the racket value out.
    var print = function(elt) {
	var outputPort =
	    M.params.currentOutputPort;
	if (elt !== plt.runtime.VOID) {
	    outputPort.writeDomNode(
                M,
                plt.runtime.toDomNode(elt, M.params['print-mode']));
	    outputPort.writeDomNode(M, plt.runtime.toDomNode("\n", 'display'));
	}
    };

    var interruptEvaluation = function() {
        console.log('scheduling an interruption');
        M.scheduleBreak();
    };


    // In evaluation, we'll send compilation requests to the server,
    // and get back bytecode that we should evaluate.
    var compileAndEvaluate = function(src, after) {
        $("<tt/>").text('> ' + src).appendTo(output);
        $("<br/>").appendTo(output);
        var onCompile = function(compiledResult) {
            if (compiledResult.type === 'repl') {
                return onGoodReplCompile(compiledResult);
            } else if (compiledResult.type === 'module') {
                alert('internal error: module unexpected');
                after();
            } else if (compiledResult.type === 'error') {
                return onCompileTimeError(compiledResult);
            }
        };


        var onCompileTimeError = function(compiledResult) {
            writeErrorMessage(compiledResult.message);
            after();
        };


        var onGoodReplCompile = function(compiledResult) {
            // compiledResult.compiledCodes is an array of function chunks.
            // The evaluation leaves the value register of the machine
            // to contain the list of values from toplevel evaluation.
            var compiledCodes = compiledResult.compiledCodes;
            forEachK(compiledCodes,
                     function(code, k) {
                         var codeFunction = (0,eval)(code);
                         var onGoodEvaluation = function() {
                             var resultList = M.v;
                             while(resultList !== plt.baselib.lists.EMPTY) {
                                 print(resultList.first);
                                 resultList = resultList.rest;
                             };
                             k();
                         };
                         var onBadEvaluation = function(M, err) {
                             console.log(err);
                             if (err.stack) {
                                 console.log(err.stack);
                             }
                             if (err.message) { 
                                 writeErrorMessage(err.message);
                             }

                             after();
                         };
                         codeFunction(M, onGoodEvaluation, onBadEvaluation);
                     },
                     after);
        };
        var onCompileError = function(err) {
        };

        var onServerError = function(err) {
            writeErrorMessage("internal server error");
            after();
        };

        $.ajax({dataType: 'json',
                url: '/compile',
                data: { src: src },
                success: onCompile,
                error: onServerError});
    };


    // Things that we need to make as automated tests:
    //
    // Make sure: (let () (define (f x) (f x)) (f 42))
    // is interruptable.
    //
    // Test: simple expressions, functions, etc.
    //
    // Test: multiple value return, even zero
    //
    // Test: require image library, try drawing a few things.
    //
    // Test: compile a module.
    //


});
