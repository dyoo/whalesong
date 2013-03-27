jQuery(document).ready(function() {
    "use strict";    

    // if (! console.log) { console.log = function() { }; }

    var repl = jQuery("#repl");
    var output = jQuery("#output");
    var breakButton = jQuery("#break");
    var resetButton = jQuery("#reset");


    // The machine.
    var M;

    var interactionsCount = 0;

    var sendOutputToBottom = function() {
        output.get(0).scrollTop = output.get(0).scrollHeight;
    };


    var xhr = new easyXDM.Rpc(
        { remote: 'rpc.html' },
        { remote: { replCompile: {} } });


    var onBreak = function() {
        if (M.running) { 
            interruptEvaluation(function(){}); 
        } 
    };

    var onReset = function() {
        if (M.running) {
            M.params.currentDisplayer = 
                function(MACHINE, domNode) {};
            M.params.currentErrorDisplayer =
                function(MACHINE, domNode) {};
            interruptEvaluation(
                function() {
                    output.empty(); 
                    setupMachine();
                });
        } else {
            output.empty(); 
            setupMachine();
        }
    };

    
    var setupMachine = function() { 
        M = plt.runtime.currentMachine;
        M.reset();
        // We configure output to send it to the "output" DOM node.
        M.params.currentDisplayer = function(MACHINE, domNode) {
            jQuery(domNode).appendTo(output);
            sendOutputToBottom();
        };
        M.params.currentErrorDisplayer = function(MACHINE, domNode) {
            jQuery(domNode).css("color", "red").appendTo(output);
            sendOutputToBottom();
        };


        // We then want to initialize the language module.
        var initializeLanguage = function(afterLanguageInitialization) {
            // Load up the language.
            M.loadAndInvoke('whalesong/wescheme/lang/semantics.rkt',
                            function() {
                                var semanticsModule =
                                    M.modules['whalesong/wescheme/lang/semantics.rkt'];
                                M.params.currentNamespace = semanticsModule.getExports();
                                afterLanguageInitialization();
                            },
                            function(err) {
                                // Nothing should work if we can't get this to work.
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
                        jQuery(this).val("");
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
        M.params.currentErrorDisplayer(M,
                                  jQuery("<span/>")
                                  .text(''+msg)
                                  .css("color", "red"));
        M.params.currentErrorDisplayer(M, jQuery("<br/>"));
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

    var interruptEvaluation = function(afterBreak) {
        if (! M.running) {
            throw new Error("internal error: trying to interrupt evaluation but nothing is running.");
        }
        M.scheduleBreak(afterBreak);
    };


    // In evaluation, we'll send compilation requests to the server,
    // and get back bytecode that we should evaluate.
    var compileAndEvaluate = function(src, after) {
        M.params.currentDisplayer(M, jQuery("<tt/>").text('> ' + src));
        M.params.currentDisplayer(M, jQuery("<br/>"));
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
                         // Indirect eval usage here is deliberate.
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
                             if (err.message) { 
                                 writeErrorMessage(err.message);
                             }

                             after();
                         };
                         codeFunction(M, onGoodEvaluation, onBadEvaluation);
                     },
                     after);
        };
        var onServerError = function(err) {
            writeErrorMessage("internal server error");
            after();
        };
        xhr.replCompile("<interactions" + interactionsCount + ">",
                        src, onCompile, onServerError);
        interactionsCount = interactionsCount + 1;
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

    breakButton.hide();
    breakButton.click(onBreak);
    resetButton.click(onReset);
    setupMachine();
});
