$(document).ready(function() {
    "use strict";    

    var repl = $("#repl");
    var output = $("#output");

    var M = plt.runtime.currentMachine;


    // We configure output to send it to the "output" DOM node.
    M.params.currentDisplayer = function(MACHINE, domNode) {
        $(domNode).appendTo(output);
    }
    M.params.currentErrorDisplayer = function(MACHINE, domNode) {
        $(domNode).appendTo(output);
    }


    // We then want to initialize the language module.
    var initializeLanguage = function(afterLanguageInitialization) {
        // Load up the language.
        M.modules['whalesong/lang/whalesong.rkt'].invoke(
            M,
            function() {
                console.log("Environment initialized.");
                afterLanguageInitialization();
            },
            function() {
                // Nothing should work if we can't get this to work.
                alert("uh oh!");
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
                    evaluate(src, 
                             function() { repl.removeAttr('disabled');
                                          repl.val("");});
                } 
            });
        });



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


    // In evaluation, we'll send compilation requests to the server,
    // and get back bytecode that we should evaluate.
    var evaluate = function(src, after) {
        console.log("about to eval", src);
        var onCompile = function(compiledResult) {
            // compiledResult.compiledCodes is an array of function chunks.
            var compiledCodes = compiledResult.compiledCodes;
            forEachK(compiledCodes,
                     function(code, k) {
                         var codeFunction = eval(code);
                         var onGoodEvaluation = function() {
                             console.log('good evaluation');
                             k();
                         };
                         var onBadEvaluation = function(M, err) {
                             console.log('bad evaluation');
                             console.log(err);
                             if (err.stack) {
                                 console.log(err.stack);
                             }
                             after();
                         };
                         codeFunction(M, onGoodEvaluation, onBadEvaluation);
                     },
                     after);
            //eval(compiledResult.compiled);
            // FIXME
            // plt.runtime.currentMachine.modules['whalesong/repl-prototype/anonymous-module.rkt'].invoke(
            //     plt.runtime.currentMachine,
            //     function() {
            //         after();
            //     },
            //     function() {
            //         after();
            //     });
        };
        var onError = function(err) {
            console.log("error", err);
            after();
        };

        $.ajax({dataType: 'json',
                url: '/compile',
                data: { src: src },
                success: onCompile,
                error: onError});
    };

});
