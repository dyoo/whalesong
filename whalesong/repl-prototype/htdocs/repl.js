var COMPILED = [];

$(document).ready(function() {
    "use strict";    

    var repl = $("#repl");
    // Hook up a simple one-line REPL with enter triggering evaluation.
    $("#repl").keypress(function(e) {
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

    var evaluate = function(src, after) {
        console.log("about to eval", src);
        var onCompile = function(compiledResult) {
            console.log("compilation got", compiledResult);
            COMPILED.push(compiledResult);
            eval(compiledResult.compiled);
            // FIXME
            plt.runtime.currentMachine.modules['whalesong/repl-prototype/anonymous-module.rkt'].invoke();
            after();
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
