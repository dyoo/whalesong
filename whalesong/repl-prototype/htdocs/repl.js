$(document).ready(function() {
    "use strict";    

    // Hook up a simple one-line REPL with enter triggering evaluation.
    $("#repl").keypress(function(e) {
        if (e.which == 13) {
            var repl = $(this);
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
            after();
        };
        var onError = function(x) {
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
