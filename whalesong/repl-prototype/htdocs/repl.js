"use strict";

$(document).ready(function() {
    
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

        // fill me in.
        setTimeout(after, 1000);
    };

});
