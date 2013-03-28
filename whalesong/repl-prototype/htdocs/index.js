jQuery(document).ready(function() {
    "use strict";    

    var prompt = jQuery("#prompt");
    var output = jQuery("#output");
    var breakButton = jQuery("#break");
    var resetButton = jQuery("#reset");

    var write = function(dom) {
        output.append(dom);
        output.get(0).scrollTop = output.get(0).scrollHeight;
    };


    var onBreak = function() { 
        repl.requestBreak(allowInput);
    };

    
    var allowInput = function() {
        prompt.val('');
        prompt.removeAttr('disabled');
        breakButton.hide();
    };


    var onReset = function() { 
        repl.reset(allowInput);
    };      
       

    var onExpressionEntered = function() {
        var src = prompt.val();
        jQuery(this).val("");
        prompt.attr('disabled', 'true');
        prompt.val("... evaluating...");
        breakButton.show();
        repl.compileAndExecuteProgram('interactions',
                                      src, 
                                      allowInput,
                                      onError);
    };


    var onError = function(err) {
        console.log(err);
        if (err.message) {
            write(jQuery('<span/>').css('color', 'red').append(err.message));
            write(jQuery('<br/>'));
        }
        allowInput();
    };
    

    breakButton.hide();
    breakButton.click(onBreak);
    resetButton.click(onReset);
    prompt.attr('disabled', 'true');
    prompt.val('Please wait, initializing...');
    prompt.keypress(function(e) {
        if (e.which == 13 && !prompt.attr('disabled')) { 
            onExpressionEntered();
        }});
    var afterReplSetup = function() {
        prompt.val('');
        prompt.removeAttr('disabled');
    };
    var repl = new plt.runtime.Repl({ write: write }, afterReplSetup);
});
