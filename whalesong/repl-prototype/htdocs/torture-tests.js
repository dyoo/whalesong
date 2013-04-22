jQuery(document).ready(function() {
    "use strict";    
    var i = 0;
    // Torture test: keep rerunning the tests over and over.
    plt.tests.initTests(function(runTests) {
        var k = function() {
            i = i + 1;
            $("#is-running").text(
                "Pass " + i + ": " +
                    plt.tests.getTestsRunCount() + " tests executed."); 
            setTimeout(
                function() {
                    plt.tests.resetTests();
                    runTests(k);
                },
                0);
        };

        runTests(k);
    });
});

