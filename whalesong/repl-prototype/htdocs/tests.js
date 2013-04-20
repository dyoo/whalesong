jQuery(document).ready(function() {
    "use strict";    
    
    plt.tests.initTests(function(runTests) {
        runTests(function() { $("#is-running").text(
            "Tests finished.  " +
                plt.tests.getTestsRunCount() + " tests executed."); });
    });
});

