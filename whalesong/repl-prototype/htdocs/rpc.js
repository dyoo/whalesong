(function () {
    "use strict";
    var url = "/compile";
    var replCompile = function(name, code, onDone, onDoneError) {
        jQuery.ajax({ 'url': url,
                      'cache': false,
                      'success': function(data, textStatus, jqXHR) { 
                          onDone(data); 
                      },
                      'error': function(jqXHR, textStatus, errorThrown) {
                          onDoneError(errorThrown); 
                      },
                      'data': {'name' : name,
                               'src' : code },
                      'dataType': 'json'
                    });
    };
    
    // If we're in the context of an iframe, provide an easyXDM
    // interface to the compiler.
    if (window.top !== window) {
        new easyXDM.Rpc({}, {
            local: {
                'replCompile': { method: replCompile }
            }
        });
    }
}());
