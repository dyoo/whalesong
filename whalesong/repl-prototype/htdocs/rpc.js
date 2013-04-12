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
                      'dataType': 'json',
                      'type' : 'post',
                      'statusCode': {
                          // On a 503, try again.
                          503: function() {
                              replCompile(name, code, onDone, onDoneError);
                          }
                      }
                    });
    };
    


    var moduleCompile = function(options, onDone, onDoneError) {
        jQuery.ajax({ 'url': url,
                      'cache': false,
                      'success': function(data, textStatus, jqXHR) { 
                          onDone(data); 
                      },
                      'error': function(jqXHR, textStatus, errorThrown) {
                          onDoneError(errorThrown); 
                      },
                      'data': {'name' : options.name,    // source
                               'mname' : options.mname,
                               'lang' : options.lang,
                               'src' : options.code,
                               'm' : 't'},
                      'dataType': 'json',
                      'type' : 'post',
                      'statusCode': {
                          // On 503, try again.
                          503: function() {
                              moduleCompile(options, onDone, onDoneError);
                          }
                      }
                    });
    };




    // If we're in the context of an iframe, provide an easyXDM
    // interface to the compiler.
    if (window.top !== window) {
        new easyXDM.Rpc({}, {
            local: {
                'replCompile': { method: replCompile },
                'moduleCompile': { method: moduleCompile }
            }
        });
    }
}());
