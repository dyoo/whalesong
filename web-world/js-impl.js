var makePrimitiveProcedure = plt.baselib.functions.makePrimitiveProcedure;
var makeClosure = plt.baselib.functions.makeClosure;
var finalizeClosureCall = plt.baselib.functions.finalizeClosureCall;
var PAUSE = plt.runtime.PAUSE;



var resourceStructType = 
    MACHINE.modules['whalesong/resource/structs.rkt'].namespace['struct:resource'];



// A View represents a functional representation of the DOM tree.
var View = function(top, focused, eventHandlers, pendingActions) {
    // top: dom node
    this.top = top;
    this.focused = focused;
    this.eventHandlers = eventHandlers;
    this.pendingActions = pendingActions;
};

View.prototype.updateFocused = function(focused) {
    return new View(this.top, focused, eventHandlers, pendingActions);
};


var isView = plt.baselib.makeClassPredicate(View);
var isResource = resourceStructType.predicate;


var resourcePath = function(r) { return resourceStructType.accessor(r, 0); };
var resourceKey = function(r) { return resourceStructType.accessor(r, 1); };
var resourceContent = function(r) { return resourceStructType.accessor(r, 2); };


var checkResource = plt.baselib.check.makeCheckArgumentType(
    isResource, 'resource');
var checkResourceOrView = plt.baselib.check.makeCheckArgumentType(
    function(x) { return isView(x) || isResource(x); },
    'resource or view');
var checkView = plt.baselib.check.makeCheckArgumentType(
    isView, 'view');



// coerseToView: (U resource View) -> View
var coerseToView = function(x, onSuccess, onFail) {
    if (isView(x)) { 
        return onSuccess(x); 
    } else if (isResource(x)) {
        console.log(resourcePath(x), resourceKey(x), resourceContent(x).toString());
        $.ajax({
            url: "res/" + resourceKey(x),
            dataType : "html",
            success: function(data, textStatus, jqXHR) {
                console.log("data is: ", data);
                return onSuccess(new View(data, [], [], []));
            },
            error: function(jqXHR, textStatus, errorThrown) {
                return onFail(new Error(errorThrown));
            }});
    } else {
        return onFail(new Error("Unable to coerse to view"));
    }
};




var WorldHandler = function(args) {
    this.args = args;
};


var InitialViewHandler = function(args, view) {
    WorldHandler.call(this, args);
    // view: View
    this.view = view;
};

InitialViewHandler.prototype = plt.baselib.heir(WorldHandler.prototype);



var StopWhenHandler = function(args, stopWhen) {
    WorldHandler.call(this, args);
    // stopWhen: Racket procedure (World -> boolean)
    this.stopWhen = stopWhen;
};

StopWhenHandler.prototype = plt.baselib.heir(WorldHandler.prototype);







//////////////////////////////////////////////////////////////////////


EXPORTS['big-bang'] = makeClosure(
    'big-bang',
    plt.baselib.arity.makeArityAtLeast(1),
    function(MACHINE) {
        var oldArgcount = MACHINE.argcount;

        PAUSE(function(restart) {

            // FILL ME IN

            var onRestart = function() {
                restart(function(MACHINE) {
                    MACHINE.argcount = oldArgcount;
                    finalizeClosureCall(MACHINE, "ok");
                });
            };
        });
    });


EXPORTS['initial-view'] = makeClosure(
    'initial-view',
    1,
    function(MACHINE) {
        var resourceOrView = checkResourceOrView(MACHINE, 'initial-view', 0);;
        var oldArgcount = MACHINE.argcount;
        PAUSE(function(restart) {
            coerseToView(resourceOrView,
                         function(v) {
                             restart(function(MACHINE) {
                                 finalizeClosureCall(MACHINE,
                                                     new InitialViewHandler(
                                                         { onStart : function(MACHINE, k) {k()},
                                                           onPause : function(MACHINE, k) {k()},
                                                           onResume : function(MACHINE, k) {k()},
                                                           onStop : function(MACHINE, k) {k()}
                                                         },
                                                         v));
                             });
                         },
                         function(exn) {
                            plt.baselib.exceptions.raise(
                                MACHINE, 
                                new Error(plt.baselib.format.format(
                                    "unable to translate resource to view: ~a",
                                    [exn.message])));
                         });
        });
    });



EXPORTS['stop-when'] = makePrimitiveProcedure(
    'stop-when',
    1,
    function(MACHINE) {
        var stopWhen = checkProcedure(MACHINE, 'stop-when', 0);
        return new StopWhenHandler(
            { onStart : function(MACHINE, k) {k()},
              onPause : function(MACHINE, k) {k()},
              onResume : function(MACHINE, k) {k()},
              onStop : function(MACHINE, k) {k()}
            },
            stopWhen
        );
        return undefined;
    });





//////////////////////////////////////////////////////////////////////
