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
View.prototype.toString = function() { return "#<View>"; }

View.prototype.updateFocused = function(focused) {
    return new View(this.top, focused, eventHandlers, pendingActions);
};

View.prototype.initialRender = function(top) {
    top.empty();
    $(document.head).append(this.top.find("head").children());
    top.append(this.top.find("body").children());
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
    var dom, v;
    if (isView(x)) { 
        return onSuccess(x); 
    } else if (isResource(x)) {
        dom = $(resourceContent(x).toString())
            .css("margin", "0px")
            .css("padding", "0px")
            .css("border", "0px");
        dom.children("body").css("margin", "0px");
        return onSuccess(new View(dom,
                                  [],
                                  [],
                                  []));
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
InitialViewHandler.prototype.toString = function() { return "#<initial-view>"; };
var isInitialViewHandler = plt.baselib.makeClassPredicate(InitialViewHandler);



var StopWhenHandler = function(args, stopWhen) {
    WorldHandler.call(this, args);
    // stopWhen: Racket procedure (World -> boolean)
    this.stopWhen = stopWhen;
};

StopWhenHandler.prototype = plt.baselib.heir(WorldHandler.prototype);
StopWhenHandler.prototype.toString = function() { return "#<stop-when>"; };

var isStopWhenHandler = plt.baselib.makeClassPredicate(StopWhenHandler);



var OnTickHandler = function(args, onTick, delay) {
    WorldHandler.call(this, args);
    // stopWhen: Racket procedure (World -> boolean)
    this.onTick = onTick;
    this.delay = delay;
};

OnTickHandler.prototype = plt.baselib.heir(WorldHandler.prototype);
OnTickHandler.prototype.toString = function() { return "#<on-tick>"; };

var isOnTickHandler = plt.baselib.makeClassPredicate(OnTickHandler);






var findHandler = function(MACHINE, pred) {
    var i;
    for (i = 1; i < MACHINE.argcount; i++) {
        if (pred(MACHINE.env[MACHINE.env.length - 1 - i])) {
            return MACHINE.env[MACHINE.env.length - 1 - i];
        }
    }
    return undefined;
};


// var startHandlers = function(MACHINE, handlers, onEvent, success, fail) {
//     if (handlers.length === 0) {
//         success();
//     }
//     handlers[0].onStart(MACHINE, onEvent, 
//                         function(data) {
//                             startHandlers(MACHINE, handlers.slice(1), onEvent, success, fail);
//                         });
// }




//////////////////////////////////////////////////////////////////////


EXPORTS['big-bang'] = makeClosure(
    'big-bang',
    plt.baselib.arity.makeArityAtLeast(1),
    function(MACHINE) {
        var oldArgcount = MACHINE.argcount;
        var world = MACHINE.env[MACHINE.env.length - 1];
        var initialViewHandler = findHandler(MACHINE, isInitialViewHandler);
        var top = $("<div/>");
        MACHINE.params.currentDisplayer(MACHINE, top);
        PAUSE(function(restart) {

            initialViewHandler.view.initialRender(top);

            
            // Initialize event handlers to send to that channel.


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
                                                         { 
                                                             onStart : function(MACHINE, onEvent, k) {
                                                                 k();
                                                             },
                                                             onPause : function(MACHINE, data, k) { 
                                                                 k(data);
                                                             },
                                                             onResume : function(MACHINE, data, k) { 
                                                                 k(data);
                                                             },
                                                             onStop : function(MACHINE, data, k) { 
                                                                 k();
                                                             }
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
            { 
                onStart : function(MACHINE, onEvent, k) {
                    k()
                },
                onPause : function(MACHINE, data, k) { 
                    k(data) 
                },
                onResume : function(MACHINE, data, k) { 
                    k(data)
                },
                onStop : function(MACHINE, data, k) { 
                    k() 
                }
            },
            stopWhen
        );
        return undefined;
    });


EXPORTS['on-tick'] = makePrimitiveProcedure(
    'on-tick',
    1,
    function(MACHINE) {
        var onTick = checkProcedure(MACHINE, 'on-tick', 0);
        return new OnTickHandler(
            { 
                onStart : function(MACHINE, onEvent, k) { 
                    var id = setInterval(function () { onEvent(); },
                                         this.delay);
                    k( { id : id,
                         onEvent : onEvent }) ;
                },
                onPause : function(MACHINE, data, k) { 
                    clearInterval(data.id);
                    k(data)
                },
                onResume : function(MACHINE, data, k) {
                    data.id = setInterval(function () { data.onEvent() },
                                          this.delay);
                    k(data)
                },
                onStop : function(MACHINE, data, k) { 
                    clearInterval(data.id);
                    k() 
                }
            },
            onTick
        );
        return undefined;
    });






//////////////////////////////////////////////////////////////////////
