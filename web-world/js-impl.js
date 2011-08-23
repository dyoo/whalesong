/*jslint browser: true, unparam: true, vars: true, white: true, plusplus: true, maxerr: 50, indent: 4 */
/*global plt,MACHINE,$,EXPORTS*/
(function() {

    "use strict";

    var makePrimitiveProcedure = plt.baselib.functions.makePrimitiveProcedure;
    var makeClosure = plt.baselib.functions.makeClosure;
    var finalizeClosureCall = plt.baselib.functions.finalizeClosureCall;
    var PAUSE = plt.runtime.PAUSE;



    var resourceStructType = 
        MACHINE.modules['whalesong/resource/structs.rkt'].namespace['struct:resource'];



    // A View represents a functional representation of the DOM tree.
    var View = function(top, focused, eventSources, pendingActions) {
        // top: dom node
        this.top = top;
        this.focused = focused;
        this.eventSources = eventSources;
        this.pendingActions = pendingActions;
    };

    View.prototype.toString = function() { return "#<View>"; };

    View.prototype.updateFocused = function(focused) {
        return new View(this.top, focused, this.eventSources, this.pendingActions);
    };

    View.prototype.initialRender = function(top) {
        top.empty();
        $(document.head).append(this.top.find("head").children());

        // FIXME: we should pull in the styles applied to body and its
        // children and apply them here?
        if (this.top.find("body").length > 0) {
            top.append(this.top.find("body").children());
        } else {
            top.append(this.top);
        }
    };

    // Return a list of the event sources from the view.
    // fixme: may need to apply the pending actions to get the real set.
    View.prototype.getEventSources = function() {
        return this.eventSources;
    };
    



    var isView = plt.baselib.makeClassPredicate(View);
    var isResource = resourceStructType.predicate;


    // var resourcePath = function(r) { return resourceStructType.accessor(r, 0); };
    // var resourceKey = function(r) { return resourceStructType.accessor(r, 1); };
    var resourceContent = function(r) { return resourceStructType.accessor(r, 2); };





 
   // coerseToView: (U resource View) -> View
    // Coerse a value into a view.
    var coerseToView = function(x, onSuccess, onFail) {
        var dom;
        if (isView(x)) { 
            return onSuccess(x); 
        } else  if (isResource(x)) {
            try {
                dom = $(resourceContent(x).toString())
                    .css("margin", "0px")
                    .css("padding", "0px")
                    .css("border", "0px");
                dom.children("body").css("margin", "0px");
            } catch (exn) {
                return onFail(exn);
            }
            return onSuccess(new View(dom,
                                      [],
                                      [],
                                      []));
        } else {
            try {
                dom = $(plt.baselib.format.toDomNode(x))
            } catch (exn) {
                return onFail(exn);
            }
            return onSuccess(new View(dom,
                                      [],
                                      [],
                                      []));
        }
    };




    //////////////////////////////////////////////////////////////////////
    //
    // The inputs into a big bang are WorldHandlers, which configure the big
    // bang in terms of the initial view, the inputs, event sources, etc.
    //

    var WorldHandler = function() {};
    var isWorldHandler = plt.baselib.makeClassPredicate(WorldHandler);


    var InitialViewHandler = function(view) {
        WorldHandler.call(this);
        // view: View
        this.view = view;
    };

    InitialViewHandler.prototype = plt.baselib.heir(WorldHandler.prototype);
    InitialViewHandler.prototype.toString = function() { return "#<initial-view>"; };
    var isInitialViewHandler = plt.baselib.makeClassPredicate(InitialViewHandler);


    var StopWhenHandler = function(stopWhen) {
        WorldHandler.call(this);
        // stopWhen: Racket procedure (World -> boolean)
        this.stopWhen = stopWhen;
    };

    StopWhenHandler.prototype = plt.baselib.heir(WorldHandler.prototype);
    StopWhenHandler.prototype.toString = function() { return "#<stop-when>"; };
    var isStopWhenHandler = plt.baselib.makeClassPredicate(StopWhenHandler);




    var EventHandler = function(name, eventSource, racketWorldCallback) {
        WorldHandler.call(this);
        this.name = name;
        this.eventSource = eventSource;
        this.racketWorldCallback = racketWorldCallback
    };
    EventHandler.prototype = plt.baselib.heir(WorldHandler.prototype);
    EventHandler.prototype.toString = function() { return "#<" + this.name + ">"; };
    var isEventHandler = plt.baselib.makeClassPredicate(EventHandler);

    //////////////////////////////////////////////////////////////////////


    var find = function(handlers, pred) {
        var i;
        for (i = 0; i < handlers.length; i++) {
            if (pred(handlers[i])) {
                return handlers[i];
            }
        }
        return undefined;
    };





    /* Event sources.
       
       An event source are the inputs to a web world program.


       Pause and Unpause are semantically meant to be cheaper than start, stop, so
       that's why they're a part of this API.
    */

    var EventSource = function() {};
    EventSource.prototype.onStart = function(fireEvent) {
    };

    EventSource.prototype.onStop = function() {
    };

    // The default behavior of pause is to cause the event source to stop.
    EventSource.prototype.onPause = function() {
        this.onStop();
    };

    // The default behavior of unpause is to start an event source up again.
    EventSource.prototype.onUnpause = function(fireEvent) {
        this.onStart(fireEvent);
    };


    
    // Clock ticks.
    var TickEventSource = function(delay) {
        this.delay = delay; // delay in milliseconds.

        this.id = undefined;
        // either undefined, or an integer representing the
        // id to cancel a timeout.
    };

    TickEventSource.prototype = plt.baselib.heir(EventSource.prototype);

    TickEventSource.prototype.onStart = function(fireEvent) {
        this.id = setInterval(
            function() {
                fireEvent();
            },
            this.delay);
    };

    TickEventSource.prototype.onStop = function() {
        if (this.id !== undefined) {
            clearInterval(this.id);
            this.id = undefined;
        }
    };



    var BindEventSource = function(type, element) {
        this.type = type;
        this.element = element;
        this.handler = undefined;
    };

    BindEventSource.prototype = plt.baselib.heir(EventSource.prototype);

    BindEventSource.prototype.onStart = function(fireEvent) {
        this.handler = 
            function(evt) {
                fireEvent(evt);
            };
        $(this.element).bind(this.type,
                             this.handler);
    };

    BindEventSource.prototype.onStop = function() {
        if (this.handler !== undefined) {
            $(this.element).unbind(this.type, this.handler);
            this.handler = undefined;
        }
    };





    // bigBang.
    var bigBang = function(MACHINE, world, handlers) {
        var oldArgcount = MACHINE.argcount;
        var worldSetter = function(v) { world = v; };
        var worldGetter = function(v) { return world; };

        var view = find(handlers, isInitialViewHandler).view;
        var stopWhen = find(handlers, isInitialViewHandler).stopWhen;

        var top = $("<div/>");
        MACHINE.params.currentDisplayer(MACHINE, top);



        PAUSE(function(restart) {
            var onRestart = function() {
                restart(function(MACHINE) {
                    MACHINE.argcount = oldArgcount;
                    finalizeClosureCall(MACHINE, "ok");
                });
            };


            view.initialRender(top);

            // fixme: set up the event sources
            // fixme: set up the world updater
            // fixme: re-render the view on world changes.

            
            // Initialize event handlers to send to that channel.

            

        });
    };



    //////////////////////////////////////////////////////////////////////


    var checkProcedure = plt.baselib.check.checkProcedure;

    var checkResourceOrView = plt.baselib.check.makeCheckArgumentType(
        function(x) { return isView(x) || isResource(x); },
        'resource or view');

    var checkWorldHandler = plt.baselib.check.makeCheckArgumentType(
        isWorldHandler,
        'world handler');

    var checkView = plt.baselib.check.makeCheckArgumentType(
        isView, 'view');


    EXPORTS['big-bang'] = makeClosure(
        'big-bang',
        plt.baselib.arity.makeArityAtLeast(1),
        function(MACHINE) {
            var world = MACHINE.env[MACHINE.env.length - 1];
            var handlers = [];
            var i;
            for (i = 1; i < MACHINE.argcount; i++) {
                handlers.push(checkWorldHandler(MACHINE, 'big-bang', i));
            }
            return bigBang(MACHINE, world, handlers);
        });


    EXPORTS['initial-view'] = makeClosure(
        'initial-view',
        1,
        function(MACHINE) {
            var viewable = MACHINE.env[MACHINE.env.length - 1];
            var oldArgcount = MACHINE.argcount;
            PAUSE(function(restart) {
                coerseToView(viewable,
                             function(v) {
                                 restart(function(MACHINE) {
                                     MACHINE.argcount = oldArgcount;
                                     finalizeClosureCall(MACHINE,
                                                         new InitialViewHandler(v));
                                 });
                             },
                             function(exn) {
                                 restart(function(MACHINE) {
                                     plt.baselib.exceptions.raise(
                                         MACHINE, 
                                         new Error(plt.baselib.format.format(
                                             "unable to translate ~s to view: ~a",
                                             [viewable, exn.message])));
                                 });
                             });
            });
        });


    EXPORTS['stop-when'] = makePrimitiveProcedure(
        'stop-when',
        1,
        function(MACHINE) {
            var stopWhen = checkProcedure(MACHINE, 'stop-when', 0);
            return new StopWhenHandler(stopWhen);
        });


    EXPORTS['on-tick'] = makePrimitiveProcedure(
        'on-tick',
        plt.baselib.lists.makeList(1, 2),
        function(MACHINE) {
            var onTick = checkProcedure(MACHINE, 'on-tick', 0);
            var delay = Math.floor(1000/28);
            if (MACHINE.argcount === 2) {
                delay = plt.baselib.numbers.toFixnum(checkReal(MACHINE, 'on-tick', 1));
            }
            return new EventHandler('on-tick', 
                                    new TickEventSource(delay), 
                                    onTick);
        });






    //////////////////////////////////////////////////////////////////////
}());