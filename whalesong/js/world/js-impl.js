
(function() {
    "use strict";

    var WebWorld = 
        MACHINE.modules['whalesong/web-world/impl.rkt'].privateExports;
    var EventSource = WebWorld.EventSource;
    var EventHandler = WebWorld.EventHandler;
    var wrapFunction = WebWorld.wrapFunction;

    var makeClosure = plt.baselib.functions.makeClosure;
    var makePrimitiveProcedure = plt.baselib.functions.makePrimitiveProcedure;
    var finalizeClosureCall = plt.runtime.finalizeClosureCall;

    var checkProcedure = plt.baselib.check.checkProcedure;



    /**
     * Creates an event source coupled to a JavaScript function.  Calling the function
     * should cause the event source to fire.
     */
    var makeJsEventSource = function(setupProcedure, shutdownProcedure) {
        var enabled = false;
        var fireEvent;

        var sender = function() {
            var args;
            if (enabled) {
                args = Array.prototype.slice.call(arguments, 0);
                args.unshift(void(0));
                fireEvent.apply(void(0), args);
            }
        };

        var JsEventSource = function() {
            this.startupData = void(0);
        };
        JsEventSource.prototype = plt.baselib.heir(EventSource.prototype);
        JsEventSource.prototype.onStart = function(_fireEvent, internalCall, k) {
            var that = this;
            that.internalCall = internalCall;
            setupProcedure(internalCall,
                           sender, 
                           function(v) {
                               that.startupData = v;
                               enabled = true;
                               fireEvent = _fireEvent;
                               k();
                           },
                           function(err) {
                           });
        };
        JsEventSource.prototype.onStop = function(k) {
            shutdownProcedure(this.internalCall,
                              this.startupData,
                              function() {
                                  enabled = false;
                                  fireEvent = void(0);
                                  k();
                              },
                              function(err) {
                                  enabled = false;
                                  fireEvent = void(0);
                                  k();
                              });
        };

        return new JsEventSource();
    };


    var makeWorldEventHandler = makeClosure(
        'make-world-event-handler',
        2,
        function(M) {
            var setupProcedure = wrapFunction(checkProcedure(M, 'make-world-event-handler', 0));
            var shutdownProcedure = wrapFunction(checkProcedure(M, 'make-world-event-handler', 1));
            var eventSource = makeJsEventSource(setupProcedure, shutdownProcedure);
            var makeHandler = makePrimitiveProcedure(
                'world-event-handler',
                1,
                function(M) {
                    var onEvent = wrapFunction(checkProcedure(M, 'world-event-handler', 0));
                    return new EventHandler('world-event-handler', eventSource, onEvent);
                });
            finalizeClosureCall(M, makeHandler);
        });


    EXPORTS['make-world-event-handler'] = makeWorldEventHandler;

}());