
(function() {
    "use strict";

    var WebWorld = 
        MACHINE.modules['whalesong/web-world/impl.rkt'].privateExports;
    var EventSource = WebWorld.EventSource;
    var EventHandler = WebWorld.EventHandler;

    var makeClosure = plt.baselib.functions.makeClosure;
    var makePrimitive = plt.baselib.functions.makePrimitive;
    var finalizeClosureCall = plt.runtime.finalizeClosureCall;

    var checkProcedure = plt.baselib.check.checkProcedure;



    /**
     * Creates an event source coupled to a JavaScript function.  Calling the function
     * should cause the event source to fire.
     */
    var makeJsEventSource = function() {
        var enabled = false;
        var fireEvent;

        var JsEventSource = function() {};
        JsEventSource.prototype = plt.baselib.heir(EventSource.prototype);
        JsEventSource.prototype.onStart = function(_fireEvent) {
            enabled = true;
            fireEvent = _fireEvent;
        };
        JsEventSource.prototype.onStop = function() {
            enabled = false;
            fireEvent = void(0);
        };

        var sender = function() {
            if (enabled) {
                var args = Array.prototype.slice.call(arguments, 0);
                args.unshift(void(0));
                fireEvent.apply(null, args);
            }
        };
        return { eventSource: new JsEventSource(),
                 sender: sender };
    };


    var makeJsWorldEvent = makeClosure(
        'make-js-world-event',
        0,
        function(M) {
            var eventSourceRecord = makeJsEventSource();
            eventSourceRecord.eventSource
            var makeHandler = makePrimitive('make-js-world-event',
                                            1,
                                            function(M) {
                                                var onEvent = checkProcedure(M, 'js-world-event-handler', 0);
                                                return new EventHandler('js-world-event',
                                                                        eventSourceRecord.eventSource,
                                                                        onEvent);
                                            });
            finalizeClosureCall(M,
                                "first value", 
                                eventSourceRecord.sender);
        });


    EXPORTS['make-js-world-event'] = makeJsWorldEvent;

}());