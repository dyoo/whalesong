// Lightweight linking of the modules.
// There are circular dependencies across the modules unfortunately, so we
// need a mechanism for letting them link to each other.
if (! this['plt']) { this['plt'] = {}; }
(function(scope) {
    var link = {};
    scope['link'] = link;


    // link.ready: (string (string -> void)) -> void
    // When the name announces that it's ready, calls the function f.
    link.ready = function(name, f) {
        readyWaiters[name] = readyWaiters[name] || [];
        readyWaiters[name].push(f);

        if (linkIsReady[name]) {
            notifySingle(f, name);
        }
    };

    // link.announceReady: string -> void
    // Lets the world know that the name is ready.
    link.announceReady = function(name) {
        var i;
        linkIsReady[name] = true;
        notifyAll(name);
    };
 


    // notifyAll: string -> void
    // Tell all listeners that the name is ready.
    var notifyAll = function(name) {
        var waiters = readyWaiters[name] || [], i;
        for (i = 0 ; i < waiters.length; i++) {
            notifySingle(waiters[i], name);
        }                
        readyWaiters[name] = [];
    };


    // Tell a single listener that the name is ready.
    var notifySingle = function(f, name) {
        setTimeout(function() { f(name); },
                   0);
    };


    // linkIsReady: (Hashtable String Boolean)
    var linkIsReady = {};   

    // readyWaiters: (Hashtable String (Arrayof (String -> Void)))
    var readyWaiters = {};


})(this['plt']);