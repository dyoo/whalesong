EXPORTS['alert'] =
    RUNTIME.makePrimitiveProcedure(
        'alert',
        1,
        function(MACHINE) {
            var elt = MACHINE.env[MACHINE.env.length - 1];
            alert(String(elt));
            return RUNTIME.VOID;
        });


EXPORTS['body'] = $(document.body);

EXPORTS['$'] =
    RUNTIME.makePrimitiveProcedure(
        '$',
        1,
        function(MACHINE) {
            var obj = MACHINE.env[MACHINE.env.length - 1];
            return $(obj);
        });

EXPORTS['call-method'] = 
    RUNTIME.makePrimitiveProcedure(
        'call-method',
        plt.baselib.arity.makeArityAtLeast(2),
        function(MACHINE) {
            var obj = MACHINE.env[MACHINE.env.length - 1];
            var methodName = MACHINE.env[MACHINE.env.length - 2];
            var args = [];
            for (var i = 0; i < MACHINE.argcount - 2; i++) {
                args.push(MACHINE.env[MACHINE.env.length -1 - 2 - i]);
            }
            var result = obj[methodName].apply(obj, args);
            return result;
        });




// Javascript-specific extensions.  A small experiment.
EXPORTS['viewport-width'] = 
    RUNTIME.makePrimitiveProcedure(
        'viewport-width',
        0,
        function(MACHINE) {
            return $(window).width();
        });

EXPORTS['viewport-height'] = 
    RUNTIME.makePrimitiveProcedure(
        'viewport-height',
        0,
        function(MACHINE) {
            return $(window).height();
        });


EXPORTS['in-javascript-context?'] =
    RUNTIME.makePrimitiveProcedure(
        'in-javascript-context?',
        0,
        function(MACHINE) {
            return true;
        });
