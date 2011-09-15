var VOID = plt.baselib.constants.VOID_VALUE;
var makePrimitiveProcedure = plt.baselib.functions.makePrimitiveProcedure;

EXPORTS['alert'] =
    makePrimitiveProcedure(
        'alert',
        1,
        function(MACHINE) {
            var elt = MACHINE.e[MACHINE.e.length - 1];
            alert(String(elt));
            return VOID;
        });


EXPORTS['body'] = $(document.body);

EXPORTS['$'] =
    makePrimitiveProcedure(
        '$',
        1,
        function(MACHINE) {
            var obj = MACHINE.e[MACHINE.e.length - 1];
            return $(obj);
        });

EXPORTS['call-method'] = 
    makePrimitiveProcedure(
        'call-method',
        plt.baselib.arity.makeArityAtLeast(2),
        function(MACHINE) {
            var obj = MACHINE.e[MACHINE.e.length - 1];
            var methodName = MACHINE.e[MACHINE.e.length - 2];
            var args = [];
            for (var i = 0; i < MACHINE.a - 2; i++) {
                args.push(MACHINE.e[MACHINE.e.length -1 - 2 - i]);
            }
            var result = obj[methodName].apply(obj, args);
            return result;
        });




// Javascript-specific extensions.  A small experiment.
EXPORTS['viewport-width'] = 
    makePrimitiveProcedure(
        'viewport-width',
        0,
        function(MACHINE) {
            return $(window).width();
        });

EXPORTS['viewport-height'] = 
    makePrimitiveProcedure(
        'viewport-height',
        0,
        function(MACHINE) {
            return $(window).height();
        });


EXPORTS['in-javascript-context?'] =
    makePrimitiveProcedure(
        'in-javascript-context?',
        0,
        function(MACHINE) {
            return true;
        });
