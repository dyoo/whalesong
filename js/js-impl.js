EXPORTS['alert'] =
    RUNTIME.makePrimitiveProcedure(
        'is-color?',
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

EXPORTS['call'] = 
    RUNTIME.makePrimitiveProcedure(
        'call',
        new RUNTIME.ArityAtLeast(2),
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
