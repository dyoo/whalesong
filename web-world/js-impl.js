var makePrimitiveProcedure = plt.baselib.functions.makePrimitiveProcedure;
var makeClosure = plt.baselib.functions.makeClosure;
var finalizeClosureCall = plt.baselib.functions.finalizeClosureCall;
var PAUSE = plt.runtime.PAUSE;



EXPORTS['big-bang'] = makeClosure(
    'big-bang',
    plt.baselib.arity.makeArityAtLeast(1),
    function(MACHINE) {
        var oldArgcount = MACHINE.argcount;

        PAUSE(function(restart) {




            restart(function(MACHINE) {
                MACHINE.argcount = oldArgcount;
                finalizeClosureCall(MACHINE, "ok");
            })});
    });



EXPORTS['initial-view'] = makePrimitiveProcedure(
    'initial-view',
    1,
    function(MACHINE) {
        return undefined;
    });


EXPORTS['stop-when'] = makePrimitiveProcedure(
    'stop-when',
    1,
    function(MACHINE) {
        return undefined;
    });


//////////////////////////////////////////////////////////////////////
