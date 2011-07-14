var PAUSE = plt.runtime.PAUSE;
var makeClosure = plt.baselib.functions.makeClosure;
var makeRational = plt.baselib.numbers.makeRational;
var finalizeClosureCall = plt.baselib.functions.finalizeClosureCall;
var makePrimitiveProcedure = plt.baselib.functions.makePrimitiveProcedure;



var checkProcedure = plt.baselib.check.checkProcedure;
var checkNonNegativeReal = plt.baselib.check.checkNonNegativeReal;


// The default tick delay is 28 times a second.
var DEFAULT_TICK_DELAY = makeRational(1, 28);



EXPORTS['big-bang'] = 
    makeClosure(
        'big-bang',
        plt.baselib.arity.makeArityAtLeast(1),
        function(MACHINE) {
            var initialWorldValue = MACHINE.env[MACHINE.env.length - 1];
	    var handlers = [];
	    for (var i = 1; i < MACHINE.argcount; i++) {
		// FIXME: typecheck for configuration options
		handlers.push(MACHINE.env[MACHINE.env.length - 1 - i]);
	    }
	    bigBang(MACHINE, initialWorldValue, handlers);
        });



EXPORTS['on-tick'] = 
    makePrimitiveProcedure(
        'on-tick',
        plt.baselib.lists.makeList(1, 2),
        function(MACHINE) {
	    if (MACHINE.argcount === 1) {
		var f = checkProcedure(MACHINE, "on-tick", 0);
		return new OnTick(f, DEFAULT_TICK_DELAY);
	    } else if (MACHINE.argcount === 2) {
		var f = checkProcedure(MACHINE, "on-tick", 0);
		var delay = checkNonNegativeReal(MACHINE, "on-tick", 1);
		return new OnTick(f, delay);
	    }
        });

















// EXPORTS['on-tick'] =
// 	new CasePrimitive(
// 	    'on-tick',
// 	    [new PrimProc('on-tick',
// 			  1,
// 			  false, false,
// 			  function(f) {
// 			      check(f, isFunction, "on-tick", "procedure", 1);
// 			      return new OnTickBang(f,
// 						    new PrimProc('', 1, false, false,
// 								 function(w) { return types.effectDoNothing(); }),
// 						    DEFAULT_TICK_DELAY);
// 			  }),
// 	     new PrimProc('on-tick',
// 			  2,
// 			  false, false,
// 			  function(f, aDelay) {
// 			      check(f, isFunction, "on-tick", "procedure", 1, arguments);
// 			      check(aDelay, isNumber, "on-tick", "number", 2, arguments);
// 			      return new OnTickBang(f,
// 						    new PrimProc('', 1, false, false,
// 								 function(w) { return types.effectDoNothing(); }),
// 						    aDelay);
// 			  }) ]);



// EXPORTS['on-tick!'] =
//     new CasePrimitive('on-tick!',
// 	[new PrimProc('on-tick!',
// 		      2,
// 		      false, false,
// 		      function(handler, effectHandler) {
// 			  check(handler, isFunction, "on-tick!", "procedure", 1, arguments);
// 			  check(effectHandler, isFunction, "on-tick!","procedure", 2, arguments);
// 			  return new OnTickBang(handler, effectHandler, DEFAULT_TICK_DELAY);
// 		      }),
// 	 new PrimProc('on-tick!',
// 		      3,
// 		      false, false,
// 		      function(handler, effectHandler, aDelay)  {
// 			  check(handler, isFunction, "on-tick!", "procedure", 1, arguments);
// 			  check(effectHandler, isFunction, "on-tick!","procedure", 2, arguments);
// 			  check(aDelay, isNumber, "on-tick!", "number", 3, arguments);
// 			  return new OnTickBang(handler, effectHandler, aDelay);
// 		      }) ]);
