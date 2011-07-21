// runtime.js: the main runtime library for whalesong.
//

if(this['plt'] === undefined) { this['plt'] = {}; }


// All of the values here are namespaced under "plt.runtime".
(function(scope) {
    var runtime = {};
    scope['runtime'] = runtime;



    //////////////////////////////////////////////////////////////////////
    //////////////////////////////////////////////////////////////////////
    // We try to isolate the effect of external modules: all the identifiers we
    // pull from external modules should be listed here, and should otherwise not
    // show up outside this section!
    var isNumber = plt.baselib.numbers.isNumber;
    var isNatural = plt.baselib.numbers.isNatural;
    var isReal = plt.baselib.numbers.isReal;
    var isPair = plt.baselib.lists.isPair;
    var isList = plt.baselib.lists.isList;
    var isVector = plt.baselib.vectors.isVector;
    var isString = plt.baselib.strings.isString;
    var isSymbol = plt.baselib.symbols.isSymbol;
    var isNonNegativeReal = plt.baselib.numbers.isNonNegativeReal;
    var equals = plt.baselib.equality.equals;

    var NULL = plt.baselib.lists.EMPTY;
    var VOID = plt.baselib.constants.VOID_VALUE;
    var EOF = plt.baselib.constants.EOF_VALUE;

    var NEGATIVE_ZERO = plt.baselib.numbers.negative_zero;
    var INF = plt.baselib.numbers.inf;
    var NEGATIVE_INF = plt.baselib.numbers.negative_inf;
    var NAN = plt.baselib.numbers.nan;

    var makeFloat = plt.baselib.numbers.makeFloat;
    var makeRational = plt.baselib.numbers.makeRational;
    var makeBignum = plt.baselib.numbers.makeBignum;
    var makeComplex = plt.baselib.numbers.makeComplex;

    var makeSymbol = plt.baselib.symbols.makeSymbol;

    var makeBox = plt.baselib.boxes.makeBox;
    var isBox = plt.baselib.boxes.isBox;

    var makeVector = plt.baselib.vectors.makeVector;
    var makeList = plt.baselib.lists.makeList;
    var makePair = plt.baselib.lists.makePair;


    var Closure = plt.baselib.functions.Closure;
    var finalizeClosureCall = plt.baselib.functions.finalizeClosureCall;
    var makePrimitiveProcedure = plt.baselib.functions.makePrimitiveProcedure;


    // Other helpers
    var withArguments = plt.baselib.withArguments;
    var heir = plt.baselib.heir;
    var makeClassPredicate = plt.baselib.makeClassPredicate;
    var toDomNode = plt.baselib.format.toDomNode;
    var toWrittenString = plt.baselib.format.toWrittenString;
    var toDisplayedString = plt.baselib.format.toDisplayedString;



    // Frame structures.
    var Frame = plt.baselib.frames.Frame;
    var CallFrame = plt.baselib.frames.CallFrame;
    var PromptFrame = plt.baselib.frames.PromptFrame;

    // Module structure
    var ModuleRecord = plt.baselib.modules.ModuleRecord;



    // Ports
    var OutputPort = plt.baselib.ports.OutputPort;
    var isOutputPort = plt.baselib.ports.isOutputPort;
    var StandardOutputPort = plt.baselib.ports.StandardOutputPort;
    var StandardErrorPort = plt.baselib.ports.StandardErrorPort;
    var OutputStringPort = plt.baselib.ports.OutputStringPort;
    var isOutputStringPort = plt.baselib.ports.isOutputStringPort;




    // Exceptions and error handling.
    var raise = plt.baselib.exceptions.raise;
    var raiseUnboundToplevelError = plt.baselib.exceptions.raiseUnboundToplevelError;
    var raiseArgumentTypeError = plt.baselib.exceptions.raiseArgumentTypeError;
    var raiseContextExpectedValuesError = plt.baselib.exceptions.raiseContextExpectedValuesError;
    var raiseArityMismatchError = plt.baselib.exceptions.raiseArityMismatchError;
    var raiseOperatorApplicationError = plt.baselib.exceptions.raiseOperatorApplicationError;
    var raiseOperatorIsNotPrimitiveProcedure = plt.baselib.exceptions.raiseOperatorIsNotPrimitiveProcedure;
    var raiseOperatorIsNotClosure = plt.baselib.exceptions.raiseOperatorIsNotClosure;
    var raiseUnimplementedPrimitiveError = plt.baselib.exceptions.raiseUnimplementedPrimitiveError;




    

    var testArgument = plt.baselib.check.testArgument;
    var testArity = plt.baselib.check.testArity;
    var makeCheckArgumentType = plt.baselib.check.makeCheckArgumentType;

    var checkOutputPort = plt.baselib.check.checkOutputPort;
    var checkString = plt.baselib.check.checkString;
    var checkSymbol = plt.baselib.check.checkSymbol;
    var checkProcedure = plt.baselib.check.checkProcedure;
    var checkNumber = plt.baselib.check.checkNumber;
    var checkReal = plt.baselib.check.checkReal;
    var checkNonNegativeReal = plt.baselib.check.checkNonNegativeReal;
    var checkNatural = plt.baselib.check.checkNatural;
    var checkNaturalInRange = plt.baselib.check.checkNaturalInRange;
    var checkInteger = plt.baselib.check.checkInteger;
    var checkRational = plt.baselib.check.checkRational;
    var checkPair = plt.baselib.check.checkPair;
    var checkList = plt.baselib.check.checkList;
    var checkVector = plt.baselib.check.checkVector;
    var checkBox = plt.baselib.check.checkBox;
    var checkMutableBox = plt.baselib.check.checkMutableBox;
    var checkInspector = plt.baselib.check.checkInspector;



    //////////////////////////////////////////////////////////////////////]
    // The MACHINE


    // This value will be dynamically determined.
    // See findStackLimit later in this file.
    var STACK_LIMIT_ESTIMATE = 100;


    var Machine = function() {
	this.callsBeforeTrampoline = STACK_LIMIT_ESTIMATE;
	this.val = undefined;
	this.proc = undefined;
	this.argcount = undefined;
	this.env = [];
	this.control = [];     // Arrayof (U Frame CallFrame PromptFrame)
	this.running = false;
	this.modules = {};     // String -> ModuleRecord
        this.mainModules = []; // Arrayof String
	this.params = {

	    // currentDisplayer: DomNode -> Void
	    // currentDisplayer is responsible for displaying to the browser.
	    'currentDisplayer': function(MACHINE, domNode) {
		$(domNode).appendTo(document.body);
	    },
	    
	    // currentErrorDisplayer: DomNode -> Void
	    // currentErrorDisplayer is responsible for displaying errors to the browser.
	    'currentErrorDisplayer': function(MACHINE, domNode) {
                $(domNode).appendTo(document.body);
	    },

            'currentInspector': plt.baselib.inspectors.DEFAULT_INSPECTOR,
	    
	    'currentOutputPort': new StandardOutputPort(),
	    'currentErrorPort': new StandardErrorPort(),
	    'currentSuccessHandler': function(MACHINE) {},
	    'currentErrorHandler': function(MACHINE, exn) {
                MACHINE.params.currentErrorDisplayer(
                    MACHINE,
                    toDomNode(exn));
            },
	    
	    'currentNamespace': {},
	    
	    // These parameters control how often
	    // control yields back to the browser
	    // for response.  The implementation is a
	    // simple PID controller.
	    //
	    // To tune this, adjust desiredYieldsPerSecond.
	    // Do no touch numBouncesBeforeYield or
	    // maxNumBouncesBeforeYield, because those
	    // are adjusted automatically by the
	    // recomputeMaxNumBouncesBeforeYield
	    // procedure.
	    'desiredYieldsPerSecond': 5,
	    'numBouncesBeforeYield': 2000,   // self-adjusting
	    'maxNumBouncesBeforeYield': 2000, // self-adjusting

	    'currentPrint': defaultCurrentPrint


	};
	this.primitives = Primitives;
    };




    // Approximately find the stack limit.
    // This function assumes, on average, five variables or
    // temporaries per stack frame.
    // This will never report a number greater than MAXIMUM_CAP.
    var findStackLimit = function(after) {
	var MAXIMUM_CAP = 32768;
	var n = 1;
	var limitDiscovered = false;
	setTimeout(
	    function() {
		if(! limitDiscovered) {
		    limitDiscovered = true;
		    after(n);
		}
	    },
	    0);
	var loop1 = function(x, y, z, w, k) {
	    // Ensure termination, just in case JavaScript ever
	    // does eliminate stack limits.
	    if (n >= MAXIMUM_CAP) { return; }
	    n++;
	    return 1 + loop2(y, z, w, k, x);
	};
	var loop2 = function(x, y, z, w, k) {
	    n++;
	    return 1 + loop1(y, z, w, k, x);
	};
	try {
	    var dontCare = 1 + loop1(2, "seven", [1], {number: 8}, 2);
	} catch (e) {
	    // ignore exceptions.
	}
	if (! limitDiscovered) { 
	    limitDiscovered = true;
	    after(n);
	}
    };


    // Schedule a stack limit estimation.  If it fails, no harm, no
    // foul (hopefully!)
    setTimeout(function() {
	findStackLimit(function(v) {
	    // Trying to be a little conservative.
	    STACK_LIMIT_ESTIMATE = Math.floor(v / 10);
	});
    },
	       0);


    // captureControl implements the continuation-capturing part of
    // call/cc.  It grabs the control frames up to (but not including) the
    // prompt tagged by the given tag.
    var captureControl = function(MACHINE, skip, tag) {
	var i;
	for (i = MACHINE.control.length - 1 - skip; i >= 0; i--) {
	    if (MACHINE.control[i].tag === tag) {
		return MACHINE.control.slice(i + 1,
					     MACHINE.control.length - skip);
	    }
	} 
	raise(MACHINE, new Error("captureControl: unable to find tag " + tag));
    };



    // restoreControl clears the control stack (up to, but not including the
    // prompt tagged by tag), and then appends the rest of the control frames.
    // At the moment, the rest of the control frames is assumed to be in the 
    // top of the environment.
    var restoreControl = function(MACHINE, tag) {
	var i;
	for (i = MACHINE.control.length - 1; i >= 0; i--) {
	    if (MACHINE.control[i].tag === tag) {
		MACHINE.control = 
		    MACHINE.control.slice(0, i+1).concat(
			MACHINE.env[MACHINE.env.length - 1]);
		return;
	    }
	}
	raise(MACHINE, new Error("restoreControl: unable to find tag " + tag));     

    };


    // Splices the list argument in the environment.  Adjusts MACHINE.argcount
    // appropriately.
    var spliceListIntoStack = function(MACHINE, depth) {
	var lst = MACHINE.env[MACHINE.env.length - 1 - depth];
	var vals = [];
	while(lst !== NULL) {
	    vals.push(lst.first);
	    lst = lst.rest;
	}
	vals.reverse();
	MACHINE.env.splice.apply(MACHINE.env,
				 [MACHINE.env.length - 1 - depth, 1].concat(vals));
	MACHINE.argcount = MACHINE.argcount + vals.length - 1;
    };


    // Unsplices a list from the MACHINE stack.
    var unspliceRestFromStack = function(MACHINE, depth, length) {
	var lst = NULL;
	var i;
	for (i = 0; i < length; i++) {
	    lst = makePair(MACHINE.env[MACHINE.env.length - depth - length + i], 
                           lst);
	}
	MACHINE.env.splice(MACHINE.env.length - depth - length,
			   length, 
			   lst);
	MACHINE.argcount = MACHINE.argcount - length + 1;
    };



    // recomputeGas: state number -> number
    var recomputeMaxNumBouncesBeforeYield = function(MACHINE, observedDelay) {
	// We'd like to see a delay of DESIRED_DELAY_BETWEEN_BOUNCES so
	// that we get MACHINE.params.desiredYieldsPerSecond bounces per
	// second.
	var DESIRED_DELAY_BETWEEN_BOUNCES = 
	    (1000 / MACHINE.params.desiredYieldsPerSecond);
	var ALPHA = 50;
	var delta = (ALPHA * ((DESIRED_DELAY_BETWEEN_BOUNCES -
			       observedDelay) / 
			      DESIRED_DELAY_BETWEEN_BOUNCES));
	MACHINE.params.maxNumBouncesBeforeYield = 
            Math.max(MACHINE.params.maxNumBouncesBeforeYield + delta,
                     1);
    };


    var HaltError = function(onHalt) {
        // onHalt: MACHINE -> void
        this.onHalt = onHalt || function(MACHINE) {};
    };


    var Pause = function(onPause) {
        // onPause: MACHINE -> void
        this.onPause = onPause || function(MACHINE) {};
    };

    var PAUSE = function(onPause) {
        throw(new Pause(onPause));
    };
    



    //////////////////////////////////////////////////////////////////////
    //////////////////////////////////////////////////////////////////////
    //////////////////////////////////////////////////////////////////////
    //////////////////////////////////////////////////////////////////////
    // The toplevel trampoline.
    //
    //
    // trampoline: MACHINE (MACHINE -> void) -> void
    //
    // All evaluation in Racketland happens in the context of this
    // trampoline.
    //
    var trampoline = function(MACHINE, initialJump) {
	var thunk = initialJump;
	var startTime = (new Date()).valueOf();
	MACHINE.callsBeforeTrampoline = STACK_LIMIT_ESTIMATE;
	MACHINE.params.numBouncesBeforeYield = 
	    MACHINE.params.maxNumBouncesBeforeYield;
	MACHINE.running = true;

	while(true) {
            try {
		thunk(MACHINE);
		break;
            } catch (e) {
                // There are a few kinds of things that can get thrown
                // during racket evaluation:
                //
                // functions: this gets thrown if the Racket code
                // realizes that the number of bounces has grown too
                // large.  The thrown function represents a restarter
                // function.  The running flag remains true.
                //
                // Pause: causes the machine evaluation to pause, with
                // the expectation that it will restart momentarily.
                // The running flag on the machine will remain true.
                //
                // HaltError: causes evaluation to immediately halt.
                // We schedule the onHalt function of the HaltError to
                // call afterwards.  The running flag on the machine
                // is set to false.
                //
                // Everything else: otherwise, we send the exception value
                // to the current error handler and exit.
                // The running flag is set to false.
		if (typeof(e) === 'function') {
                    thunk = e;
                    MACHINE.callsBeforeTrampoline = STACK_LIMIT_ESTIMATE;

		    if (MACHINE.params.numBouncesBeforeYield-- < 0) {
			recomputeMaxNumBouncesBeforeYield(
			    MACHINE,
			    (new Date()).valueOf() - startTime);
			setTimeout(
			    function() { 
                                trampoline(MACHINE, thunk); 
                            },
			    0);
			return;
		    } else {
                        continue;
                    }
		} else if (e instanceof Pause) {
                    var restart = function(thunk) {
		        setTimeout(
			    function() { trampoline(MACHINE, thunk); },
			    0);
                    };
                    e.onPause(restart);
                    return;
                } else if (e instanceof HaltError) {
		    MACHINE.running = false;
                    e.onHalt(MACHINE);
                    return;
                } else {
		    // General error condition: just exit out
		    // of the trampoline and call the current error handler.
		    MACHINE.running = false;
                    MACHINE.params.currentErrorHandler(MACHINE, e);
	            return;
		}
            }
	}
	MACHINE.running = false;
        setTimeout(
            function() { MACHINE.params.currentSuccessHandler(MACHINE); },
            0);
	return;
    };



    //////////////////////////////////////////////////////////////////////
    //////////////////////////////////////////////////////////////////////
    //////////////////////////////////////////////////////////////////////







    var defaultCurrentPrint = new Closure(
	function(MACHINE) {
            if(--MACHINE.callsBeforeTrampoline < 0) { 
                throw arguments.callee; 
            }
            var oldArgcount = MACHINE.argcount;

	    var elt = MACHINE.env[MACHINE.env.length - 1];
	    var outputPort = 
		MACHINE.params.currentOutputPort;
	    if (elt !== VOID) {
		outputPort.writeDomNode(MACHINE, toDomNode(elt, 'print'));
		outputPort.writeDomNode(MACHINE, toDomNode("\n", 'display'));
	    }
            MACHINE.argcount = oldArgcount;
            return finalizeClosureCall(MACHINE, VOID);
	},
	1,
	[],
	"printer");





    var VariableReference = function(prefix, pos) {
        this.prefix = prefix;
        this.pos = pos;
    };



    // A continuation prompt tag labels a prompt frame.
    var ContinuationPromptTag = function(name) {
	this.name = name;
    };



    // There is a single, distinguished default continuation prompt tag
    // that's used to wrap around toplevel prompts.
    var DEFAULT_CONTINUATION_PROMPT_TAG = 
	new ContinuationPromptTag("default-continuation-prompt-tag");







    

    // Primitives are the set of primitive values.  Not all primitives
    // are coded here; several of them (including call/cc) are injected by
    // the bootstrapping code in compiler/boostrapped-primitives.rkt
    var Primitives = {};

    var installPrimitiveProcedure = function(name, arity, f) {
        Primitives[name] = f;
        Primitives[name].arity = arity;
        Primitives[name].displayName = name;
    };

    var installPrimitiveClosure = function(name, arity, f) {
        Primitives[name] = 
            new Closure(f, arity, [], name);
    };


    var installPrimitiveConstant = function(name, v) {
        Primitives[name] = v;
    };



    installPrimitiveConstant('pi', plt.baselib.numbers.pi);
    installPrimitiveConstant('e', plt.baselib.numbers.e);
    installPrimitiveConstant('null', NULL);
    installPrimitiveConstant('true', true);
    installPrimitiveConstant('false', false);


    installPrimitiveProcedure(
        'display', makeList(1, 2),
        function(MACHINE) {
	    var firstArg = MACHINE.env[MACHINE.env.length-1];
	    var outputPort = MACHINE.params.currentOutputPort;
	    if (MACHINE.argcount === 2) {
	        outputPort = checkOutputPort(MACHINE, 'display', 1);
	    }
	    outputPort.writeDomNode(MACHINE, toDomNode(firstArg, 'display'));
            return VOID;
        });

    installPrimitiveProcedure(
        'newline', makeList(0, 1),
        function(MACHINE) {
	    var outputPort = MACHINE.params.currentOutputPort;
	    if (MACHINE.argcount === 1) { 
	        outputPort = checkOutputPort(MACHINE, 'newline', 1);
	    }
	    outputPort.writeDomNode(MACHINE, toDomNode("\n", 'display'));
            return VOID;
        });

    installPrimitiveProcedure(
        'displayln',
        makeList(1, 2),
        function(MACHINE){
	    var firstArg = MACHINE.env[MACHINE.env.length-1];
	    var outputPort = MACHINE.params.currentOutputPort;
	    if (MACHINE.argcount === 2) {
                outputPort = checkOutputPort(MACHINE, 'displayln', 1);
	    }
	    outputPort.writeDomNode(MACHINE, toDomNode(firstArg, 'display'));
	    outputPort.writeDomNode(MACHINE, toDomNode("\n", 'display'));
            return VOID;
        });



    installPrimitiveProcedure(
        'format',
        plt.baselib.arity.makeArityAtLeast(1),
        function(MACHINE) {
            var args = [], i, formatString;
            formatString = checkString(MACHINE, 'format', 0).toString();
            for(i = 1; i < MACHINE.argcount; i++) {
                args.push(MACHINE.env[MACHINE.env.length - 1 - i]);
            }
            return plt.baselib.format.format(formatString, args, 'format');
        });


    installPrimitiveProcedure(
        'printf',
        plt.baselib.arity.makeArityAtLeast(1),
        function(MACHINE) {
            var args = [], i, formatString, result, outputPort;
            formatString = checkString(MACHINE, 'printf', 0).toString();
            for(i = 1; i < MACHINE.argcount; i++) {
                args.push(MACHINE.env[MACHINE.env.length - 1 - i]);
            }
            result = plt.baselib.format.format(formatString, args, 'format');
            outputPort = MACHINE.params.currentOutputPort;            
	    outputPort.writeDomNode(MACHINE, toDomNode(result, 'display'));
            return VOID;
        });


    installPrimitiveProcedure(
        'fprintf',
        plt.baselib.arity.makeArityAtLeast(2),
        function(MACHINE) {
            var args = [], i, formatString, outputPort, result;
            outputPort = checkOutputPort(MACHINE, 'fprintf', 0);
            formatString = checkString(MACHINE, 'fprintf', 1).toString();
            for(i = 2; i < MACHINE.argcount; i++) {
                args.push(MACHINE.env[MACHINE.env.length - 1 - i]);
            }
            result = plt.baselib.format.format(formatString, args, 'format');
	    outputPort.writeDomNode(MACHINE, toDomNode(result, 'display'));
            return VOID;
        });






    installPrimitiveProcedure(
        'current-print',
        makeList(0, 1),
        function(MACHINE) {
            if (MACHINE.argcount === 1) {
                MACHINE.params['currentPrint'] =                 
                    checkProcedure(MACHINE, 'current-print', 0);
                return VOID;
            } else {
	        return MACHINE.params['currentPrint'];
            }
        });


    installPrimitiveProcedure(
        'current-output-port',
        makeList(0, 1),
        function(MACHINE) {
            if (MACHINE.argcount === 1) {
                MACHINE.params['currentOutputPort'] = 
                    checkOutputPort(MACHINE, 'current-output-port', 0);
                return VOID;
            } else {
	        return MACHINE.params['currentOutputPort'];
            }
        });





    installPrimitiveProcedure(
        '=',
        plt.baselib.arity.makeArityAtLeast(2),
        function(MACHINE) {
	    var firstArg = checkNumber(MACHINE, '=', 0), secondArg;
	    for (var i = 1; i < MACHINE.argcount; i++) {
                var secondArg = checkNumber(MACHINE, '=', i);
	        if (! (plt.baselib.numbers.equals(firstArg, secondArg))) {
		    return false; 
	        }
	    }
	    return true;
        });


    
    installPrimitiveProcedure(
        '=~',
        3,
        function(MACHINE) {
	    var x = checkReal(MACHINE, '=~', 0);
	    var y = checkReal(MACHINE, '=~', 1);
	    var range = checkNonNegativeReal(MACHINE, '=~', 2);
            return plt.baselib.numbers.lessThanOrEqual(
                plt.baselib.numbers.abs(plt.baselib.numbers.subtract(x, y)), 
                range);
        });



    var makeChainingBinop = function(predicate, name) {
        return function(MACHINE) {
	    var firstArg = checkNumber(MACHINE, name, 0), secondArg;
	    for (var i = 1; i < MACHINE.argcount; i++) {
	        secondArg = checkNumber(MACHINE, name, i);
	        if (! (predicate(firstArg, secondArg))) {
		    return false; 
	        }
                firstArg = secondArg;
	    }
	    return true;
        };
    };

    installPrimitiveProcedure(
        '<',
        plt.baselib.arity.makeArityAtLeast(2),
        makeChainingBinop(plt.baselib.numbers.lessThan, '<'));


    installPrimitiveProcedure(
        '>',
        plt.baselib.arity.makeArityAtLeast(2),
        makeChainingBinop(plt.baselib.numbers.greaterThan, '>'));


    installPrimitiveProcedure(
        '<=',
        plt.baselib.arity.makeArityAtLeast(2),
        makeChainingBinop(plt.baselib.numbers.lessThanOrEqual, '<='));


    installPrimitiveProcedure(
        '>=',
        plt.baselib.arity.makeArityAtLeast(2),
        makeChainingBinop(plt.baselib.numbers.greaterThanOrEqual, '>='));
    

    installPrimitiveProcedure(
        '+',
        plt.baselib.arity.makeArityAtLeast(0),
        function(MACHINE) {
	    var result = 0;
	    var i = 0;
	    for (i = 0; i < MACHINE.argcount; i++) {
	        result = plt.baselib.numbers.add(
                    result, 
                    checkNumber(MACHINE, '+', i));
	    };
	    return result;
        });
    

    installPrimitiveProcedure(
        '*',
        plt.baselib.arity.makeArityAtLeast(0),
        function(MACHINE) {
	    var result = 1;
	    var i = 0;
	    for (i=0; i < MACHINE.argcount; i++) {
	        result = plt.baselib.numbers.multiply(
                    result, 
                    checkNumber(MACHINE, '*', i));
	    }
	    return result;
        });

    installPrimitiveProcedure(
        '-',
        plt.baselib.arity.makeArityAtLeast(1),
        function(MACHINE) {
	    if (MACHINE.argcount === 1) { 
	        return plt.baselib.numbers.subtract(
                    0, 
                    checkNumber(MACHINE, '-', 0));
	    }
	    var result = checkNumber(MACHINE, '-', 0);
	    for (var i = 1; i < MACHINE.argcount; i++) {
	        result = plt.baselib.numbers.subtract(
                    result, 
                    checkNumber(MACHINE, '-', i));
	    }
	    return result;
        });
    
    installPrimitiveProcedure(
        '/',
        plt.baselib.arity.makeArityAtLeast(1),
        function(MACHINE) {
	    var result = checkNumber(MACHINE, '/', 0);
	    for (var i = 1; i < MACHINE.argcount; i++) {
	        result = plt.baselib.numbers.divide(
                    result,
                    checkNumber(MACHINE, '/', i));
	    }
	    return result;
        });
    

    installPrimitiveProcedure(
        'add1',
        1,
        function(MACHINE) {
	    var firstArg = checkNumber(MACHINE, 'add1', 0);
	    return plt.baselib.numbers.add(firstArg, 1);
        });


    installPrimitiveProcedure(
        'sub1',
        1,
        function(MACHINE) {
	    var firstArg = checkNumber(MACHINE, 'sub1', 0);
	    return plt.baselib.numbers.subtract(firstArg, 1);
        });


    installPrimitiveProcedure(
        'zero?',
        1,
        function(MACHINE) {
	    var firstArg = MACHINE.env[MACHINE.env.length-1];
	    return plt.baselib.numbers.equals(firstArg, 0);
        });


    installPrimitiveProcedure(
        'cons',
        2,
        function(MACHINE) {
	    var firstArg = MACHINE.env[MACHINE.env.length-1];
	    var secondArg = MACHINE.env[MACHINE.env.length-2];
	    return makePair(firstArg, secondArg);
        });


    installPrimitiveProcedure(
        'list',
        plt.baselib.arity.makeArityAtLeast(0),
        function(MACHINE) {
	    var result = NULL;
	    for (var i = 0; i < MACHINE.argcount; i++) {
	        result = makePair(MACHINE.env[MACHINE.env.length - (MACHINE.argcount - i)],
		                  result);
	    }
	    return result;
        });

    installPrimitiveProcedure(
        'list-ref',
        2,
        function(MACHINE) {
            var lst = checkList(MACHINE, 'list-ref', 0);
            var index = checkNaturalInRange(MACHINE, 'list-ref', 1,
                                            0, plt.baselib.lists.length(lst));
            return plt.baselib.lists.listRef(lst, plt.baselib.numbers.toFixnum(index));
        });




    installPrimitiveProcedure(
        'car',
        1,
        function(MACHINE) {
	    var firstArg = checkPair(MACHINE, 'car', 0);
	    return firstArg.first;
        });

    installPrimitiveProcedure(
        'cdr',
        1,
        function(MACHINE) {
	    var firstArg = checkPair(MACHINE, 'cdr', 0);
	    return firstArg.rest;
        });

    installPrimitiveProcedure(
        'pair?',
        1,
        function(MACHINE) {
	    var firstArg = MACHINE.env[MACHINE.env.length-1];
	    return isPair(firstArg);
        });

    installPrimitiveProcedure(
        'set-car!',
        2,
        function(MACHINE) {
	    var firstArg = checkPair(MACHINE, 'set-car!', 0);
	    var secondArg = MACHINE.env[MACHINE.env.length-2];
	    firstArg.first = secondArg;
            return VOID;
        });


    installPrimitiveProcedure(
        'set-cdr!',
        2,
        function(MACHINE) {
	    var firstArg = checkPair(MACHINE, 'set-car!', 0);
	    var secondArg = MACHINE.env[MACHINE.env.length-2];
	    firstArg.rest = secondArg;
            return VOID;
        });

    
    installPrimitiveProcedure(
        'not',
        1,
        function(MACHINE) {
	    var firstArg = MACHINE.env[MACHINE.env.length-1];
	    return (firstArg === false);
        });


    installPrimitiveProcedure(
        'null?',
        1,
        function(MACHINE) {
	    var firstArg = MACHINE.env[MACHINE.env.length-1];
	    return firstArg === NULL;
        });


    installPrimitiveProcedure(
        'vector',
        plt.baselib.arity.makeArityAtLeast(0),
        function(MACHINE) {
	    var i;
	    var result = [];
	    for (i = 0; i < MACHINE.argcount; i++) {
	        result.push(MACHINE.env[MACHINE.env.length-1-i]);
	    }
	    var newVector = makeVector.apply(null, result);
            return newVector;
        });


    installPrimitiveProcedure(
        'vector->list',
        1,
        function(MACHINE) {
	    var elts = checkVector(MACHINE, 'vector->list', 0).elts;
	    var i;
	    var result = NULL;
	    for (i = 0; i < elts.length; i++) {
	        result = makePair(elts[elts.length - 1 - i], result);
	    }
	    return result;
        });

    
    installPrimitiveProcedure(
        'list->vector',
        1,
        function(MACHINE) {
	    var firstArg = checkList(MACHINE, 'list->vector', 0);
	    var result = [];
	    while (firstArg !== NULL) {
	        result.push(firstArg.first);
	        firstArg = firstArg.rest;
	    }
            return makeVector.apply(null, result);
        });


    installPrimitiveProcedure(
        'vector-ref',
        2,
        function(MACHINE) {
	    var elts = checkVector(MACHINE, 'vector-ref', 0).elts;
	    var index = MACHINE.env[MACHINE.env.length-2];
	    return elts[index];
        });


    installPrimitiveProcedure(
        'vector-set!',
        3,
        function(MACHINE) {
	    var elts = checkVector(MACHINE, 'vector-set!', 0).elts;
            // FIXME: check out-of-bounds vector
	    var index = plt.baselib.numbers.toFixnum(
                checkNaturalInRange(MACHINE, 'vector-set!', 1,
                                    0, elts.length));
	    var val = MACHINE.env[MACHINE.env.length - 1 - 2];
	    elts[index] = val;
	    return VOID;
        });


    installPrimitiveProcedure(
        'vector-length',
        1,
        function(MACHINE) {
	    return checkVector(MACHINE, 'vector-length', 0).elts.length;
        });


    installPrimitiveProcedure(
        'make-vector',
        makeList(1, 2),
        function(MACHINE) {
	    var value = 0;
	    var length = plt.baselib.numbers.toFixnum(
                checkNatural(MACHINE, 'make-vector', 0));
	    if (MACHINE.argcount == 2) {
	        value = MACHINE.env[MACHINE.env.length - 2];
	    }
	    var arr = [];
	    for(var i = 0; i < length; i++) {
	        arr[i] = value;
	    }
            return makeVector.apply(null, arr);
        });
    


    installPrimitiveProcedure(
        'symbol?',
        1,
        function(MACHINE) {
	    var firstArg = MACHINE.env[MACHINE.env.length-1];
	    return isSymbol(firstArg);
        });

    installPrimitiveProcedure(
        'symbol->string',
        1,
        function(MACHINE) {
	    var firstArg = checkSymbol(MACHINE, 'symbol->string', 0);
	    return firstArg.toString();
        });

    installPrimitiveProcedure(
        'string-append',
        plt.baselib.arity.makeArityAtLeast(0),
        function(MACHINE) {
	    var buffer = [];
	    var i;
	    for (i = 0; i < MACHINE.argcount; i++) {
	        buffer.push(checkString(MACHINE, 'string-append', i).toString());
	    }
	    return buffer.join('');
        });

    installPrimitiveProcedure(
        'string-length',
        1,
        function(MACHINE) {
	    var firstArg = checkString(MACHINE, 'string-length', 0).toString();
	    return firstArg.length;
        });
    
    installPrimitiveProcedure(
        'box',
        1,
        function(MACHINE) {
	    var firstArg = MACHINE.env[MACHINE.env.length-1];
	    return makeBox(firstArg);
        });

    installPrimitiveProcedure(
        'unbox',
        1,
        function(MACHINE) {
	    var firstArg = checkBox(MACHINE, 'unbox', 0);
            return firstArg.ref();
        });

    installPrimitiveProcedure(
        'set-box!',
        2,
        function(MACHINE) {
	    var firstArg = checkMutableBox(MACHINE, 'set-box!', 0);
	    var secondArg = MACHINE.env[MACHINE.env.length-2];
            firstArg.set(secondArg);
	    return VOID;
        });

    installPrimitiveProcedure(
        'void',
        plt.baselib.arity.makeArityAtLeast(0),
        function(MACHINE) {
	    return VOID;
        });


    installPrimitiveProcedure(
        'random',
        plt.baselib.lists.makeList(0, 1),
        function(MACHINE) {
            if (MACHINE.argcount === 0) {
                return plt.baselib.numbers.makeFloat(Math.random());
            } else {
                var n = checkNatural(MACHINE, 'random', 0);
		return Math.floor(Math.random() * plt.baselib.numbers.toFixnum(n));
            }
        });


    installPrimitiveProcedure(
        'eq?',
        2,
        function(MACHINE) {
	    var firstArg = MACHINE.env[MACHINE.env.length-1];
	    var secondArg = MACHINE.env[MACHINE.env.length-2];
	    return firstArg === secondArg;
        });

    installPrimitiveProcedure(
        'equal?',
        2,
        function(MACHINE) {
	    var firstArg = MACHINE.env[MACHINE.env.length-1];
	    var secondArg = MACHINE.env[MACHINE.env.length-2];
	    return equals(firstArg, secondArg);
        });



    installPrimitiveProcedure(
        'member',
        2,
        function(MACHINE) {
	    var x = MACHINE.env[MACHINE.env.length-1];
	    var lst = MACHINE.env[MACHINE.env.length-2];
	    var originalLst = lst;
	    while (true) {
	        if (lst === NULL) {
		    return false;
	        }
	        if (! isPair(lst)) {
		    raiseArgumentTypeError(MACHINE,
                                           'member',
                                           'list',
                                           1,
                                           MACHINE.env[MACHINE.env.length - 1 - 1]);
	        }
	        if (equals(x, (lst.first))) {
		    return lst;
	        }
	        lst = lst.rest;
	    }	
        });
    


    installPrimitiveProcedure(
        'reverse',
        1,
        function(MACHINE) {
	    var rev = NULL;
	    var lst = MACHINE.env[MACHINE.env.length-1];
	    while(lst !== NULL) {
	        testArgument(MACHINE,
			     'pair', isPair, lst, 0, 'reverse');
	        rev = makePair(lst.first, rev);
	        lst = lst.rest;
	    }
	    return rev;
        });




    installPrimitiveProcedure(
        'abs',
        1,
        function(MACHINE) {
            return plt.baselib.numbers.abs(
                checkNumber(MACHINE, 'abs', 0));
        });

    installPrimitiveProcedure(
        'acos',
        1,
        function(MACHINE) {
            return plt.baselib.numbers.acos(
                checkNumber(MACHINE, 'acos', 0));
        });


    installPrimitiveProcedure(
        'asin',
        1,
        function(MACHINE) {
            return plt.baselib.numbers.asin(
                checkNumber(MACHINE, 'asin', 0));
        });

    installPrimitiveProcedure(
        'sin',
        1,
        function(MACHINE) {
            return plt.baselib.numbers.sin(
                checkNumber(MACHINE, 'sin', 0));
        });



    installPrimitiveProcedure(
        'sinh',
        1,
        function(MACHINE) {
            return plt.baselib.numbers.sinh(
                checkNumber(MACHINE, 'sinh', 0));
        });


    installPrimitiveProcedure(
        'tan',
        1,
        function(MACHINE) {
            return plt.baselib.numbers.tan(
                checkNumber(MACHINE, 'tan', 0));
        });

    

    installPrimitiveProcedure(
        'atan',
        makeList(1, 2),
        function(MACHINE) {
            if (MACHINE.argcount === 1) {
		return plt.baselib.numbers.atan(
                    checkNumber(MACHINE, 'atan', 0));
            } else {
                testArgument(MACHINE,
                             'number',
                             isNumber,
                             MACHINE.env[MACHINE.env.length - 1],
                             0,
                             'atan');
                testArgument(MACHINE,
                             'number',
                             isNumber,
                             MACHINE.env[MACHINE.env.length - 2],
                             1,
                             'atan');
                return plt.baselib.numbers.makeFloat(
		    Math.atan2(
                        plt.baselib.numbers.toFixnum(checkNumber(MACHINE, 'atan', 0)),
                        plt.baselib.numbers.toFixnum(checkNumber(MACHINE, 'atan', 1))));
            }
        });


    installPrimitiveProcedure(
        'angle',
        1,
        function(MACHINE) {
            return plt.baselib.numbers.angle(
                checkNumber(MACHINE, 'angle', 0));
        });

    installPrimitiveProcedure(
        'magnitude',
        1,
        function(MACHINE) {
            return plt.baselib.numbers.magnitude(
                checkNumber(MACHINE, 'magnitude', 0));
        });

    installPrimitiveProcedure(
        'conjugate',
        1,
        function(MACHINE) {
            return plt.baselib.numbers.conjugate(
                checkNumber(MACHINE, 'conjugate', 0));
        });




    installPrimitiveProcedure(
        'cos',
        1,
        function(MACHINE) {
            return plt.baselib.numbers.cos(
                checkNumber(MACHINE, 'cos', 0));
        });


    installPrimitiveProcedure(
        'cosh',
        1,
        function(MACHINE) {
            return plt.baselib.numbers.cosh(
                checkNumber(MACHINE, 'cosh', 0));
        });

    installPrimitiveProcedure(
        'gcd',
        plt.baselib.arity.makeArityAtLeast(1),
        function(MACHINE) {
            var args = [], i, x;
            for (i = 0; i < MACHINE.argcount; i++) {
                args.push(checkNumber(MACHINE, 'gcd', i));
            }
            x = args.shift();
	    return plt.baselib.numbers.gcd(x, args);
        });

    installPrimitiveProcedure(
        'lcm',
        plt.baselib.arity.makeArityAtLeast(1),
        function(MACHINE) {
            var args = [], i, x;
            for (i = 0; i < MACHINE.argcount; i++) {
                args.push(checkNumber(MACHINE, 'lcm', i));
            }
            x = args.shift();
	    return plt.baselib.numbers.lcm(x, args);
        });




    installPrimitiveProcedure(
        'exp',
        1,
        function(MACHINE) {
            return plt.baselib.numbers.exp(
                checkNumber(MACHINE, 'exp', 0));
        });


    installPrimitiveProcedure(
        'expt',
        2,
        function(MACHINE) {
            return plt.baselib.numbers.expt(
                checkNumber(MACHINE, 'expt', 0),
                checkNumber(MACHINE, 'expt', 1));
        });


    installPrimitiveProcedure(
        'exact?',
        1,
        function(MACHINE) {
            return plt.baselib.numbers.isExact(
                checkNumber(MACHINE, 'exact?', 0));
        });


    installPrimitiveProcedure(
        'integer?',
        1,
        function(MACHINE) {
            return plt.baselib.numbers.isInteger(MACHINE.env[MACHINE.env.length - 1]);
        });



    installPrimitiveProcedure(
        'imag-part',
        1,
        function(MACHINE) {
            return plt.baselib.numbers.imaginaryPart(
                checkNumber(MACHINE, 'imag-part', 0));
        });


    installPrimitiveProcedure(
        'real-part',
        1,
        function(MACHINE) {
            return plt.baselib.numbers.realPart(
                checkNumber(MACHINE, 'real-part', 0));
        });


    installPrimitiveProcedure(
        'make-polar',
        2,
        function(MACHINE) {
            return plt.baselib.numbers.makeComplexPolar(
                checkReal(MACHINE, 'make-polar', 0),
                checkReal(MACHINE, 'make-polar', 1));
        });


    installPrimitiveProcedure(
        'make-rectangular',
        2,
        function(MACHINE) {
            return plt.baselib.numbers.makeComplex(
                checkReal(MACHINE, 'make-rectangular', 0),
                checkReal(MACHINE, 'make-rectangular', 1));
        });

    installPrimitiveProcedure(
        'modulo',
        2,
        function(MACHINE) {
            return plt.baselib.numbers.modulo(
                checkInteger(MACHINE, 'modulo', 0),
                checkInteger(MACHINE, 'modulo', 1));
        });


    installPrimitiveProcedure(
        'remainder',
        2,
        function(MACHINE) {
            return plt.baselib.numbers.remainder(
                checkInteger(MACHINE, 'remainder', 0),
                checkInteger(MACHINE, 'remainder', 1));
        });


    installPrimitiveProcedure(
        'quotient',
        2,
        function(MACHINE) {
            return plt.baselib.numbers.quotient(
                checkInteger(MACHINE, 'quotient', 0),
                checkInteger(MACHINE, 'quotient', 1));
        });



    installPrimitiveProcedure(
        'floor',
        1,
        function(MACHINE) {
            return plt.baselib.numbers.floor(
                checkReal(MACHINE, 'floor', 0));
        });
    

    installPrimitiveProcedure(
        'ceiling',
        1,
        function(MACHINE) {
            return plt.baselib.numbers.ceiling(
                checkReal(MACHINE, 'ceiling', 0));
        });
   

    installPrimitiveProcedure(
        'round',
        1,
        function(MACHINE) {
            return plt.baselib.numbers.round(
                checkReal(MACHINE, 'round', 0));
        });
    

    installPrimitiveProcedure(
        'truncate',
        1,
        function(MACHINE) {
            var n = checkReal(MACHINE, 'truncate', 0);
	    if (plt.baselib.numbers.lessThan(n, 0)) {
		return plt.baselib.numbers.ceiling(n);
	    } else {
		return plt.baselib.numbers.floor(n);
	    }
        });
    

    installPrimitiveProcedure(
        'numerator',
        1,
        function(MACHINE) {
            return plt.baselib.numbers.numerator(
                checkRational(MACHINE, 'numerator', 0));
        });


    installPrimitiveProcedure(
        'denominator',
        1,
        function(MACHINE) {
            return plt.baselib.numbers.denominator(
                checkRational(MACHINE, 'denominator', 0));
        });


    installPrimitiveProcedure(
        'log',
        1,
        function(MACHINE) {
            return plt.baselib.numbers.log(
                checkNumber(MACHINE, 'log', 0));
        });


    installPrimitiveProcedure(
        'sqr',
        1,
        function(MACHINE) {
            return plt.baselib.numbers.sqr(
                checkNumber(MACHINE, 'sqr', 0));
        });




    installPrimitiveProcedure(
        'sqrt',
        1,
        function(MACHINE) {
            return plt.baselib.numbers.sqrt(
                checkNumber(MACHINE, 'sqrt', 0));
        });



    installPrimitiveProcedure(
        'integer-sqrt',
        1,
        function(MACHINE) {
            return plt.baselib.numbers.integerSqrt(
                checkInteger(MACHINE, 'integer-sqrt', 0));
        });



    installPrimitiveProcedure(
        'sgn',
        1,
        function(MACHINE) {
            return plt.baselib.numbers.sign(
                checkInteger(MACHINE, 'sgn', 0));
        });


    installPrimitiveProcedure(
        'number->string',
        1,
        function(MACHINE) {
            return checkNumber(MACHINE, 'number->string', 0).toString();
        });


    installPrimitiveProcedure(
	'string->symbol',
	1,
	function(MACHINE) {
	    return makeSymbol(checkString(MACHINE, 'string->symbol', 0).toString());
	});


    installPrimitiveProcedure(
        'string->number',
        1,
        function(MACHINE) {
            return plt.baselib.numbers.fromString(
                checkString(MACHINE, 'string->number', 0).toString());
        });

    

    installPrimitiveClosure(
        'make-struct-type',
        makeList(4, 5, 6, 7, 8, 9, 10, 11),
        function(MACHINE) {
            withArguments(
                MACHINE,
                4,
                [false, 
	         NULL,
	         false,
	         false,
	         NULL,
	         false,
	         false],
                function(name, 
                         superType,
	                 initFieldCount,
	                 autoFieldCount,
	                 autoV,
	                 props,	 // FIXME: currently ignored
	                 inspector,  // FIXME: currently ignored
	                 procSpec,	 // FIXME: currently ignored
	                 immutables, // FIXME: currently ignored
	                 guard,      // FIXME: currently ignored
                         constructorName
                        ) {

                    // FIXME: typechecks.

                    var structType = plt.baselib.structs.makeStructureType(
                        name,
                        superType,
                        initFieldCount,
                        autoFieldCount,
                        autoV,
                        //props,
                        //inspector,
                        //procSpec,
                        //immutables,
                        guard);

                    var constructorValue = 
                        makePrimitiveProcedure(
                            constructorName,
                            plt.baselib.numbers.toFixnum(initFieldCount),
                            function(MACHINE) {
                                var args = [];
                                for(var i = 0; i < initFieldCount; i++) {
                                    args.push(MACHINE.env[MACHINE.env.length - 1 - i]);
                                }
                                return structType.constructor.apply(null, args);
                            });

                    var predicateValue = 
                        makePrimitiveProcedure(
                            String(name) + "?",
                            1,
                            function(MACHINE) {
                                return structType.predicate(MACHINE.env[MACHINE.env.length - 1]);
                            });

                    var accessorValue = 
                        makePrimitiveProcedure(
                            String(name) + "-accessor",
                            2,
                            function(MACHINE) {
                                // FIXME: typechecks
                                return structType.accessor(
                                    MACHINE.env[MACHINE.env.length - 1],
                                    plt.baselib.numbers.toFixnum(MACHINE.env[MACHINE.env.length - 2]));
                            });
                    accessorValue.structType = structType;

                    var mutatorValue = 
                        makePrimitiveProcedure(
                            String(name) + "-mutator",
                            3,
                            function(MACHINE) {
                                // FIXME: typechecks
                                return structType.mutator(
                                    MACHINE.env[MACHINE.env.length - 1],
                                    plt.baselib.numbers.toFixnum(MACHINE.env[MACHINE.env.length - 2]),
                                    MACHINE.env[MACHINE.env.length - 3]);
                            });
                    mutatorValue.structType = structType;


                    finalizeClosureCall(MACHINE,
                                        structType,
                                        constructorValue,
                                        predicateValue,
                                        accessorValue,
                                        mutatorValue);
                });
        });
        

     installPrimitiveProcedure(
         'current-inspector',
         makeList(0, 1),
         function(MACHINE) {
            if (MACHINE.argcount === 1) {
                MACHINE.params['currentInspector'] = 
                    checkInspector(MACHINE, 'current-inspector', 0);
                return VOID;
            } else {
	        return MACHINE.params['currentInspector'];
            }
         }
     ); 


    installPrimitiveProcedure(
        'make-struct-field-accessor',
        makeList(2, 3),
        function(MACHINE){
            // FIXME: typechecks
            // We must guarantee that the ref argument is good.
            var structType = MACHINE.env[MACHINE.env.length - 1].structType;
            var index = MACHINE.env[MACHINE.env.length - 2];
            var name;
            if (MACHINE.argcount === 3) {
                name = String(MACHINE.env[MACHINE.env.length - 3]);
            } else {
                name = 'field' + index;
            }
            return makePrimitiveProcedure(
                name,
                1,
                function(MACHINE) {
                    return structType.accessor(
                        MACHINE.env[MACHINE.env.length - 1],
                        plt.baselib.numbers.toFixnum(index));
                });
            
        });


    installPrimitiveProcedure(
        'make-struct-field-mutator',
        makeList(2, 3),
        function(MACHINE){
            // FIXME: typechecks
            // We must guarantee that the set! argument is good.
            var structType = MACHINE.env[MACHINE.env.length - 1].structType;
            var index = MACHINE.env[MACHINE.env.length - 2];
            var name;
            if (MACHINE.argcount === 3) {
                name = String(MACHINE.env[MACHINE.env.length - 3]);
            } else {
                name = 'field' + index;
            }
            return makePrimitiveProcedure(
                name,
                2,
                function(MACHINE) {
                    return structType.mutator(
                        MACHINE.env[MACHINE.env.length - 1],
                        plt.baselib.numbers.toFixnum(index),
                        MACHINE.env[MACHINE.env.length - 2]);
                });            
        });






    //////////////////////////////////////////////////////////////////////
    //////////////////////////////////////////////////////////////////////
    //////////////////////////////////////////////////////////////////////






    //////////////////////////////////////////////////////////////////////
    //////////////////////////////////////////////////////////////////////
    //////////////////////////////////////////////////////////////////////

    // Implementation of the ready function.  This will fire off when
    // setReadyTrue is called.

    (function(scope) {
        scope.ready = function(f) {
            if (runtimeIsReady) {
                notifyWaiter(f);
            } else {
                readyWaiters.push(f);
            }
        };

        scope.setReadyTrue = function() {
            runtimeIsReady = true;
            while(runtimeIsReady && readyWaiters.length > 0) {
                notifyWaiter(readyWaiters.shift());
            }
        };

        scope.setReadyFalse = function() {
            runtimeIsReady = false;
        };


        var runtimeIsReady = false;
        var readyWaiters = [];
        var notifyWaiter = function(w) {
            w();
        };
    })(this);


    //////////////////////////////////////////////////////////////////////
    //////////////////////////////////////////////////////////////////////
    //////////////////////////////////////////////////////////////////////

    // Executes all programs that have been labeled as a main module
    var invokeMains = function(machine, succ, fail) {
        runtime.ready(function() {
            setReadyFalse();
            machine = machine || runtime.currentMachine;
            succ = succ || function() {};
            fail = fail || function() {};
            var mainModules = machine.mainModules.slice();
            var loop = function() {
                if (mainModules.length > 0) {
                    var nextModule = mainModules.shift();
                    nextModule.invoke(machine, loop, fail);
                } else {
                    setReadyTrue();
                    succ();
                }
            };
            setTimeout(loop, 0);
        });
    };

    // Looks up a name in any of the machine's main modules.
    var lookupInMains = function(name, machine) {
        machine = machine || runtime.currentMachine;
        for (var i = 0; i < machine.mainModules.length; i++) {
            var ns = machine.mainModules[i].getNamespace();
            if(ns.hasOwnProperty(name)) {
                return ns[name];
            }
        }
    };



    //////////////////////////////////////////////////////////////////////
    //////////////////////////////////////////////////////////////////////
    //////////////////////////////////////////////////////////////////////


    // Exports
    var exports = runtime;
    exports['currentMachine'] = new Machine();
    exports['invokeMains'] = invokeMains;
    exports['lookupInMains'] = lookupInMains;


    // installing new primitives
    exports['installPrimitiveProcedure'] = installPrimitiveProcedure;
    exports['installPrimitiveClosure'] = installPrimitiveClosure;
    exports['makePrimitiveProcedure'] = makePrimitiveProcedure;
    exports['Primitives'] = Primitives;
    
    exports['ready'] = ready;
    // Private: the runtime library will set this flag to true when
    // the library has finished loading.
    exports['setReadyTrue'] = setReadyTrue;
    exports['setReadyFalse'] = setReadyFalse;

    exports['Machine'] = Machine;
    exports['Frame'] = Frame;
    exports['CallFrame'] = CallFrame;
    exports['PromptFrame'] = PromptFrame;
    exports['Closure'] = Closure;
    exports['ModuleRecord'] = ModuleRecord;
    exports['VariableReference'] = VariableReference;
    exports['ContinuationPromptTag'] = ContinuationPromptTag;
    exports['DEFAULT_CONTINUATION_PROMPT_TAG'] = 
	DEFAULT_CONTINUATION_PROMPT_TAG;
    exports['NULL'] = NULL;
    exports['VOID'] = VOID;

    exports['NEGATIVE_ZERO'] = NEGATIVE_ZERO;
    exports['INF'] = INF;
    exports['NEGATIVE_INF'] = NEGATIVE_INF;
    exports['NAN'] = NAN;





    exports['testArgument'] = testArgument;
    exports['testArity'] = testArity;
    exports['makeCheckArgumentType'] = makeCheckArgumentType;


    exports['raise'] = raise;
    exports['raiseUnboundToplevelError'] = raiseUnboundToplevelError;
    exports['raiseArgumentTypeError'] = raiseArgumentTypeError;
    exports['raiseContextExpectedValuesError'] = raiseContextExpectedValuesError;
    exports['raiseArityMismatchError'] = raiseArityMismatchError;
    exports['raiseOperatorApplicationError'] = raiseOperatorApplicationError;
    exports['raiseOperatorIsNotPrimitiveProcedure'] = raiseOperatorIsNotPrimitiveProcedure;
    exports['raiseOperatorIsNotClosure'] = raiseOperatorIsNotClosure;
    exports['raiseUnimplementedPrimitiveError'] = raiseUnimplementedPrimitiveError;


    exports['captureControl'] = captureControl;
    exports['restoreControl'] = restoreControl;

    exports['trampoline'] = trampoline;
    exports['spliceListIntoStack'] = spliceListIntoStack;
    exports['unspliceRestFromStack'] = unspliceRestFromStack;


    exports['finalizeClosureCall'] = finalizeClosureCall;


    //////////////////////////////////////////////////////////////////////


    // Type constructors

    // numbers
    exports['makeList'] = makeList;
    exports['makePair'] = makePair;
    exports['makeVector'] = makeVector;
    exports['makeBox'] = makeBox;
    exports['makeFloat'] = makeFloat;
    exports['makeRational'] = makeRational;
    exports['makeBignum'] = makeBignum;
    exports['makeComplex'] = makeComplex;
    exports['makeSymbol'] = makeSymbol;


    // Type predicates
    exports['isPair'] = isPair;
    exports['isList'] = isList;
    exports['isVector'] = isVector;
    exports['isOutputPort'] = isOutputPort;
    exports['isOutputStringPort'] = isOutputStringPort;
    exports['isBox'] = isBox;
    exports['isString'] = isString;
    exports['isSymbol'] = isSymbol;
    exports['isNumber'] = isNumber;
    exports['isNatural'] = isNatural;
    exports['isReal'] = isReal;
    exports['equals'] = equals;

    exports['toDomNode'] = toDomNode;
    exports['toWrittenString'] = toWrittenString;
    exports['toDisplayedString'] = toDisplayedString;

    exports['ArityAtLeast'] = plt.baselib.arity.ArityAtLeast;
    exports['makeArityAtLeast'] = plt.baselib.arity.makeArityAtLeast;
    exports['isArityMatching'] = plt.baselib.arity.isArityMatching;

    exports['heir'] = heir;
    exports['makeClassPredicate'] = makeClassPredicate;

    exports['PAUSE'] = PAUSE;
    exports['HaltError'] = HaltError;



    exports['makeStructureType'] = plt.baselib.structs.makeStructureType;
    exports['Struct'] = plt.baselib.structs.Struct;
    exports['StructType'] = plt.baselib.structs.StructType;


})(this['plt']);