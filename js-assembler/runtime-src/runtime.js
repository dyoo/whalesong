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
    var makeClosure = plt.baselib.functions.makeClosure;


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
    var checkMutableString = plt.baselib.check.checkMutableString;
    var checkSymbol = plt.baselib.check.checkSymbol;
    var checkByte = plt.baselib.check.checkByte;
    var checkChar = plt.baselib.check.checkChar;
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



    var Primitives = plt.baselib.primitives.Primitives;
    var installPrimitiveProcedure = plt.baselib.primitives.installPrimitiveProcedure;
    var installPrimitiveConstant = plt.baselib.primitives.installPrimitiveConstant;
    var installPrimitiveClosure = plt.baselib.primitives.installPrimitiveClosure;





    // This value will be dynamically determined.
    // See findStackLimit later in this file.
    var STACK_LIMIT_ESTIMATE = 100;

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




    //////////////////////////////////////////////////////////////////////]
    // The MACHINE

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
    


    // captureControl implements the continuation-capturing part of
    // call/cc.  It grabs the control frames up to (but not including) the
    // prompt tagged by the given tag.
    Machine.prototype.captureControl = function(skip, tag) {
	var MACHINE = this;
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
    Machine.prototype.restoreControl = function(tag) {
	var MACHINE = this;
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
    Machine.prototype.spliceListIntoStack = function(depth) {
	var MACHINE = this;
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
    Machine.prototype.unspliceRestFromStack = function(depth, length) {
	var MACHINE = this;
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
    Machine.prototype.trampoline = function(initialJump) {
	var MACHINE = this;
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
                                MACHINE.trampoline(thunk); 
                            },
			    0);
			return;
		    } else {
                        continue;
                    }
		} else if (e instanceof Pause) {
                    var restart = function(thunk) {
		        setTimeout(
			    function() { MACHINE.trampoline(thunk); },
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







    var defaultCurrentPrint = makeClosure(
	"default-printer",
	1,
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
	});





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