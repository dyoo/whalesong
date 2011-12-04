/*jslint browser: true, undef: true, unparam: true, sub: true, vars: true, white: true, plusplus: true, maxerr: 50, indent: 4 */


// runtime.js: the main runtime library for whalesong.
//

// All of the values here are namespaced under "plt.runtime".
/*global $*/
(function(plt, baselib) {
    'use strict';
    var runtime = {};
    plt.runtime = runtime;



    //////////////////////////////////////////////////////////////////////
    //////////////////////////////////////////////////////////////////////
    // We try to isolate the effect of external modules: all the identifiers we
    // pull from external modules should be listed here, and should otherwise not
    // show up outside this section!
    var isNumber = baselib.numbers.isNumber;
    var isNatural = baselib.numbers.isNatural;
    var isReal = baselib.numbers.isReal;
    var isPair = baselib.lists.isPair;
    var isCaarPair = function(x) { return isPair(x) && isPair(x.first); };
    var isList = baselib.lists.isList;
    var isVector = baselib.vectors.isVector;
    var isString = baselib.strings.isString;
    var isSymbol = baselib.symbols.isSymbol;
    var isPath = baselib.paths.isPath;

    var equals = baselib.equality.equals;

    var NULL = baselib.lists.EMPTY;
    var VOID = baselib.constants.VOID_VALUE;

    var NEGATIVE_ZERO = baselib.numbers.negative_zero;
    var INF = baselib.numbers.inf;
    var NEGATIVE_INF = baselib.numbers.negative_inf;
    var NAN = baselib.numbers.nan;

    var makeFloat = baselib.numbers.makeFloat;
    var makeRational = baselib.numbers.makeRational;
    var makeBignum = baselib.numbers.makeBignum;
    var makeComplex = baselib.numbers.makeComplex;


    var makeSymbol = baselib.symbols.makeSymbol;
    var makePath = baselib.paths.makePath;
    var makeBytes = baselib.bytes.makeBytes;

    var makeBox = baselib.boxes.makeBox;
    var isBox = baselib.boxes.isBox;

    var makeVector = baselib.vectors.makeVector;
    var makeList = baselib.lists.makeList;
    var makePair = baselib.lists.makePair;
    var makeChar = baselib.chars.makeChar;

    var makeStructureType = baselib.structs.makeStructureType;


    var Struct = baselib.structs.Struct;
    var StructType = baselib.structs.StructType;

    var Closure = baselib.functions.Closure;
    var finalizeClosureCall = baselib.functions.finalizeClosureCall;
    var makePrimitiveProcedure = baselib.functions.makePrimitiveProcedure;
    var makeClosure = baselib.functions.makeClosure;

    var ContinuationPromptTag = baselib.contmarks.ContinuationPromptTag;


    // Other helpers
    var heir = baselib.heir;
    var makeClassPredicate = baselib.makeClassPredicate;
    var toDomNode = baselib.format.toDomNode;
    var toWrittenString = baselib.format.toWrittenString;
    var toDisplayedString = baselib.format.toDisplayedString;



    // Frame structures.
    var Frame = baselib.frames.Frame;
    var CallFrame = baselib.frames.CallFrame;
    var PromptFrame = baselib.frames.PromptFrame;

    // Module structure
    var ModuleRecord = baselib.modules.ModuleRecord;



    // Ports
    var isOutputPort = baselib.ports.isOutputPort;
    var StandardOutputPort = baselib.ports.StandardOutputPort;
    var StandardErrorPort = baselib.ports.StandardErrorPort;
    var StandardInputPort = baselib.ports.StandardInputPort;
    var isOutputStringPort = baselib.ports.isOutputStringPort;




    // Exceptions and error handling.
    var raise = baselib.exceptions.raise;
    var raiseUnboundToplevelError = baselib.exceptions.raiseUnboundToplevelError;
    var raiseArgumentTypeError = baselib.exceptions.raiseArgumentTypeError;
    var raiseContextExpectedValuesError = baselib.exceptions.raiseContextExpectedValuesError;
    var raiseArityMismatchError = baselib.exceptions.raiseArityMismatchError;
    var raiseOperatorApplicationError = baselib.exceptions.raiseOperatorApplicationError;
    var raiseOperatorIsNotPrimitiveProcedure = baselib.exceptions.raiseOperatorIsNotPrimitiveProcedure;
    var raiseUnimplementedPrimitiveError = baselib.exceptions.raiseUnimplementedPrimitiveError;


    var ArityAtLeast = baselib.arity.ArityAtLeast;
    var makeArityAtLeast = baselib.arity.makeArityAtLeast;
    var isArityMatching = baselib.arity.isArityMatching;
    

    var testArgument = baselib.check.testArgument;
    var testArity = baselib.check.testArity;
    var makeCheckArgumentType = baselib.check.makeCheckArgumentType;


    var Primitives = baselib.primitives.Primitives;
    var installPrimitiveProcedure = baselib.primitives.installPrimitiveProcedure;



    // This value used to be dynamically determined, but something on iOS5
    // breaks badly when I try this.
    // We're very conservative now.
     var STACK_LIMIT_ESTIMATE = 200;



    //////////////////////////////////////////////////////////////////////



    var defaultCurrentPrintImplementation = function (MACHINE) {
        if(--MACHINE.cbt < 0) { 
            throw defaultCurrentPrintImplementation; 
        }
        var oldArgcount = MACHINE.a;

	var elt = MACHINE.e[MACHINE.e.length - 1];
	var outputPort = 
	    MACHINE.params.currentOutputPort;
	if (elt !== VOID) {
	    outputPort.writeDomNode(
                MACHINE, 
                toDomNode(elt, MACHINE.params['print-mode']));
	    outputPort.writeDomNode(MACHINE, toDomNode("\n", 'display'));
	}
        MACHINE.a = oldArgcount;
        return finalizeClosureCall(MACHINE, VOID);
    };
    var defaultCurrentPrint = makeClosure(
	"default-printer",
	1,
	defaultCurrentPrintImplementation);



    //////////////////////////////////////////////////////////////////////

    // Exclusive Locks.  Even though JavaScript is a single-threaded
    // evaluator, we still have a need to create exclusive regions
    // of evaluation, since we might inadvertantly access some state
    // with two computations, with use of setTimeout.
    var ExclusiveLock = function() {
        this.locked = false;  // (U false string)
        this.waiters = [];
    };

    // makeRandomNonce: -> string
    // Creates a randomly-generated nonce.
    ExclusiveLock.makeRandomNonce = function() {
        var chars = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXTZabcdefghiklmnopqrstuvwxyz";
        var LEN = 32;
        var result = [];
        var i;
        for (i = 0; i < LEN; i++) {
            result.push(chars.charAt(Math.floor(Math.random() * chars.length)));
        }
        return result.join('');
    };

    ExclusiveLock.prototype.acquire = function(id, onAcquire) {
        var that = this;
        if (id === undefined) {
            id = ExclusiveLock.makeRandomNonce();
        }
        // Allow for re-entrancy if the id is the same as the
        // entity who is locking.
        if (this.locked === false || this.locked === id) {
            this.locked = id;
            onAcquire.call(
                this,
                // NOTE: the caller must release the lock or else deadlock.
                function() {
                    setTimeout(
                        function() {
                            var waiter;
                            if (that.locked === false) {
                                throw new Error(
                                    "Internal error: trying to unlock the lock, but already unlocked");
                            }
                            that.locked = false;
                            if (that.waiters.length > 0) {
                                waiter = that.waiters.shift();
                                that.acquire(waiter.id, waiter.onAcquire);
                            },
                            0);
                });
        } else {
            this.waiters.push({ id: id, 
                                onAcquire: onAcquire } );
        }
    };
    //////////////////////////////////////////////////////////////////////





    //////////////////////////////////////////////////////////////////////]
    // The MACHINE

    var Machine = function() {
	this.cbt = STACK_LIMIT_ESTIMATE;  // calls before trampoline
	this.v = undefined;         // value register
	this.p = undefined;        // procedure register
	this.a = undefined;           // argument count
	this.e = [];                // environment
	this.c = [];            // control: Arrayof (U Frame CallFrame PromptFrame)
	this.running = false;
	this.modules = {};     // String -> ModuleRecord
        this.mainModules = []; // Arrayof String
	this.params = {

            // print-as-expression: boolean
            'print-as-expression' : false,

            // print-mode: (one-of "write" "print" "constructor")
            'print-mode' : 'write',


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

            'currentInspector': baselib.inspectors.DEFAULT_INSPECTOR,
	    
	    'currentOutputPort': new StandardOutputPort(),
	    'currentErrorPort': new StandardErrorPort(),
            'currentInputPort': new StandardInputPort(),
	    'currentSuccessHandler': function(MACHINE) {},
	    'currentErrorHandler': function(MACHINE, exn) {
                MACHINE.params.currentErrorDisplayer(
                    MACHINE,
                    toDomNode(exn, MACHINE.params['print-mode']));
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
        this.exclusiveLock = new ExclusiveLock();
    };


    // Try to get the continuation mark key used for procedure application tracing.
    var getTracedAppKey = function(MACHINE) {
        if (MACHINE.modules['whalesong/lang/private/traced-app.rkt']) {
            return MACHINE.modules['whalesong/lang/private/traced-app.rkt'].namespace['traced-app-key'];
        }
        return undefined;
    };

    var getTracedCalleeKey = function(MACHINE) {
        if (MACHINE.modules['whalesong/lang/private/traced-app.rkt']) {
            return MACHINE.modules['whalesong/lang/private/traced-app.rkt'].namespace['traced-callee-key'];
        }
        return undefined;
    };



    // captureControl implements the continuation-capturing part of
    // call/cc.  It grabs the control frames up to (but not including) the
    // prompt tagged by the given tag.
    Machine.prototype.captureControl = function(skip, tag) {
	var MACHINE = this;
	var i;
	for (i = MACHINE.c.length - 1 - skip; i >= 0; i--) {
	    if (MACHINE.c[i].tag === tag) {
		return MACHINE.c.slice(i + 1,
					     MACHINE.c.length - skip);
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
	for (i = MACHINE.c.length - 1; i >= 0; i--) {
	    if (MACHINE.c[i].tag === tag) {
		MACHINE.c = 
		    MACHINE.c.slice(0, i+1).concat(
			MACHINE.e[MACHINE.e.length - 1]);
		return;
	    }
	}
	raise(MACHINE, new Error("restoreControl: unable to find tag " + tag));     

    };


    // Splices the list argument in the environment.  Adjusts MACHINE.a
    // appropriately.
    Machine.prototype.spliceListIntoStack = function(depth) {
	var MACHINE = this;
	var lst = MACHINE.e[MACHINE.e.length - 1 - depth];
	var vals = [];
	while(lst !== NULL) {
	    vals.push(lst.first);
	    lst = lst.rest;
	}
	vals.reverse();
	MACHINE.e.splice.apply(MACHINE.e,
				 [MACHINE.e.length - 1 - depth, 1].concat(vals));
	MACHINE.a = MACHINE.a + vals.length - 1;
    };


    // Unsplices a list from the MACHINE stack.
    Machine.prototype.unspliceRestFromStack = function(depth, length) {
	var MACHINE = this;
	var lst = NULL;
	var i;
	for (i = 0; i < length; i++) {
	    lst = makePair(MACHINE.e[MACHINE.e.length - depth - length + i], 
                           lst);
	}
	MACHINE.e.splice(MACHINE.e.length - depth - length,
			   length, 
			   lst);
	MACHINE.a = MACHINE.a - length + 1;
    };


    // Save the continuation mark on the top control frame.
    Machine.prototype.installContinuationMarkEntry = function(key, value) {
        var frame = this.c[this.c.length - 1];
        var marks = frame.marks;
        var i;
        for (i = 0; i < marks.length; i++) {
            if (key === marks[i][0]) {
                marks[i][1] = value;
                return;
            }
        }
        marks.push([key, value]);
    };


    Machine.prototype.captureContinuationMarks = function(promptTag) {
        var kvLists = [];
        var i;
        var control = this.c;
        var tracedCalleeKey = getTracedCalleeKey(this);
        for (i = control.length-1; i >= 0; i--) {
            if (promptTag !== null &&
                control[i] instanceof PromptFrame && control[i].tag === promptTag) {
                break;
            }
            if (control[i].marks.length !== 0) {
                kvLists.push(control[i].marks);
            }
            
            if (tracedCalleeKey !== null && 
                control[i] instanceof CallFrame &&
                control[i].p !== null) {
                kvLists.push([[tracedCalleeKey, control[i].p]]);
            }
        }
        return new baselib.contmarks.ContinuationMarkSet(kvLists);
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
    var recomputeMaxNumBouncesBeforeYield;

    var scheduleTrampoline = function(MACHINE, f) {
        setTimeout(
	    function() { 
 
               // FIXME!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                return MACHINE.trampoline(f); 
            },
	    0);
    };

    // Creates a restarting function, that reschedules f in a context
    // with the old argcount in place. 
    // Meant to be used only by the trampoline.
    var makeRestartFunction = function(MACHINE) {
        var oldArgcount = MACHINE.a;
        return function(f) { 
            MACHINE.a = oldArgcount;
            return scheduleTrampoline(MACHINE, f);
        };
    };


    // These are exception values that are treated specially in the context
    // of the trampoline.

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


    // WARNING WARNING WARNING
    //
    // Make sure to get an exclusive lock before jumping into trampoline.
    // Otherwise, Bad Things will happen.
    //
    // e.g. machine.lock.acquire(function() { machine.trampoline... machine.lock.release();});
    
    Machine.prototype.trampoline = function(initialJump, noJumpingOff) {
	var thunk = initialJump;
	var startTime = (new Date()).valueOf();
	this.cbt = STACK_LIMIT_ESTIMATE;
	this.params.numBouncesBeforeYield = 
	    this.params.maxNumBouncesBeforeYield;
	this.running = true;

	while(true) {
            try {
		thunk(this);
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
                    this.cbt = STACK_LIMIT_ESTIMATE;


                    // If we're running an a model that prohibits
                    // jumping off the trampoline, continue.
                    if (noJumpingOff) {
                        continue;
                    }

		    if (this.params.numBouncesBeforeYield-- < 0) {
			recomputeMaxNumBouncesBeforeYield(
			    this,
			    (new Date()).valueOf() - startTime);
			scheduleTrampoline(this, thunk);
			return;
		    }
		} else if (e instanceof Pause) {
                    var restart = makeRestartFunction(this);
                    e.onPause(restart);
                    return;
                } else if (e instanceof HaltError) {
		    this.running = false;
                    e.onHalt(this);
                    return;
                } else {
		    // General error condition: just exit out
		    // of the trampoline and call the current error handler.
		    this.running = false;
                    this.params.currentErrorHandler(this, e);
	            return;
		}
            }
	}
	this.running = false;
        var that = this;
        this.params.currentSuccessHandler(this);
	return;
    };

    // recomputeGas: state number -> number
    recomputeMaxNumBouncesBeforeYield = function(MACHINE, observedDelay) {
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










    //////////////////////////////////////////////////////////////////////
    //////////////////////////////////////////////////////////////////////
    //////////////////////////////////////////////////////////////////////









    // There is a single, distinguished default continuation prompt tag
    // that's used to wrap around toplevel prompts.
    var DEFAULT_CONTINUATION_PROMPT_TAG =
        baselib.contmarks.DEFAULT_CONTINUATION_PROMPT_TAG;




    



    //////////////////////////////////////////////////////////////////////
    //////////////////////////////////////////////////////////////////////
    //////////////////////////////////////////////////////////////////////

    var VariableReference = function(prefix, pos) {
        this.prefix = prefix;
        this.pos = pos;
    };








    //////////////////////////////////////////////////////////////////////
    //////////////////////////////////////////////////////////////////////
    //////////////////////////////////////////////////////////////////////

    // Implementation of the ready function.  This will fire off when
    // setReadyTrue is called.
    var ready, setReadyTrue, setReadyFalse;
    (function() {
        var runtimeIsReady = true;
        var readyWaiters = [];
        var notifyWaiter = function(w) {
            w();
        };

        ready = function(f) {
            if (runtimeIsReady) {
                notifyWaiter(f);
            } else {
                readyWaiters.push(f);
            }
        };

        setReadyTrue = function() {
            runtimeIsReady = true;
            while(runtimeIsReady && readyWaiters.length > 0) {
                notifyWaiter(readyWaiters.shift());
            }
        };

        setReadyFalse = function() {
            runtimeIsReady = false;
        };

    }());


    //////////////////////////////////////////////////////////////////////
    //////////////////////////////////////////////////////////////////////
    //////////////////////////////////////////////////////////////////////

    // Executes all programs that have been labeled as a main module
    var invokeMains = function(machine, succ, fail) {
        runtime.ready(function () {
            if (window.console && window.console.log) {
                window.console.log("invoking main modules");
            }
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
        var i;
        machine = machine || runtime.currentMachine;
        for (i = 0; i < machine.mainModules.length; i++) {
            var ns = machine.mainModules[i].getNamespace();
            if(ns.hasOwnProperty(name)) {
                return ns[name];
            }
        }
    };



    var checkClosureAndArity = function(M) {
        if(!(M.p instanceof Closure)){
            raiseOperatorApplicationError(M,M.p);
        }
        if(!isArityMatching(M.p.racketArity,M.a)) {
            raiseArityMismatchError(M,M.p,M.a);
        }
    };

    var checkPrimitiveArity = function(M) {
        if(!isArityMatching(M.p.racketArity,M.a)) {
            raiseArityMismatchError(M,M.p,M.a);
        }
    };


    //////////////////////////////////////////////////////////////////////
    // Superinstructions to try to reduce code size.
    var si_context_expected = function(n) {
        if (n === 1) { return si_context_expected_1; }
        var f = function(M) { raiseContextExpectedValuesError(M, n); };
        return f;
    };
    var si_context_expected_1 = function(M) { raiseContextExpectedValuesError(M, 1); }








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
    exports['raiseUnimplementedPrimitiveError'] = raiseUnimplementedPrimitiveError;


    exports['finalizeClosureCall'] = finalizeClosureCall;


    //////////////////////////////////////////////////////////////////////


    // Type constructors

    // numbers
    exports['makeList'] = makeList;
    exports['makePair'] = makePair;
    exports['makeChar'] = makeChar;
    exports['makeVector'] = makeVector;
    exports['makeBox'] = makeBox;
    exports['makeFloat'] = makeFloat;
    exports['makeRational'] = makeRational;
    exports['makeBignum'] = makeBignum;
    exports['makeComplex'] = makeComplex;
    exports['makeSymbol'] = makeSymbol;
    exports['makePath'] = makePath;
    exports['makeBytes'] = makeBytes;


    exports['checkPair'] = baselib.check.checkPair;
    exports['checkNumber'] = baselib.check.checkNumber;
    exports['checkString'] = baselib.check.checkString;



    // Type predicates
    exports['isPair'] = isPair;
    exports['isCaarPair'] = isCaarPair;
    exports['isList'] = isList;
    exports['isVector'] = isVector;
    exports['isOutputPort'] = isOutputPort;
    exports['isOutputStringPort'] = isOutputStringPort;
    exports['isBox'] = isBox;
    exports['isString'] = isString;
    exports['isSymbol'] = isSymbol;
    exports['isPath'] = isPath;
    exports['isNumber'] = isNumber;
    exports['isNatural'] = isNatural;
    exports['isReal'] = isReal;
    exports['isProcedure'] = plt.baselib.functions.isProcedure;
    exports['equals'] = equals;

    exports['toDomNode'] = toDomNode;
    exports['toWrittenString'] = toWrittenString;
    exports['toDisplayedString'] = toDisplayedString;

    exports['ArityAtLeast'] = ArityAtLeast;
    exports['makeArityAtLeast'] = makeArityAtLeast;
    exports['isArityMatching'] = isArityMatching;

    exports['heir'] = heir;
    exports['makeClassPredicate'] = makeClassPredicate;

    exports['PAUSE'] = PAUSE;
    exports['HaltError'] = HaltError;



    exports['makeStructureType'] = makeStructureType;
    exports['Struct'] = Struct;
    exports['StructType'] = StructType;

    exports['getTracedAppKey'] = getTracedAppKey;
    exports['getTracedCalleeKey'] = getTracedCalleeKey;

    exports['si_context_expected'] = si_context_expected;
    exports['si_context_expected_1'] = si_context_expected_1;
    exports['checkClosureAndArity'] = checkClosureAndArity;
    exports['checkPrimitiveArity'] = checkPrimitiveArity;

}(this.plt, this.plt.baselib));