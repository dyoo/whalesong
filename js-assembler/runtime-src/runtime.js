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
    var isOutputStringPort = baselib.ports.isOutputStringPort;




    // Exceptions and error handling.
    var raise = baselib.exceptions.raise;
    var raiseUnboundToplevelError = baselib.exceptions.raiseUnboundToplevelError;
    var raiseArgumentTypeError = baselib.exceptions.raiseArgumentTypeError;
    var raiseContextExpectedValuesError = baselib.exceptions.raiseContextExpectedValuesError;
    var raiseArityMismatchError = baselib.exceptions.raiseArityMismatchError;
    var raiseOperatorApplicationError = baselib.exceptions.raiseOperatorApplicationError;
    var raiseOperatorIsNotPrimitiveProcedure = baselib.exceptions.raiseOperatorIsNotPrimitiveProcedure;
    var raiseOperatorIsNotClosure = baselib.exceptions.raiseOperatorIsNotClosure;
    var raiseUnimplementedPrimitiveError = baselib.exceptions.raiseUnimplementedPrimitiveError;


    var ArityAtLeast = baselib.arity.ArityAtLeast;
    var makeArityAtLeast = baselib.arity.makeArityAtLeast;
    var isArityMatching = baselib.arity.isArityMatching;
    

    var testArgument = baselib.check.testArgument;
    var testArity = baselib.check.testArity;
    var makeCheckArgumentType = baselib.check.makeCheckArgumentType;


    var Primitives = baselib.primitives.Primitives;
    var installPrimitiveProcedure = baselib.primitives.installPrimitiveProcedure;



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
        var loop1, loop2;
	loop1 = function loop1(x, y, z, w, k) {
	    // Ensure termination, just in case JavaScript ever
	    // does eliminate stack limits.
	    if (n >= MAXIMUM_CAP) { return; }
	    n++;
	    return 1 + loop2(y, z, w, k, x);
	};
	loop2 = function loop2(x, y, z, w, k) {
	    n++;
	    return 1 + loop1(y, z, w, k, x);
	};
	try {
	    findStackLimit.dontCare = 1 + loop1(2, "seven", [1], {number: 8}, 2);
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



    //////////////////////////////////////////////////////////////////////



    var defaultCurrentPrintImplementation = function defaultCurrentPrintImplementation(MACHINE) {
        if(--MACHINE.callsBeforeTrampoline < 0) { 
            throw defaultCurrentPrintImplementation; 
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
    };
    var defaultCurrentPrint = makeClosure(
	"default-printer",
	1,
	defaultCurrentPrintImplementation);


    //////////////////////////////////////////////////////////////////////]
    // The MACHINE

    var Machine = function() {
	this.callsBeforeTrampoline = STACK_LIMIT_ESTIMATE;
	this.val = undefined;         // value register
	this.proc = undefined;        // procedure register
	this.argcount = undefined;    // argument count
	this.env = [];                // environment
	this.control = [];            // control: Arrayof (U Frame CallFrame PromptFrame)
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

            'currentInspector': baselib.inspectors.DEFAULT_INSPECTOR,
	    
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


    // Save the continuation mark on the top control frame.
    Machine.prototype.installContinuationMarkEntry = function(key, value) {
        var frame = this.control[this.control.length - 1];
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


    Machine.prototype.captureContinuationMarks = function() {
        var kvLists = [];
        var i;
        var control = this.control;
        var tracedCalleeKey = getTracedCalleeKey(this);
        for (i = control.length-1; i >= 0; i--) {
            if (control[i].marks.length !== 0) {
                kvLists.push(control[i].marks);
            }
            
            if (tracedCalleeKey !== null && 
                control[i] instanceof CallFrame &&
                control[i].proc !== null) {
                kvLists.push([[tracedCalleeKey, control[i].proc]]);
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
                return MACHINE.trampoline(f); 
            },
	    0);
    };
    var makeRestartFunction = function(MACHINE) {
        return function(f) { 
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


    Machine.prototype.trampoline = function(initialJump) {
	var thunk = initialJump;
	var startTime = (new Date()).valueOf();
	this.callsBeforeTrampoline = STACK_LIMIT_ESTIMATE;
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
                    this.callsBeforeTrampoline = STACK_LIMIT_ESTIMATE;

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
        setTimeout(
            function() { that.params.currentSuccessHandler(that); },
            0);
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
	new ContinuationPromptTag("default-continuation-prompt-tag");




    



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

}(this.plt, this.plt.baselib));