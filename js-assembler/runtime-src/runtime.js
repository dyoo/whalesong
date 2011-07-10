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






    //////////////////////////////////////////////////////////////////////]






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
	var MAXIMUM_CAP = 100000;
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








    var defaultCurrentPrint = new Closure(
	function(MACHINE) {
            if(--MACHINE.callsBeforeTrampoline < 0) { 
                throw arguments.callee; 
            }
	    var elt = MACHINE.env[MACHINE.env.length - 1];
	    var outputPort = 
		MACHINE.params.currentOutputPort;
	    if (elt !== VOID) {
		outputPort.writeDomNode(MACHINE, toDomNode(elt, 'print'));
		outputPort.writeDomNode(MACHINE, toDomNode("\n", 'display'));
	    }
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


    installPrimitiveProcedure(
        'display', makeList(1, 2),
        function(MACHINE) {
	    var firstArg = MACHINE.env[MACHINE.env.length-1];
	    var outputPort = MACHINE.params.currentOutputPort;
	    if (MACHINE.argcount === 2) {
	        testArgument(MACHINE,
			     'output-port', 
			     isOutputPort, 
			     MACHINE.env.length-2,
			     1,
			     'display');
	        outputPort = MACHINE.env[MACHINE.env.length-2];
	    }
	    outputPort.writeDomNode(MACHINE, toDomNode(firstArg, 'display'));
            return VOID;
        });

    installPrimitiveProcedure(
        'newline', makeList(0, 1),
        function(MACHINE) {
	    var outputPort = MACHINE.params.currentOutputPort;
	    if (MACHINE.argcount === 1) { 
	        testArgument(MACHINE,
			     'output-port', 
			     isOutputPort, 
			     MACHINE.env.length-1,
			     1,
			     'newline');
	        outputPort = MACHINE.env[MACHINE.env.length-1];
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
	        testArgument(MACHINE,
			     'output-port', 
			     isOutputPort, 
			     MACHINE.env.length-2,
			     1,
			     'displayln'); 
	        outputPort = MACHINE.env[MACHINE.env.length-2];
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
            testArgument(MACHINE,
                         'string',
                         isString,
                         MACHINE.env[MACHINE.env.length-1],
                         0,
                         'format');
            for(i = 0; i < MACHINE.argcount; i++) {
                args.push(MACHINE.env[MACHINE.env.length - 1 - i]);
            }
            formatString = args.shift();
            return plt.baselib.format.format(formatString, args, 'format');
        });



    installPrimitiveProcedure(
        'printf',
        plt.baselib.arity.makeArityAtLeast(1),
        function(MACHINE) {
            var args = [], i, formatString;
            testArgument(MACHINE,
                         'string',
                         isString,
                         MACHINE.env[MACHINE.env.length-1],
                         0,
                         'printf');
            for(i = 0; i < MACHINE.argcount; i++) {
                args.push(MACHINE.env[MACHINE.env.length - 1 - i]);
            }
            formatString = args.shift();
            var result = plt.baselib.format.format(formatString, args, 'format');
            var outputPort = MACHINE.params.currentOutputPort;            
	    outputPort.writeDomNode(MACHINE, toDomNode(result, 'display'));
            return VOID;
        });


    installPrimitiveProcedure(
        'fprintf',
        plt.baselib.arity.makeArityAtLeast(2),
        function(MACHINE) {
            var args = [], i, formatString;
            testArgument(MACHINE,
                         'output-port',
                         isOutputPort,
                         MACHINE.env[MACHINE.env.length-1],
                         0,
                         'fprintf');
            testArgument(MACHINE,
                         'string',
                         isString,
                         MACHINE.env[MACHINE.env.length-2],
                         1,
                         'fprintf');
            for(i = 1; i < MACHINE.argcount; i++) {
                args.push(MACHINE.env[MACHINE.env.length - 1 - i]);
            }
            formatString = args.shift();
            var result = plt.baselib.format.format(formatString, args, 'format');
            var outputPort = MACHINE.env[MACHINE.env.length-1];
	    outputPort.writeDomNode(MACHINE, toDomNode(result, 'display'));
            return VOID;
        });






    installPrimitiveProcedure(
        'current-print',
        makeList(0, 1),
        function(MACHINE) {
            if (MACHINE.argcount === 1) {
                MACHINE.params['currentPrint'] = MACHINE.env[MACHINE.env.length - 1];
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
                MACHINE.params['currentOutputPort'] = MACHINE.env[MACHINE.env.length - 1];
                return VOID;
            } else {
	        return MACHINE.params['currentOutputPort'];
            }
        });





    installPrimitiveProcedure(
        '=',
        plt.baselib.arity.makeArityAtLeast(2),
        function(MACHINE) {
	    var firstArg = MACHINE.env[MACHINE.env.length-1];
	    testArgument(MACHINE, 'number', isNumber, firstArg, 0, '=');
	    for (var i = 0; i < MACHINE.argcount - 1; i++) {
	        testArgument(MACHINE, 
			     'number',
			     isNumber, 
			     MACHINE.env[MACHINE.env.length - 1 - i],
			     i,
			     '=');
	        if (! (plt.baselib.numbers.equals(
                    MACHINE.env[MACHINE.env.length - 1 - i],
		    MACHINE.env[MACHINE.env.length - 1 - i - 1]))) {
		    return false; 
	        }
	    }
	    return true;
        });


    
    installPrimitiveProcedure(
        '=~',
        3,
        function(MACHINE) {
	    testArgument(MACHINE,
		         'real',
		         isReal,
		         MACHINE.env[MACHINE.env.length - 1],
		         0,
		         '=~');
            testArgument(MACHINE,
		         'real',
		         isReal,
		         MACHINE.env[MACHINE.env.length - 2],
		         1,
		         '=~');
            testArgument(MACHINE,
		         'nonnegative real',
		         isNonNegativeReal,
		         MACHINE.env[MACHINE.env.length - 3],
		         2,
		         '=~');
	    var x = MACHINE.env[MACHINE.env.length-1];
	    var y = MACHINE.env[MACHINE.env.length-2];
	    var range = MACHINE.env[MACHINE.env.length-3];
            return plt.baselib.numbers.lessThanOrEqual(plt.baselib.numbers.abs(plt.baselib.numbers.subtract(x, y)), range);
        });




    installPrimitiveProcedure(
        '<',
        plt.baselib.arity.makeArityAtLeast(2),
        function(MACHINE) {
	    var firstArg = MACHINE.env[MACHINE.env.length-1];
	    testArgument(MACHINE,
		         'number', isNumber, firstArg, 0, '<');
	    for (var i = 0; i < MACHINE.argcount - 1; i++) {
	        testArgument(MACHINE, 
			     'number',
			     isNumber, 
			     MACHINE.env[MACHINE.env.length - 1 - i],
			     i,
			     '<');
	        if (! (plt.baselib.numbers.lessThan(MACHINE.env[MACHINE.env.length - 1 - i],
		                       MACHINE.env[MACHINE.env.length - 1 - i - 1]))) {
		    return false; 
	        }
	    }
	    return true;
        });


    installPrimitiveProcedure(
        '>',
        plt.baselib.arity.makeArityAtLeast(2),
        function(MACHINE) {
	    var firstArg = MACHINE.env[MACHINE.env.length-1];
	    testArgument(MACHINE,
		         'number', isNumber, firstArg, 0, '>');
	    for (var i = 0; i < MACHINE.argcount - 1; i++) {
	        testArgument(MACHINE,
			     'number',
			     isNumber, 
			     MACHINE.env[MACHINE.env.length - 1 - i],
			     i,
			     '>');
	        if (! (plt.baselib.numbers.greaterThan(MACHINE.env[MACHINE.env.length - 1 - i],
		                          MACHINE.env[MACHINE.env.length - 1 - i - 1]))) {
		    return false; 
	        }
	    }
	    return true;
        });

    installPrimitiveProcedure(
        '<=',
        plt.baselib.arity.makeArityAtLeast(2),
        function(MACHINE) {
	    var firstArg = MACHINE.env[MACHINE.env.length-1];
	    testArgument(MACHINE,
		         'number', isNumber, firstArg, 0, '<=');
	    for (var i = 0; i < MACHINE.argcount - 1; i++) {
	        testArgument(MACHINE,
			     'number',
			     isNumber, 
			     MACHINE.env[MACHINE.env.length - 1 - i],
			     i,
			     '<=');
	        if (! (plt.baselib.numbers.lessThanOrEqual(MACHINE.env[MACHINE.env.length - 1 - i],
		                              MACHINE.env[MACHINE.env.length - 1 - i - 1]))) {
		    return false; 
	        }
	    }
	    return true;
        });


    installPrimitiveProcedure(
        '>=',
        plt.baselib.arity.makeArityAtLeast(2),
        function(MACHINE) {
	    var firstArg = MACHINE.env[MACHINE.env.length-1];
	    testArgument(MACHINE,
		         'number', isNumber, firstArg, 0, '>=');
	    for (var i = 0; i < MACHINE.argcount - 1; i++) {
	        testArgument(MACHINE, 
			     'number',
			     isNumber, 
			     MACHINE.env[MACHINE.env.length - 1 - i],
			     i,
			     '>=');
	        if (! (plt.baselib.numbers.greaterThanOrEqual(MACHINE.env[MACHINE.env.length - 1 - i],
		                                 MACHINE.env[MACHINE.env.length - 1 - i - 1]))) {
		    return false; 
	        }
	    }
	    return true;
        });
    

    installPrimitiveProcedure(
        '+',
        plt.baselib.arity.makeArityAtLeast(0),
        function(MACHINE) {
	    var result = 0;
	    var i = 0;
	    for (i=0; i < MACHINE.argcount; i++) {
	        testArgument(MACHINE,
			     'number',
			     isNumber, 
			     MACHINE.env[MACHINE.env.length - 1 - i],
			     i,
			     '+');
	        result = plt.baselib.numbers.add(result, MACHINE.env[MACHINE.env.length - 1 - i]);
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
	        testArgument(MACHINE,
			     'number',
			     isNumber, 
			     MACHINE.env[MACHINE.env.length - 1 - i],
			     i,
			     '*');
	        result = plt.baselib.numbers.multiply(result, MACHINE.env[MACHINE.env.length - 1 - i]);
	    }
	    return result;
        });

    installPrimitiveProcedure(
        '-',
        plt.baselib.arity.makeArityAtLeast(1),
        function(MACHINE) {
	    if (MACHINE.argcount === 1) { 
	        testArgument(MACHINE,
			     'number',
			     isNumber,
			     MACHINE.env[MACHINE.env.length-1],
			     0,
			     '-');
	        return plt.baselib.numbers.subtract(0, MACHINE.env[MACHINE.env.length-1]);
	    }
	    var result = MACHINE.env[MACHINE.env.length - 1];
	    for (var i = 1; i < MACHINE.argcount; i++) {
	        testArgument(MACHINE,
			     'number',
			     isNumber,
			     MACHINE.env[MACHINE.env.length-1-i],
			     i,
			     '-');
	        result = plt.baselib.numbers.subtract(result, MACHINE.env[MACHINE.env.length - 1 - i]);
	    }
	    return result;
        });
    
    installPrimitiveProcedure(
        '/',
        plt.baselib.arity.makeArityAtLeast(1),
        function(MACHINE) {
	    testArgument(MACHINE,
		         'number',
		         isNumber,
		         MACHINE.env[MACHINE.env.length - 1],
		         0,
		         '/');
	    var result = MACHINE.env[MACHINE.env.length - 1];
	    for (var i = 1; i < MACHINE.argcount; i++) {
	        testArgument(MACHINE,
			     'number',
			     isNumber,
			     MACHINE.env[MACHINE.env.length-1-i],
			     i,
			     '/');
	        result = plt.baselib.numbers.divide(result, MACHINE.env[MACHINE.env.length - 1 - i]);
	    }
	    return result;
        });
    

    installPrimitiveProcedure(
        'add1',
        1,
        function(MACHINE) {
	    testArgument(MACHINE,
		         'number',
		         isNumber,
		         MACHINE.env[MACHINE.env.length - 1],
		         0,
		         'add1');
	    var firstArg = MACHINE.env[MACHINE.env.length-1];
	    return plt.baselib.numbers.add(firstArg, 1);
        });


    installPrimitiveProcedure(
        'sub1',
        1,
        function(MACHINE) {
	    testArgument(MACHINE,
		         'number',
		         isNumber,
		         MACHINE.env[MACHINE.env.length - 1],
		         0,
		         'sub1');
	    var firstArg = MACHINE.env[MACHINE.env.length-1];
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
        'car',
        1,
        function(MACHINE) {
	    testArgument(MACHINE, 
		         'pair',
		         isPair,
		         MACHINE.env[MACHINE.env.length - 1],
		         0,
		         'car');
	    var firstArg = MACHINE.env[MACHINE.env.length-1];
	    return firstArg.first;
        });

    installPrimitiveProcedure(
        'cdr',
        1,
        function(MACHINE) {
	    testArgument(MACHINE,
		         'pair',
		         isPair,
		         MACHINE.env[MACHINE.env.length - 1],
		         0,
		         'cdr');
	    var firstArg = MACHINE.env[MACHINE.env.length-1];
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
	    testArgument(MACHINE,
		         'pair',
		         isPair,
		         MACHINE.env[MACHINE.env.length - 1],
		         0,
		         'set-car!');
	    var firstArg = MACHINE.env[MACHINE.env.length-1];
	    var secondArg = MACHINE.env[MACHINE.env.length-2];
	    firstArg.first = secondArg;
            return VOID;
        });


    installPrimitiveProcedure(
        'set-cdr!',
        2,
        function(MACHINE) {
	    testArgument(MACHINE,
		         'pair',
		         isPair,
		         MACHINE.env[MACHINE.env.length - 1],
		         0,
		         'set-cdr!');
	    var firstArg = MACHINE.env[MACHINE.env.length-1];
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
	    testArgument(MACHINE,
		         'vector',
		         isVector,
		         MACHINE.env[MACHINE.env.length - 1],
		         0,
		         'vector->list');
	    var elts = MACHINE.env[MACHINE.env.length-1].elts;
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
	    var firstArg = MACHINE.env[MACHINE.env.length-1];
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
	    testArgument(MACHINE,
		         'vector',
		         isVector,
		         MACHINE.env[MACHINE.env.length - 1],
		         0,
		         'vector-ref');
	    var elts = MACHINE.env[MACHINE.env.length-1].elts;
	    var index = MACHINE.env[MACHINE.env.length-2];
	    return elts[index];
        });


    installPrimitiveProcedure(
        'vector-set!',
        3,
        function(MACHINE) {
	    testArgument(MACHINE,
		         'vector',
		         isVector,
		         MACHINE.env[MACHINE.env.length - 1],
		         0,
		         'vector-set!');
	    testArgument(MACHINE,
		         'natural',
		         isNatural,
		         MACHINE.env[MACHINE.env.length - 2],
		         1,
		         'vector-set!');
	    var elts = MACHINE.env[MACHINE.env.length-1].elts;
	    var index = plt.baselib.numbers.toFixnum(MACHINE.env[MACHINE.env.length-2]);
	    var val = MACHINE.env[MACHINE.env.length-3];
	    elts[index] = val;
	    return VOID;
        });

    installPrimitiveProcedure(
        'vector-length',
        1,
        function(MACHINE) {
	    testArgument(MACHINE,
		         'vector',
		         isVector,
		         MACHINE.env[MACHINE.env.length - 1],
		         0,
		         'vector-length');
	    var firstArg = MACHINE.env[MACHINE.env.length-1].elts;
	    return firstArg.length;
        });


    installPrimitiveProcedure(
        'make-vector',
        makeList(1, 2),
        function(MACHINE) {
	    var value = 0;
	    testArgument(MACHINE,
		         'natural',
		         isNatural,
		         MACHINE.env[MACHINE.env.length - 1],
		         0,
		         'make-vector');
	    if (MACHINE.argcount == 2) {
	        value = MACHINE.env[MACHINE.env.length - 2];
	    }
	    var length = plt.baselib.numbers.toFixnum(MACHINE.env[MACHINE.env.length-1]);
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
	    return typeof(firstArg) === 'string';
        });

    installPrimitiveProcedure(
        'symbol->string',
        1,
        function(MACHINE) {
	    var firstArg = MACHINE.env[MACHINE.env.length-1];
	    return firstArg;
        });

    installPrimitiveProcedure(
        'string-append',
        plt.baselib.arity.makeArityAtLeast(0),
        function(MACHINE) {
	    var buffer = [];
	    var i;
	    for (i = 0; i < MACHINE.argcount; i++) {
	        buffer.push(MACHINE.env[MACHINE.env.length - 1 - i]);
	    }
	    return buffer.join('');
        });

    installPrimitiveProcedure(
        'string-length',
        1,
        function(MACHINE) {
	    var firstArg = MACHINE.env[MACHINE.env.length-1];
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
	    var firstArg = MACHINE.env[MACHINE.env.length-1];
            // FIXME: typecheck for box
            return firstArg.ref();
        });

    installPrimitiveProcedure(
        'set-box!',
        2,
        function(MACHINE) {
	    var firstArg = MACHINE.env[MACHINE.env.length-1];
	    var secondArg = MACHINE.env[MACHINE.env.length-2];
            // FIXME: typecheck for box
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
	        if (! isList(lst)) {
		    raise(MACHINE, new Error("member: expected list" 
					     + " as argument #2"
					     + " but received " + originalLst + " instead"));
	        }
	        if (lst === NULL) {
		    return false;
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
            testArgument(MACHINE,
                         'number',
                         isNumber,
                         MACHINE.env[MACHINE.env.length-1],
                         0,
                         'abs');
            return plt.baselib.numbers.abs(MACHINE.env[MACHINE.env.length-1]);
        });

    installPrimitiveProcedure(
        'acos',
        1,
        function(MACHINE) {
            testArgument(MACHINE,
                         'number',
                         isNumber,
                         MACHINE.env[MACHINE.env.length-1],
                         0,
                         'acos');
            return plt.baselib.numbers.acos(MACHINE.env[MACHINE.env.length-1]);
        });


    installPrimitiveProcedure(
        'asin',
        1,
        function(MACHINE) {
            testArgument(MACHINE,
                         'number',
                         isNumber,
                         MACHINE.env[MACHINE.env.length-1],
                         0,
                         'asin');
            return plt.baselib.numbers.asin(MACHINE.env[MACHINE.env.length-1]);
        });

    installPrimitiveProcedure(
        'sin',
        1,
        function(MACHINE) {
            testArgument(MACHINE,
                         'number',
                         isNumber,
                         MACHINE.env[MACHINE.env.length-1],
                         0,
                         'sin');
            return plt.baselib.numbers.sin(MACHINE.env[MACHINE.env.length-1]);
        });



    installPrimitiveProcedure(
        'sinh',
        1,
        function(MACHINE) {
            testArgument(MACHINE,
                         'number',
                         isNumber,
                         MACHINE.env[MACHINE.env.length-1],
                         0,
                         'sinh');
            return plt.baselib.numbers.sinh(MACHINE.env[MACHINE.env.length-1]);
        });


    installPrimitiveProcedure(
        'tan',
        1,
        function(MACHINE) {
            testArgument(MACHINE,
                         'number',
                         isNumber,
                         MACHINE.env[MACHINE.env.length-1],
                         0,
                         'tan');
            return plt.baselib.numbers.tan(MACHINE.env[MACHINE.env.length-1]);
        });

    

    installPrimitiveProcedure(
        'atan',
        makeList(1, 2),
        function(MACHINE) {
            if (MACHINE.argcount === 1) {
                testArgument(MACHINE,
                             'number',
                             isNumber,
                             MACHINE.env[MACHINE.env.length - 1],
                             0,
                             'atan');
		return plt.baselib.numbers.atan(MACHINE.env[MACHINE.env.length - 1]);
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
		    Math.atan2(plt.baselib.numbers.toFixnum(MACHINE.env[MACHINE.env.length - 1]),
			       plt.baselib.numbers.toFixnum(MACHINE.env[MACHINE.env.length - 2])));
            }
        });


    installPrimitiveProcedure(
        'angle',
        1,
        function(MACHINE) {
            testArgument(MACHINE,
                         'number',
                         isNumber,
                         MACHINE.env[MACHINE.env.length-1],
                         0,
                         'angle');
            return plt.baselib.numbers.angle(MACHINE.env[MACHINE.env.length-1]);
        });

    installPrimitiveProcedure(
        'magnitude',
        1,
        function(MACHINE) {
            testArgument(MACHINE,
                         'number',
                         isNumber,
                         MACHINE.env[MACHINE.env.length-1],
                         0,
                         'magnitude');
            return plt.baselib.numbers.magnitude(MACHINE.env[MACHINE.env.length-1]);
        });

    installPrimitiveProcedure(
        'conjugate',
        1,
        function(MACHINE) {
            testArgument(MACHINE,
                         'number',
                         isNumber,
                         MACHINE.env[MACHINE.env.length-1],
                         0,
                         'conjugate');
            return plt.baselib.numbers.conjugate(MACHINE.env[MACHINE.env.length-1]);
        });




    installPrimitiveProcedure(
        'cos',
        1,
        function(MACHINE) {
            testArgument(MACHINE,
                         'number',
                         isNumber,
                         MACHINE.env[MACHINE.env.length-1],
                         0,
                         'cos');
            return plt.baselib.numbers.cos(MACHINE.env[MACHINE.env.length-1]);
        });


    installPrimitiveProcedure(
        'cosh',
        1,
        function(MACHINE) {
            testArgument(MACHINE,
                         'number',
                         isNumber,
                         MACHINE.env[MACHINE.env.length-1],
                         0,
                         'cosh');
            return plt.baselib.numbers.cosh(MACHINE.env[MACHINE.env.length-1]);
        });

    installPrimitiveProcedure(
        'gcd',
        plt.baselib.arity.makeArityAtLeast(1),
        function(MACHINE) {
            var args = [], i, x;
            for (i = 0; i < MACHINE.argcount; i++) {
                testArgument(MACHINE,
                             'integer',
                             plt.baselib.numbers.isInteger,
                             MACHINE.env[MACHINE.env.length - 1 - i],
                             i,
                             'gcd');
                args.push(MACHINE.env[MACHINE.env.length - 1 - i]);

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
                testArgument(MACHINE,
                             'integer',
                             plt.baselib.numbers.isInteger,
                             MACHINE.env[MACHINE.env.length - 1 - i],
                             i,
                             'lcm');
                args.push(MACHINE.env[MACHINE.env.length - 1 - i]);

            }
            x = args.shift();
	    return plt.baselib.numbers.lcm(x, args);
        });




    installPrimitiveProcedure(
        'exp',
        1,
        function(MACHINE) {
            testArgument(MACHINE,
                         'number',
                         isNumber,
                         MACHINE.env[MACHINE.env.length-1],
                         0,
                         'exp');
            return plt.baselib.numbers.exp(MACHINE.env[MACHINE.env.length-1]);
        });


    installPrimitiveProcedure(
        'expt',
        2,
        function(MACHINE) {
            testArgument(MACHINE,
                         'number',
                         isNumber,
                         MACHINE.env[MACHINE.env.length - 1],
                         0,
                         'expt');
            testArgument(MACHINE,
                         'number',
                         isNumber,
                         MACHINE.env[MACHINE.env.length - 2],
                         1,
                         'expt');
            return plt.baselib.numbers.expt(MACHINE.env[MACHINE.env.length - 1],
                               MACHINE.env[MACHINE.env.length - 2]);
        });


    installPrimitiveProcedure(
        'exact?',
        1,
        function(MACHINE) {
            testArgument(MACHINE,
                         'number',
                         isNumber,
                         MACHINE.env[MACHINE.env.length - 1],
                         0,
                         'exact?');
            return plt.baselib.numbers.isExact(MACHINE.env[MACHINE.env.length - 1]);
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
            testArgument(MACHINE,
                         'number',
                         isNumber,
                         MACHINE.env[MACHINE.env.length - 1],
                         0,
                         'imag-part');
            return plt.baselib.numbers.imaginaryPart(MACHINE.env[MACHINE.env.length - 1]);
        });

    installPrimitiveProcedure(
        'real-part',
        1,
        function(MACHINE) {
            testArgument(MACHINE,
                         'number',
                         isNumber,
                         MACHINE.env[MACHINE.env.length - 1],
                         0,
                         'real-part');
            return plt.baselib.numbers.realPart(MACHINE.env[MACHINE.env.length - 1]);
        });



    installPrimitiveProcedure(
        'make-polar',
        2,
        function(MACHINE) {
            testArgument(MACHINE,
                         'real',
                         isReal,
                         MACHINE.env[MACHINE.env.length - 1],
                         0,
                         'make-polar');
            testArgument(MACHINE,
                         'real',
                         isReal,
                         MACHINE.env[MACHINE.env.length - 2],
                         1,
                         'make-polar');
            return plt.baselib.numbers.makeComplexPolar(MACHINE.env[MACHINE.env.length - 1],
                                           MACHINE.env[MACHINE.env.length - 2]);
        });


    installPrimitiveProcedure(
        'make-rectangular',
        2,
        function(MACHINE) {
            testArgument(MACHINE,
                         'real',
                         isReal,
                         MACHINE.env[MACHINE.env.length - 1],
                         0,
                         'make-rectangular');
            testArgument(MACHINE,
                         'real',
                         isReal,
                         MACHINE.env[MACHINE.env.length - 2],
                         1,
                         'make-rectangular');
            return plt.baselib.numbers.makeComplex(MACHINE.env[MACHINE.env.length - 1],
                                      MACHINE.env[MACHINE.env.length - 2]);
        });

    installPrimitiveProcedure(
        'modulo',
        2,
        function(MACHINE) {
            testArgument(MACHINE,
                         'integer',
                         plt.baselib.numbers.isInteger,
                         MACHINE.env[MACHINE.env.length - 1],
                         0,
                         'modulo');
            testArgument(MACHINE,
                         'integer',
                         plt.baselib.numbers.isInteger,
                         MACHINE.env[MACHINE.env.length - 2],
                         1,
                         'modulo');
            return plt.baselib.numbers.modulo(MACHINE.env[MACHINE.env.length - 1],
                                 MACHINE.env[MACHINE.env.length - 2]);
        });


    installPrimitiveProcedure(
        'remainder',
        2,
        function(MACHINE) {
            testArgument(MACHINE,
                         'integer',
                         plt.baselib.numbers.isInteger,
                         MACHINE.env[MACHINE.env.length - 1],
                         0,
                         'remainder');
            testArgument(MACHINE,
                         'integer',
                         plt.baselib.numbers.isInteger,
                         MACHINE.env[MACHINE.env.length - 2],
                         1,
                         'remainder');
            return plt.baselib.numbers.remainder(MACHINE.env[MACHINE.env.length - 1],
                                    MACHINE.env[MACHINE.env.length - 2]);
        });


    installPrimitiveProcedure(
        'quotient',
        2,
        function(MACHINE) {
            testArgument(MACHINE,
                         'integer',
                         plt.baselib.numbers.isInteger,
                         MACHINE.env[MACHINE.env.length - 1],
                         0,
                         'quotient');
            testArgument(MACHINE,
                         'integer',
                         plt.baselib.numbers.isInteger,
                         MACHINE.env[MACHINE.env.length - 2],
                         1,
                         'quotient');
            return plt.baselib.numbers.quotient(MACHINE.env[MACHINE.env.length - 1],
                                   MACHINE.env[MACHINE.env.length - 2]);
        });



    installPrimitiveProcedure(
        'floor',
        1,
        function(MACHINE) {
            testArgument(MACHINE,
                         'real',
                         isReal,
                         MACHINE.env[MACHINE.env.length - 1],
                         0,
                         'floor');
            return plt.baselib.numbers.floor(MACHINE.env[MACHINE.env.length - 1]);
        });
    

    installPrimitiveProcedure(
        'ceiling',
        1,
        function(MACHINE) {
            testArgument(MACHINE,
                         'real',
                         isReal,
                         MACHINE.env[MACHINE.env.length - 1],
                         0,
                         'ceiling');
            return plt.baselib.numbers.ceiling(MACHINE.env[MACHINE.env.length - 1]);
        });
   

    installPrimitiveProcedure(
        'round',
        1,
        function(MACHINE) {
            testArgument(MACHINE,
                         'real',
                         isReal,
                         MACHINE.env[MACHINE.env.length - 1],
                         0,
                         'round');
            return plt.baselib.numbers.round(MACHINE.env[MACHINE.env.length - 1]);
        });
    

    installPrimitiveProcedure(
        'truncate',
        1,
        function(MACHINE) {
            testArgument(MACHINE,
                         'real',
                         isReal,
                         MACHINE.env[MACHINE.env.length - 1],
                         0,
                         'truncate');
	    if (plt.baselib.numbers.lessThan(MACHINE.env[MACHINE.env.length - 1], 0)) {
		return plt.baselib.numbers.ceiling(MACHINE.env[MACHINE.env.length - 1]);
	    } else {
		return plt.baselib.numbers.floor(MACHINE.env[MACHINE.env.length - 1]);
	    }
        });
    

    installPrimitiveProcedure(
        'numerator',
        1,
        function(MACHINE) {
            testArgument(MACHINE,
                         'rational',
                         plt.baselib.numbers.isRational,
                         MACHINE.env[MACHINE.env.length-1],
                         0,
                         'numerator');
            return plt.baselib.numbers.numerator(MACHINE.env[MACHINE.env.length-1]);
        });

    installPrimitiveProcedure(
        'denominator',
        1,
        function(MACHINE) {
            testArgument(MACHINE,
                         'rational',
                         plt.baselib.numbers.isRational,
                         MACHINE.env[MACHINE.env.length-1],
                         0,
                         'denominator');
            return plt.baselib.numbers.denominator(MACHINE.env[MACHINE.env.length-1]);
        });


    installPrimitiveProcedure(
        'log',
        1,
        function(MACHINE) {
            testArgument(MACHINE,
                         'number',
                         isNumber,
                         MACHINE.env[MACHINE.env.length-1],
                         0,
                         'log');
            return plt.baselib.numbers.log(MACHINE.env[MACHINE.env.length-1]);
        });


    installPrimitiveProcedure(
        'sqr',
        1,
        function(MACHINE) {
            testArgument(MACHINE,
                         'number',
                         isNumber,
                         MACHINE.env[MACHINE.env.length-1],
                         0,
                         'sqr');
            return plt.baselib.numbers.sqr(MACHINE.env[MACHINE.env.length-1]);
        });




    installPrimitiveProcedure(
        'sqrt',
        1,
        function(MACHINE) {
            testArgument(MACHINE,
                         'number',
                         isNumber,
                         MACHINE.env[MACHINE.env.length-1],
                         0,
                         'sqrt');
            return plt.baselib.numbers.sqrt(MACHINE.env[MACHINE.env.length-1]);
        });




    installPrimitiveProcedure(
        'integer-sqrt',
        1,
        function(MACHINE) {
            testArgument(MACHINE,
                         'integer',
                         plt.baselib.numbers.isInteger,
                         MACHINE.env[MACHINE.env.length-1],
                         0,
                         'integer-sqrt');
            return plt.baselib.numbers.integerSqrt(MACHINE.env[MACHINE.env.length-1]);
        });


    // rawSgn: number -> number
    var rawSgn = function(x) {
        if (plt.baselib.numbers.isInexact(x)) {
	    if ( plt.baselib.numbers.greaterThan(x, 0) ) {
		return plt.baselib.numbers.makeFloat(1);
	    } else if ( plt.baselib.numbers.lessThan(x, 0) ) {
		return plt.baselib.numbers.makeFloat(-1);
	    } else {
		return plt.baselib.numbers.makeFloat(0);
	    }
	} else {
	    if ( plt.baselib.numbers.greaterThan(x, 0) ) {
		return 1;
	    } else if ( plt.baselib.numbers.lessThan(x, 0) ) {
		return -1;
	    } else {
		return 0;
	    }
	}
    };

    installPrimitiveProcedure(
        'sgn',
        1,
        function(MACHINE) {
            testArgument(MACHINE,
                         'integer',
                         plt.baselib.numbers.isInteger,
                         MACHINE.env[MACHINE.env.length-1],
                         0,
                         'sgn');
            return rawSgn(MACHINE.env[MACHINE.env.length-1]);
        });


    installPrimitiveProcedure(
        'number->string',
        1,
        function(MACHINE) {
            testArgument(MACHINE,
                         'integer',
                         isNumber,
                         MACHINE.env[MACHINE.env.length-1],
                         0,
                         'number->string');
            return MACHINE.env[MACHINE.env.length-1].toString();
        });

    installPrimitiveProcedure(
        'string->number',
        1,
        function(MACHINE) {
            testArgument(MACHINE,
                         'string',
                         isString,
                         MACHINE.env[MACHINE.env.length-1],
                         0,
                         'string->number');
            return plt.baselib.numbers.fromString(MACHINE.env[MACHINE.env.length-1].toString());
        });



    


    installPrimitiveProcedure(
        'format',
        plt.baselib.arity.makeArityAtLeast(1),
        function(MACHINE) {
            var args = [], i, formatString;
            testArgument(MACHINE,
                         'string',
                         isString,
                         MACHINE.env[MACHINE.env.length-1],
                         0,
                         'format');
            for(i = 0; i < MACHINE.argcount; i++) {
                args.push(MACHINE.env[MACHINE.env.length - 1 - i]);
            }
            formatString = args.shift();
            return plt.baselib.format.format(formatString, args, 'format');
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
                MACHINE.params['currentInspector'] = MACHINE.env[MACHINE.env.length - 1];
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







//     installPrimitiveProcedure(
//         'make-struct-field-accessor',
//         makeList(2, 3),
//         function(MACHINE) {

//             var accessor, fieldPos, fieldName;
//             accessor = MACHINE.env[MACHINE.env.length-1];
//             fieldPos = MACHINE.env[MACHINE.env.length-2];
//             if (MACHINE.argcount === 2) {
//                 fieldName = 'field' + fieldPos;
//             } else {
//                 fieldName = MACHINE.env[MACHINE.env.length-3];
//             }

// 	    testArgument(MACHINE,
//                          'accessor procedure that requires a field index',
//                          function(x) {
//                              return (x instanceof types.StructAccessorProc &&
//                                      x.numParams > 1);
//                          },
//                          accessor,
//                          0,
// 		         'make-struct-field-accessor');

// 	    testArgument(MACHINE,
//                          'exact non-negative integer', 
//                          isNatural,
//                          fieldPos,
//                          'make-struct-field-accessor', 
//                          1)

// 	    testArgument(MACHINE,
//                          'symbol or #f',
//                          function(x) { 
//                              return x === false || isSymbol(x);
//                          },
// 		         'make-struct-field-accessor', 
//                          fieldName,
//                          2);


// 	    var procName = accessor.type.name + '-' fieldName;           
// 	    return new types.StructAccessorProc(
//                 accessor.type,
//                 procName,
//                 1, 
//                 false,
//                 false,
// 		function(MACHINE) {
// 		    testArgument(MACHINE, 
//                                  'struct:' + accessor.type.name, 
//                                  accessor.type.predicate,
//                                  MACHINE.env[MACHINE.env.length - 1],
//                                  procName, 
//                                  0);
// 		    return accessor.impl(x, fieldPos);
// 		});

//         });






















    // Javascript-specific extensions.  A small experiment.
    installPrimitiveProcedure(
        'viewport-width',
        0,
        function(MACHINE) {
            return $(window).width();
        });

    installPrimitiveProcedure(
        'viewport-height',
        0,
        function(MACHINE) {
            return $(window).height();
        });


    installPrimitiveProcedure(
        'in-javascript-context?',
        0,
        function(MACHINE) {
            return true;
        });




    // recomputeGas: state number -> number
    var recomputeMaxNumBouncesBeforeYield = function(MACHINE, observedDelay) {
	// We'd like to see a delay of DESIRED_DELAY_BETWEEN_BOUNCES so
	// that we get MACHINE.params.desiredYieldsPerSecond bounces per
	// second.
	var DESIRED_DELAY_BETWEEN_BOUNCES = 
	    (1000 / MACHINE.params.desiredYieldsPerSecond);
	var ALPHA = 256;
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
                // functions: this gets thrown if the Racket code realizes
                // that the number of bounces has grown too large.  The thrown
                // function represents a restarter function.
                //
                // HaltError: causes evaluation to immediately halt.  We schedule
                // the onHalt function of the HaltError to call afterwards.
                //
                // everything else: otherwise, we send the exception value
                // to the current error handler and exit.
		if (typeof(e) === 'function') {
                    thunk = e;
                    MACHINE.callsBeforeTrampoline = STACK_LIMIT_ESTIMATE;

		    if (MACHINE.params.numBouncesBeforeYield-- < 0) {
			recomputeMaxNumBouncesBeforeYield(
			    MACHINE,
			    (new Date()).valueOf() - startTime);
			setTimeout(
			    function() { trampoline(MACHINE, thunk); },
			    0);
			return;
		    } else {
                        continue;
                    }
		} else if (e instanceof HaltError) {
		    MACHINE.running = false;
                    setTimeout(
                        function() { e.onHalt(MACHINE); },
                        0);
                    return;
                } else {
		    MACHINE.running = false;
                    setTimeout(
                        function() { MACHINE.params.currentErrorHandler(MACHINE, e); },
                        0);
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
            var i;
            runtimeIsReady = true;
            for (i = 0; i < readyWaiters.length; i++) {
                notifyWaiter(readyWaiters[i]);
            }
            readyWaiters = [];
        };

        var runtimeIsReady = false;
        var readyWaiters = [];
        var notifyWaiter = function(w) {
            setTimeout(w, 0);
        };
    })(this);


    //////////////////////////////////////////////////////////////////////
    //////////////////////////////////////////////////////////////////////
    //////////////////////////////////////////////////////////////////////

    // Executes all programs that have been labeled as a main module
    var invokeMains = function(machine, succ, fail) {
        runtime.ready(function() {
            machine = machine || runtime.currentMachine;
            succ = succ || function() {};
            fail = fail || function() {};
            var mainModules = machine.mainModules.slice();
            var loop = function() {
                if (mainModules.length > 0) {
                    var nextModule = mainModules.shift();
                    nextModule.invoke(machine, loop, fail);
                } else {
                    succ();
                }
            };
            setTimeout(loop, 0);
        });
    };



    //////////////////////////////////////////////////////////////////////
    //////////////////////////////////////////////////////////////////////
    //////////////////////////////////////////////////////////////////////


    // Exports
    var exports = runtime;
    exports['currentMachine'] = new Machine();
    exports['invokeMains'] = invokeMains;


    // installing new primitives
    exports['installPrimitiveProcedure'] = installPrimitiveProcedure;
    exports['installPrimitiveClosure'] = installPrimitiveClosure;
    exports['makePrimitiveProcedure'] = makePrimitiveProcedure;
    exports['Primitives'] = Primitives;
    
    exports['ready'] = ready;
    // Private: the runtime library will set this flag to true when
    // the library has finished loading.
    exports['setReadyTrue'] = setReadyTrue;


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



    // Type predicates
    exports['isPair'] = isPair;
    exports['isList'] = isList;
    exports['isVector'] = isVector;
    exports['isOutputPort'] = isOutputPort;
    exports['isOutputStringPort'] = isOutputStringPort;
    exports['isBox'] = isBox;
    exports['isString'] = isString;
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

    exports['HaltError'] = HaltError;



    exports['makeStructureType'] = plt.baselib.structs.makeStructureType;
    exports['Struct'] = plt.baselib.structs.Struct;
    exports['StructType'] = plt.baselib.structs.StructType;


})(this['plt']);