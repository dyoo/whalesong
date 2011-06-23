if(this['plt'] === undefined) { this['plt'] = {}; }


// All of the values here are namespaced under "plt.runtime".
(function(scope) {
    var runtime = {};
    scope['runtime'] = runtime;

    var helpers = plt.helpers;
    var types = plt.types;



    // Consumes a class and creates a predicate that recognizes subclasses.
    var makeClassPredicate = function(aClass) {
	return function(x) { return x instanceof aClass; };
    };


    //////////////////////////////////////////////////////////////////////
    //////////////////////////////////////////////////////////////////////
    // We try to isolate the effect of external modules: all the identifiers we
    // pull from external modules should be listed here, and should otherwise not
    // show up outside this section!
    var isNumber = jsnums.isSchemeNumber;
    var isNatural = types.isNatural;
    var isPair = types.isPair;
    var isList = types.isList;
    var isVector = types.isVector;
    var equals = types.equals;


    var NULL = types.EMPTY;
    var VOID = types.VOID;

    var makeVector = types.vector;
    var makeList = types.list;
    var makePair = types.pair;

    var heir = helpers.heir;
    var toDomNode = helpers.toDomNode;
    var toWrittenString = helpers.toWrittenString;
    var toDisplayedString = helpers.toDisplayedString;


    var makeBox = types.box;
    var isBox = types.isBox;
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
	    

	    'currentOutputPort': new StandardOutputPort(),
	    'currentErrorPort': new StandardErrorPort(),
	    'currentSuccessHandler': function(MACHINE) {},
	    'currentErrorHandler': function(MACHINE, exn) {
                MACHINE.params.currentErrorDisplayer(
                    MACHINE,
                    exn);
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

	    'current-print': new Closure(
		function(MACHINE) {
                    if(--MACHINE.callsBeforeTrampoline<0) { throw arguments.callee; }

		    var elt = MACHINE.env.pop();
		    var outputPort = 
			MACHINE.params.currentOutputPort;
		    if (elt !== VOID) {
			outputPort.writeDomNode(MACHINE, toDomNode(elt, 'print'));
			outputPort.writeDomNode(MACHINE, toDomNode("\n", 'display'));
		    }
		    var frame = MACHINE.control.pop();
		    return frame.label(MACHINE);
		},
		1,
		[],
		"printer")


	};
	this.primitives = Primitives;
    };



    var ModuleRecord = function(name, label) {
	this.name = name;
	this.label = label;
	this.isInvoked = false;
        this.prefix = false;
	this.namespace = {};
    };

    // Returns access to the names defined in the module.
    ModuleRecord.prototype.getNamespace = function() {
	return this.namespace;
    };

    ModuleRecord.prototype.finalizeModuleInvokation = function() {
	var i, len = this.prefix.names.length;
	for (i=0; i < len; i++) {
	    this.namespace[this.prefix.names[i]] = this.prefix[i];
	}
    };
    
    // External invokation of a module.
    ModuleRecord.prototype.invoke = function(MACHINE, succ, fail) {
        MACHINE = MACHINE || plt.runtime.currentMachine;
        succ = succ || function(){};
        fail = fail || function(){};

        var oldErrorHandler = MACHINE.params['currentErrorHandler'];
        var afterGoodInvoke = function(MACHINE) { 
            MACHINE.params['currentErrorHandler'] = oldErrorHandler;
            setTimeout(succ, 0);
        };

        if (this.isInvoked) {
            setTimeout(succ, 0);
        } else {
            MACHINE.params['currentErrorHandler'] = function(MACHINE, anError) {
                MACHINE.params['currentErrorHandler'] = oldErrorHandler;
                setTimeout(
		    function() { 
			fail(MACHINE, anError)
		    },
		    0);
            };
            MACHINE.control.push(new CallFrame(afterGoodInvoke, null));
            trampoline(MACHINE, this.label);
        }
    };





    // A generic frame just holds marks.
    var Frame = function() {
	// The set of continuation marks.
	this.marks = [];

	// When we're in the middle of computing with-cont-mark, we
	// stash the key in here temporarily.
	this.pendingContinuationMarkKey = undefined;
	this.pendingApplyValuesProc = undefined;
	this.pendingBegin0Count = undefined;
	this.pendingBegin0Values = undefined;
    };


    // Frames must support marks and the temporary variables necessary to
    // support with-continuation-mark and with-values.

    // Specialized frames support more features:

    // A CallFrame represents a call stack frame, and includes the return address
    // as well as the function being called.
    var CallFrame = function(label, proc) {
	this.label = label;
	this.proc = proc;

	// When we're in the middle of computing with-cont-mark, we
	// stash the key in here temporarily.
	this.pendingContinuationMarkKey = undefined;

	// The set of continuation marks.
	this.marks = [];
    };
    CallFrame.prototype = heir(Frame.prototype);

    // A prompt frame includes a return address, as well as a prompt tag
    // for supporting delimited continuations.
    var PromptFrame = function(label, tag) {
	this.label = label;
	this.tag = tag; // ContinuationPromptTag

	// The set of continuation marks.
	this.marks = [];

	// When we're in the middle of computing with-cont-mark, we
	// stash the key in here temporarily.
	this.pendingContinuationMarkKey = undefined;	
    };
    PromptFrame.prototype = heir(Frame.prototype);




    // Output Ports

    var OutputPort = function() {};
    var isOutputPort = makeClassPredicate(OutputPort);


    var StandardOutputPort = function() {
        OutputPort.call(this);
    };
    StandardOutputPort.prototype = heir(OutputPort.prototype);
    StandardOutputPort.prototype.writeDomNode = function(MACHINE, domNode) {
	MACHINE.params['currentDisplayer'](MACHINE, domNode);
    };

    var StandardErrorPort = function() {
        OutputPort.call(this);
    };
    StandardErrorPort.prototype = heir(OutputPort.prototype);
    StandardErrorPort.prototype.writeDomNode = function(MACHINE, domNode) {
	MACHINE.params['currentErrorDisplayer'](MACHINE, domNode);
    };






    var OutputStringPort = function() {
	this.buf = [];
    };
    OutputStringPort.prototype = heir(OutputPort.prototype);
    OutputStringPort.prototype.writeDomNode = function(MACHINE, v) {
	this.buf.push($(v).text());
    };
    OutputStringPort.prototype.getOutputString = function() {
	return this.buf.join('');
    };
    var isOutputStringPort = makeClassPredicate(OutputStringPort);




    // Function types: a function is either a Primitive or a Closure.

    // A Primitive is a function that's expected to return.  It is not
    // allowed to call into Closures.  Its caller is expected to pop off
    // its argument stack space.
    //
    //
    // A Closure is a function that takes on more responsibilities: it is
    // responsible for popping off stack space before it finishes, and it
    // is also explicitly responsible for continuing the computation by 
    // popping off the control stack and doing the jump.  Because of this,
    // closures can do pretty much anything to the machine.



    // A closure consists of its free variables as well as a label
    // into its text segment.
    var Closure = function(label, arity, closedVals, displayName) {
	this.label = label;              // (MACHINE -> void)
	this.arity = arity;              // number
	this.closedVals = closedVals;    // arrayof number
	this.displayName = displayName;  // string
    };



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




    var raise = function(MACHINE, e) { 
	if (typeof(window.console) !== 'undefined' &&
	    typeof(console.log) === 'function') {
	    console.log(MACHINE);
	    if (e.stack) { console.log(e.stack); }
	    else { console.log(e); }
	} 
	throw e; 
    };



    // testArgument: (X -> boolean) X number string string -> boolean
    // Produces true if val is true, and otherwise raises an error.
    var testArgument = function(MACHINE,
				expectedTypeName,
				predicate, 			    
				val, 
				index, 
				callerName) {
	if (predicate(val)) {
	    return true;
	} else {
	    raiseArgumentTypeError(MACHINE, 
                                   callerName,
                                   expectedTypeName,
				   index,
				   val);
	}
    };

    var testArity = function(callerName, observed, minimum, maximum) {
	if (observed < minimum || observed > maximum) {
	    raise(MACHINE, new Error(callerName + ": expected at least " + minimum
				     + " arguments "
				     + " but received " + observed));

	}
    };


    var raiseUnboundToplevelError = function(MACHINE, name) {
        raise(MACHINE, new Error("Not bound: " + name)); 
    };

    var raiseArgumentTypeError = function(MACHINE, 
                                          callerName,
                                          expectedTypeName,
                                          argumentOffset,
                                          actualValue) {
	raise(MACHINE,
              new Error(callerName + ": expected " + expectedTypeName
			+ " as argument " + (argumentOffset + 1)
			+ " but received " + helpers.toWrittenString(actualValue)));
    };

    var raiseContextExpectedValuesError = function(MACHINE, expected) {
	raise(MACHINE, 
	      new Error("expected " + expected +
			" values, received " + 
			MACHINE.argcount + " values"));
    };

    var raiseArityMismatchError = function(MACHINE, proc, expected, received) {
	raise(MACHINE, 
	      new Error(proc.displayName + ": " + "expected " + expected 
                        + " value(s), received " + received + " value(s)"));
    };

    var raiseOperatorApplicationError = function(MACHINE, operator) {
	raise(MACHINE, 
	      new Error("not a procedure: " + helpers.toWrittenString(operator)));
    };

    var raiseOperatorIsNotClosure = function(MACHINE, operator) {
        raise(MACHINE,
              new Error("not a closure: " + helpers.toWrittenString(operator)));
    };

    var raiseOperatorIsNotPrimitiveProcedure = function(MACHINE, operator) {
        raise(MACHINE,
              new Error("not a primitive procedure: " + helpers.toWrittenString(operator)));
    };


    var raiseUnimplementedPrimitiveError = function(MACHINE, name) {
	raise(MACHINE, 
	      new Error("unimplemented kernel procedure: " + name))
    };





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



    // An arity is either a primitive number, an ArityAtLeast instance,
    // or a list of either primitive numbers or ArityAtLeast instances.

    var ArityAtLeast = function(n) {
	this.value = n;
    };

    // isArityMatching: arity natural -> boolean
    // Produces true if n satisfies the arity.
    var isArityMatching = function(arity, n) {
	if (typeof(arity) === 'number') {
	    return arity === n;
	} else if (arity instanceof ArityAtLeast) {
	    return n >= arity.value;
	} else {
	    while (arity !== NULL) {
		if (typeof(arity.first) === 'number') {
		    if (arity.first === n) { return true; }
		} else if (arity instanceof ArityAtLeast) {
		    if (n >= arity.first.value) { return true; }
		}
		arity = arity.rest;
	    }
	    return false;
	}
    }




    

    // Primitives are the set of primitive values.  Not all primitives
    // are coded here; several of them (including call/cc) are injected by
    // the bootstrapping code.
    var Primitives = {};

    var installPrimitiveProcedure = function(name, arity, f) {
        Primitives[name] = f;
        Primitives[name].arity = arity;
        Primitives[name].displayName = name;
    };

    var makePrimitiveProcedure = function(name, arity, f) {
        f.arity = arity;
        f.displayName = name;
        return f;
    };

    var installPrimitiveConstant = function(name, v) {
        Primitives[name] = v;
    };



    installPrimitiveConstant('pi', jsnums.pi);
    installPrimitiveConstant('e', jsnums.e);
    installPrimitiveConstant('null', NULL);


    installPrimitiveProcedure(
        'display', makeList(1, 2),
        function(MACHINE) {
	    var firstArg = MACHINE.env[MACHINE.env.length-1];
	    var outputPort = MACHINE.params.currentOutputPort;
	    if (MACHINE.argcount === 2) {
	        testArgument(MACHINE,
			     'isOutputPort', 
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
			     'isOutputPort', 
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
			     'isOutputPort', 
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
        'current-print',
        makeList(0, 1),
        function(MACHINE) {
            if (MACHINE.argcount === 1) {
                MACHINE.params['current-print'] = MACHINE.env[MACHINE.env.length - 1];
                return VOID;
            } else {
	        return MACHINE.params['current-print'];
            }
        });


    installPrimitiveProcedure(
        '=',
        new ArityAtLeast(2),
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
	        if (! (jsnums.equals(MACHINE.env[MACHINE.env.length - 1 - i],
		                     MACHINE.env[MACHINE.env.length - 1 - i - 1]))) {
		    return false; 
	        }
	    }
	    return true;
        });


    // TODO: use installPrimitiveProcedure for the rest...

    installPrimitiveProcedure(
        '<',
        new ArityAtLeast(2),
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
	        if (! (jsnums.lessThan(MACHINE.env[MACHINE.env.length - 1 - i],
		                       MACHINE.env[MACHINE.env.length - 1 - i - 1]))) {
		    return false; 
	        }
	    }
	    return true;
        });


    installPrimitiveProcedure(
        '>',
        new ArityAtLeast(2),
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
	        if (! (jsnums.greaterThan(MACHINE.env[MACHINE.env.length - 1 - i],
		                          MACHINE.env[MACHINE.env.length - 1 - i - 1]))) {
		    return false; 
	        }
	    }
	    return true;
        });

    installPrimitiveProcedure(
        '<=',
        new ArityAtLeast(2),
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
	        if (! (jsnums.lessThanOrEqual(MACHINE.env[MACHINE.env.length - 1 - i],
		                              MACHINE.env[MACHINE.env.length - 1 - i - 1]))) {
		    return false; 
	        }
	    }
	    return true;
        });


    installPrimitiveProcedure(
        '>=',
        new ArityAtLeast(2),
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
	        if (! (jsnums.greaterThanOrEqual(MACHINE.env[MACHINE.env.length - 1 - i],
		                                 MACHINE.env[MACHINE.env.length - 1 - i - 1]))) {
		    return false; 
	        }
	    }
	    return true;
        });
    

    installPrimitiveProcedure(
        '+',
        new ArityAtLeast(0),
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
	        result = jsnums.add(result, MACHINE.env[MACHINE.env.length - 1 - i]);
	    };
	    return result;
        });
    

    installPrimitiveProcedure(
        '*',
        new ArityAtLeast(0),
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
	        result = jsnums.multiply(result, MACHINE.env[MACHINE.env.length - 1 - i]);
	    }
	    return result;
        });

    installPrimitiveProcedure(
        '-',
        new ArityAtLeast(1),
        function(MACHINE) {
	    if (MACHINE.argcount === 1) { 
	        testArgument(MACHINE,
			     'number',
			     isNumber,
			     MACHINE.env[MACHINE.env.length-1],
			     0,
			     '-');
	        return jsnums.subtract(0, MACHINE.env[MACHINE.env.length-1]);
	    }
	    var result = MACHINE.env[MACHINE.env.length - 1];
	    for (var i = 1; i < MACHINE.argcount; i++) {
	        testArgument(MACHINE,
			     'number',
			     isNumber,
			     MACHINE.env[MACHINE.env.length-1-i],
			     i,
			     '-');
	        result = jsnums.subtract(result, MACHINE.env[MACHINE.env.length - 1 - i]);
	    }
	    return result;
        });
    
    installPrimitiveProcedure(
        '/',
        new ArityAtLeast(1),
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
	        result = jsnums.divide(result, MACHINE.env[MACHINE.env.length - 1 - i]);
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
	    return jsnums.add(firstArg, 1);
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
	    return jsnums.subtract(firstArg, 1);
        });


    installPrimitiveProcedure(
        'zero?',
        1,
        function(MACHINE) {
	    var firstArg = MACHINE.env[MACHINE.env.length-1];
	    return jsnums.equals(firstArg, 0);
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
        new ArityAtLeast(0),
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
        new ArityAtLeast(0),
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
	    var index = jsnums.toFixnum(MACHINE.env[MACHINE.env.length-2]);
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
	    var length = jsnums.toFixnum(MACHINE.env[MACHINE.env.length-1]);
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
        new ArityAtLeast(0),
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
	    var result = [firstArg];
	    return result;
        });

    installPrimitiveProcedure(
        'unbox',
        1,
        function(MACHINE) {
	    var firstArg = MACHINE.env[MACHINE.env.length-1];
	    return firstArg[0];
        });

    installPrimitiveProcedure(
        'set-box!',
        2,
        function(MACHINE) {
	    var firstArg = MACHINE.env[MACHINE.env.length-1];
	    var secondArg = MACHINE.env[MACHINE.env.length-2];
	    firstArg[0] = secondArg;
	    return VOID;
        });

    installPrimitiveProcedure(
        'void',
        new ArityAtLeast(0),
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


    var HaltError = function() {}


    var trampoline = function(MACHINE, initialJump) {
	var thunk = initialJump;
	var startTime = (new Date()).valueOf();
	MACHINE.callsBeforeTrampoline = STACK_LIMIT_ESTIMATE;
	MACHINE.params.numBouncesBeforeYield = 
	    MACHINE.params.maxNumBouncesBeforeYield;
	MACHINE.running = true;

	while(thunk) {
            try {
		thunk(MACHINE);
		break;
            } catch (e) {
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
		    }
		} else if (e instanceof HaltError) {
                    // FIXME: work out what it really means to Halt.
                    return;
                } else {
		    MACHINE.running = false;
	            return MACHINE.params.currentErrorHandler(MACHINE, e);
		}
            }
	}
	MACHINE.running = false;
	return MACHINE.params.currentSuccessHandler(MACHINE);
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




    //////////////////////////////////////////////////////////////////////
    //////////////////////////////////////////////////////////////////////
    //////////////////////////////////////////////////////////////////////

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

    exports['testArgument'] = testArgument;
    exports['testArity'] = testArity;


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


    //////////////////////////////////////////////////////////////////////


    // Type constructors

    // numbers
    exports['makeList'] = makeList;
    exports['makePair'] = makePair;
    exports['makeVector'] = makeVector;
    exports['makeBox'] = makeBox;


    // Type predicates
    exports['isPair'] = isPair;
    exports['isList'] = isList;
    exports['isVector'] = isVector;
    exports['isOutputPort'] = isOutputPort;
    exports['isOutputStringPort'] = isOutputStringPort;
    exports['isBox'] = isBox;
    exports['equals'] = equals;

    exports['toDomNode'] = toDomNode;
    exports['toWrittenString'] = toWrittenString;
    exports['toDisplayedString'] = toDisplayedString;

    exports['ArityAtLeast'] = ArityAtLeast;
    exports['isArityMatching'] = isArityMatching;

    exports['heir'] = heir;
    exports['makeClassPredicate'] = makeClassPredicate;

    exports['HaltError'] = HaltError;


    scope.link.announceReady('runtime');
})(this['plt']);