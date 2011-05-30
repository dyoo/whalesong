    if(this['plt'] === undefined) {
	this['plt'] = {};
    }


// All of the values here are namespaced under "plt.runtime".

(function() {
    this['plt']['runtime'] = {};
    var exports = this['plt']['runtime'];



    // Type helpers
    //
    // Defines inheritance between prototypes.
    var heir = function(parentPrototype) {
	var f = function() {}
	f.prototype = parentPrototype;
	return new f();
    };

    // Consumes a class and creates a predicate that recognizes subclasses.
    var makeClassPredicate = function(aClass) {
	return function(x) { return x instanceof aClass; };
    };


    var isNumber = function(x) { return typeof(x) === 'number'; };

    var isNatural = function(x) { return typeof(x) === 'number' &&
				  x >= 0 &&
				  Math.floor(x) === x; };


    var isPair = function(x) { return (typeof(x) == 'object' && 
				       x.length === 2 &&
				       x.type !== 'vector') };
    var isList = function(x) {
	while (x !== NULL) {
	    if (typeof(x) == 'object' && x.length === 2) {
		x = x[1];
	    } else {
		return false;
	    }
	}
	return true;
    };

    var isVector = function(x) { return (typeof(x) == 'object' && 
					 x.type === 'vector') };


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
	    'currentDisplayer': function(v) {
		    $(document.body).append(v);
	    },
	    
	    'currentOutputPort': new StandardOutputPort(),
	    'currentSuccessHandler': function(MACHINE) {},
	    'currentErrorHandler': function(MACHINE, exn) {},
	    
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
		    var elt = MACHINE.env.pop();
		    var outputPort = 
			MACHINE.params.currentOutputPort;
		    if (elt !== undefined) {
			outputPort.write(MACHINE, elt);
			outputPort.write(MACHINE, "\n");
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








    var OutputPort = function() {};
    var isOutputPort = makeClassPredicate(OutputPort);


    var StandardOutputPort = function() {};
    StandardOutputPort.prototype = heir(OutputPort.prototype);
    StandardOutputPort.prototype.write = function(MACHINE, v) {
	var domNode;
	// TODO: v must be coerced into a DOMNode in a more systematic way.
	// This function may need to be a Closure.
	if(typeof(v) === 'string' || 
	   typeof(v) === 'number' ||
	   typeof(v) === 'boolean' ||
	   typeof(v) === 'null' ||
	   typeof(v) === 'undefined') {
	    domNode = $('<span/>').text(String(v)).css('white-space', 'pre');
        } else {
	    domNode = $('<span/>').text(String(v)).css('white-space', 'pre');
        }
	MACHINE.params['currentDisplayer'](domNode);
    };



    var OutputStringPort = function() {
	this.buf = [];
    };
    OutputStringPort.prototype = heir(OutputPort.prototype);
    OutputStringPort.prototype.write = function(MACHINE, v) {
	this.buf.push(String(v));
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


    var NULL = [];


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
				     + " but received " + observer));

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
			+ " but received " + actualValue));
    };

    var raiseContextExpectedValuesError = function(MACHINE, expected) {
	raise(MACHINE, 
	      new Error("expected " + expected +
			" values, received " + 
			MACHINE.argcount + " values"));
    };

    var raiseArityMismatchError = function(MACHINE, proc, expected, received) {
	raise(MACHINE, 
	      new Error("expected " + expected + " values, received " + received + " values"));
    };

    var raiseOperatorApplicationError = function(MACHINE, operator) {
	raise(MACHINE, 
	      new Error("not a procedure: " + expected +
                        operator));
    };

    var raiseOperatorIsNotClosure = function(MACHINE, operator) {
        raise(MACHINE,
              new Error("not a closure: " + operator));
    };

    var raiseOperatorIsNotPrimitiveProcedure = function(MACHINE, operator) {
        raise(MACHINE,
              new Error("not a primitive procedure: " + operator));
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
	    vals.push(lst[0]);
	    lst = lst[1];
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
	    lst = [MACHINE.env[MACHINE.env.length - depth - length + i], lst];
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
		if (typeof(arity[0]) === 'number') {
		    if (arity[0] === n) { return true; }
		} else if (arity instanceof ArityAtLeast) {
		    if (n >= arity[0].value) { return true; }
		}
		arity = arity[1];
	    }
	    return false;
	}
    }




    

    // Primitives are the set of primitive values.  Not all primitives
    // are coded here; several of them (including call/cc) are injected by
    // the bootstrapping code.
    var Primitives = {};
    Primitives['display'] = function(MACHINE) {
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
	outputPort.write(MACHINE, firstArg);
    };
    Primitives['display'].arity = [1, [2, NULL]];
    Primitives['display'].displayName = 'display';


    Primitives['newline'] = function(MACHINE) {
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
	outputPort.write(MACHINE, "\n");
    };
    Primitives['newline'].arity = [0, [1, NULL]];
    Primitives['newline'].displayName = 'newline';


    Primitives['displayln'] = function(MACHINE){
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
	outputPort.write(MACHINE, firstArg);
	outputPort.write(MACHINE, "\n");
    };
    Primitives['displayln'].arity = [1, [2, NULL]];
    Primitives['displayln'].displayName = 'displayln';



    Primitives['current-print'] = function(MACHINE) {
	return MACHINE.params['current-print'];
    };
    Primitives['current-print'].arity = [0, [1, NULL]];
    Primitives['current-print'].displayName = "current-print";
    

//     // This should be attached to the module corresponding for print-values
//     Primitives['print-values'] = new Closure(
//         function(MACHINE) {
// 	    var outputPort = MACHINE.params.currentOutputPort;
//             var prependNewline = false;
//             if (MACHINE.argcount > 0) {
//                 if (MACHINE.val !== undefined) {
//                     if (prependNewline) {
//                         outputPort.write(MACHINE, "\n");
//                     }
// 	            outputPort.write(MACHINE, MACHINE.val);
//                     prependNewline = true;
//                 }

//                 for(var i = 0; i < MACHINE.argcount - 1; i++) {
//                     if (MACHINE.env[MACHINE.env.length - 1 - i] !== undefined) {
// 	                if (prependNewline) {
//                             outputPort.write(MACHINE, "\n");
//                         }
//                         outputPort.write(MACHINE,
//                                          MACHINE.env[MACHINE.env.length - 1 - i]);
//                         prependNewline = true;
//                     }
//                 }
// 	        outputPort.write(MACHINE, "\n");
//             }
//             MACHINE.env.length = MACHINE.env.length - MACHINE.argcount;
//             var frame = MACHINE.control.pop();
//             return frame.label(MACHINE);
//         },
//         new ArityAtLeast(0),
//         [],
//         "print-values"
//     );



    Primitives['pi'] = Math.PI;

    Primitives['e'] = Math.E;

    Primitives['='] = function(MACHINE) {
	var firstArg = MACHINE.env[MACHINE.env.length-1];
	testArgument(MACHINE, 'number', isNumber, firstArg, 0, '=');
	for (var i = 0; i < MACHINE.argcount - 1; i++) {
	    testArgument(MACHINE, 
			 'number',
			 isNumber, 
			 MACHINE.env[MACHINE.env.length - 1 - i],
			 i,
			 '=');
	    if (MACHINE.env[MACHINE.env.length - 1 - i] !==
		MACHINE.env[MACHINE.env.length - 1 - i - 1]) {
		return false; 
	    }
	}
	return true;
    };
    Primitives['='].arity = new ArityAtLeast(2);
    Primitives['='].displayName = '=';


    Primitives['<'] = function(MACHINE) {
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
	    if (! (MACHINE.env[MACHINE.env.length - 1 - i] <
		   MACHINE.env[MACHINE.env.length - 1 - i - 1])) {
		return false; 
	    }
	}
	return true;
    };
    Primitives['<'].arity = new ArityAtLeast(2);
    Primitives['<'].displayName = '<';

    Primitives['>'] = function(MACHINE) {
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
	    if (! (MACHINE.env[MACHINE.env.length - 1 - i] >
		   MACHINE.env[MACHINE.env.length - 1 - i - 1])) {
		return false; 
	    }
	}
	return true;
    };
    Primitives['>'].arity = new ArityAtLeast(2);
    Primitives['>'].displayName = '>';

    Primitives['<='] = function(MACHINE) {
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
	    if (! (MACHINE.env[MACHINE.env.length - 1 - i] <=
		   MACHINE.env[MACHINE.env.length - 1 - i - 1])) {
		return false; 
	    }
	}
	return true;
    };
    Primitives['<='].arity = new ArityAtLeast(2);
    Primitives['<='].displayName = '<=';


    Primitives['>='] = function(MACHINE) {
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
	    if (! (MACHINE.env[MACHINE.env.length - 1 - i] >=
		   MACHINE.env[MACHINE.env.length - 1 - i - 1])) {
		return false; 
	    }
	}
	return true;
    };
    Primitives['>='].arity = new ArityAtLeast(2);
    Primitives['>='].displayName = '>=';
    

    Primitives['+'] = function(MACHINE) {
	var result = 0;
	var i = 0;
	for (i=0; i < MACHINE.argcount; i++) {
	    testArgument(MACHINE,
			 'number',
			 isNumber, 
			 MACHINE.env[MACHINE.env.length - 1 - i],
			 i,
			 '+');
	    result += MACHINE.env[MACHINE.env.length - 1 - i];
	};
	return result;
    };
    Primitives['+'].arity = new ArityAtLeast(0);
    Primitives['+'].displayName = '+';
    

    Primitives['*'] = function(MACHINE) {
	var result = 1;
	var i = 0;
	for (i=0; i < MACHINE.argcount; i++) {
	    testArgument(MACHINE,
			 'number',
			 isNumber, 
			 MACHINE.env[MACHINE.env.length - 1 - i],
			 i,
			 '*');
	    result *= MACHINE.env[MACHINE.env.length - 1 - i];
	}
	return result;
    };
    Primitives['*'].arity = new ArityAtLeast(0);
    Primitives['*'].displayName = '*';
    
    Primitives['-'] = function(MACHINE) {
	if (MACHINE.argcount === 1) { 
	    testArgument(MACHINE,
			 'number',
			 isNumber,
			 MACHINE.env[MACHINE.env.length-1],
			 0,
			 '-');
	    return -(MACHINE.env[MACHINE.env.length-1]);
	}
	var result = MACHINE.env[MACHINE.env.length - 1];
	for (var i = 1; i < MACHINE.argcount; i++) {
	    testArgument(MACHINE,
			 'number',
			 isNumber,
			 MACHINE.env[MACHINE.env.length-1-i],
			 i,
			 '-');
	    result -= MACHINE.env[MACHINE.env.length - 1 - i];
	}
	return result;
    };
    Primitives['-'].arity = new ArityAtLeast(1);
    Primitives['-'].displayName = '-';
    
    Primitives['/'] = function(MACHINE) {
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
	    result /= MACHINE.env[MACHINE.env.length - 1 - i];
	}
	return result;
    };
    Primitives['/'].arity = new ArityAtLeast(1);
    Primitives['/'].displayName = '/';
    

    Primitives['cons'] = function(MACHINE) {
	var firstArg = MACHINE.env[MACHINE.env.length-1];
	var secondArg = MACHINE.env[MACHINE.env.length-2];
	return [firstArg, secondArg];
    };
    Primitives['cons'].arity = 2;
    Primitives['cons'].displayName = 'cons';


    Primitives['list'] = function(MACHINE) {
	var result = NULL;
	for (var i = 0; i < MACHINE.argcount; i++) {
	    result = [MACHINE.env[MACHINE.env.length - (MACHINE.argcount - i)],
		      result];
	}
	return result;
    };
    Primitives['list'].arity = new ArityAtLeast(0);
    Primitives['list'].displayName = 'list';

    Primitives['car'] = function(MACHINE) {
	testArgument(MACHINE, 
		     'pair',
		     isPair,
		     MACHINE.env[MACHINE.env.length - 1],
		     0,
		     'car');
	var firstArg = MACHINE.env[MACHINE.env.length-1];
	return firstArg[0];
    };
    Primitives['car'].arity = 1;
    Primitives['car'].displayName = 'car';

    Primitives['cdr'] = function(MACHINE) {
	testArgument(MACHINE,
		     'pair',
		     isPair,
		     MACHINE.env[MACHINE.env.length - 1],
		     0,
		     'cdr');
	var firstArg = MACHINE.env[MACHINE.env.length-1];
	return firstArg[1];
    };
    Primitives['cdr'].arity = 1;
    Primitives['cdr'].displayName = 'cdr';

    Primitives['pair?'] = function(MACHINE) {
	var firstArg = MACHINE.env[MACHINE.env.length-1];
	return isPair(firstArg);
    };
    Primitives['pair?'].arity = 1;
    Primitives['pair?'].displayName = 'pair?';

    Primitives['set-car!'] = function(MACHINE) {
	testArgument(MACHINE,
		     'pair',
		     isPair,
		     MACHINE.env[MACHINE.env.length - 1],
		     0,
		     'set-car!');
	var firstArg = MACHINE.env[MACHINE.env.length-1];
	var secondArg = MACHINE.env[MACHINE.env.length-2];
	firstArg[0] = secondArg;
    };
    Primitives['set-car!'].arity = 2;
    Primitives['set-car!'].displayName = 'set-car!';

    Primitives['set-cdr!'] = function(MACHINE) {
	testArgument(MACHINE,
		     'pair',
		     isPair,
		     MACHINE.env[MACHINE.env.length - 1],
		     0,
		     'set-cdr!');
	var firstArg = MACHINE.env[MACHINE.env.length-1];
	var secondArg = MACHINE.env[MACHINE.env.length-2];
	firstArg[1] = secondArg;
    };
    Primitives['set-cdr!'].arity = 2;
    Primitives['set-cdr!'].displayName = 'set-cdr!';
    
    Primitives['not'] = function(MACHINE) {
	var firstArg = MACHINE.env[MACHINE.env.length-1];
	return (firstArg === false);
    };
    Primitives['not'].arity = 1;
    Primitives['not'].displayName = 'not';

    Primitives['null'] = NULL;

    Primitives['null?'] = function(MACHINE) {
	var firstArg = MACHINE.env[MACHINE.env.length-1];
	return firstArg === NULL;
    };
    Primitives['null?'].arity = 1;
    Primitives['null?'].displayName = 'null?';

    Primitives['add1'] = function(MACHINE) {
	testArgument(MACHINE,
		     'number',
		     isNumber,
		     MACHINE.env[MACHINE.env.length - 1],
		     0,
		     'add1');
	var firstArg = MACHINE.env[MACHINE.env.length-1];
	return firstArg + 1;
    };
    Primitives['add1'].arity = 1;
    Primitives['add1'].displayName = 'add1';

    Primitives['sub1'] = function(MACHINE) {
	testArgument(MACHINE,
		     'number',
		     isNumber,
		     MACHINE.env[MACHINE.env.length - 1],
		     0,
		     'sub1');
	var firstArg = MACHINE.env[MACHINE.env.length-1];
	return firstArg - 1;
    };
    Primitives['sub1'].arity = 1;
    Primitives['sub1'].displayName = 'sub1';

    Primitives['zero?'] = function(MACHINE) {
	var firstArg = MACHINE.env[MACHINE.env.length-1];
	return firstArg === 0;
    };
    Primitives['zero?'].arity = 1;
    Primitives['zero?'].displayName = 'zero?';

    Primitives['vector'] = function(MACHINE) {
	var i;
	var result = [];
	for (i = 0; i < MACHINE.argcount; i++) {
	    result.push(MACHINE.env[MACHINE.env.length-1-i]);
	}
	result.type = 'vector';
	return result;
    };
    Primitives['vector'].arity = new ArityAtLeast(0);
    Primitives['vector'].displayName = 'vector';

    Primitives['vector->list'] = function(MACHINE) {
	testArgument(MACHINE,
		     'vector',
		     isVector,
		     MACHINE.env[MACHINE.env.length - 1],
		     0,
		     'vector->list');
	var firstArg = MACHINE.env[MACHINE.env.length-1];
	var i;
	var result = NULL;
	for (i = 0; i < firstArg.length; i++) {
	    result = [firstArg[firstArg.length - 1 - i], result];
	}
	return result;
    };
    Primitives['vector->list'].arity = 1;
    Primitives['vector->list'].displayName = 'vector->list';
    
    Primitives['list->vector'] = function(MACHINE) {
	var firstArg = MACHINE.env[MACHINE.env.length-1];
	var result = [];
	while (firstArg !== NULL) {
	    result.push(firstArg[0]);
	    firstArg = firstArg[1];
	}
	result.type='vector';
	return result;
    };
    Primitives['list->vector'].arity = 1;
    Primitives['list->vector'].displayName = 'list->vector';

    Primitives['vector-ref'] = function(MACHINE) {
	testArgument(MACHINE,
		     'vector',
		     isVector,
		     MACHINE.env[MACHINE.env.length - 1],
		     0,
		     'vector-ref');
	var firstArg = MACHINE.env[MACHINE.env.length-1];
	var secondArg = MACHINE.env[MACHINE.env.length-2];
	return firstArg[secondArg];
    };
    Primitives['vector-ref'].arity = 2;
    Primitives['vector-ref'].displayName = 'vector-ref';

    Primitives['vector-set!'] = function(MACHINE) {
	testArgument(MACHINE,
		     'vector',
		     isVector,
		     MACHINE.env[MACHINE.env.length - 1],
		     0,
		     'vector-set!');
	var firstArg = MACHINE.env[MACHINE.env.length-1];
	var secondArg = MACHINE.env[MACHINE.env.length-2];
	var thirdArg = MACHINE.env[MACHINE.env.length-3];
	firstArg[secondArg] = thirdArg;
	return null;
    };
    Primitives['vector-set!'].arity = 3;
    Primitives['vector-set!'].displayName = 'vector-set!';


    Primitives['vector-length'] = function(MACHINE) {
	testArgument(MACHINE,
		     'vector',
		     isVector,
		     MACHINE.env[MACHINE.env.length - 1],
		     0,
		     'vector-length');
	var firstArg = MACHINE.env[MACHINE.env.length-1];
	return firstArg.length;
    };
    Primitives['vector-length'].arity = 1;
    Primitives['vector-length'].displayName = 'vector-length';


    Primitives['make-vector'] = function(MACHINE) {
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
	var length = MACHINE.env[MACHINE.env.length-1];
	var arr = [];
	for(var i = 0; i < length; i++) {
	    arr[i] = value;
	}
	arr.type='vector';
	return arr;
    };
    Primitives['make-vector'].arity = [1, [2, NULL]];
    Primitives['make-vector'].displayName = 'make-vector';


    


    Primitives['symbol?'] = function(MACHINE) {
	var firstArg = MACHINE.env[MACHINE.env.length-1];
	return typeof(firstArg) === 'string';
    };
    Primitives['symbol?'].arity = 1;
    Primitives['symbol?'].displayName = 'symbol?';

    Primitives['symbol->string'] = function(MACHINE) {
	var firstArg = MACHINE.env[MACHINE.env.length-1];
	return firstArg;
    };
    Primitives['symbol->string'].arity = 1;
    Primitives['symbol->string'].displayName = 'symbol->string';

    Primitives['string-append'] = function(MACHINE) {
	var buffer = [];
	var i;
	for (i = 0; i < MACHINE.argcount; i++) {
	    buffer.push(MACHINE.env[MACHINE.env.length - 1 - i]);
	}
	return buffer.join('');
    };
    Primitives['string-append'].arity = new ArityAtLeast(0);
    Primitives['string-append'].displayName = 'string-append';

    Primitives['string-length'] = function(MACHINE) {
	var firstArg = MACHINE.env[MACHINE.env.length-1];
	return firstArg.length;
    };
    Primitives['string-length'].arity = 1;
    Primitives['string-length'].displayName = 'string-length';
    
    Primitives['box'] = function(MACHINE) {
	var firstArg = MACHINE.env[MACHINE.env.length-1];
	var result = [firstArg];
	return result;
    };
    Primitives['box'].arity = 1;
    Primitives['box'].displayName = 'box';

    Primitives['unbox'] = function(MACHINE) {
	var firstArg = MACHINE.env[MACHINE.env.length-1];
	return firstArg[0];
    };
    Primitives['unbox'].arity = 1;
    Primitives['unbox'].displayName = 'unbox';

    Primitives['set-box!'] = function(MACHINE) {
	var firstArg = MACHINE.env[MACHINE.env.length-1];
	var secondArg = MACHINE.env[MACHINE.env.length-2];
	firstArg[0] = secondArg;
	return;
    };
    Primitives['set-box!'].arity = 2;
    Primitives['set-box!'].displayName = 'set-box!';

    Primitives['void'] = function(MACHINE) {
	return;
    };
    Primitives['void'].arity = new ArityAtLeast(0);
    Primitives['void'].displayName = 'void';

    Primitives['eq?'] = function(MACHINE) {
	var firstArg = MACHINE.env[MACHINE.env.length-1];
	var secondArg = MACHINE.env[MACHINE.env.length-2];
	return firstArg === secondArg;
    };
    Primitives['eq?'].arity = 2;
    Primitives['eq?'].displayName = 'eq?';

    Primitives['equal?'] = function(MACHINE) {
	var firstArg = MACHINE.env[MACHINE.env.length-1];
	var secondArg = MACHINE.env[MACHINE.env.length-2];
	return isEqual(firstArg, secondArg);
    };
    Primitives['equal?'].arity = 2;
    Primitives['equal?'].displayName = 'equal?';



    var isEqual = function(firstArg, secondArg) {
	var lset = [firstArg], rset = [secondArg];
	while (lset.length !== 0 && rset.length !== 0) {
	    var lhs = lset.pop();
	    var rhs = rset.pop();
	    if (lhs === rhs) {
		continue;
	    } else if (typeof(lhs) === 'object' &&
		       typeof(rhs) === 'object' &&
		       typeof(lhs.length) === 'number' &&
		       typeof(rhs.length) === 'number' &&
		       lhs.length === rhs.length) {
		lset.push.apply(lset, lhs);
		rset.push.apply(rset, rhs);
	    } else {
		return false;
	    }
	}
	return true;
    };


    Primitives['member'] = function(MACHINE) {
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
	    if (isEqual(x, (lst[0]))) {
		return lst;
	    }
	    lst = lst[1];
	}	
    };
    Primitives['member'].arity = 2;
    Primitives['member'].displayName = 'member';



    Primitives['reverse'] = function(MACHINE) {
	var rev = NULL;
	var lst = MACHINE.env[MACHINE.env.length-1];
	while(lst !== NULL) {
	    testArgument(MACHINE,
			 'pair', isPair, lst, 0, 'reverse');
	    rev = [lst[0], rev];
	    lst = lst[1];
	}
	return rev;
    };
    Primitives['reverse'].arity = 1;
    Primitives['reverse'].displayName = 'reverse';








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
	    STACK_LIMIT_ESTIMATE = Math.floor(v / 2);
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
        plt.runtime.ready(function() {
            machine = machine || plt.runtime.currentMachine;
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
 
    exports['currentMachine'] = new Machine();
    exports['invokeMains'] = invokeMains;

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


    exports['isNumber'] = isNumber;
    exports['isNatural'] = isNatural;
    exports['isPair'] = isPair;
    exports['isList'] = isList;
    exports['isVector'] = isVector;
    exports['isOutputPort'] = isOutputPort;
    exports['isOutputStringPort'] = isOutputStringPort;
    exports['isEqual'] = isEqual;

    exports['ArityAtLeast'] = ArityAtLeast;
    exports['isArityMatching'] = isArityMatching;

    exports['heir'] = heir;
    exports['makeClassPredicate'] = makeClassPredicate;

    exports['HaltError'] = HaltError;

}).call(this);