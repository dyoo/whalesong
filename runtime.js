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

    var isPair = function(x) { return (typeof(x) == 'object' && 
				       x.length === 2) };
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
					 x.length !== undefined) };

    var Machine = function() {
	this.callsBeforeTrampoline = 100;
	this.val = undefined;
	this.proc = undefined;
	this.argcount = undefined;
	this.env = [];
	this.control = [];     // Arrayof (U CallFrame PromptFrame)
	this.running = false;
	this.params = { 'currentDisplayer': function(v) {},
			
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
			'maxNumBouncesBeforeYield': 2000 // self-adjusting
		      };
	this.primitives = Primitives;
    };



    var Frame = function() {};
    // Control stack elements:

    // A CallFrame represents a call stack frame.
    var CallFrame = function(label, proc) {
	this.label = label;
	this.proc = proc;
    };
    CallFrame.prototype = heir(Frame.prototype);

    // PromptFrame represents a prompt frame.
    var PromptFrame = function(label, tag) {
	this.label = label;
	this.tag = tag; // ContinuationPromptTag
    };
    PromptFrame.prototype = heir(Frame.prototype);








    var OutputPort = function() {};
    var isOutputPort = makeClassPredicate(OutputPort);


    var StandardOutputPort = function() {};
    StandardOutputPort.prototype = heir(OutputPort.prototype);
    StandardOutputPort.prototype.write = function(MACHINE, v) {
	MACHINE.params['currentDisplayer'](v);
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








    // A continuation prompt tag labels a prompt frame.
    var ContinuationPromptTag = function(name) {
	this.name = name;
    };



    // There is a single, distinguished default continuation prompt tag
    // that's used to wrap around toplevel prompts.
    var DEFAULT_CONTINUATION_PROMPT_TAG = 
	new ContinuationPromptTag("default-continuation-prompt-tag");


    var NULL = [];


    var raise = function(e) { throw e; }




    // testArgument: (X -> boolean) X number string string -> boolean
    // Produces true if val is true, and otherwise raises an error.
    var testArgument = function(expectedTypeName,
				predicate, 			    
				val, 
				position, 
				callerName) {
	if (predicate(val)) {
	    return true;
	}
	else {
	    raise(new Error(callerName + ": expected " + expectedTypeName
			    + " as argument #" + position 
			    + " but received " + val + " instead"));
	}
    };

    var testArity = function(callerName, observed, minimum, maximum) {
	if (observed < minimum || observed > maximum) {
	    raise(new Error(callerName + ": expected at least " + minimum
			    + " arguments "
			    + " but received " + observer));

	}
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
	raise(new Error("captureControl: unable to find tag " + tag));
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
	raise(new Error("restoreControl: unable to find tag " + tag));     

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
    Primitives['display'] = function(MACHINE, arity) {
	var firstArg = MACHINE.env[MACHINE.env.length-1];
	var outputPort = MACHINE.params.currentOutputPort;
	if (arity === 2) { 
	    outputPort = MACHINE.env[MACHINE.env.length-2];
	}
	outputPort.write(MACHINE, firstArg);
    };
    Primitives['display'].arity = [1, [2, NULL]];
    Primitives['display'].displayName = 'display';


    Primitives['newline'] = function(MACHINE, arity) {
	var outputPort = MACHINE.params.currentOutputPort;
	if (arity === 1) { 
	    outputPort = MACHINE.env[MACHINE.env.length-1];
	}
	outputPort.write(MACHINE, "\n");
    };
    Primitives['newline'].arity = [0, [1, NULL]];
    Primitives['newline'].displayName = 'newline';


    Primitives['displayln'] = function(MACHINE, arity){
	var firstArg = MACHINE.env[MACHINE.env.length-1];
	var outputPort = MACHINE.params.currentOutputPort;
	if (arity === 2) { 
	    outputPort = MACHINE.env[MACHINE.env.length-2];
	}
	outputPort.write(MACHINE, firstArg);
	outputPort.write(MACHINE, "\n");
    };
    Primitives['displayln'].arity = [1, [2, NULL]];
    Primitives['displayln'].displayName = 'displayln';

    Primitives['pi'] = Math.PI;

    Primitives['e'] = Math.E;

    Primitives['='] = function(MACHINE, arity) {
	var firstArg = MACHINE.env[MACHINE.env.length-1];
	testArgument('number', isNumber, firstArg, 0, '=');
	for (var i = 1; i < arity; i++) {
	    testArgument('number',
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


    Primitives['<'] = function(MACHINE, arity) {
	var firstArg = MACHINE.env[MACHINE.env.length-1];
	testArgument('number', isNumber, firstArg, 0, '<');
	for (var i = 1; i < arity; i++) {
	    testArgument('number',
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

    Primitives['>'] = function(MACHINE, arity) {
	var firstArg = MACHINE.env[MACHINE.env.length-1];
	testArgument('number', isNumber, firstArg, 0, '>');
	for (var i = 1; i < arity; i++) {
	    testArgument('number',
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

    Primitives['<='] = function(MACHINE, arity) {
	var firstArg = MACHINE.env[MACHINE.env.length-1];
	testArgument('number', isNumber, firstArg, 0, '<=');
	for (var i = 1; i < arity; i++) {
	    testArgument('number',
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


    Primitives['>='] = function(MACHINE, arity) {
	var firstArg = MACHINE.env[MACHINE.env.length-1];
	testArgument('number', isNumber, firstArg, 0, '>=');
	for (var i = 1; i < arity; i++) {
	    testArgument('number',
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
    

    Primitives['+'] = function(MACHINE, arity) {
	var result = 0;
	var i = 0;
	for (i=0; i < arity; i++) {
	    testArgument(
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
    

    Primitives['*'] = function(MACHINE, arity) {
	var result = 1;
	var i = 0;
	for (i=0; i < arity; i++) {
	    testArgument(
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
    
    Primitives['-'] = function(MACHINE, arity) {
	if (arity === 1) { 
	    testArgument('number',
			 isNumber,
			 MACHINE.env[MACHINE.env.length-1],
			 0,
			 '-');
	    return -(MACHINE.env[MACHINE.env.length-1]);
	}
	var result = MACHINE.env[MACHINE.env.length - 1];
	for (var i = 1; i < arity; i++) {
	    testArgument('number',
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
    
    Primitives['/'] = function(MACHINE, arity) {
	testArgument('number',
		     isNumber,
		     MACHINE.env[MACHINE.env.length - 1],
		     0,
		     '/');
	var result = MACHINE.env[MACHINE.env.length - 1];
	for (var i = 1; i < arity; i++) {
	    result /= MACHINE.env[MACHINE.env.length - 1 - i];
	}
	return result;
    };
    Primitives['/'].arity = new ArityAtLeast(1);
    Primitives['/'].displayName = '/';
    

    Primitives['cons'] = function(MACHINE, arity) {
	var firstArg = MACHINE.env[MACHINE.env.length-1];
	var secondArg = MACHINE.env[MACHINE.env.length-2];
	return [firstArg, secondArg];
    };
    Primitives['cons'].arity = 2;
    Primitives['cons'].displayName = 'cons';


    Primitives['list'] = function(MACHINE, arity) {
	var result = NULL;
	for (var i = 0; i < arity; i++) {
	    result = [MACHINE.env[MACHINE.env.length - (arity - i)],
		      result];
	}
	return result;
    };
    Primitives['list'].arity = new ArityAtLeast(0);
    Primitives['list'].displayName = 'list';

    Primitives['car'] = function(MACHINE, arity) {
	testArgument('pair',
		     isPair,
		     MACHINE.env[MACHINE.env.length - 1],
		     0,
		     'car');
	var firstArg = MACHINE.env[MACHINE.env.length-1];
	return firstArg[0];
    };
    Primitives['car'].arity = 1;
    Primitives['car'].displayName = 'car';

    Primitives['cdr'] = function(MACHINE, arity) {
	testArgument('pair',
		     isPair,
		     MACHINE.env[MACHINE.env.length - 1],
		     0,
		     'cdr');
	var firstArg = MACHINE.env[MACHINE.env.length-1];
	return firstArg[1];
    };
    Primitives['cdr'].arity = 1;
    Primitives['cdr'].displayName = 'cdr';

    Primitives['pair?'] = function(MACHINE, arity) {
	var firstArg = MACHINE.env[MACHINE.env.length-1];
	return isPair(firstArg);
    };
    Primitives['pair?'].arity = 1;
    Primitives['pair?'].displayName = 'pair?';

    Primitives['set-car!'] = function(MACHINE, arity) {
	testArgument('pair',
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

    Primitives['set-cdr!'] = function(MACHINE, arity) {
	testArgument('pair',
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
    
    Primitives['not'] = function(MACHINE, arity) {
	var firstArg = MACHINE.env[MACHINE.env.length-1];
	return (!firstArg);
    };
    Primitives['not'].arity = 1;
    Primitives['not'].displayName = 'not';

    Primitives['null'] = NULL;

    Primitives['null?'] = function(MACHINE, arity) {
	var firstArg = MACHINE.env[MACHINE.env.length-1];
	return firstArg === NULL;
    };
    Primitives['null?'].arity = 1;
    Primitives['null?'].displayName = 'null?';

    Primitives['add1'] = function(MACHINE, arity) {
	testArgument('number',
		     isNumber,
		     MACHINE.env[MACHINE.env.length - 1],
		     0,
		     'add1');
	var firstArg = MACHINE.env[MACHINE.env.length-1];
	return firstArg + 1;
    };
    Primitives['add1'].arity = 1;
    Primitives['add1'].displayName = 'add1';

    Primitives['sub1'] = function(MACHINE, arity) {
	testArgument('number',
		     isNumber,
		     MACHINE.env[MACHINE.env.length - 1],
		     0,
		     'sub1');
	var firstArg = MACHINE.env[MACHINE.env.length-1];
	return firstArg - 1;
    };
    Primitives['sub1'].arity = 1;
    Primitives['sub1'].displayName = 'sub1';

    Primitives['zero?'] = function(MACHINE, arity) {
	var firstArg = MACHINE.env[MACHINE.env.length-1];
	return firstArg === 0;
    };
    Primitives['zero?'].arity = 1;
    Primitives['zero?'].displayName = 'zero?';

    Primitives['vector'] = function(MACHINE, arity) {
	var i;
	var result = [];
	for (i = 0; i < arity; i++) {
	    result.push(MACHINE.env[MACHINE.env.length-1-i]);
	}
	return result;
    };
    Primitives['vector'].arity = new ArityAtLeast(0);
    Primitives['vector'].displayName = 'vector';

    Primitives['vector->list'] = function(MACHINE, arity) {
	testArgument('vector',
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
    
    Primitives['list->vector'] = function(MACHINE, arity) {
	var firstArg = MACHINE.env[MACHINE.env.length-1];
	var result = [];
	while (firstArg !== NULL) {
	    result.push(firstArg[0]);
	    firstArg = firstArg[1];
	}
	return result;
    };
    Primitives['list->vector'].arity = 1;
    Primitives['list->vector'].displayName = 'list->vector';

    Primitives['vector-ref'] = function(MACHINE, arity) {
	testArgument('vector',
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

    Primitives['vector-set!'] = function(MACHINE, arity) {
	testArgument('vector',
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

    Primitives['symbol?'] = function(MACHINE, arity) {
	var firstArg = MACHINE.env[MACHINE.env.length-1];
	return typeof(firstArg) === 'string';
    };
    Primitives['symbol?'].arity = 1;
    Primitives['symbol?'].displayName = 'symbol?';

    Primitives['symbol->string'] = function(MACHINE, arity) {
	var firstArg = MACHINE.env[MACHINE.env.length-1];
	return firstArg;
    };
    Primitives['symbol->string'].arity = 1;
    Primitives['symbol->string'].displayName = 'symbol->string';

    Primitives['string-append'] = function(MACHINE, arity) {
	var buffer = [];
	var i;
	for (i = 0; i < arity; i++) {
	    buffer.push(MACHINE.env[MACHINE.env.length - 1 - i]);
	}
	return buffer.join('');
    };
    Primitives['string-append'].arity = new ArityAtLeast(0);
    Primitives['string-append'].displayName = 'string-append';

    Primitives['string-length'] = function(MACHINE, arity) {
	var firstArg = MACHINE.env[MACHINE.env.length-1];
	return firstArg.length;
    };
    Primitives['string-length'].arity = 1;
    Primitives['string-length'].displayName = 'string-length';
    
    Primitives['box'] = function(MACHINE, arity) {
	var firstArg = MACHINE.env[MACHINE.env.length-1];
	var result = [firstArg];
	return result;
    };
    Primitives['box'].arity = 1;
    Primitives['box'].displayName = 'box';

    Primitives['unbox'] = function(MACHINE, arity) {
	var firstArg = MACHINE.env[MACHINE.env.length-1];
	return firstArg[0];
    };
    Primitives['unbox'].arity = 1;
    Primitives['unbox'].displayName = 'unbox';

    Primitives['set-box!'] = function(MACHINE, arity) {
	var firstArg = MACHINE.env[MACHINE.env.length-1];
	var secondArg = MACHINE.env[MACHINE.env.length-2];
	firstArg[0] = secondArg;
	return;
    };
    Primitives['set-box!'].arity = 2;
    Primitives['set-box!'].displayName = 'set-box!';

    Primitives['void'] = function(MACHINE, arity) {
	return;
    };
    Primitives['void'].arity = new ArityAtLeast(0);
    Primitives['void'].displayName = 'void';

    Primitives['eq?'] = function(MACHINE, arity) {
	var firstArg = MACHINE.env[MACHINE.env.length-1];
	var secondArg = MACHINE.env[MACHINE.env.length-2];
	return firstArg === secondArg;
    };
    Primitives['eq?'].arity = 2;
    Primitives['eq?'].displayName = 'eq?';

    Primitives['equal?'] = function(MACHINE, arity) {
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


    Primitives['member'] = function(MACHINE, arity) {
	var x = MACHINE.env[MACHINE.env.length-1];
	var lst = MACHINE.env[MACHINE.env.length-2];
	var originalLst = lst;
	while (true) {
	    if (! isList(lst)) {
		raise(new Error("member: expected list" 
				+ " as argument #2"
				+ " but received " + originalLst + " instead"));
	    };
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


    var trampoline = function(MACHINE, initialJump) {
	var thunk = initialJump;
	var startTime = (new Date()).valueOf();
	MACHINE.callsBeforeTrampoline = 100;
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
                    MACHINE.callsBeforeTrampoline = 100;

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
		} else {
		    MACHINE.running = false;
	            return MACHINE.params.currentErrorHandler(MACHINE, e);
		}
            }
	}
	MACHINE.running = false;
	return MACHINE.params.currentSuccessHandler(MACHINE);
    };




    // Exports
    exports['Machine'] = Machine;
    exports['CallFrame'] = CallFrame;
    exports['PromptFrame'] = PromptFrame;
    exports['Closure'] = Closure;
    exports['ContinuationPromptTag'] = ContinuationPromptTag;
    exports['DEFAULT_CONTINUATION_PROMPT_TAG'] = 
	DEFAULT_CONTINUATION_PROMPT_TAG;
    exports['NULL'] = NULL;

    exports['testArgument'] = testArgument;
    exports['testArity'] = testArity;
    exports['raise'] = raise;

    exports['captureControl'] = captureControl;
    exports['restoreControl'] = restoreControl;

    exports['trampoline'] = trampoline;
    exports['spliceListIntoStack'] = spliceListIntoStack;
    exports['unspliceRestFromStack'] = unspliceRestFromStack;


    exports['isNumber'] = isNumber;
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



}).call(this);