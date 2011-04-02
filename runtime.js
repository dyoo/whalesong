if(this['plt'] === undefined) {
    this['plt'] = {};
}


// All of the values here are namespaced under "plt.runtime".

(function() {
    this['plt']['runtime'] = {};
    var exports = this['plt']['runtime'];


    var Machine = function() {
	this.callsBeforeTrampoline = 100;
	this.val = undefined;
	this.proc = undefined;
	this.env = [];
	this.control = [];     // Arrayof (U CallFrame PromptFrame)
	this.running = false;
	this.params = { currentDisplayer: function(v) {},
			
			currentSuccessHandler: function(MACHINE) {},
			currentErrorHandler: function(MACHINE, exn) {},
			
			currentNamespace: {},
			
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
			desiredYieldsPerSecond: 5,
			numBouncesBeforeYield: 2000,   // self-adjusting
			maxNumBouncesBeforeYield: 2000 // self-adjusting
		      };
	this.primitives = Primitives;
    };


    // Control stack elements:

    // A CallFrame represents a call stack frame.
    var CallFrame = function(label, proc) {
	this.label = label;
	this.proc = proc;
    };

    // PromptFrame represents a prompt frame.
    var PromptFrame = function(label, tag) {
	this.label = label;
	this.tag = tag; // ContinuationPromptTag
    };




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



    // A primitive function is just a Javascript function.





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

    }




    var isNumber = function(x) { return typeof(x) === 'number'; };


    var raise = function(e) { throw e; }


    var NULL = [];
    

    // Primtitives are the set of primitive values.  Not all primitives
    // are coded here; several of them (including call/cc) are injected by
    // the bootstrapping code.
    var Primitives = (function() {
	return {
	    'display': function(MACHINE, arity) {
		var firstArg = MACHINE.env[MACHINE.env.length-1];
		MACHINE.params.currentDisplayer(firstArg);
	    },

	    'newline': function(MACHINE, arity) {
		MACHINE.params.currentDisplayer("\n");
	    },

	    'displayln': function(MACHINE, arity){
		var firstArg = MACHINE.env[MACHINE.env.length-1];
		MACHINE.params.currentDisplayer(firstArg);
		MACHINE.params.currentDisplayer("\n");
	    },

	    'pi' : Math.PI,

	    'e' : Math.E,

	    '=': function(MACHINE, arity) {
		var firstArg = MACHINE.env[MACHINE.env.length-1];
		var secondArg = MACHINE.env[MACHINE.env.length-2];
		testArgument('number', isNumber, firstArg, 0, '=');
		testArgument('number', isNumber, secondArg, 1, '=');
		return firstArg === secondArg;
	    },

	    '<': function(MACHINE, arity) {
		var firstArg = MACHINE.env[MACHINE.env.length-1];
		var secondArg = MACHINE.env[MACHINE.env.length-2];
		testArgument('number', isNumber, firstArg, 0, '<');
		testArgument('number', isNumber, secondArg, 1, '<');
		return firstArg < secondArg;
	    },

	    '>': function(MACHINE, arity) {
		var firstArg = MACHINE.env[MACHINE.env.length-1];
		var secondArg = MACHINE.env[MACHINE.env.length-2];
		testArgument('number', isNumber, firstArg, 0, '>');
		testArgument('number', isNumber, secondArg, 1, '>');
		return firstArg > secondArg;
	    },

	    '<=': function(MACHINE, arity) {
		var firstArg = MACHINE.env[MACHINE.env.length-1];
		var secondArg = MACHINE.env[MACHINE.env.length-2];
		testArgument('number', isNumber, firstArg, 0, '<=');
		testArgument('number', isNumber, secondArg, 1, '<=');
		return firstArg <= secondArg;
	    },

	    '>=': function(MACHINE, arity) {
		var firstArg = MACHINE.env[MACHINE.env.length-1];
		var secondArg = MACHINE.env[MACHINE.env.length-2];
		testArgument('number', isNumber, firstArg, 0, '>=');
		testArgument('number', isNumber, secondArg, 1, '>=');
		return firstArg >= secondArg;
	    },
	    
	    '+': function(MACHINE, arity) {
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
	    },
	    
	    '*': function(MACHINE, arity) {
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
	    },
	    
	    '-': function(MACHINE, arity) {
		if (arity === 0) { raise(new Error()); }
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
	    },
	    
	    '/': function(MACHINE, arity) {
		if (arity === 0) { raise(new Error()); }
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
	    },

	    'cons': function(MACHINE, arity) {
		var firstArg = MACHINE.env[MACHINE.env.length-1];
		var secondArg = MACHINE.env[MACHINE.env.length-2];
		return [firstArg, secondArg];
	    },

	    'list': function(MACHINE, arity) {
		var result = NULL;
		for (var i = 0; i < arity; i++) {
		    result = [MACHINE.env[MACHINE.env.length - (arity - i)],
			      result];
		}
		return result;
	    },

	    'car': function(MACHINE, arity) {
		var firstArg = MACHINE.env[MACHINE.env.length-1];
		return firstArg[0];
	    },

	    'cdr': function(MACHINE, arity) {
		var firstArg = MACHINE.env[MACHINE.env.length-1];
		return firstArg[1];
	    },

	    'pair?': function(MACHINE, arity) {
		var firstArg = MACHINE.env[MACHINE.env.length-1];
		return (typeof(firstArg) == 'object' && 
			firstArg.length === 2);
	    },

	    'set-car!': function(MACHINE, arity) {
		var firstArg = MACHINE.env[MACHINE.env.length-1];
		var secondArg = MACHINE.env[MACHINE.env.length-2];
		firstArg[0] = secondArg;
	    },

	    'set-cdr!': function(MACHINE, arity) {
		var firstArg = MACHINE.env[MACHINE.env.length-1];
		var secondArg = MACHINE.env[MACHINE.env.length-2];
		firstArg[1] = secondArg;
	    },
	    
	    'not': function(MACHINE, arity) {
		var firstArg = MACHINE.env[MACHINE.env.length-1];
		return (!firstArg);
	    },

	    'null' : NULL,

	    'null?': function(MACHINE, arity) {
		var firstArg = MACHINE.env[MACHINE.env.length-1];
		return firstArg === NULL;
	    },

	    'add1': function(MACHINE, arity) {
		var firstArg = MACHINE.env[MACHINE.env.length-1];
		return firstArg + 1;
	    },

	    'sub1': function(MACHINE, arity) {
		var firstArg = MACHINE.env[MACHINE.env.length-1];
		return firstArg - 1;
	    },

	    'zero?': function(MACHINE, arity) {
		var firstArg = MACHINE.env[MACHINE.env.length-1];
		return firstArg === 0;
	    },

	    'vector': function(MACHINE, arity) {
		var i;
		var result = [];
		for (i = 0; i < arity; i++) {
		    result.push(MACHINE.env[MACHINE.env.length-1-i]);
		}
		return result;
	    },

	    'vector->list': function(MACHINE, arity) {
		var firstArg = MACHINE.env[MACHINE.env.length-1];
		var i;
		var result = NULL;
		for (i = 0; i < firstArg.length; i++) {
		    result = [firstArg[firstArg.length - 1 - i], result];
		}
		return result;
	    },
	    
	    'list->vector': function(MACHINE, arity) {
		var firstArg = MACHINE.env[MACHINE.env.length-1];
		var result = [];
		while (firstArg !== NULL) {
		    result.push(firstArg[0]);
		    firstArg = firstArg[1];
		}
		return result;
	    },

	    'vector-ref': function(MACHINE, arity) {
		var firstArg = MACHINE.env[MACHINE.env.length-1];
		var secondArg = MACHINE.env[MACHINE.env.length-2];
		return firstArg[secondArg];
	    },

	    'vector-set!': function(MACHINE, arity) {
		var firstArg = MACHINE.env[MACHINE.env.length-1];
		var secondArg = MACHINE.env[MACHINE.env.length-2];
		var thirdArg = MACHINE.env[MACHINE.env.length-3];
		firstArg[secondArg] = thirdArg;
		return null;
	    },

	    'symbol?': function(MACHINE, arity) {
		var firstArg = MACHINE.env[MACHINE.env.length-1];
		return typeof(firstArg) === 'string';
	    },

	    'symbol->string': function(MACHINE, arity) {
		var firstArg = MACHINE.env[MACHINE.env.length-1];
		return firstArg;
	    },

	    'string-append': function(MACHINE, arity) {
		var buffer = [];
		var i;
		for (i = 0; i < arity; i++) {
		    buffer.push(MACHINE.env[MACHINE.env.length - 1 - i]);
		}
		return buffer.join('');
	    },

	    'string-length': function(MACHINE, arity) {
		var firstArg = MACHINE.env[MACHINE.env.length-1];
		return firstArg.length;
	    },
	    
	    'box': function(MACHINE, arity) {
		var firstArg = MACHINE.env[MACHINE.env.length-1];
		var result = [firstArg];
		return result;
	    },

	    'unbox': function(MACHINE, arity) {
		var firstArg = MACHINE.env[MACHINE.env.length-1];
		return firstArg[0];
            },

	    'set-box!': function(MACHINE, arity) {
		var firstArg = MACHINE.env[MACHINE.env.length-1];
		var secondArg = MACHINE.env[MACHINE.env.length-2];
		firstArg[0] = secondArg;
		return;
	    },

	    'void': function(MACHINE, arity) {
		return;
	    },


	    'eq?': function(MACHINE, arity) {
		var firstArg = MACHINE.env[MACHINE.env.length-1];
		var secondArg = MACHINE.env[MACHINE.env.length-2];
		return firstArg === secondArg;
	    },

	    'equal?': function(MACHINE, arity) {
		var firstArg = MACHINE.env[MACHINE.env.length-1];
		var secondArg = MACHINE.env[MACHINE.env.length-2];
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
	    }
	};
    })();





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
    exports.Machine = Machine;
    exports.CallFrame = CallFrame;
    exports.PromptFrame = PromptFrame;
    exports.Closure = Closure;
    exports.ContinuationPromptTag = ContinuationPromptTag;
    exports.DEFAULT_CONTINUATION_PROMPT_TAG = DEFAULT_CONTINUATION_PROMPT_TAG;
    exports.testArgument = testArgument;
    exports.captureControl = captureControl;
    exports.restoreControl = restoreControl;
    exports.isNumber = isNumber;
    exports.raise = raise;
    exports.NULL = NULL;
    exports.trampoline = trampoline;

}).call(this);