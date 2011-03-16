// Type representations:
//
// number are numbers
//
// cons pairs are [first, rest]
// 
// function closures are Closures
// primitive procedures are regular functions.


// No error trapping at the moment.


var Frame = function(label, proc) {
    this.label = label;
    this.proc = proc;
};


// A closure consists of its free variables as well as a label
// into its text segment.
var Closure = function(label, arity, closedVals, displayName) {
    this.label = label;
    this.arity = arity;
    this.closedVals = closedVals;
    this.displayName = displayName;
};



var Primitives = (function() {
    var NULL = [];
    return {
	'display': function(arity, returnLabel) {
	    var firstArg = MACHINE.env[MACHINE.env.length-1];
            MACHINE.params.currentDisplayer(firstArg);
	},

	'newline': function(arity, returnLabel) {
            MACHINE.params.currentDisplayer("\n");
	},

	'displayln': function(arity, returnLabel){
	    var firstArg = MACHINE.env[MACHINE.env.length-1];
            MACHINE.params.currentDisplayer(firstArg);
            MACHINE.params.currentDisplayer("\n");
	},

	'pi' : Math.PI,

	'e' : Math.E,

	'=': function(arity, returnLabel) {
	    var firstArg = MACHINE.env[MACHINE.env.length-1];
	    var secondArg = MACHINE.env[MACHINE.env.length-2];
            return firstArg === secondArg;
	},

	'<': function(arity, returnLabel) {
	    var firstArg = MACHINE.env[MACHINE.env.length-1];
	    var secondArg = MACHINE.env[MACHINE.env.length-2];
	    return firstArg < secondArg;
	},

	'>': function(arity, returnLabel) {
	    var firstArg = MACHINE.env[MACHINE.env.length-1];
	    var secondArg = MACHINE.env[MACHINE.env.length-2];
	    return firstArg > secondArg;
	},

	'<=': function(arity, returnLabel) {
	    var firstArg = MACHINE.env[MACHINE.env.length-1];
	    var secondArg = MACHINE.env[MACHINE.env.length-2];
	    return firstArg <= secondArg;
	},

	'>=': function(arity, returnLabel) {
	    var firstArg = MACHINE.env[MACHINE.env.length-1];
	    var secondArg = MACHINE.env[MACHINE.env.length-2];
	    return firstArg >= secondArg;
	},
	
	'+': function(arity, returnLabel) {
	    var firstArg = MACHINE.env[MACHINE.env.length-1];
	    var secondArg = MACHINE.env[MACHINE.env.length-2];

            return firstArg + secondArg;
	},
	
	'*': function(arity, returnLabel) {
	    var firstArg = MACHINE.env[MACHINE.env.length-1];
	    var secondArg = MACHINE.env[MACHINE.env.length-2];
            return firstArg * secondArg;
	},
	
	'-': function(arity, returnLabel) {
	    var firstArg = MACHINE.env[MACHINE.env.length-1];
	    var secondArg = MACHINE.env[MACHINE.env.length-2];
            return firstArg - secondArg;
	},
	
	'/': function(arity, returnLabel) {
	    var firstArg = MACHINE.env[MACHINE.env.length-1];
	    var secondArg = MACHINE.env[MACHINE.env.length-2];
	    return firstArg / secondArg;
	},

	'cons': function(arity, returnLabel) {
	    var firstArg = MACHINE.env[MACHINE.env.length-1];
	    var secondArg = MACHINE.env[MACHINE.env.length-2];
	    return [firstArg, secondArg];
	},

	'list': function(arity, returnLabel) {
	    var result = NULL;
	    for (var i = 0; i < arity; i++) {
		result = [MACHINE.env[MACHINE.env.length - (arity - i)],
			  result];
	    }
	    return result;
	},

	'car': function(arity, returnLabel) {
	    var firstArg = MACHINE.env[MACHINE.env.length-1];
	    return firstArg[0];
	},

	'cdr': function(arity, returnLabel) {
	    var firstArg = MACHINE.env[MACHINE.env.length-1];
	    return firstArg[1];
	},

	'pair?': function(arity, returnLabel) {
	    var firstArg = MACHINE.env[MACHINE.env.length-1];
	    return (typeof(firstArg) == 'object' && 
		    firstArg.length === 2);
	},

	'set-car!': function(arity, returnLabel) {
	    var firstArg = MACHINE.env[MACHINE.env.length-1];
	    var secondArg = MACHINE.env[MACHINE.env.length-2];
	    firstArg[0] = secondArg;
	},

	'set-cdr!': function(arity, returnLabel) {
	    var firstArg = MACHINE.env[MACHINE.env.length-1];
	    var secondArg = MACHINE.env[MACHINE.env.length-2];
	    firstArg[1] = secondArg;
	},
	
	'not': function(arity, returnLabel) {
	    var firstArg = MACHINE.env[MACHINE.env.length-1];
	    return (!firstArg);
	},

	'null' : NULL,

	'null?': function(arity, returnLabel) {
	    var firstArg = MACHINE.env[MACHINE.env.length-1];
	    return firstArg === NULL;
	},

	'add1': function(arity, returnLabel) {
	    var firstArg = MACHINE.env[MACHINE.env.length-1];
	    return firstArg + 1;
	},

	'sub1': function(arity, returnLabel) {
	    var firstArg = MACHINE.env[MACHINE.env.length-1];
	    return firstArg - 1;
	},

	'zero?': function(arity, returnLabel) {
	    var firstArg = MACHINE.env[MACHINE.env.length-1];
	    return firstArg === 0;
	},

	'vector': function(arity, returnLabel) {
	    var i;
	    var result = [];
	    for (i = 0; i < arity; i++) {
		result.push(MACHINE.env[MACHINE.env.length-1-i]);
	    }
	    return result;
	},

	'vector->list': function(arity, returnLabel) {
	    var firstArg = MACHINE.env[MACHINE.env.length-1];
	    var i;
	    var result = NULL;
	    for (i = 0; i < firstArg.length; i++) {
		result = [firstArg[firstArg.length - 1 - i], result];
	    }
	    return result;
	},
	
	'list->vector': function(arity, returnLabel) {
	    var firstArg = MACHINE.env[MACHINE.env.length-1];
	    var result = [];
	    while (firstArg !== NULL) {
		result.push(firstArg[0]);
		firstArg = firstArg[1];
	    }
	    return result;
	},

	'vector-ref': function(arity, returnLabel) {
	    var firstArg = MACHINE.env[MACHINE.env.length-1];
	    var secondArg = MACHINE.env[MACHINE.env.length-2];
	    return firstArg[secondArg];
	},

	'vector-set!': function(arity, returnLabel) {
	    var firstArg = MACHINE.env[MACHINE.env.length-1];
	    var secondArg = MACHINE.env[MACHINE.env.length-2];
	    var thirdArg = MACHINE.env[MACHINE.env.length-3];
	    firstArg[secondArg] = thirdArg;
	    return null;
	},

	'symbol?': function(arity, returnLabel) {
	    var firstArg = MACHINE.env[MACHINE.env.length-1];
	    return typeof(firstArg) === 'string';
	},

	'symbol->string': function(arity, returnLabel) {
	    var firstArg = MACHINE.env[MACHINE.env.length-1];
	    return firstArg;
	},

	'string-append': function(arity, returnLabel) {
	    var buffer = [];
	    var i;
	    for (i = 0; i < arity; i++) {
		buffer.push(MACHINE.env[MACHINE.env.length - 1 - i]);
	    }
	    return buffer.join('');
	},

	'string-length': function(arity, returnLabel) {
	    var firstArg = MACHINE.env[MACHINE.env.length-1];
	    return firstArg.length;
	},
	
	'box': function(arity, returnLabel) {
	    var firstArg = MACHINE.env[MACHINE.env.length-1];
	    var result = [firstArg];
	    return result;
	},

	'unbox': function(arity, returnLabel) {
	    var firstArg = MACHINE.env[MACHINE.env.length-1];
	    return firstArg[0];
        },

	'set-box!': function(arity, returnLabel) {
	    var firstArg = MACHINE.env[MACHINE.env.length-1];
	    var secondArg = MACHINE.env[MACHINE.env.length-2];
	    firstArg[0] = secondArg;
	    return;
	},

	'void': function(arity, returnLabel) {
	    return;
	},


	'eq?': function(arity, returnLabel) {
	    var firstArg = MACHINE.env[MACHINE.env.length-1];
	    var secondArg = MACHINE.env[MACHINE.env.length-2];
	    return firstArg === secondArg;
	},

	'equal?': function(arity, returnLabel) {
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

// 	, 
//  	'call/cc': new Closure(callCCEntry,
//  			       1,
//  			       [],
//  			       "call/cc"),
// 	'call-with-current-continuation': new Closure(callCCEntry,
//  						      1,
//  						      [],
//  						      "call-with-current-continuation")

    };
})();




// // adaptToJs: closure -> (array (X -> void) -> void)
// // Converts closures to functions that can be called from the
// // JavaScript toplevel.
// Closure.prototype.adaptToJs = function() {
//     var that = this;
//     return function(args, success, fail) {
//         var oldEnv = MACHINE.env;
// 	var oldCont = MACHINE.cont;
// 	var oldProc = MACHINE.proc;
// 	var oldArgl = MACHINE.argl;
// 	var oldVal = MACHINE.val;
// 	trampoline(
// 	    function() {
// 		var proc = that;
// 		MACHINE.proc = proc;
// 		MACHINE.argl = undefined;
// 		for(var i = args.length - 1; i >= 0; i--) {
// 		    MACHINE.argl = [args[i], MACHINE.argl];
// 		}
		
// 		MACHINE.cont = function() {
// 		    var result = MACHINE.val;
//                     MACHINE.env = oldEnv;
// 		    MACHINE.cont = oldCont;
// 		    MACHINE.proc = oldProc;
// 		    MACHINE.argl = oldArgl;
// 		    MACHINE.val = oldVal;
//                     success(result);
// 		};
		
// 		proc.label();
//             },
//             function() {
//             },
//             function(e) {
// 		return fail(e);
// 	    });
//     }
// };



var MACHINE = { callsBeforeTrampoline: 100, 
		val:undefined,
		proc:undefined, 
		env: [],
		control : [],
		running : false,
		params: { currentDisplayer: function(v) {},
			  currentErrorHandler: function(e) {},
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
			}
	      };



// recomputeGas: state number -> number
var recomputeMaxNumBouncesBeforeYield = function(observedDelay) {
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


var trampoline = function(initialJump, success, fail) {
    var thunk = initialJump;
    var startTime = (new Date()).valueOf();
    MACHINE.callsBeforeTrampoline = 100;
    MACHINE.params.numBouncesBeforeYield = 
	MACHINE.params.maxNumBouncesBeforeYield;
    MACHINE.running = true;

    while(thunk) {
        try {
            thunk();
	    break;
        } catch (e) {
            if (typeof(e) === 'function') {
                thunk = e;
                MACHINE.callsBeforeTrampoline = 100;

		if (MACHINE.params.numBouncesBeforeYield-- < 0) {
		    recomputeMaxNumBouncesBeforeYield(
			(new Date()).valueOf() - startTime);
		    setTimeout(
			function() {
			    trampoline(thunk, success, fail);
			},
			0);
		    return;
		}
            } else {
		MACHINE.running = false;
	        return fail(e);
            }
        }
    }
    MACHINE.running = false;
    return success();
};
