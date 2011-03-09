// Type representations:
//
// number are numbers
//
// cons pairs are [first, rest]
// 
// function closures are Closures
// primitive procedures are regular functions.


// No error trapping at the moment.

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

	'null' : NULL,

	'null?': function(arity, returnLabel) {
	    var firstArg = MACHINE.env[MACHINE.env.length-1];
	    return firstArg === NULL;
	}
    };
})();


var Frame = function(label) {
    this.label = label;
};


// A closure consists of its free variables as well as a label
// into its text segment.
var Closure = function(label, arity, closedVals, displayName) {
    this.label = label;
    this.arity = arity;
    this.closedVals = closedVals;
    this.displayName = displayName;
};


// adaptToJs: closure -> (array (X -> void) -> void)
// Converts closures to functions that can be called from the
// JavaScript toplevel.
Closure.prototype.adaptToJs = function() {
    var that = this;
    return function(args, success, fail) {
        var oldEnv = MACHINE.env;
	var oldCont = MACHINE.cont;
	var oldProc = MACHINE.proc;
	var oldArgl = MACHINE.argl;
	var oldVal = MACHINE.val;
	trampoline(
	    function() {
		var proc = that;
		MACHINE.proc = proc;
		MACHINE.argl = undefined;
		for(var i = args.length - 1; i >= 0; i--) {
		    MACHINE.argl = [args[i], MACHINE.argl];
		}
		
		MACHINE.cont = function() {
		    var result = MACHINE.val;
                    MACHINE.env = oldEnv;
		    MACHINE.cont = oldCont;
		    MACHINE.proc = oldProc;
		    MACHINE.argl = oldArgl;
		    MACHINE.val = oldVal;
                    success(result);
		};
		
		proc.label();
            },
            function() {
            },
            function(e) {
		return fail(e);
	    });
    }
};



var MACHINE={callsBeforeTrampoline: 100, 
             val:undefined,
             proc:undefined, 
             env: [],
	     control : [],
             params: { currentDisplayer: function(v) {},
		       currentErrorHandler: function(e) {},
		       currentNamespace: {}}};


var trampoline = function(initialJump, success, fail) {
    var thunk = initialJump;
    MACHINE.callsBeforeTrampoline = 100;
    while(thunk) {
        try {
            thunk();
	    break;
        } catch (e) {
            if (typeof(e) === 'function') {
                thunk = e;
                MACHINE.callsBeforeTrampoline = 100;
            } else {
	        return fail(e);
            }
        }
    }
    return success();
};
