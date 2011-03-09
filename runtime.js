// Type representations:
//
// number are numbers
//
// cons pairs are [first, rest]
// 
// function closures are Closures
// primitive procedures are regular functions.


// No error trapping at the moment.
var Primitives = {
    'display': function(argl) {
        MACHINE.params.currentDisplayer(argl[0]);
    },

    'newline': function(argl) {
        MACHINE.params.currentDisplayer("\n");
    },

    'displayln': function(argl){
        MACHINE.params.currentDisplayer(argl[0]);
        MACHINE.params.currentDisplayer("\n");
    },

    'pi' : Math.PI,

    'e' : Math.E,

    '=': function(argl) {
        return argl[0] === argl[1][0];
    },

    '<': function(argl) {
	return argl[0] < argl[1][0];
    },
    
    '+': function(argl) {
        return argl[0] + argl[1][0];
    },
    
    '*': function(argl) {
        return argl[0] * argl[1][0];
    },
    
    '-': function(argl) {
        return argl[0] - argl[1][0];
    },
    
    '/': function(argl) {
	return argl[0] / argl[1][0];
    },

    'cons': function(argl) {
	return [argl[0], argl[1][0]];
    },

    'list': function(argl) {
	return argl;
    },

    'car': function(argl) {
	return argl[0][0];
    },

    'cdr': function(argl) {
	return argl[0][1];
    },

    'null' : undefined,

    'null?': function(argl) {
	return argl[0] === undefined;
    }
};



// A closure consists of its free variables as well as a label
// into its text segment.
var Closure = function(env, label) {
    this.env = env;
    this.label = label;
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
             params: {currentDisplayer: function(v) {},
		      currentErrorHandler: function(e) {}}};


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
