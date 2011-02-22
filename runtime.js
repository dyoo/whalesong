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

    '=': function(argl) {
        return argl[0] === argl[1][0];
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
    'null?': function(argl) {
	return argl[0] === undefined;
    }
};


var TopEnvironment = function() {    
    this.valss = [];
};


var ExtendedPrefixEnvironment = function(parent, vs) {
    var vals = [];
    this.names = [];
    while(vs) {
	this.names.push(vs[0]);
	if (Primitives[vs[0]]) {
	    vals.push(Primitives[vs[0]]);
	} else {
	    vals.push(undefined);
	}	
	vs = vs[1];
    }

    this.valss = parent.valss.slice();
    this.valss.unshift(vals);
};

ExtendedPrefixEnvironment.prototype.lookup = function(name) {
    var i;
    for (i = 0; i < this.names.length; i++) {
	if (this.names[i] === name) {
	    return this.valss[0][i];
	}
    }
    return undefined;
};

var ExtendedEnvironment = function(parent, vs) {
    var vals = [];
    while(vs) {
	vals.push(vs[0]);
	vs = vs[1];
    }
    this.valss = parent.valss.slice();
    this.valss.unshift(vals);
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
             env: new TopEnvironment(),
             proc:undefined, 
             argl:undefined,
             val:undefined,
             cont:undefined,
             stack: [],
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
