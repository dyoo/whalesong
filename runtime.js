// Type representations:
//
// number are numbers
//
// cons pairs are [first, rest]
// 
// function closures are Closures
// primitive procedures are regular functions.

var TopEnvironment = function() {
    this.globalBindings = {
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
	}
    };
    this.valss = [];
};

var ExtendedPrefixEnvironment = function(parent, vs) {
    var vals = [];
    while(vs) {
	if (parent.globalBindings[vs[0]]) {
	    vals.push(parent.globalBindings[vs[0]]);
	} else {
	    vals.push(undefined);
	}	
	vs = vs[1];
    }
    this.valss = parent.valss.slice();
    this.valss.unshift(vals);
    this.globalBindings = parent.globalBindings;
};

var ExtendedEnvironment = function(parent, vs) {
    var vals = [];
    while(vs) {
	vals.push(vs[0]);
	vs = vs[1];
    }
    this.valss = parent.valss.slice();
    this.valss.unshift(vals);
    this.globalBindings = parent.globalBindings;
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
    return function(args, k) {
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
                    k(result);
		};
		
		proc.label();
            },
            function() {
            });
    }
};



var MACHINE={callsBeforeTrampoline: 100, 
             env: new TopEnvironment(),
             proc:undefined, 
             argl:undefined,
             val:undefined,
             cont:undefined,
             stack: []};


// harness: (->) (->) -> void
var trampoline = function(initialJump, k) {
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
	        throw e;
            }
        }
    }
    k();
};
