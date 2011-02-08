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

	'-': function(argl) {
            return argl[0] - argl[1][0];
        }
    };
    this.valss = [];
};

var ExtendedEnvironment = function(parent, vs) {
    var vals = [];
    while(vs) {
	vals.push(vs[0]);
	vs = vs[1];
    }
    this.valss = parent.valss.slice();
    this.valss.shift(vals);
    this.globalBindings = parent.globalBindings;
};


// A closure consists of its free variables as well as a label
// into its text segment.
var Closure = function(env, label) {
    this.env = env;
    this.label = label;
};


var MACHINE={callsBeforeTrampoline: 100, 
             env: new TopEnvironment(),
             proc:undefined, 
             argl:undefined,
             val:undefined,
             cont:undefined,
             stack: []};


// harness: (->) (->) -> void
var _harness = function(thunk, k) {
    var toCall;
    MACHINE.callsBeforeTrampoline = 100;
    while(thunk) {
        try {
            toCall = thunk;
            thunk = undefined;
            toCall();
        } catch (e) {
            if (typeof(e) === 'function') {
                thunk = e;
                MACHINE.callsBeforeTrampoline = 100;
            } else if (e === 'done') {
                break;
            } else {
	        throw e;
            }
        }
    }
    k();
};
