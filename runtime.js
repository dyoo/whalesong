var TopEnvironment = function() {
    this.bindings = {'=': function(argl) {
                         return argl[0] === argl[1][0];
                     },
                     '+': function(argl) {
                         return argl[0] + argl[1][0];
                     },
		     '-': function(argl) {
                         return argl[0] - argl[1][0];
                     }
		     };
    this.parent = undefined;
};

var ExtendedEnvironment = function(parent) {
    this.bindings = {};
    this.parent = parent;
};


var Closure = function(env, label) {
    this.env = env;
    this.label = label;
};


var _isFalse = function(x) { if(x) return false; return true; }
var _isPrimProc = function(x) { return typeof(x) === 'function'; };
var _applyPrimProc = function(p, argl) { return p(argl); }
var _closureEnv = function(c) { return c.env; }
var _closureEntry = function(c) { return c.label; }
var _makeClosure = function(l, e) { return new Closure(e, l); }
var _envDefine = function(n, v, e) {
    e.bindings[n] = v;
};
var _envExtend = function(ns, vs, e) {
    var e2 = new ExtendedEnvironment(e);
    while(ns) {
        e2.bindings[ns[0]] = vs[0];
        ns = ns[1]; vs = vs[1];
    }
    return e2;
};
var _envLookup = function(n, e) {
    while (e) {
        if (e.bindings.hasOwnProperty(n)) { 
	    return e.bindings[n]; 
	}
        e = e.parent;
    }
    throw new Error("Not bound: " + n);
};


//////////////////////////////////////////////////////////////////////
// Lexical addressing
var _lexicalAddressLookup = function(depth, pos, env) {
    // FIXME
};

var _lexicalAddressAssign = function(depth, pos, env, value) {
    // FIXME
};

//////////////////////////////////////////////////////////////////////





var _cons = function(x, y) { return [x, y]; }
var _list = function() {
    var i;
    var result;
    for (i = arguments.length - 1; i >= 0; i--) {
        result = [arguments[i], result];
    }
    return result;
}


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
