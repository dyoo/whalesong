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
	
	MACHINE.label = proc.label;
	program();
    }
};



var MACHINE={env: new TopEnvironment(),
             proc:undefined, 
             argl:undefined,
             val:undefined,
             cont:undefined,
             stack: [],
	     label: undefined};



var invoke = function(k) {
    MACHINE.cont = k;
    MACHINE.label = 1;
    program();
};



var program = function() {
    while(true) {
	switch(MACHINE.label) {

	case 1:
	    MACHINE.val=(new Closure(MACHINE.env, 2));
	    MACHINE.label = 3;
	    break;


	case 2:
	    MACHINE.env=(MACHINE.proc.env);
	    MACHINE.env=new ExtendedEnvironment(MACHINE.env, MACHINE.argl);
	    if (! (MACHINE.env).globalBindings.hasOwnProperty("fact-iter")) { throw new Error("Not bound: " + "fact-iter"); }
	    MACHINE.proc=((MACHINE.env).globalBindings["fact-iter"]);
	    MACHINE.val=1;
	    MACHINE.argl=[MACHINE.val, undefined];
	    MACHINE.val=(MACHINE.env).valss[0][0];
	    MACHINE.argl=[MACHINE.val,MACHINE.argl];
	    if((typeof(MACHINE.proc) === 'function')){
		MACHINE.val=MACHINE.proc(MACHINE.argl);
		MACHINE.label = MACHINE.cont;
		break;
	    }
	    MACHINE.val=(MACHINE.proc.label);
	    MACHINE.label = MACHINE.val;
	    break;

	case 3:
	    (MACHINE.env).globalBindings["factorial"] = MACHINE.val;
	    MACHINE.val="ok";
	    MACHINE.val=(new Closure(MACHINE.env, 4));
	    MACHINE.label = 9;
	    break;

	case 4:
	    MACHINE.env=(MACHINE.proc.env);
	    MACHINE.env=new ExtendedEnvironment(MACHINE.env, MACHINE.argl);
	    MACHINE.stack.push(MACHINE.cont);
	    MACHINE.stack.push(MACHINE.env);
	    if (! (MACHINE.env).globalBindings.hasOwnProperty("=")) { throw new Error("Not bound: " + "="); }
	    MACHINE.proc=((MACHINE.env).globalBindings["="]);
	    MACHINE.val=0;
	    MACHINE.argl=[MACHINE.val, undefined];
	    MACHINE.val=(MACHINE.env).valss[0][0];
	    MACHINE.argl=[MACHINE.val,MACHINE.argl];
	    if((typeof(MACHINE.proc) === 'function')){
		MACHINE.val=MACHINE.proc(MACHINE.argl);
		MACHINE.label=5;
		break;
	    }
	    MACHINE.cont=5;
	    MACHINE.val=(MACHINE.proc.label);
	    MACHINE.label = MACHINE.val;
	    break;

	case 5:
	    MACHINE.env=MACHINE.stack.pop();
	    MACHINE.cont=MACHINE.stack.pop();
	    if((!(MACHINE.val))){
		MACHINE.label=6;
		break;}
	    MACHINE.val=(MACHINE.env).valss[0][1];
	    MACHINE.label = MACHINE.cont;
	    break;


	case 6:
	    if (! (MACHINE.env).globalBindings.hasOwnProperty("fact-iter")) { throw new Error("Not bound: " + "fact-iter"); }
	    MACHINE.proc=((MACHINE.env).globalBindings["fact-iter"]);
	    MACHINE.stack.push(MACHINE.cont);
	    MACHINE.stack.push(MACHINE.proc);
	    MACHINE.stack.push(MACHINE.env);
	    if (! (MACHINE.env).globalBindings.hasOwnProperty("*")) { throw new Error("Not bound: " + "*"); }
	    MACHINE.proc=((MACHINE.env).globalBindings["*"]);
	    MACHINE.val=(MACHINE.env).valss[0][0];
	    MACHINE.argl=[MACHINE.val, undefined];
	    MACHINE.val=(MACHINE.env).valss[0][1];
	    MACHINE.argl=[MACHINE.val,MACHINE.argl];
	    if((typeof(MACHINE.proc) === 'function')){
		MACHINE.val=MACHINE.proc(MACHINE.argl);
		MACHINE.label=7;
		break;
	    }
	    MACHINE.cont=7;
	    MACHINE.val=(MACHINE.proc.label);
	    MACHINE.label = MACHINE.val;
	    break;
	    
	case 7:
	    MACHINE.argl=[MACHINE.val, undefined];
	    MACHINE.env=MACHINE.stack.pop();
	    MACHINE.stack.push(MACHINE.argl);
	    if (! (MACHINE.env).globalBindings.hasOwnProperty("-")) { throw new Error("Not bound: " + "-"); }
	    MACHINE.proc=((MACHINE.env).globalBindings["-"]);
	    MACHINE.val=1;
	    MACHINE.argl=[MACHINE.val, undefined];
	    MACHINE.val=(MACHINE.env).valss[0][0];
	    MACHINE.argl=[MACHINE.val,MACHINE.argl];
	    if((typeof(MACHINE.proc) === 'function')){
		MACHINE.val=MACHINE.proc(MACHINE.argl);
		MACHINE.label=8;
		break;
	    }
	    MACHINE.cont=8;
	    MACHINE.val = (MACHINE.proc.label);
	    MACHINE.label = MACHINE.val;
	    break;


	case 8:
	    MACHINE.argl=MACHINE.stack.pop();
	    MACHINE.argl=[MACHINE.val,MACHINE.argl];
	    MACHINE.proc=MACHINE.stack.pop();
	    MACHINE.cont=MACHINE.stack.pop();
	    if((typeof(MACHINE.proc) === 'function')){
		MACHINE.val=MACHINE.proc(MACHINE.argl);
		MACHINE.label = MACHINE.cont;
		break;
	    }
	    MACHINE.val=(MACHINE.proc.label);
	    MACHINE.label = MACHINE.val;
	    break;

	case 9:
	    (MACHINE.env).globalBindings["fact-iter"] = MACHINE.val;
	    MACHINE.val="ok";
	    MACHINE.label = MACHINE.cont;
	    break;


	default:
	    return MACHINE.label();
	    return;
	}

    }
}