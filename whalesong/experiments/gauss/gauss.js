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

var invoke = function(k) {
var start23=function(){
if(--MACHINE.callsBeforeTrampoline < 0) { throw start23; }
MACHINE.val=(new Closure(MACHINE.env, entry1));
return afterLambda2();};

var entry1=function(){
if(--MACHINE.callsBeforeTrampoline < 0) { throw entry1; }
MACHINE.env=(MACHINE.proc.env);
MACHINE.env=new ExtendedEnvironment(MACHINE.env, MACHINE.argl);
if (! (MACHINE.env).globalBindings.hasOwnProperty("gauss-iter")) { throw new Error("Not bound: " + "gauss-iter"); }
MACHINE.proc=((MACHINE.env).globalBindings["gauss-iter"]);
MACHINE.val=0;
MACHINE.argl=[MACHINE.val, undefined];
MACHINE.val=(MACHINE.env).valss[0][0];
MACHINE.argl=[MACHINE.val,MACHINE.argl];
if((typeof(MACHINE.proc) === 'function')){
MACHINE.val=MACHINE.proc(MACHINE.argl);
return MACHINE.cont();}
MACHINE.val=(MACHINE.proc.label);
return MACHINE.val();};


var afterLambda2=function(){
if(--MACHINE.callsBeforeTrampoline < 0) { throw afterLambda2; }
(MACHINE.env).globalBindings["gauss"] = MACHINE.val;
MACHINE.val="ok";
MACHINE.val=(new Closure(MACHINE.env, entry6));
return afterLambda7();};

var entry6=function(){
if(--MACHINE.callsBeforeTrampoline < 0) { throw entry6; }
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
return afterCall13();}
MACHINE.cont=afterCall13;
MACHINE.val=(MACHINE.proc.label);
return MACHINE.val();};


var afterCall13=function(){
if(--MACHINE.callsBeforeTrampoline < 0) { throw afterCall13; }
MACHINE.env=MACHINE.stack.pop();
MACHINE.cont=MACHINE.stack.pop();
if((!(MACHINE.val))){
return falseBranch9();}
MACHINE.val=(MACHINE.env).valss[0][1];
return MACHINE.cont();};

var falseBranch9=function(){
if(--MACHINE.callsBeforeTrampoline < 0) { throw falseBranch9; }
if (! (MACHINE.env).globalBindings.hasOwnProperty("gauss-iter")) { throw new Error("Not bound: " + "gauss-iter"); }
MACHINE.proc=((MACHINE.env).globalBindings["gauss-iter"]);
MACHINE.stack.push(MACHINE.cont);
MACHINE.stack.push(MACHINE.proc);
MACHINE.stack.push(MACHINE.env);
if (! (MACHINE.env).globalBindings.hasOwnProperty("+")) { throw new Error("Not bound: " + "+"); }
MACHINE.proc=((MACHINE.env).globalBindings["+"]);
MACHINE.val=(MACHINE.env).valss[0][0];
MACHINE.argl=[MACHINE.val, undefined];
MACHINE.val=(MACHINE.env).valss[0][1];
MACHINE.argl=[MACHINE.val,MACHINE.argl];
if((typeof(MACHINE.proc) === 'function')){
MACHINE.val=MACHINE.proc(MACHINE.argl);
return afterCall19();}
MACHINE.cont=afterCall19;
MACHINE.val=(MACHINE.proc.label);
return MACHINE.val();};


var afterCall19=function(){
if(--MACHINE.callsBeforeTrampoline < 0) { throw afterCall19; }
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
return afterCall16();}
MACHINE.cont=afterCall16;
MACHINE.val=(MACHINE.proc.label);
return MACHINE.val();};



var afterCall16=function(){
if(--MACHINE.callsBeforeTrampoline < 0) { throw afterCall16; }
MACHINE.argl=MACHINE.stack.pop();
MACHINE.argl=[MACHINE.val,MACHINE.argl];
MACHINE.proc=MACHINE.stack.pop();
MACHINE.cont=MACHINE.stack.pop();
if((typeof(MACHINE.proc) === 'function')){
MACHINE.val=MACHINE.proc(MACHINE.argl);
return MACHINE.cont();}
MACHINE.val=(MACHINE.proc.label);
return MACHINE.val();};


var afterLambda7=function(){
if(--MACHINE.callsBeforeTrampoline < 0) { throw afterLambda7; }
(MACHINE.env).globalBindings["gauss-iter"] = MACHINE.val;
MACHINE.val="ok";
return MACHINE.cont();};

MACHINE.cont = k;
trampoline(start23, function() {}); };