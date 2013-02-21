//////////////////////////////////////////////////////////////////////


var run = function(state) {
	while (!state.isStuck()) {
	    interpret.step(state);
	}
	return state.v;
}

var step = interpret.step;


//////////////////////////////////////////////////////////////////////

var EXIT_ON_FIRST_ERROR = true;


//////////////////////////////////////////////////////////////////////


var StateModule = state;


var makeStateWithConstant = function(c) {
    var s = new StateModule.State();
    s.v = c;
    return s;
};


var makePrefix = function(n) {
    var arr = [];    
    for (var i = 0; i < n; i++) {
	arr.push(false);
    }
    return new control.Prefix({numLifts: 0,
			       toplevels: arr });
};

var makeMod = function(prefix, body) {
    return new control.ModControl(prefix, [], body);
};

var makeConstant = function(c) {
    return new control.ConstantControl(c);
};

var makeBranch = function(x, y, z) { 
    return new control.BranchControl(x, y, z);
};

var makeSeq = function() {
    return new control.SeqControl(arguments);
};

var makeBeg0 = function() {
    return new control.Beg0Control(arguments);
};

var makeToplevel = function(depth, pos) {
    return new control.ToplevelControl(depth, pos);
};


var makeDefValues = function(ids, body) {
    return new control.DefValuesControl(ids, body);
};


var makeLam = function(arity, closureMap, body) {
    var aClosureMap = [];
    var aClosureTypes = [];
    var aParamTypes = [];
    for (var i = 0; i < closureMap.length; i++) {
	aClosureMap.push(closureMap[i]);
	aClosureTypes.push("val/ref");
    }
    for (var i = 0; i < arity; i++) {
	aParamTypes.push("val");
    }

    return new control.LamControl({'numParams': arity,
				   'paramTypes': aParamTypes,
				   'isRest': false,
				   'closureMap' : aClosureMap,
				   'closureTypes' : aClosureTypes,
				   'body': body});    
};


var makeLamWithRest = function(arity, closureMap, body) {
    var aClosureMap = [];
    var aClosureTypes = [];
    var aParamTypes = [];
    for (var i = 0; i < closureMap.length; i++) {
	aClosureMap.push(closureMap[i]);
	aClosureTypes.push("val/ref");
    }
    for (var i = 0; i < arity; i++) {
	aParamTypes.push("val");
    }

    return new control.LamControl({'numParams': arity,
				   'paramTypes': aParamTypes,
				   'isRest': true,
				   'closureMap' : aClosureMap,
				   'closureTypes' : aClosureTypes,
				   'body': body});    
};






var makePrimval = function(name) {
    return new control.PrimvalControl(name);
};


var makeApplication = function(rator, rands) {
    assert.ok(typeof(rands) === 'object' && rands.length !== undefined);
    return new control.ApplicationControl(rator, rands);
};


var makeLocalRef = function(n) {
    return new control.LocalrefControl(n);
};


var makeApplyValues = function(proc, argsExpr) {
    return new control.ApplyValuesControl(proc, argsExpr);
};


var makeLet1 = function(rhs, body) {
    return new control.LetOneControl(rhs, body);
};


var makeLetVoid = function(count, isBoxes, body) {
    return new control.LetVoidControl({count: count,
				       isBoxes : isBoxes,
				       body : body});
};

var makeBoxenv = function(pos, body) {
    return new control.BoxenvControl(pos, body);
};


var makeInstallValue = function(count, pos, isBoxes, rhs, body) {
    return new control.InstallValueControl({count: count,
					    pos: pos,
					    isBoxes: isBoxes,
					    rhs: rhs,
					    body: body});

};


var makeWithContMark = function(key, val, body) {
    return new control.WithContMarkControl(key, val, body);
};


var makeAssign = function(id, rhs, isUndefOk) {
    return new control.AssignControl({id: id,
				      rhs: rhs,
				      isUndefOk: isUndefOk});
};

  
var makeVarref = function(aToplevel) {
    return new control.VarrefControl(aToplevel);
};


var makeClosure = function(genId) {
    return new control.ClosureControl(genId);
};


var makeCaseLam = function(name, clauses) {
    assert.ok(typeof(clauses) === 'object' && clauses.length !== undefined);
    return new control.CaseLamControl(name, clauses);
};


var makeLetrec = function(procs, body) {
    return new control.LetRecControl(procs, body);
};


/////////////////////////////////////////////////////////////////////


var testPrim = function(funName, f, baseArgs, expectedValue) {
    var state = new StateModule.State();
    var args = [];
    for (var i = 0; i < baseArgs.length; i++) {
	args.push(makeConstant(f(baseArgs[i])));
    }
    state.pushControl(makeApplication(makePrimval(funName), args));
    assert.ok(types.isEqual(run(state), 
			    expectedValue));
};

var testPrimF = function(funName, f, baseArgs, expectedValue, transform) {
    var state = new StateModule.State();
    var args = [];
    for (var i = 0; i < baseArgs.length; i++) {
	args.push(makeConstant(f(baseArgs[i])));
    }
    state.pushControl(makeApplication(makePrimval(funName), args));
    assert.deepEqual(transform(run(state)),
		     expectedValue);
}

var listToStringArray = function(lst) {
	var ret = [];
	while ( !lst.isEmpty() ) {
		ret.push( lst.first().toString() );
		lst = lst.rest();
	}
	return ret;
}

var id = function(x) {return x;};



//////////////////////////////////////////////////////////////////////

var runTest = function(name, thunk) {
    sys.print("running " + name + "... ");
    try {
	thunk();
    } catch(e) {
	sys.print(" FAIL\n");
	sys.print(e);
	if (EXIT_ON_FIRST_ERROR) {
	    if (typeof(console) !== 'undefined' && console.log && e.stack) {
			console.log(e.stack);
		}
//		if (typeof(console) !== 'undefined' && console.log && e.stack) {
//			console.log(e.stack);
//		}
//		sys.print(sys.inspect(e) + '\n');
	    throw e;
	}
    }
    sys.print(" ok\n")
    
};

//////////////////////////////////////////////////////////////////////


sys.print("START TESTS\n\n");

runTest("simple empty state",
	// Simple running should just terminate, and always be at the "stuck" state.
	function() { 
	    var state = new StateModule.State();
	    assert.ok(state.isStuck());
	    run(state);
	    assert.ok(state.isStuck());
	});



// Numeric constants should just evaluate through.
runTest("Numeric constant", 
	function() {
	    var state = new StateModule.State();
	    state.pushControl(makeConstant(42));
	    var result = run(state);
	    assert.deepEqual(result, 
			     42);
	    
	    assert.deepEqual(state, makeStateWithConstant(42));
	});



// String constant.
runTest("String constant",
	function() {
	    var state = new StateModule.State();
	    state.pushControl(makeConstant("hello world"));
	    var result = run(state);
	    assert.deepEqual(result, 
			     "hello world");

	    assert.deepEqual(state, makeStateWithConstant("hello world"));
	});


// boolean constant.
runTest("Boolean constant", 
	function() {
	    var state = new StateModule.State();
	    state.pushControl(makeConstant(true));
	    var result = run(state);
	    assert.deepEqual(result, true);

	    assert.deepEqual(state, makeStateWithConstant(true));
	});



runTest("external call",
	function() {
	    var state = new StateModule.State();
	    interpret.call(state, 
			   primitive.getPrimitive("*"),
			   [2, 3],
			   function(v) { assert.equal(v, 6) });
	});



// Simple branch to true
runTest("Simple boolean branch to true",
	function() {
	    var state = new StateModule.State();
	    state.pushControl(makeBranch(makeConstant(true),
					 makeConstant(true),
					 makeConstant(false)));
	    var result = run(state);
	    assert.deepEqual(result, true);
	});


// Simple branch to false
runTest("Simple boolean branch to false",
	function() {
	    var state = new StateModule.State();
	    state.pushControl(makeBranch(makeConstant(false),
					 makeConstant(false),
					 makeConstant(true)));
	    var result = run(state);
	    assert.deepEqual(result, 
			     true);

	    assert.deepEqual(state, makeStateWithConstant(true));
	});



// (if (if true false true) "apple" "pie") --> "pie"
runTest("nested booleans",
	function() {
	    var state = new StateModule.State();
	    state.pushControl(makeBranch(makeBranch(makeConstant(true), makeConstant(false), makeConstant(true)),
					 makeConstant("apple"),
					 makeConstant("pie")));
	    var result = run(state);
	    assert.deepEqual(result, "pie");

	    assert.deepEqual(state, makeStateWithConstant("pie"));
	});



// Sequences
runTest("Sequences",
	function() {
	    var state1 = new StateModule.State();
	    state1.pushControl(makeSeq(makeConstant(3),
				       makeConstant(4),
				       makeConstant(5)));
	    step(state1);
	    step(state1);
	    assert.ok(!state1.isStuck());
	    assert.deepEqual(state1.v, 3);
	    step(state1);
	    assert.deepEqual(state1.v, 4);
	    var result = run(state1);
	    assert.deepEqual(result, 5);

	    assert.deepEqual(state1, makeStateWithConstant(5));    
	});



// Module prefix
runTest("module prefix",
	function() {
	    var state = new StateModule.State();
	    state.pushControl(makeMod(makePrefix(3),
				      []));
	    run(state);   
	    assert.equal(1, state.vstack.length);
	    assert.ok(state.vstack[0] instanceof types.PrefixValue);
	    assert.equal(state.vstack[0].length(), 3);
	});


runTest("toplevel lookup",
	// toplevel lookup
	function() {
	    var state = new StateModule.State();
	    state.pushControl(makeMod(makePrefix(3),
				      []));
	    run(state);   

	    state.vstack[0].set(0, "zero");
	    state.vstack[0].set(1, "one");
	    state.vstack[0].set(2, "two");

	    state.pushControl(makeToplevel(0, 0));
	    assert.equal(run(state), "zero");

	    state.pushControl(makeToplevel(0, 1));
	    assert.equal(run(state), "one");

	    state.pushControl(makeToplevel(0, 2));
	    assert.equal(run(state), "two");
	});



runTest("define-values",
	// define-values
	function() {
	    var state = new StateModule.State();
	    state.pushControl(makeMod(makePrefix(3), []));
	    run(state);   
	    state.pushControl(makeDefValues([makeToplevel(0, 0)],
					    makeConstant("try it")));
	    run(state);

	    var expectedState = new StateModule.State();
	    expectedState.pushControl(makeMod(makePrefix(3),
					      []));
	    run(expectedState);   
	    expectedState.v = "try it";
	    expectedState.vstack[0].set(0, "try it");
	    assert.deepEqual(state, expectedState);
	});


runTest("lambda",
	// lambda
	function() {
	    var state = new StateModule.State();
	    state.pushControl(makeMod(makePrefix(3), []));
	    run(state);   
	    state.pushControl(makeDefValues([makeToplevel(0, 0)],
					    makeConstant("Some toplevel value")));

	    run(state);
	    state.pushControl(makeLam(3, [0], makeConstant("I'm a body")));

	    var result = run(state);

	    // result should be a lambda.
	    assert.ok(result instanceof types.ClosureValue);
	    assert.equal(result.closureVals.length, 1);
	    assert.ok(result.closureVals[0] instanceof types.PrefixValue);
	    assert.deepEqual(result.body, makeConstant("I'm a body"));
	    assert.equal(result.numParams, 3);
	});



runTest("primval (current-print)",
	// primval
	function() {
	    var state = new StateModule.State();
	    state.pushControl(makePrimval("current-print"));
	    var result = run(state);
	    assert.ok(result instanceof types.PrimProc);
	});


runTest("primval on bad primitive should throw error",
	// primval on unknowns should throw error
	function() {
	    var state = new StateModule.State();
	    state.pushControl(makePrimval("foobar"));
	    assert.throws(function() { run(state); });
	});


runTest("Primval on *",
	// primval on *
	// primval
	function() {
	    var state = new StateModule.State();
	    state.pushControl(makePrimval("*"));
	    var result = run(state);
	    assert.ok(result instanceof types.PrimProc);
	});


runTest("My own list function",
	function() {
	    var state = new StateModule.State();
	    state.pushControl(makeApplication(makeLamWithRest(0, [], makeLocalRef(0)),
					      [makeConstant("one"),
					       makeConstant("two"),
					       makeConstant("three")]))
	    var result = run(state);
	    assert.deepEqual(result,
			     types.list(["one", "two", "three"]));
	});


runTest("primitive application",
	// primitive application.
	function() {
	    var state = new StateModule.State();
	    state.pushControl(makeApplication(makePrimval("*"),
					      [makeConstant(types.rational(3)),
					       makeConstant(types.rational(5))]));
	    var result = run(state);
	    assert.deepEqual(result, types.rational(15));
	    assert.equal(state.vstack.length, 0);
	});


runTest("primitive application, no arguments",
	// primitive application with no arguments.
	function() {
	    var state = new StateModule.State();
	    state.pushControl(makeApplication(makePrimval("*"),
					      []));
	    var result = run(state);
	    assert.deepEqual(result, types.rational(1));
	    assert.equal(state.vstack.length, 0);
	});


runTest("primitive application, nested application",
	// primitive application, with nesting
	function() {
	    var state = new StateModule.State();
	    state.pushControl(makeApplication(
		makePrimval("*"),
		[makeApplication(
		    makePrimval("*"),
		    [makeConstant(types.rational(3)),
		     makeConstant(types.rational(5))]),
		 makeConstant(types.rational(7))]));
	    var result = run(state);
	    assert.deepEqual(result, types.rational(105));
	    assert.equal(state.vstack.length, 0);
	});


runTest("primitive appliation, nesting, testing non-commutativity",
	// primitive application, with nesting, testing order
	function() {
	    var state = new StateModule.State();
	    state.pushControl(makeApplication(
		makePrimval("string-append"),
		[makeApplication(
		    makePrimval("string-append"),
		    [makeConstant(types.string("hello")),
		     makeConstant(types.string("world"))]),
		 makeConstant(types.string("testing"))]));
	    var result = run(state);
	    assert.deepEqual(result, types.string("helloworldtesting"));
	    assert.equal(state.vstack.length, 0);
	});

runTest("primitive application, subtraction",
	// subtraction
	function() {
	    var state = new StateModule.State();
	    state.pushControl(makeApplication(
		makePrimval("-"),
		[makeApplication(
		    makePrimval("-"),
		    [makeConstant(types.rational(3)),
		     makeConstant(types.rational(4))]),
		 makeConstant(types.rational(15))]));
	    var result = run(state);
	    assert.deepEqual(result, types.rational(-16));
	    assert.equal(state.vstack.length, 0);
	});

runTest("primitive application, unary subtraction (negation)", 
	// Checking negation.
	function() {
	    var state = new StateModule.State();
	    state.pushControl(makeApplication(
		makePrimval("-"),
		[makeConstant(types.rational(1024))]));
	    var result = run(state);
	    assert.deepEqual(result, types.rational(-1024));
	    assert.equal(state.vstack.length, 0);
	});


runTest("closure application",
	// Closure application
	// lambda will just return a constant value
	function() {
	    var state = new StateModule.State();
	    state.pushControl(makeMod(makePrefix(1), []));
	    run(state);   
	    assert.equal(state.vstack.length, 1);
	    
	    state.pushControl(makeDefValues([makeToplevel(0, 0)],
					    makeLam(1, [],
						    makeConstant("I'm a body"))));
	    run(state);
	    state.pushControl(makeApplication(makeToplevel(1, 0), [makeConstant("boo")]));
	    var result = run(state);
	    assert.equal(result, "I'm a body");

	    assert.equal(state.vstack.length, 1);
	});


runTest("closure application, defining square",
	// Closure application
	// lambda will square its argument
	function() {
	    var state = new StateModule.State();
	    state.pushControl(makeMod(makePrefix(1), []));
	    run(state);   
	    assert.equal(state.vstack.length, 1);
	    
	    state.pushControl(makeDefValues([makeToplevel(0, 0)],
					    makeLam(1, [],
						    makeApplication(makePrimval("*"),
								    [makeLocalRef(2),
								     makeLocalRef(2)]))));
	    run(state);
	    state.pushControl(makeApplication(makeToplevel(1, 0), 
					      [makeConstant(types.rational(4))]));
	    var result = run(state);
	    assert.deepEqual(result, types.rational(16));
	    assert.equal(state.vstack.length, 1);
	});



runTest("closure application, testing tail calls",
	// Checking tail calling behavior
	// The standard infinite loop should consume bounded control stack.
	// (define (f) (f)) (begin (f)) --> infinite loop, but with bounded control stack.
	function() {
	    var state = new StateModule.State();
	    state.pushControl(makeMod(makePrefix(1), []));
	    run(state);   
	    assert.equal(state.vstack.length, 1);
	    
	    state.pushControl(makeDefValues([makeToplevel(0, 0)],
					    makeLam(0, [0],
						    makeApplication(makeToplevel(0, 0),
								    []))));
	    run(state);
	    state.pushControl(makeApplication(makeToplevel(0, 0), []));
	    var MAXIMUM_BOUND = 5;
	    var ITERATIONS = 1000000;
	    for (var i = 0; i < ITERATIONS; i++) {
		step(state);
		assert.ok(state.cstack.length < MAXIMUM_BOUND);
	    }
	});



runTest("closure application, testing tail calls with even/odd",
	// Checking tail calling behavior
	// The standard infinite loop should consume bounded control stack.
	// (define (even? x) (if (zero? x) true (odd? (sub1 x))))
	// (define (odd? x) (if (zero? x) false (even? (sub1 x))))
	function() {
	    var state = new StateModule.State();
	    state.pushControl(makeMod(makePrefix(2), []));
	    run(state);   
	    assert.equal(state.vstack.length, 1);
	    state.pushControl(makeDefValues
			      ([makeToplevel(0, 0)],
			       makeLam(1, [0],
				       makeBranch(
					   makeApplication(makePrimval("zero?"),
							   [makeLocalRef(2)]),
					   makeConstant(true),
					   makeApplication(makeToplevel(1, 1),
							   [makeApplication(
							       makePrimval("sub1"),
							       [makeLocalRef(3)])])))));
	    state.pushControl(makeDefValues
			      ([makeToplevel(0, 1)],
			       makeLam(1, [0],
				       makeBranch(
					   makeApplication(makePrimval("zero?"),
							   [makeLocalRef(2)]),
					   makeConstant(false),
					   makeApplication(makeToplevel(1, 0),
							   [makeApplication(
							       makePrimval("sub1"),
							       [makeLocalRef(3)])])))));
	    
	    run(state);

	    var even = function(n) {
		state.pushControl(makeApplication(makeToplevel(1, 0),
						  [makeConstant(types.rational(n))]));
		var MAXIMUM_BOUND = 10;
		while (!state.isStuck()) {
		    step(state);
		    assert.ok(state.cstack.length < MAXIMUM_BOUND);
		    //sys.print(state.cstack.length + "\n");
		}
		return state.v;
	    }
	    assert.equal(even(0), true);
	    assert.equal(even(1), false);
	    assert.equal(even(50), true);
	    assert.equal(even(51), false);
	    assert.equal(even(501), false);
	    assert.equal(even(1001), false);
	    assert.equal(even(10000), true);
	    assert.equal(even(10001), false);
	});


runTest("factorial",
	function() {
	    var state = new StateModule.State();
	    state.pushControl(makeMod(makePrefix(1), []));
	    run(state);   
	    assert.equal(state.vstack.length, 1);
	    
	    state.pushControl(makeDefValues(
		[makeToplevel(0, 0)],
		makeLam(1, [0],
			makeBranch(
			    makeApplication(makePrimval("zero?"),
					    [makeLocalRef(2)]),
			    makeConstant(types.rational(1)),
			    makeApplication(makePrimval("*"),
					    [makeLocalRef(3),
					     makeApplication(
						 makeToplevel(3, 0),
						 [makeApplication(makePrimval("sub1"),
								  [makeLocalRef(5)])])])))));

	    run(state);

	    var fact = function(n) {
		state.pushControl(makeApplication(makeToplevel(1, 0),
						  [makeConstant(types.rational(n))]));
		return run(state);
	    }

 	    assert.equal(fact(0), 1);
 	    assert.equal(fact(1), 1);
 	    assert.equal(fact(2), 2);
 	    assert.equal(fact(3), 6);
 	    assert.equal(fact(4), 24);
	    assert.equal(fact(5), 120);
	    assert.equal(fact(6), 720);
	    assert.equal(fact(10), 3628800);
	    assert.equal(fact(11), 39916800);
	    assert.equal(fact(12), 479001600);
	});



runTest("apply on a primitive *",
	function() {
	    var state = new StateModule.State();
	    state.pushControl(makeApplication(
		makePrimval("apply"),
		[makePrimval("*"),
		 makeConstant(
		     types.list([types.rational(3),
				   types.rational(9)]))]));
	    assert.deepEqual(run(state),
			     27);
	    assert.equal(state.vstack.length, 0);
	});



runTest("apply on a primitive -",
	function() {
	    var state = new StateModule.State();
	    state.pushControl(makeApplication(
		makePrimval("apply"),
		[makePrimval("-"),
		 makeConstant(
		     types.list([types.rational(3),
				   types.rational(9)]))]));
	    assert.deepEqual(run(state),
			     -6);
	    assert.equal(state.vstack.length, 0);
	});

runTest("apply on a primitive -, three arguments",
	function() {
	    var state = new StateModule.State();
	    state.pushControl(makeApplication(
		makePrimval("apply"),
		[makePrimval("-"),
		 makeConstant(
		     types.list([types.rational(3),
				   types.rational(9),
				   types.rational(12)]))]));
	    assert.deepEqual(run(state),
			     -18);
	    assert.equal(state.vstack.length, 0);
	});


runTest("values",
	function() {
	    var state = new StateModule.State();
	    state.pushControl(makeApplication(
		makePrimval("values"),
		[makePrimval("*"),
		 makeConstant(
		     types.list([types.rational(3),
				   types.rational(9),
				   types.rational(12)]))]));
	    var result = run(state);
	    assert.equal(state.vstack.length, 0);
	    assert.ok(result instanceof types.ValuesWrapper);
	    assert.equal(result.elts.length, 2);
	});



runTest("values with no arguments",
	function() {
	    var state = new StateModule.State();
	    state.pushControl(makeApplication(
		makePrimval("values"),[]));
	    var result = run(state);
	    assert.equal(state.vstack.length, 0);
	    assert.ok(result instanceof types.ValuesWrapper);
	    assert.equal(result.elts.length, 0);
	});




runTest("current-inexact-milliseconds",
	function() {
	    var state = new StateModule.State();
	    for (var i = 0; i < 2000; i++) {
		state.pushControl(makeApplication(
		    makePrimval("current-inexact-milliseconds"),[]));
		var result1 = run(state);


		state.pushControl(makeApplication(
		    makePrimval("current-inexact-milliseconds"),[]));
		var result2 = run(state);
		assert.ok(jsnums.lessThanOrEqual(result1, result2));
	    }
	});




runTest("values with def-values",
	function() {
	    var state = new StateModule.State();
	    state.pushControl(makeMod(makePrefix(2), []));
	    run(state);   
	    assert.equal(state.vstack.length, 1);
	    
	    state.pushControl(makeDefValues(
		[makeToplevel(0, 0),
		 makeToplevel(0, 1)],
		makeApplication(makePrimval("values"),
				[makeConstant("hello"),
				 makeConstant("world")])));
	    run(state);
	    assert.equal(state.vstack.length, 1);
	    assert.ok(state.vstack[0] instanceof types.PrefixValue);
	    assert.equal(state.vstack[0].ref(0), "hello");
	    assert.equal(state.vstack[0].ref(1), "world");
	});



runTest("apply-values",
	function() {
	    var state = new StateModule.State();
	    state.pushControl(makeMod(makePrefix(2), []));
	    run(state);   
	    state.pushControl(makeDefValues(
		[makeToplevel(0, 0),
		 makeToplevel(0, 1)],
		makeApplication(makePrimval("values"),
				[makeConstant(types.string("hello")),
				 makeConstant(types.string("world"))])));
	    run(state);

	    state.pushControl(makeApplyValues(
		makeLam(2, [], makeApplication(makePrimval("string-append"),
					       [makeLocalRef(2),
						makeLocalRef(3)])),
		makeApplication(makePrimval("values"),
				[makeToplevel(2, 0),
				 makeToplevel(2, 1)])));
	    assert.deepEqual(run(state), types.string("helloworld"));
	});



runTest("apply-values, testing no stack usage",
	function() {
	    var state = new StateModule.State();
	    state.pushControl(makeMod(makePrefix(2), []));
	    run(state);   
	    state.pushControl(makeDefValues(
		[makeToplevel(0, 0),
		 makeToplevel(0, 1)],
		makeApplication(makePrimval("values"),
				[makePrimval("zero?"),
				 makeConstant(types.rational(0))])));
	    run(state);

	    state.pushControl(makeApplyValues(
		makeToplevel(0, 0),
		makeToplevel(0, 1)));
	    assert.equal(run(state), true);
	    assert.equal(state.vstack.length, 1);
	});

runTest("let-one, trivial",
	function() {
	    var state = new StateModule.State();
	    assert.equal(state.vstack.length, 0);
	    var body = makeLocalRef(0);
	    state.pushControl(makeLet1(makeConstant("someValue"),
				       body));
	    while (state.cstack[state.cstack.length - 1] !== body) {
		step(state);
	    }
	    assert.equal(state.vstack.length, 1);
	    assert.equal(state.vstack[0], "someValue");
	    var result = run(state);
	    assert.equal(state.vstack.length, 0);
	    assert.deepEqual(result, "someValue");
	});


runTest("let-one, different body",
	function() {
	    var state = new StateModule.State();
	    assert.equal(state.vstack.length, 0);
	    var body = makeConstant("something else");
	    state.pushControl(makeLet1(makeConstant("someValue"),
				       body));
	    while (state.cstack[state.cstack.length - 1] !== body) {
		step(state);
	    }
	    assert.equal(state.vstack.length, 1);
	    assert.equal(state.vstack[0], "someValue");
	    var result = run(state);
	    assert.equal(state.vstack.length, 0);
	    assert.deepEqual(result, "something else");
	});


runTest("let-void, no boxes",
	function() {
	    var state = new StateModule.State();
	    var body = makeConstant("blah");
	    state.pushControl(makeLetVoid(2, false, body));
	    while (state.cstack[state.cstack.length - 1] !== body) {
		step(state);
	    }
	    assert.equal(state.vstack.length, 2);
	    for(var i = 0; i < state.vstack.length; i++) {
		assert.ok(state.vstack[i] === types.UNDEFINED);
	    }
	    var result = run(state);
	    assert.equal(result, "blah");
	    assert.equal(state.vstack.length, 0);
	});


runTest("let-void, with boxes",
	function() {
	    var state = new StateModule.State();
	    var body = makeConstant("blah");
	    state.pushControl(makeLetVoid(2, true, body));
	    while (state.cstack[state.cstack.length - 1] !== body) {
		step(state);
	    }
	    assert.equal(state.vstack.length, 2);
	    for(var i = 0; i < state.vstack.length; i++) {
		assert.ok( types.isBox(state.vstack[i]) );
	    }
	    var result = run(state);
	    assert.equal(result, "blah");
	    assert.equal(state.vstack.length, 0);
	});


runTest("beg0 with just one argument should immediately reduce to its argument",
	function() {
	    var state = new StateModule.State();
	    state.pushControl(makeBeg0(makeConstant("first post")));
	    step(state);
	    assert.equal(state.cstack.length, 1);
	    assert.deepEqual(state.cstack[0], 
			     makeConstant("first post"));
	    var result = run(state);
	    assert.equal(result, "first post");
	});



runTest("beg0, more general",
	function() {
	    var state = new StateModule.State();
	    state.pushControl(makeBeg0(makeConstant("first post"),
				       makeConstant("second post"),
				       makeConstant("third post"),
				       makeConstant("fourth post")));
	    step(state);

	    // By this point, there should be two elements
	    // in the control stack, the evaluation of the first
	    // argument, and a control to continue the
	    // rest of the sequence evaluation.
	    assert.equal(state.cstack.length, 2); 
	    var result = run(state);
	    assert.equal(result, "first post");
	});



runTest("boxenv",
	function() {
	    var state = new StateModule.State();
	    state.pushControl(makeLet1(makeConstant("foo"),
				       makeBoxenv(0, 
						  makeLocalRef(0))));
	    var result = run(state);
	    assert.ok( types.isBox(result) );
	    assert.deepEqual(result, types.box("foo"));
	});


runTest("install-value, without boxes",
	function() {
	    var state = new StateModule.State();
	    var aBody = makeConstant("peep");
	    state.pushControl
		(makeLetVoid
		 (4,
		  false,
		  makeInstallValue
		  (3, 1, false,
		   makeApplication(makePrimval("values"),
				   [makeConstant("3"),
				    makeConstant("1"),
				    makeConstant("4")]),
		   aBody)));
	    while (state.cstack[state.cstack.length - 1] !== aBody) {
		step(state);
	    }
	    assert.equal(state.vstack.length, 4);
	    assert.equal(state.vstack[0], "4");
	    assert.equal(state.vstack[1], "1");
	    assert.equal(state.vstack[2], "3");
	    var result = run(state);
	    assert.equal(result, "peep");
	    assert.equal(state.vstack.length, 0);
	});



runTest("install-value, with boxes",
	function() {
	    var state = new StateModule.State();
	    var aBody = makeConstant("peep");
	    state.pushControl
		(makeLetVoid
		 (4,
		  true,
		  makeInstallValue
		  (3, 1, true,
		   makeApplication(makePrimval("values"),
				   [makeConstant("3"),
				    makeConstant("1"),
				    makeConstant("4")]),
		   aBody)));
	    while (state.cstack[state.cstack.length - 1] !== aBody) {
		step(state);
	    }
	    assert.equal(state.vstack.length, 4);
	    assert.deepEqual(state.vstack[0], types.box("4"));
	    assert.deepEqual(state.vstack[1], types.box("1"));
	    assert.deepEqual(state.vstack[2], types.box("3"));
	    var result = run(state);
	    assert.equal(result, "peep");
	    assert.equal(state.vstack.length, 0);
	});


runTest("assign",
	function() {
	    var state = new StateModule.State();
	    state.pushControl(makeMod(makePrefix(1), 
				    [makeAssign(makeToplevel(0, 0),
						makeConstant("some value"),
						true)]));
	    run(state);
	    assert.equal(state.vstack.length, 1);
	    assert.equal(state.vstack[0].ref(0), "some value");
	});


runTest("varref",
	function() {
	    var state = new StateModule.State();
	    state.pushControl(makeMod(makePrefix(2),
				      [makeSeq(makeAssign(makeToplevel(0, 0),
						makeConstant("a toplevel value"),
							  true),
					       makeAssign(
						   makeToplevel(0, 1),
						   makeVarref(makeToplevel(0, 0))))]));
	    var prefixValue = run(state);
	    // WARNING: breaking abstractions.
	    // Let's look directly at the representation structures
	    // and make sure we are dealing with a variable reference.
	    var result = prefixValue.slots[1];
	    assert.ok(result instanceof types.VariableReference);
	    assert.equal(result.ref(), "a toplevel value");
	    result.set("something else!");
	    assert.equal(state.vstack.length, 1);
	    assert.equal(state.vstack[0].ref(0), "something else!");
	});


runTest("closure",
	function() {
	    var state = new StateModule.State();
	    state.heap['some-closure'] = 42;
	    state.pushControl(makeClosure('some-closure'));
	    // The way we process closures in bytecode-compiler
	    // should make this a direct heap lookup.
	    assert.equal(run(state), 42);
	});


runTest("with-cont-mark", 
	function() {
	    var state = new StateModule.State();
	    var aBody = makeConstant("peep");
	    state.pushControl
		(makeWithContMark(makeConstant
				  (types.symbol("x")),
				  makeConstant("42"),
				  aBody));
	    while (state.cstack[state.cstack.length -1] !== aBody) {
		step(state);
	    }
	    assert.equal(state.cstack.length, 2);
	    assert.ok( types.isContMarkRecordControl(state.cstack[0]) );
	    assert.equal(state.cstack[0].dict.get(types.symbol('x')),
			 "42");
	    var result = run(state);
	    assert.equal(result, "peep");
	});




runTest("closure application, testing tail calls in the presence of continuation marks",
	// Checking tail calling behavior
	// The standard infinite loop should consume bounded control stack.
	// (define (f) (call-with-continuation-marks 'x 1 (f))) (begin (f)) --> infinite loop, but with bounded control stack.
	function() {
	    var state = new StateModule.State();
	    state.pushControl(makeMod(makePrefix(1), []));
	    run(state);   
	    assert.equal(state.vstack.length, 1);
	    
	    state.pushControl(makeDefValues([makeToplevel(0, 0)],
					    makeLam(0, [0],
						    (makeWithContMark
						     (makeConstant(types.symbol("x")),
						      makeConstant(types.rational(1)),
						      
						      makeApplication(makeToplevel(0, 0),
								      []))))));
	    run(state);
	    state.pushControl(makeApplication(makeToplevel(0, 0), []));
	    var MAXIMUM_BOUND = 6;
	    var ITERATIONS = 1000000;
	    for (var i = 0; i < ITERATIONS; i++) {
		step(state);
		assert.ok(state.cstack.length < MAXIMUM_BOUND);
	    }
	});


runTest("case-lambda, with a function that consumes one or two values",
	function() {
	    var state = new StateModule.State();
	    state.pushControl
		(makeMod(makePrefix(1), 
			 [makeDefValues
			  ([makeToplevel(0, 0)],
			   makeCaseLam(types.symbol("last"),
				       [makeLam(1, [], makeLocalRef(0)),
					makeLam(2, [], makeLocalRef(1))]))]));
	    run(state);
	    state.pushControl(makeApplication(makeToplevel(1, 0),
					      [makeConstant(types.rational(5))]));
	    var result = run(state);
	    assert.deepEqual(result, types.rational(5));

	    state.pushControl(makeApplication(makeToplevel(2, 0),
					      [makeConstant(types.rational(7)),
					       makeConstant(types.rational(42))]));
	    result = run(state);
	    assert.deepEqual(result, types.rational(42));
	});



// runTest("factorial again, testing the accumulation of continuation marks",
// 	//
// 	// (define marks #f)
// 	// (define (f x)
// 	//   (with-continuation-marks 'x x
// 	//     (if (= x 0)
// 	//         (begin (set! marks (current-continuation-marks))
// 	//                1)
// 	//         (* x (f (sub1 x))))))
// 	function() {

// 	});


runTest("let-rec",
	function() {
	    var state = new StateModule.State();
	    state.pushControl(makeLetVoid(2,
					  false,
					  makeLetrec([makeLam(1, [1],
							      makeBranch
							      (makeApplication(makePrimval("zero?"),
									       [makeLocalRef(2)]),
							       makeConstant(true),
							       makeApplication(makeLocalRef(1),
									       [makeApplication
										(makePrimval("sub1"),
										 [makeLocalRef(3)])]))),
						      makeLam(1, [0],
							      makeBranch
							      (makeApplication(makePrimval("zero?"),
									       [makeLocalRef(2)]),
							       makeConstant(false),
							       makeApplication(makeLocalRef(1),
									       [makeApplication
										(makePrimval("sub1"),
										 [makeLocalRef(3)])])))],
						     makeLocalRef(0))));
	    var evenValue = run(state);
	    var e = function(x) {
		state.pushControl(makeApplication(makeConstant(evenValue),
						  [makeConstant(types.rational(x))]));
		return run(state);
	    }
	    assert.equal(state.vstack.length, 0);

	    assert.equal(e(0), true);
	    assert.equal(e(1), false);
	    assert.equal(e(2), true);
	    assert.equal(e(3), false);
	    assert.equal(e(100), true);
	    assert.equal(e(101), false);
	    assert.equal(e(10000), true);
	    assert.equal(e(10001), false);
	});


/***************************************
 *** Primitive String Function Tests ***
 ***************************************/

runTest('symbol?',
	function() {
		testPrim('symbol?', types.symbol, ['hi'], true);
		testPrim('symbol?', types.rational, [1], false);
	});

runTest('symbol=?',
	function() {
		testPrim('symbol=?', types.symbol, ['abc', 'abd'], false);
		testPrim('symbol=?', types.symbol, ['cdf', 'cdf'], true);
	});

runTest('string->symbol',
	function() {
		testPrim('string->symbol', id, ['hello!'], types.symbol('hello!'));
		testPrim('string->symbol', types.string, [' world'], types.symbol(' world'));
	});


runTest('symbol->string',
	function() {
		testPrim('symbol->string', types.symbol, ['hello!'], types.string('hello!'));
	});


runTest('number->string',
	function() {
		testPrim('number->string', types.rational, [5], types.string('5'));
		testPrim('number->string', id, [types.complex(0, 2)], types.string('0+2i'));
		testPrim('number->string', id, [types.rational(5, 3)], types.string('5/3'));
	});


runTest('stinrg->number',
	function() {
		testPrim('string->number', types.string, ['abc'], false);
		testPrim('string->number', id, ['123'], 123);
		testPrim('string->number', types.string, ['0+3i'], types.complex(0, 3));
	});


runTest('string?',
	function() {
		testPrim('string?', id, [types.symbol('hello!')], false);
		testPrim('string?', id, ['string'], true);
		testPrim('string?', types.string, ['world'], true);
	});


runTest('make-string',
	function() {
		testPrim('make-string', id, [0, types.char('A')], types.string(""));
		testPrim('make-string', id, [types.rational(3), types.char('b')], types.string('bbb'));
	});


runTest('string',
	function() {
		testPrim('string', id, [], types.string(''));
		testPrim('string', types.char, ['a', 'b'], types.string('ab'));
	});

runTest('string-length',
	function() {
		testPrim('string-length', types.string, [''], 0);
		testPrim('string-length', id, ['5'], 1);
		testPrim('string-length', types.string, ['antidisestablishmentarianism'], 28);
	});

runTest('string-ref',
	function() {
		testPrim('string-ref', id, ['world', 3], types.char('l'));
		testPrim('string-ref', id, [types.string('abcd'), 1], types.char('b'));
		testPrim('string-ref', id, [types.string('asdfasdf'), 4], types.char('a'));
	});

runTest('string=?',
	function() {
		testPrim('string=?', id, ['asdf', 'Asdf'], false);
		testPrim('string=?', id, ['asdf', types.string('asdf')], true);
		testPrim('string=?', types.string, ['asdf', 'asdf', 'Asdf'], false);
		testPrim('string=?', types.string, ['far', 'fAr'], false);
		testPrim('string=?', id, ['', ''], true);
		testPrim('string=?', types.string, ['as', 'as', 'as'], true);
		testPrim('string=?', types.string, ['1', '1', '2'], false);
	});

runTest('string-ci=?',
	function() {
		testPrim('string-ci=?', id, ['asdf', 'Asdf'], true);
		testPrim('string-ci=?', id, ['asdf', types.string('asdf')], true);
		testPrim('string-ci=?', types.string, ['asdf', 'asdf', 'Asdf'], true);
		testPrim('string-ci=?', types.string, ['far', 'fAr'], true);
		testPrim('string-ci=?', id, ['', ''], true);
		testPrim('string-ci=?', types.string, ['as', 'as', 'as'], true);
		testPrim('string-ci=?', types.string, ['1', '1', '2'], false);
	});

runTest('string<?',
	function() {
		testPrim('string<?', id, ["", "a"], true);
		testPrim('string<?', types.string, ['abc', 'ab'], false);
		testPrim('string<?', id, [types.string('abc'), 'abc'], false);
		testPrim('string<?', types.string, ['abc', 'def', 'cde'], false);
		testPrim('string<?', id, ['A', types.string(']'), 'a'], true);
		testPrim('string<?', types.string, ['a', 'b', 'c', 'd', 'dd', 'e'], true);
	});

runTest('string>?',
	function() {
		testPrim('string>?', id, ["", "a"], false);
		testPrim('string>?', types.string, ['abc', 'ab'], true);
		testPrim('string>?', id, [types.string('abc'), 'abc'], false);
		testPrim('string>?', types.string, ['abc', 'def', 'cde'], false);
		testPrim('string>?', id, ['a', types.string(']'), 'A'], true);
		testPrim('string>?', types.string, ['e', 'd', 'cc', 'c', 'b', 'a'], true);
	});

runTest('string<=?',
	function() {
		testPrim('string<=?', id, ["", "a"], true);
		testPrim('string<=?', types.string, ['abc', 'ab'], false);
		testPrim('string<=?', id, [types.string('abc'), 'abc'], true);
		testPrim('string<=?', types.string, ['abc', 'aBc'], false);
		testPrim('string<=?', types.string, ['abc', 'def', 'cde'], false);
		testPrim('string<=?', id, ['A', types.string(']'), 'a'], true);
		testPrim('string<=?', types.string, ['a', 'b', 'b', 'd', 'dd', 'e'], true);
	});

runTest('string>=?',
	function() {
		testPrim('string>=?', id, ["", "a"], false);
		testPrim('string>=?', types.string, ['abc', 'ab'], true);
		testPrim('string>=?', id, [types.string('abc'), 'abc'], true);
		testPrim('string>=?', types.string, ['aBc', 'abc'], false);
		testPrim('string>=?', types.string, ['abc', 'def', 'cde'], false);
		testPrim('string>=?', id, ['a', types.string(']'), 'A'], true);
		testPrim('string>=?', types.string, ['e', 'e', 'cc', 'c', 'b', 'a'], true);
	});

runTest('string-ci<?',
	function() {
		testPrim('string-ci<?', id, ["", "a"], true);
		testPrim('string-ci<?', id, [types.string('Abc'), 'ab'], false);
		testPrim('string-ci<?', types.string, ['abc', 'abc'], false);
		testPrim('string-ci<?', types.string, ['abc', 'def', 'cde'], false);
		testPrim('string-ci<?', types.string, ['a', 'b', 'C', 'd', 'dd', 'e'], true);
	});

runTest('string-ci>?',
	function() {
		testPrim('string-ci>?', id, ["", "a"], false);
		testPrim('string-ci>?', id, [types.string('Abc'), 'ab'], true);
		testPrim('string-ci>?', types.string, ['abc', 'abc'], false);
		testPrim('string-ci>?', types.string, ['def', 'abc', 'cde'], false);
		testPrim('string-ci>?', types.string, ['e', 'D', 'cc', 'c', 'b', 'a'], true);
	});

runTest('string-ci<=?',
	function() {
		testPrim('string-ci<=?', id, ["", "a"], true);
		testPrim('string-ci<=?', types.string, ['Abc', 'ab'], false);
		testPrim('string-ci<=?', id, [types.string('abc'), 'abc'], true);
		testPrim('string-ci<=?', types.string, ['abc', 'aBc'], true);
		testPrim('string-ci<=?', types.string, ['abc', 'def', 'cde'], false);
		testPrim('string-ci<=?', types.string, ['a', 'b', 'b', 'D', 'dd', 'e'], true);
	});

runTest('string-ci>=?',
	function() {
		testPrim('string-ci>=?', id, ["", "a"], false);
		testPrim('string-ci>=?', types.string, ['Abc', 'ab'], true);
		testPrim('string-ci>=?', id, [types.string('abc'), 'abc'], true);
		testPrim('string-ci>=?', types.string, ['aBc', 'abc'], true);
		testPrim('string-ci>=?', types.string, ['def', 'abc', 'cde'], false);
		testPrim('string-ci>=?', types.string, ['e', 'e', 'cc', 'C', 'b', 'a'], true);
	});


runTest('substring',
	function() {
		testPrim('substring', id, ['abc', 1], types.string('bc'));
		testPrim('substring', id, [types.string('abc'), 0], types.string('abc'));
		testPrim('substring', id, ['abcdefgh', 2, 4], types.string('cd'));
		testPrim('substring', id, [types.string('abc'), 3], types.string(''));
		testPrim('substring', id, [types.string('abcd'), 2, 2], types.string(''));
	});


runTest('string-append',
	function() {
		testPrim('string-append', types.string, [], types.string(''));
		testPrim('string-append', id, ['a', types.string('b'), 'c'], types.string('abc'));
		testPrim('string-append', types.string, ['a', '', 'b', ' world'], types.string('ab world'));
	});


runTest('string->list',
	function() {
		testPrim('string->list', types.string, [''], types.EMPTY);
		testPrim('string->list', id, ['one'], types.list([types.char('o'), types.char('n'), types.char('e')]));
		testPrim('string->list', types.string, ['two'], types.list([types.char('t'),
										types.char('w'),
										types.char('o')]));
	});

runTest('list->string',
	function() {
		testPrim('list->string', id, [types.EMPTY], types.string(''));
		testPrim('list->string', id,
			 [types.list([types.char('H'),
					types.char('e'),
					types.char('l'),
					types.char('l'),
					types.char('o')])],
			 types.string('Hello'));
	});


runTest('string-copy',
	function() {
		testPrim('string-copy', types.string, [''], types.string(''));
		testPrim('string-copy', id, ['had'], types.string('had'));
		testPrim('string-copy', types.string, ['hello'], types.string('hello'));

		var state = new StateModule.State();
		var str = types.string('hello');
		state.pushControl(makeApplication(makePrimval('string-copy'), [makeConstant(str)]));
		var result = run(state);
		assert.deepEqual(result, str);
		assert.ok(result !== str);
	});


runTest('format',
	function() {
		testPrim('format', types.string, ['hello'], types.string('hello'));
		testPrim('format', id, ['hello~n'], types.string('hello\n'));
		testPrim('format', id, [types.string('Test: ~a~nTest2: ~A~%'),
					types.char('A'),
					types.list([1, 2, 3])],
			 types.string('Test: A\nTest2: (1 2 3)\n'));
		testPrim('format', id, ['~s ~S ~a',
					types.char('b'),
					types.complex(0, 2),
					types.char('b')],
			 types.string('#\\b 0+2i b'));

		testPrim('format', id, ['~s ~a', primitive.getPrimitive('+'), primitive.getPrimitive('format')],
			 types.string('#<procedure:+> #<procedure:format>'));
		
		var box1 = types.box('junk');
		var box2 = types.box(box1);
		box1.set(box2);
		testPrim('format', id, ['~s', box1], types.string('#&#&...'));
		
		var box3 = types.box('junk');
		box3.set(box3);
		testPrim('format', id, ['~a', box3], types.string('#&...'));
	});


runTest('explode',
	function() {
		testPrim('explode', id, [''], types.EMPTY);
		testPrim('explode', types.string, ['hello'], types.list([types.string('h'),
									     types.string('e'),
									     types.string('l'),
									     types.string('l'),
									     types.string('o')]));
	});


runTest('implode',
	function() {
		testPrim('implode', id, [types.EMPTY], types.string(''));
		testPrim('implode', types.list, [[types.string('h'),
						    types.string('e'),
						    types.string('l'),
						    types.string('l'),
						    types.string('o')]],
			 types.string('hello'));
	});


runTest('string->int',
	function() {
		testPrim('string->int', types.string, ['0'], 48);
		testPrim('string->int', types.string, ['\n'], 10);
	});


runTest('int->string',
	function() {
		testPrim('int->string', id, [50], types.string('2'));
		testPrim('int->string', id, [10], types.string('\n'));
	});


runTest('string-alphabetic?',
	function() {
		testPrim('string-alphabetic?', id, ['abcd'], true);
		testPrim('string-alphabetic?', types.string, ['AbCZ'], true);
		testPrim('string-alphabetic?', id, ['a b c'], false);
		testPrim('string-alphabetic?', types.string, ['1243!'], false);
	});


runTest('string-ith',
	function() {
		testPrim('string-ith', id, ['abcde', 2], types.string('c'));
		testPrim('string-ith', id, [types.string('12345'), 0], types.string('1'));
	});


runTest('string-lower-case?',
	function() {
		testPrim('string-lower-case?', types.string, ['abcd'], true);
		testPrim('string-lower-case?', id, ['abc1'], false);
		testPrim('string-lower-case?', types.string, ['Abc'], false);
	});


runTest('string-numeric?',
	function() {
		testPrim('string-numeric?', id, ['1234'], true);
		testPrim('string-numeric?', types.string, ['5432'], true);
		testPrim('string-numeric?', types.string, ['0+2i'], false);
		testPrim('string-numeric?', types.string, ['03()'], false);
	});


runTest('string-upper-case?',
	function() {
		testPrim('string-upper-case?', id, ['ABCD'], true);
		testPrim('string-upper-case?', types.string, ['ADF'], true);
		testPrim('string-upper-case?', types.string, ['AbZ'], false);
		testPrim('string-upper-case?', types.string, ['05AB'], false);
	});


runTest('string-whitespace?',
	function() {
		testPrim('string-whitespace?', types.string, ['a b c'], false);
		testPrim('string-whitespace?', id, [' \n '], true);
		testPrim('string-whitespace?', types.string, ['\t\r\n '], true);
	});


runTest('replicate',
	function() {
		testPrim('replicate', id, [3, types.string('ab')], types.string('ababab'))
		testPrim('replicate', id, [0, 'hi'], types.string(''));
		testPrim('replicate', id, [50, types.string('')], types.string(''));
	});


runTest('string->immutable-string',
	function() {
		testPrim('string->immutable-string', id, ['hello'], 'hello');
		testPrim('string->immutable-string', types.string, ['world'], 'world');
	});


runTest('string-set!',
	function() {
		var str1 = types.string('hello');
		testPrim('string-set!', id, [str1, 2, types.char('w')], types.VOID);
		assert.deepEqual(str1, types.string('hewlo'));

		var str2 = types.string('no');
		testPrim('string-set!', id, [str2, 1, types.char('!')], types.VOID);
		assert.deepEqual(str2, types.string('n!'));
	});


runTest('string-fill!',
	function() {
		var str1 = types.string('lawl');
		testPrim('string-fill!', id, [str1, types.char('q')], types.VOID);
		assert.deepEqual(str1, types.string('qqqq'));

		var str2 = types.string('');
		testPrim('string-fill!', id, [str2, types.char(' ')], types.VOID);
		assert.deepEqual(str2, types.string(''));
	});



/*************************************
 *** Primitive Math Function Tests ***
 *************************************/


runTest("zero?",
	function() {
		testPrim('zero?', types.rational, [0], true);
		testPrim('zero?', types.rational, [1], false);
		testPrim('zero?', id, [types.complex(0, 1)], false);
	});



runTest("sub1",
	function() {
		testPrim('sub1', types.rational, [25], types.rational(24));
		testPrim('sub1', id, [types.complex(3, 5)], types.complex(2, 5));
	});


runTest("add1",
	function() {
		testPrim('add1', types.rational, [25], types.rational(26));
		testPrim('add1', id, [types.complex(3, 5)], types.complex(4, 5));
	});


runTest("+",
	function() {
		testPrim('+', types.rational, [], types.rational(0));
		testPrim('+', types.rational, [2], types.rational(2));
		testPrim('+', types.rational, [1, 2], types.rational(3));
		testPrim('+', types.rational, [1, 2, 3, 4], types.rational(10));
	});


runTest("-",
	function() {
		testPrim('-', types.rational, [2], types.rational(-2));
		testPrim('-', types.rational, [1, 2], types.rational(-1));
		testPrim('-', types.rational, [1, 2, 3, 4], types.rational(-8));
	});


runTest("*",
	function() {
		testPrim('*', types.rational, [], types.rational(1));
		testPrim('*', types.rational, [2], types.rational(2));
		testPrim('*', types.rational, [1, 2], types.rational(2));
		testPrim('*', types.rational, [1, 2, 3, 4], types.rational(24));
	});


runTest("/",
	function() {
		testPrim('/', types.rational, [2], types.rational(1, 2));
		testPrim('/', types.rational, [1, 3], types.rational(1, 3));
		testPrim('/', types.rational, [18, 2, 3, 4], types.rational(3, 4));
	});


runTest('abs',
	function() {
		testPrim('abs', types.rational, [2], types.rational(2));
		testPrim('abs', types.rational, [0], types.rational(0));
		testPrim('abs', types.rational, [-2], types.rational(2));
	});


runTest('quotient',
	function() {
		testPrim('quotient', types.rational, [5, 3], types.rational(1));
	});


runTest('remainder',
	function() {
		testPrim('remainder', types.rational, [5, 3], types.rational(2));
	});


runTest('modulo',
	function() {
	    testPrim('modulo', types.rational, [-5, 3], types.rational(1));
	});


runTest('=',
	function() {
	    testPrim('=', types.rational, [2, 3], false);
	    testPrim('=', types.rational, [2, 2, 2, 2], true);
	    testPrim('=', types.rational, [2, 2, 3, 3], false);
	});


runTest('<',
	function() {
	    testPrim('<', types.rational, [1, 2], true);
	    testPrim('<', types.rational, [2, 2], false);
	    testPrim('<', types.rational, [3, 2], false);
	    testPrim('<', types.rational, [1, 2, 3, 4], true);
	    testPrim('<', types.rational, [1, 2, 2, 3], false);
	    testPrim('<', types.rational, [1, 3, 5, 4], false);
	});


runTest('>',
	function() {
	    testPrim('>', types.rational, [1, 2], false);
	    testPrim('>', types.rational, [2, 2], false);
	    testPrim('>', types.rational, [3, 2], true);
	    testPrim('>', types.rational, [4, 3, 2, 1], true);
	    testPrim('>', types.rational, [4, 3, 3, 2], false);
	    testPrim('>', types.rational, [4, 3, 5, 2], false);
	});


runTest('<=',
	function() {
	    testPrim('<=', types.rational, [1, 2], true);
	    testPrim('<=', types.rational, [2, 2], true);
	    testPrim('<=', types.rational, [3, 2], false);
	    testPrim('<=', types.rational, [1, 2, 3, 4], true);
	    testPrim('<=', types.rational, [2, 3, 3, 3], true);
	    testPrim('<=', types.rational, [1, 3, 5, 4], false);
	});


runTest('>=',
	function() {
	    testPrim('>=', types.rational, [1, 2], false);
	    testPrim('>=', types.rational, [2, 2], true);
	    testPrim('>=', types.rational, [3, 2], true);
	    testPrim('>=', types.rational, [4, 3, 2, 1], true);
	    testPrim('>=', types.rational, [4, 3, 3, 2], true);
	    testPrim('>=', types.rational, [5, 3, 5, 4], false);
	});


runTest('positive?',
	function() {
		testPrim('positive?', types.rational, [-1], false);
		testPrim('positive?', types.rational, [0], false);
		testPrim('positive?', types.rational, [1], true);
	});


runTest('negative?',
	function() {
		testPrim('negative?', types.rational, [-1], true);
		testPrim('negative?', types.rational, [0], false);
		testPrim('negative?', types.rational, [1], false);
	});


runTest('max',
	function() {
		testPrim('max', types.rational, [1], types.rational(1));
		testPrim('max', types.rational, [1, 2], types.rational(2));
		testPrim('max', types.rational, [2, 1, 4, 3, 6, 2], types.rational(6));
	});


runTest('min',
	function() {
		testPrim('min', types.rational, [1], types.rational(1));
		testPrim('min', types.rational, [1, 2], types.rational(1));
		testPrim('min', types.rational, [2, 1, 4, 3, 6, 2], types.rational(1));
	});


runTest('=~',
	function() {
		testPrim('=~', id, [1, 2, 2], true);
		testPrim('=~', id, [1, 2, types.float(0.5)], false);
		testPrim('=~', types.rational, [5, 3, 1], false);
		testPrim('=~', types.rational, [5, 3, 4], true);
	});


runTest('conjugate',
	function() {
		testPrim('conjugate', id, [1], 1);
		testPrim('conjugate', id, [types.complex(3, 3)], types.complex(3, -3));
	});


runTest('magnitude',
	function() {
		testPrim('magnitude', id, [4], 4);
		testPrim('magnitude', id, [types.complex(3, 4)], 5);
		testPrim('magnitude', id, [types.float(3.5)], types.float(3.5));
		testPrim('magnitude', id, [types.rational(3, 5)], types.rational(3, 5));
		testPrim('magnitude', id, [types.complex(12, 5)], 13);
	});


runTest('number?',
	function() {
		testPrim('number?', id, [5], true);
		testPrim('number?', types.rational, [10], true);
		testPrim('number?', id, [types.rational(10, 3)], true);
		testPrim('number?', types.float, [10.5], true);
		testPrim('number?', id, [types.complex(5, 3)], true);
		testPrim('number?', id, ['string'], false);
	});


runTest('complex?',
	function() {
		testPrim('complex?', id, [5], true);
		testPrim('complex?', types.rational, [10], true);
		testPrim('complex?', id, [types.rational(10, 3)], true);
		testPrim('complex?', types.float, [10.5], true);
		testPrim('complex?', id, [types.complex(5, 3)], true);
		testPrim('complex?', id, ['string'], false);
	});


runTest('real?',
	function() {
		testPrim('real?', id, [5], true);
		testPrim('real?', types.rational, [10], true);
		testPrim('real?', id, [types.rational(10, 3)], true);
		testPrim('real?', types.float, [10.5], true);
		testPrim('real?', id, [types.complex(5, 3)], false);
		testPrim('real?', id, ['string'], false);
	});


runTest('rational?',
	function() {
		testPrim('rational?', id, [5], true);
		testPrim('rational?', types.rational, [10], true);
		testPrim('rational?', id, [types.rational(10, 3)], true);
		testPrim('rational?', types.float, [10.5], true);
		testPrim('rational?', types.float, [Math.sqrt(2)], true);
		testPrim('rational?', id, [types.complex(5, 3)], false);
		testPrim('rational?', id, ['string'], false);
	});


runTest('integer?',
	function() {
		testPrim('integer?', id, [5], true);
		testPrim('integer?', types.rational, [10], true);
		testPrim('integer?', id, [types.complex(5, 0)], true);
		testPrim('integer?', id, [types.rational(10, 3)], false);
		testPrim('integer?', types.float, [10.5], false);
		testPrim('integer?', id, [types.complex(5, 3)], false);
		testPrim('integer?', id, ['string'], false);
	});


runTest('exact?',
	function() {
		testPrim('exact?', id, [5], true);
		testPrim('exact?', id, [types.rational(4, 3)], true);
		testPrim('exact?', types.float, [10.0], false);
		testPrim('exact?', id, [types.complex(5, 2)], true);
		testPrim('exact?', id, [types.complex(types.float(5.2), types.float(0.1))], false);
	});


runTest('inexact?',
	function() {
		testPrim('inexact?', id, [5], false);
		testPrim('inexact?', id, [types.rational(4, 3)], false);
		testPrim('inexact?', types.float, [10.0], true);
		testPrim('inexact?', id, [types.complex(5, 2)], false);
		testPrim('inexact?', id, [types.complex(types.float(5.2), types.float(0.1))], true);
	});


runTest('odd? and even?',
	function() {
		testPrim('odd?', id, [5], true);
		testPrim('odd?', types.float, [10.0], false);
		testPrim('even?', id, [15], false);
		testPrim('even?', types.float, [13.0], false);
	});


runTest('gcd and lcm',
	function() {
		testPrim('gcd', id, [1001, 98], 7);
		testPrim('gcd', id, [6, 10, 15], 1);
		testPrim('lcm', id, [91, 77], 1001);
		testPrim('lcm', id, [6, 10, 15], 30);
	});


runTest('floor, ceiling, and round',
	function() {
	    testPrim('floor', id, [14], 14);
	    testPrim('floor', types.float, [12.56], types.float(12));
	    testPrim('ceiling', id, [13], 13);
	    testPrim('ceiling', types.float, [12.23], types.float(13));
	    testPrim('ceiling', types.float, [12.00], types.float(12));
	    testPrim('round', id, [124], 124);
	    testPrim('round', types.float, [12.432], types.float(12));
	    testPrim('round', types.float, [12.543], types.float(13));
	});


runTest('numerator and denominator',
	function() {
		testPrim('numerator', id, [30], 30);
		testPrim('numerator', id, [types.rational(10, -2)], -5);
		testPrim('numerator', types.float, [10.5], types.float(21));
		testPrim('numerator', types.float, [-2.53], types.float(-253));
		testPrim('denominator', id, [43], 1);
		testPrim('denominator', id, [types.rational(12, 4)], 1);
		testPrim('denominator', id, [types.rational(23, -5)], 5);
		testPrim('denominator', types.float, [12.125], types.float(8));
		testPrim('denominator', types.float, [-2.53], types.float(100));
	});


runTest('exp and log',
	function() {
		testPrim('exp', id, [0], 1);
		testPrim('exp', types.float, [0], types.float(1));
		testPrim('exp', id, [3], types.float(Math.exp(3)));
		testPrim('log', id, [1], 0);
		testPrim('log', types.float, [1], types.float(0));
		testPrim('log', id, [primitive.getPrimitive('e')], types.float(1));
	});


runTest('sin, cos, tan, asin, acos, atan',
	function() {
	    testPrim('sin', id, [20], types.float(Math.sin(20)));
 	    testPrim('sin', id, [0], 0);
 	    testPrim('cos', id, [0], 1);
 	    testPrim('cos', types.float, [43], types.float(Math.cos(43)));
 	    testPrim('tan', types.float, [0], types.float(0));
 	    testPrim('tan', id, [-30], types.float(Math.tan(-30)));
	    
 	    testPrim('asin', types.float, [-0.5], types.float(Math.asin(-0.5)));
 	    testPrim('acos', types.float, [0.53], types.float(Math.acos(0.53)));
 	    testPrim('atan', types.float, [-543], types.float(Math.atan(-543)));
	});


runTest('sqrt, integer-sqrt, and expt',
	function() {
		testPrim('sqrt', id, [25], 5);
		testPrim('sqrt', types.float, [1.44], types.float(1.2));
		testPrim('sqrt', id, [-1], types.complex(0, 1));
		testPrim('sqrt', id, [types.complex(0, 2)], types.complex(1, 1));
		testPrim('sqrt', id, [types.complex(types.float(0), types.float(-2))],
			 types.complex(types.float(1), types.float(-1)));

		testPrim('integer-sqrt', id, [15], 3);
		testPrim('integer-sqrt', id, [88], 9);

		testPrim('expt', id, [2, 20], 1048576);
		testPrim('expt', id, [3, 3], 27);
		testPrim('expt', types.float, [12.4, 5.43], types.float(Math.pow(12.4, 5.43)));
	});


runTest('make-rectangular, make-polar, real-part, imag-part, angle',
	function() {
		testPrim('make-rectangular', id, [5, 3], types.complex(5, 3));
		testPrim('make-rectangular', id, [5, types.float(4)],
			 types.complex(types.float(5), types.float(4)));
		
		testPrim('make-polar', id, [1, 0], types.complex(1, 0));
		testPrimF('make-polar', types.float, [5, Math.PI/2], true,
			  function(res) {
			  	return (jsnums.isInexact(res) &&
					Math.abs(jsnums.toFixnum(jsnums.realPart(res))) < 0.000001 &&
					Math.abs(jsnums.toFixnum(jsnums.imaginaryPart(res)) - 5) < 0.0000001);
				});

		testPrim('real-part', id, [14], 14);
		testPrim('real-part', types.float, [4], types.float(4));
		testPrim('real-part', id, [types.complex(0, 1)], 0);
		testPrim('real-part', id, [types.complex(types.float(1.44), types.float(5))], types.float(1.44));

		testPrim('imag-part', id, [14], 0);
		testPrim('imag-part', types.float, [4], 0);
		testPrim('imag-part', id, [types.complex(0, 1)], 1);
		testPrim('imag-part', id, [types.complex(types.float(1.44), types.float(5))], types.float(5));

		testPrim('angle', id, [types.complex(3, 0)], 0);
		testPrim('angle', types.float, [4.46], 0);
		testPrim('angle', id, [-54], types.float(Math.PI));
		testPrimF('angle', id, [types.complex(1, 1)], true,
		          function(res) {
			  	return (jsnums.isInexact(res) &&
					Math.abs(jsnums.toFixnum(res) - Math.PI/4) < 0.0000001);
				});
	});


runTest('exact->inexact and inexact->exact',
	function() {
		testPrim('exact->inexact', id, [5], types.float(5));
		testPrim('exact->inexact', types.float, [5.2], types.float(5.2));
		testPrim('exact->inexact', id, [types.rational(2, 3)], types.float(2/3));
		testPrim('exact->inexact', id, [types.complex(3, 5)], types.complex(types.float(3), types.float(5)));

		testPrim('inexact->exact', types.float, [0], 0);
		testPrim('inexact->exact', types.float, [1.25], types.rational(5, 4));
		testPrim('inexact->exact', id, [5], 5);
		testPrim('inexact->exact', id, [types.complex(5, 3)], types.complex(5, 3));
		testPrim('inexact->exact', id, [types.complex(types.float(5.2), types.float(4))],
			 types.complex(types.rational(26, 5), 4));
	});


runTest('first, second, third, fourth, fifth, sixth, seventh, eighth',
	function() {
		var testList1 = types.list([1, 2, 3, 4, 5, 6, 7, 8, 9, 10]);
		var testList2 = types.list([types.list([1, 2]),
					      types.list([3, 4]),
					      types.list([5, 6]),
					      types.list([7, 8]),
					      types.list([9, 10]),
					      types.list([11, 12]),
					      types.list([13, 14]),
					      types.list([15, 16]),
					      types.list([17, 18]),
					      types.list([19, 20])]);
		testPrim('first', id, [testList1], 1);
		testPrim('first', id, [testList2], types.list([1, 2]));
		
		testPrim('second', id, [testList1], 2);
		testPrim('second', id, [testList2], types.list([3, 4]));

		testPrim('third', id, [testList1], 3);
		testPrim('third', id, [testList2], types.list([5, 6]));

		testPrim('fourth', id, [testList1], 4);
		testPrim('fourth', id, [testList2], types.list([7, 8]));

		testPrim('fifth', id, [testList1], 5);
		testPrim('fifth', id, [testList2], types.list([9, 10]));

		testPrim('sixth', id, [testList1], 6);
		testPrim('sixth', id, [testList2], types.list([11, 12]));

		testPrim('seventh', id, [testList1], 7);
		testPrim('seventh', id, [testList2], types.list([13, 14]));

		testPrim('eighth', id, [testList1], 8);
		testPrim('eighth', id, [testList2], types.list([15, 16]));
	});




/*************************************
 *** Primitive List Function Tests ***
 *************************************/


runTest('cons, car, and cdr',
	function() {
		var state = new StateModule.State();
		state.pushControl(makeApplication(makePrimval('car'),
						  [makeApplication(makePrimval('cons'),
							 	   [makeConstant(types.rational(1)),
								    makeConstant(types.EMPTY)])]));
		assert.deepEqual(run(state), types.rational(1));

		state.pushControl(makeApplication(makePrimval('cdr'),
						  [makeApplication(makePrimval('cons'),
							  	   [makeConstant(types.rational(1)),
								    makeConstant(types.EMPTY)])]));
		assert.deepEqual(run(state), types.EMPTY);

		state.pushControl(makeApplication(makePrimval('cdr'),
						  [makeApplication(makePrimval('cons'),
							[makeConstant(types.rational(1)),
							 makeApplication(makePrimval('cons'),
								[makeConstant(types.rational(2)),
								 makeConstant(types.EMPTY)])])]));
		assert.deepEqual(run(state), types.pair(2, types.EMPTY));
	});


runTest('list?',
	function() {
		testPrim('list?', id, [types.EMPTY], true);
		testPrim('list?', id, [types.pair(1, types.EMPTY)], true);
		testPrim('list?', id, [types.list([1, 2, 0, 3, 2])], true);
		testPrim('list?', id, [types.pair(1, 4)], false);
		testPrim('list?', id, [types.complex(0, 2)], false);
	});


runTest('list',
	function() {
		testPrim('list', types.rational, [], types.EMPTY);
		testPrim('list', types.rational, [1], types.pair(types.rational(1), types.EMPTY));
		testPrim('list', types.rational, [1, 5, 3], types.list([types.rational(1),
									    types.rational(5),
									    types.rational(3)]));
	});


runTest('list*',
	function() {
		testPrim('list*', id, [types.EMPTY], types.EMPTY);
		testPrim('list*', id, [types.rational(1), types.pair(types.rational(2), types.EMPTY)],
			 types.list([types.rational(1), types.rational(2)]));
		testPrim('list*', id, [1, 2, 3, types.list([4, 5])], types.list([1, 2, 3, 4, 5]));
	});


runTest('length',
	function() {
		testPrim('length', id, [types.EMPTY], 0);
		testPrim('length', types.list, [[1]], 1);
		testPrim('length', types.list, [[1, 2, 3, 4]], 4);
	});


runTest('append',
	function() {
		testPrim('append', types.list, [], types.EMPTY);
		testPrim('append', types.list, [[1]], types.list([1]));
		testPrim('append', types.list, [[], [1, 2, 3], [1, 2]],
			 types.list([1, 2, 3, 1, 2]));
		testPrim('append', id, [types.list([1, 2]), types.list([3]), 4],
			 types.pair(1, types.pair(2, types.pair(3, 4))));
		testPrim('append', id, [5], 5);
		testPrim('append', id, [types.EMPTY, 3], 3);
	});


runTest('reverse',
	function() {
		testPrim('reverse', id, [types.EMPTY], types.EMPTY);
		testPrim('reverse', id, [types.list([1])], types.list([1]));
		testPrim('reverse', id, [types.list([1, 2, 3, 4, 5])], types.list([5, 4, 3, 2, 1]));
	});


runTest('list-ref',
	function() {
		var testList = types.list([types.rational(1),
					     types.rational(1),
					     types.rational(2),
					     types.rational(3),
					     types.rational(5),
					     types.rational(8),
					     types.rational(11)]);
		testPrim('list-ref', id, [testList, types.rational(0)], types.rational(1));
		testPrim('list-ref', id, [testList, types.rational(5)], types.rational(8));
	});


runTest('memq',
	function() {
		testPrim('memq', id, [0, types.list([1, 2, 3])], false);
		testPrim('memq', id, [2, types.list([1, 2, 3])], types.list([2, 3]));
		testPrim('memq', id, [types.complex(2, 2),
				      types.list([types.complex(1, 1),
						    types.complex(2, 2),
						    types.complex(3, 3)])],
			 false);
		testPrim('memq', id, [types.char('a'),
				      types.list([types.char('c'),
						    types.char('b'),
						    types.char('a')])],
			 types.list([types.char('a')]));
		testPrim('memq', id, [types.string('a'),
				      types.list([types.string('c'),
						    types.string('b'),
						    types.string('a')])],
			 false);

		var str = types.string('hi');
		testPrim('memq', id, [str, types.list([types.string('Yo'),
						         types.string(', '),
						         str])],
			 types.list([str]));
	});


runTest('memv',
	function() {
		testPrim('memv', id, [0, types.list([1, 2, 3])], false);
		testPrim('memv', id, [2, types.list([1, 2, 3])], types.list([2, 3]));
		testPrim('memv', id, [types.complex(2, 2),
				      types.list([types.complex(1, 1),
						    types.complex(2, 2),
						    types.complex(3, 3)])],
			 types.list([types.complex(2, 2), types.complex(3, 3)]));
		testPrim('memv', id, [types.char('a'),
				      types.list([types.char('c'),
						    types.char('b'),
						    types.char('a')])],
			 types.list([types.char('a')]));
		testPrim('memv', id, [types.string('a'),
				      types.list([types.string('c'),
						    types.string('b'),
						    types.string('a')])],
			 false);

		var str = types.string('hi');
		testPrim('memv', id, [str, types.list([types.string('Yo'),
						         types.string(', '),
						         str])],
			 types.list([str]));
	});


runTest('member',
	function() {
		testPrim('member', id, [0, types.list([1, 2, 3])], false);
 		testPrim('member', id, [2, types.list([1, 2, 3])], types.list([2, 3]));
 		testPrim('member', id, [types.complex(2, 2),
 				        types.list([types.complex(1, 1),
 						      types.complex(2, 2),
 						      types.complex(3, 3)])],
 			 types.list([types.complex(2, 2), types.complex(3, 3)]));
 		testPrimF('member', id, [types.char('b'),
 					 types.list([types.char('c'),
 						       types.char('b'),
 						       types.char('a')])],
 			  ['#\\b', '#\\a'], listToStringArray);
 		testPrimF('member', id, [types.string('a'),
 					 types.list([types.string('c'),
 						       types.string('b'),
 						       types.string('a')])],
 			  ['a'], listToStringArray);

 		var str = types.string('hi');
 		testPrim('member', id, [str, types.list([types.string('Yo'),
 							   types.string(', '),
 							   str])],
 			 types.list([str]));
	});


runTest('remove',
	function() {
		testPrim('remove', id, [3, types.list([1, 2, 3, 4, 5])], types.list([1, 2, 4, 5]));
		testPrim('remove', id, [1, types.list([1, 2, 1, 2])], types.list([2, 1, 2]));
		testPrim('remove', id, [10, types.list([1, 2, 3, 4])], types.list([1,2,3,4]));
		testPrimF('remove', id, [types.string('a'), types.list([types.string('b'),
									    types.string('a'),
									    types.string('c'),
									    types.string('a')])],
			  ['b', 'c', 'a'], listToStringArray);
		var state = new StateModule.State();
		state.pushControl(makeApplication(makePrimval('remove'),
						  [makeConstant(types.string('a')),
						   makeConstant(types.list([types.string('b'),
									      types.string('a'),
									      types.string('c'),
									      types.string('a')]))]));
		var res = run(state);
		assert.deepEqual(res.first().toString(), 'b');
		assert.deepEqual(res.rest().first().toString(), 'c');
		assert.deepEqual(res.rest().rest().first().toString(), 'a');
		assert.deepEqual(res.rest().rest().rest(), types.EMPTY);
	});



runTest('map',
	function() {
		var state = new StateModule.State();
		state.pushControl(makeApplication(makePrimval('map'),
						  [makePrimval('add1'),
						   makeConstant(types.list([1, 2, 3]))]));
		assert.deepEqual(run(state), types.list([2, 3, 4]));
	});

runTest('filter',
	function() {
		var state = new StateModule.State();
		state.pushControl(makeApplication(makePrimval('filter'),
						  [makePrimval('even?'),
						   makeConstant(types.list([1, 2, 3, 4, 5, 6]))]));
		assert.deepEqual(run(state), types.list([2, 4, 6]));

		state.pushControl(makeApplication(makePrimval('filter'),
						  [makeLam(1, [], makeConstant(false)),
						   makeConstant(types.list([1, 2, 3, 4]))]));
		assert.deepEqual(run(state), types.EMPTY);

		state.pushControl(makeApplication(makePrimval('filter'),
						  [makeLam(1, [], makeConstant(true)),
						   makeConstant(types.list([1, 2, 3, 4]))]));
		assert.deepEqual(run(state), types.list([1, 2, 3, 4]));
	});


runTest('foldl',
	function() {
		var state = new StateModule.State();
		state.pushControl(makeApplication(makePrimval('foldl'),
						  [makePrimval('-'),
						   makeConstant(2),
						   makeConstant(types.list([1, 2, 3, 4]))]));
		assert.deepEqual(run(state), 4);

		state.pushControl(makeApplication(makePrimval('foldl'),
						  [makePrimval('cons'),
						   makeConstant(types.list([1, 2])),
						   makeConstant(types.list([3, 4, 5, 6]))]));
		assert.deepEqual(run(state), types.list([6, 5, 4, 3, 1, 2]));
	});


runTest('foldr',
	function() {
		var state = new StateModule.State();
		state.pushControl(makeApplication(makePrimval('foldr'),
						  [makePrimval('-'),
						   makeConstant(2),
						   makeConstant(types.list([1, 2, 3, 4]))]));
		assert.deepEqual(run(state), 0);

		state.pushControl(makeApplication(makePrimval('foldr'),
						  [makePrimval('cons'),
						   makeConstant(types.list([1, 2])),
						   makeConstant(types.list([3, 4, 5, 6]))]));
		assert.deepEqual(run(state), types.list([3, 4, 5, 6, 1, 2]));
	});



runTest('build-list',
	function() {
		var state = new StateModule.State();
		state.pushControl(makeApplication(makePrimval('build-list'),
						  [makeConstant(5), makePrimval('add1')]));
		assert.deepEqual(run(state), types.list([1, 2, 3, 4, 5]));

		state.pushControl(makeApplication(makePrimval('build-list'),
						  [makeConstant(5), makePrimval('number->string')]));
		assert.deepEqual(run(state), types.list([types.string('0'),
							   types.string('1'),
							   types.string('2'),
							   types.string('3'),
							   types.string('4')]));
	});


runTest('argmax',
	function() {
		var state = new StateModule.State();
		state.pushControl(makeApplication(makePrimval('argmax'),
						  [makePrimval('car'),
						   makeConstant(types.list([types.pair(1, 2),
									      types.list([1, 2, 3]),
									      types.pair(3, 5),
									      types.pair(2, 13)]))]));
		assert.deepEqual(run(state), types.pair(3, 5));

		state.pushControl(makeApplication(makePrimval('argmax'),
						  [makePrimval('-'),
						   makeConstant(types.list([1, 3, 5, 2, 4]))]));
		assert.deepEqual(run(state), 1);
	});


runTest('argmin',
	function() {
		var state = new StateModule.State();
		state.pushControl(makeApplication(makePrimval('argmin'),
						  [makePrimval('car'),
						   makeConstant(types.list([types.pair(1, 2),
									      types.list([1, 2, 3]),
									      types.pair(3, 5),
									      types.pair(2, 13)]))]));
		assert.deepEqual(run(state), types.pair(1, 2));

		state.pushControl(makeApplication(makePrimval('argmin'),
						  [makePrimval('-'),
						   makeConstant(types.list([1, 3, 5, 2, 4]))]));
		assert.deepEqual(run(state), 5);
	});


runTest('quicksort',
	function() {
	    var state = new StateModule.State();
	    state.pushControl(makeApplication(makePrimval('quicksort'),
					      [makeConstant(types.list([4, 3, 6, 8, 2, 9])),
					       makePrimval('<')]));
	    var result = run(state);
	    assert.ok(types.isEqual(result, types.list([2, 3, 4, 6, 8, 9])));

	    state.pushControl(makeApplication(makePrimval('quicksort'),
					      [makeConstant(types.list([types.char('k'),
									types.char('o'),
									types.char('c'),
									types.char('g')])),
					       makePrimval('char>?')]));
	    assert.ok(types.isEqual(run(state), types.list([types.char('o'),
							    types.char('k'),
							    types.char('g'),
							    types.char('c')])));
	});


runTest('compose',
	function() {
		var state = new StateModule.State();
		state.pushControl(makeApplication(makeApplication(makePrimval('compose'),
								  [makePrimval('magnitude'),
								   makePrimval('+'),
								   makePrimval('values')]),
						  [makeConstant(2),
						   makeConstant(3),
						   makeConstant(2),
						   makeConstant(types.complex(-4, 4))]));
		assert.deepEqual(run(state), types.rational(5));

		var composed = makeApplication(makePrimval('compose'),
					       [makePrimval('even?'),
						makePrimval('*'),
						makePrimval('values')]);
		state.pushControl(makeApplication(composed, [makeConstant(3), makeConstant(5)]));
		assert.deepEqual(run(state), false);
		state.pushControl(makeApplication(composed, [makeConstant(2), makeConstant(4), makeConstant(15)]));
		assert.deepEqual(run(state), true);
	});


runTest('caar, cadr, cdar, cddr, etc.',
	function() {
		var deepArrayToList = function(a) {
			if ( !(a instanceof Array) ) {
				return a;
			}
			return types.list( helpers.map(deepArrayToList, a) );
		}

		testPrim('car', types.list, [[1, 2, 3]], 1);
		testPrim('caar', deepArrayToList, [[[1, 2], [3, 4], []]], 1);
		testPrim('caar', deepArrayToList, [[[[1, 2], [3, 4]], [[5, 6], [7, 8]]]], types.list([1, 2]));
		testPrim('caar', types.list, [[types.pair(1, types.pair(2, 3))]], 1);

		testPrim('cadr', types.list, [[1, 2, 3]], 2);
		testPrim('cadr', deepArrayToList, [[[1, 2], [3, 4]]], types.list([3, 4]));

		testPrim('cdar', deepArrayToList, [[[1, 2], [3, 4], []]], types.list([2]));
		testPrim('cdar', types.list, [[types.pair(1, 2)]], 2);

		testPrim('cddr', types.list, [[1, 2, 3, 4]], types.list([3, 4]));
		testPrim('cddr', deepArrayToList, [[[], [1], [1, 2], [1, 2, 3]]], deepArrayToList([[1, 2], [1, 2, 3]]));
		testPrim('cddr', id, [types.pair(1, types.pair(2, 3))], 3);

		testPrim('caaar', deepArrayToList, [[[[1, 2], [3, 4]], [[5, 6], [7, 8]]]], 1);
		testPrim('caaar', deepArrayToList, [[[types.pair(0, 1)]]], 0);

		testPrim('caadr', deepArrayToList, [[[1, 2], [3, 4], []]], 3);
		testPrim('caadr', deepArrayToList, [[[[1, 2], [3, 4]], [[5, 6], [7, 8]]]], types.list([5, 6]));

		testPrim('cadar', deepArrayToList, [[[1, 2], [3, 4], []]], 2);
		testPrim('cadar', deepArrayToList, [[[[1, 2], [3, 4]], [[5, 6], [7, 8]]]], types.list([3, 4]));

		testPrim('cdaar', deepArrayToList, [[[[1, 2], [3, 4]], [[5, 6], [7, 8]]]], types.list([2]));
		testPrim('cdaar', deepArrayToList, [[[types.pair(0, 1)]]], 1);

		testPrim('cdadr', deepArrayToList, [[[1, 2], [3, 4], []]], types.list([4]));
		testPrim('cdadr', deepArrayToList, [[[[1, 2], [3, 4]], [[5, 6], [7, 8]]]], deepArrayToList([[7, 8]]));
		testPrim('cdadr', deepArrayToList, [[types.pair(1, 2), types.pair(3, 4)]], 4);

		testPrim('cddar', deepArrayToList, [[[1, 2], [3, 4], []]], types.EMPTY);
		testPrim('cddar', deepArrayToList, [[types.pair(1, types.pair(2, 3))]], 3);

		testPrim('caddr', types.list, [[1, 2, 3, 4]], 3);
		testPrim('caddr', deepArrayToList, [[[1, 2], [3, 4], []]], types.EMPTY);

		testPrim('cdddr', types.list, [[1, 2, 3, 4]], types.list([4]));
		testPrim('cdddr', id, [types.pair(1, types.pair(2, types.pair(3, 4)))], 4);

		testPrim('cadddr', types.list, [[1, 2, 3, 4]], 4);
		testPrim('cadddr', deepArrayToList, [[[1, 2], [3, 4], [5, 6], [7, 8]]], types.list([7, 8]));
	});




/***************************
 *** Box Primitive Tests ***
 ***************************/


runTest('box',
	function() {
		testPrim('box', id, [1], types.box(1));
		testPrim('box', types.string, ['abc'], types.box(types.string('abc')));
	});


runTest('box?',
	function() {
		testPrim('box?', types.box, [1], true);
		testPrim('box?', types.char, ['a'], false);
		testPrim('box?', id, [15], false);
	});


runTest('unbox',
	function() {
		testPrim('unbox', types.box, [2], 2);
		testPrim('unbox', types.box, [types.char('a')], types.char('a'));
	});


runTest('set-box!',
	function() {
		var testBox1 = types.box(1);
		var testBox2 = types.box(types.string('hello'));
		testPrim('set-box!', id, [testBox1, 15], types.VOID);
		testPrim('set-box!', id, [testBox2, types.string('world')], types.VOID);

		assert.deepEqual(testBox1, types.box(15));
		assert.deepEqual(testBox2, types.box(types.string('world')));
	});




/****************************
 *** Hash Primitive Tests ***
 ****************************/


runTest('hash?',
	function() {
		testPrim('hash?', id, [1], false);
		testPrim('hash?', types.vector, [[1, 2, 3]], false);
		testPrim('hash?', types.hash, [types.EMPTY], true);
		testPrim('hash?', types.hashEq, [types.EMPTY], true);
		testPrim('hash?', types.hash, [types.list([types.pair(1, 2)])], true);
		testPrim('hash?', types.hashEq, [types.list([types.pair(1, 2)])], true);
	});


runTest('str',
	function() {
	    assert.equal(typeof(types.string('a')), 'object');
	});


runTest('make-hash',
	function() {
		var state = new StateModule.State();
		state.pushControl(makeApplication(makePrimval('make-hash'), []));
		var res = run(state);
		assert.ok(types.isHash(res));
		assert.ok(res.hash.isEmpty());


		state.pushControl(makeApplication(makePrimval('make-hash'),
						  [makeConstant(types.list([types.pair(1, 2),
									      types.pair(3, 4),
									      types.pair(5, 6)]))]));
		var res2 = run(state);
		assert.ok(types.isHash(res2));
		assert.ok( !res2.hash.isEmpty() );
		assert.ok(res2.hash.containsKey(1));
		assert.ok(res2.hash.containsKey(3));
		assert.ok(res2.hash.containsKey(5));
		assert.deepEqual(res2.hash.get(1), 2);
		assert.deepEqual(res2.hash.get(3), 4);
		assert.deepEqual(res2.hash.get(5), 6);

		state.pushControl(makeApplication(makePrimval('make-hash'),
						  [makeConstant(types.list(
								  [types.pair(types.string('a'),
									  	2)]))]));
		var res3 = run(state);
		assert.deepEqual(res3.hash.get(types.string('a')), 2);
	});


runTest('make-hasheq',
	function() {
		var state = new StateModule.State();
		state.pushControl(makeApplication(makePrimval('make-hasheq'), []));
		var res = run(state);
		assert.ok(types.isHash(res));
		assert.ok(res.hash.isEmpty());


		state.pushControl(makeApplication(makePrimval('make-hasheq'),
						  [makeConstant(types.list([types.pair(1, 2),
									      types.pair(3, 4),
									      types.pair(5, 6)]))]));
		var res2 = run(state);
		assert.ok(types.isHash(res2));
		assert.ok( !res2.hash.isEmpty() );
		assert.ok(res2.hash.containsKey(1));
		assert.ok(res2.hash.containsKey(3));
		assert.ok(res2.hash.containsKey(5));
		assert.deepEqual(res2.hash.get(1), 2);
		assert.deepEqual(res2.hash.get(3), 4);
		assert.deepEqual(res2.hash.get(5), 6);

		var str1 = types.string('a');
		var str2 = types.string('a');
		state.pushControl(makeApplication(makePrimval('make-hasheq'),
						  [makeConstant(types.list(
								  [types.pair(str1, 1),
								   types.pair(str2, 2)]))]));
		var res3 = run(state);
		assert.ok( !res3.hash.containsKey(types.string('a')) );
		assert.deepEqual(res3.hash.get(str1), 1);
		assert.deepEqual(res3.hash.get(str2), 2);
	});


runTest('hash-set!',
	function() {
		var testHash = types.hash(types.list([types.pair(1, 1), types.pair(2, 3)]));
		
//		sys.print('\ntestHash = ' + sys.inspect(testHash) + "\n");
//		sys.print('testHash.hash = ' + sys.inspect(testHash.hash) + '\n');

		assert.deepEqual(testHash.hash.get(1), 1);
		assert.deepEqual(testHash.hash.containsKey(5), false);

		var state = new StateModule.State();
		state.pushControl(makeApplication(makePrimval('hash-set!'),
						  [makeConstant(testHash), makeConstant(5), makeConstant(8)]));
		var result = run(state);
		assert.deepEqual(result, types.VOID);
		assert.deepEqual(testHash.hash.get(5), 8);

		state.pushControl(makeApplication(makePrimval('hash-set!'),
						  [makeConstant(testHash), makeConstant(1), makeConstant(0)]));
		assert.deepEqual(run(state), types.VOID);
		assert.deepEqual(testHash.hash.get(1), 0);
	});


runTest('hash-ref',
	function() {
		var hash1 = types.hash(types.list([types.pair(1, 2),
						       types.pair(types.string('hello'),
								    types.string('world')),
						       types.pair(types.string('hello'),
								    types.string('world2'))]));

		testPrim('hash-ref', id, [hash1, types.string('hello')], types.string('world2'));
		testPrim('hash-ref', id, [hash1, 1, false], 2);
		testPrim('hash-ref', id, [hash1, 2, false], false);

		var str1 = types.string('hello');
		var str2 = str1.copy();
		var hash2 = types.hashEq(types.list([types.pair(str1, types.string('world')),
							 types.pair(str2, types.string('world2')),
							 types.pair(1, 2),
							 types.pair(3, 4)]));
		testPrim('hash-ref', id, [hash2, types.string('hello'), false], false);
		testPrim('hash-ref', id, [hash2, str1], types.string('world'));
		testPrim('hash-ref', id, [hash2, types.string('a'), 2], 2);

		var state = new StateModule.State();
		state.pushControl(makeApplication(makePrimval('hash-ref'),
						  [makeConstant(hash1),
						   makeConstant(2),
						   makeLam(0, [], makeConstant(15))]));
		assert.deepEqual(run(state), 15);

		state.pushControl(makeApplication(makePrimval('hash-ref'),
						  [makeConstant(hash2),
						   makeConstant(types.string('hello')),
						   makeLam(0, [], makeConstant(true))]));
		assert.deepEqual(run(state), true);
	});


runTest('hash-remove!',
	function() {
		var hash1 = types.hash(types.list([types.pair(1, 2),
						       types.pair(2, 3),
						       types.pair(3, 4),
						       types.pair(4, 5)]));
		assert.ok(hash1.hash.containsKey(1));
		testPrim('hash-remove!', id, [hash1, 1], types.VOID);
		assert.ok( !hash1.hash.containsKey(1) );

		var str1 = types.string('a');
		var str2 = types.string('b');
		var hash2 = types.hashEq(types.list([types.pair(str1, 5),
							 types.pair(str2, 3)]));
		testPrim('hash-remove!', id, [hash2, types.string('a')], types.VOID);
		assert.ok(hash2.hash.containsKey(str1));
		testPrim('hash-remove!', id, [hash2, str2], types.VOID);
		assert.ok( !hash2.hash.containsKey(str2) );
	});


runTest('hash-map',
	function() {
		var str1 = types.string('hello');
		var str2 = str1.copy();
		var str3 = str1.copy();
		var hash1 = types.hash(types.list([types.pair(str1, types.string('a')),
						       types.pair(str2, types.string('b')),
						       types.pair(str3, types.string('c'))]));

		var state = new StateModule.State();
		state.pushControl(makeApplication(makePrimval('hash-map'),
						  [makeConstant(hash1), makePrimval('string-append')]));
		assert.ok( hash1.hash.containsKey(types.string('hello')) );
		assert.deepEqual(run(state), types.list([types.string('helloc')]));

		var hash2 = types.hashEq(types.list([types.pair(str1, types.string('a')),
							 types.pair(str2, types.string('b')),
							 types.pair(str3, types.string('c'))]));

		var state = new StateModule.State();
		state.pushControl(makeApplication(makePrimval('hash-map'),
						  [makeConstant(hash2), makePrimval('string-append')]));
		assert.deepEqual(run(state), types.list([types.string('helloc'),
							   types.string('hellob'),
							   types.string('helloa')]));
	});


runTest('hash-for-each',
	function() {
		var hash1 = types.hash(types.list([types.pair(1, 2),
						       types.pair(2, 3),
						       types.pair(3, 4),
						       types.pair(4, 5)]));
		var state = new StateModule.State();
		var ret = [];
		state.pushControl(makeApplication(makePrimval('hash-for-each'),
						  [makeConstant(hash1),
						   makeConstant(new types.PrimProc('', 2, false, false,
								function(key, val) {
								  	ret.push( helpers.format('~s - ~s!~n', [key, val]) );
								}))]));
		assert.deepEqual(run(state), types.VOID);
		assert.deepEqual(ret, ['1 - 2!\n', '2 - 3!\n', '3 - 4!\n', '4 - 5!\n']);
	});





/******************************
 *** Vector Primitive Tests ***
 ******************************/


runTest('vector?',
	function() {
		testPrim('vector?', id, [1], false);
		testPrim('vector?', types.list, [[1, 2, 3]], false);
		testPrim('vector?', types.vector, [[1, 2, 3]], true);
	});


runTest('make-vector',
	function() {
		testPrim('make-vector', id, [0, types.char('a')], types.vector([]));
		testPrim('make-vector', id, [3, 5], types.vector([5, 5, 5]));
	});


runTest('vector',
	function() {
		testPrim('vector', id, [1, 2, 3, 4], types.vector([1, 2, 3, 4]));
		testPrim('vector', id, [], types.vector([]));
	});


runTest('vector-length',
	function() {
		testPrim('vector-length', types.vector, [[]], 0);
		testPrim('vector-length', types.vector, [[1, 2, 3]], 3);
	});


runTest('vector-ref',
	function() {
		testPrim('vector-ref', id, [types.vector([1, 2]), 1], 2);
		testPrim('vector-ref', id, [types.vector([3, 2, 1]), 0], 3);
	});


runTest('vector-set!',
	function() {
		testPrim('vector-set!', id, [types.vector([1, 2, 3]), 0, types.char('a')], types.VOID);

		var testVec = types.vector([1, 2, 3, 4]);
		var state = new StateModule.State();
		state.pushControl(makeApplication(makePrimval('vector-set!'),
						  [makeConstant(testVec),
						   makeConstant(2),
						   makeConstant(5)]));
		var result = run(state);
		assert.deepEqual(result, types.VOID);
		assert.deepEqual(testVec, types.vector([1, 2, 5, 4]));

		var testVec2 = types.vector([types.char('a'),
					       types.char('b'),
					       types.char('c')]);
		state.pushControl(makeApplication(makePrimval('vector-set!'),
						  [makeConstant(testVec2),
						   makeConstant(1),
						   makeConstant(types.char('B'))]));
		run(state);
		assert.deepEqual(testVec2, types.vector([types.char('a'),
							   types.char('B'),
							   types.char('c')]));
	});


runTest('vector->list',
	function() {
		testPrim('vector->list', types.vector, [[]], types.EMPTY);
		testPrim('vector->list', types.vector, [[1, 2, 3]], types.list([1, 2, 3]));
	});



/****************************
 *** Char Primitive Tests ***
 ****************************/




runTest('char?',
	function() {
		testPrim('char?', id, [types.symbol('hello!')], false);
		testPrim('char?', types.string, ['string'], false);
		testPrim('char?', types.char, ['w'], true);
	});


runTest('char=?',
	function() {
		testPrim('char=?', types.char, ['a', 's', 'D'], false);
		testPrim('char=?', types.char, ['f', 'F'], false);
		testPrim('char=?', types.char, ['a', 'a', 'a'], true);
		testPrim('char=?', types.char, ['1', '1', '2'], false);
	});

runTest('char-ci=?',
	function() {
		testPrim('char-ci=?', types.char, ['a', 's', 'D'], false);
		testPrim('char-ci=?', types.char, ['f', 'F'], true);
		testPrim('char-ci=?', types.char, ['a', 'a', 'a'], true);
		testPrim('char-ci=?', types.char, ['1', '1', '2'], false);
	});

runTest('char<?',
	function() {
		testPrim('char<?', types.char, ['A', 'a'], true);
		testPrim('char<?', types.char, ['a', 'b'], true);
		testPrim('char<?', types.char, ['b', 'a'], false);
		testPrim('char<?', types.char, ['a', 'd', 'c'], false);
		testPrim('char<?', types.char, ['a', 'b', 'b', 'd'], false);
		testPrim('char<?', types.char, ['a', 'b', 'c', 'd', 'e'], true);
	});

runTest('char>?',
	function() {
		testPrim('char>?', types.char, ['A', 'a'], false);
		testPrim('char>?', types.char, ['a', 'b'], false);
		testPrim('char>?', types.char, ['b', 'a'], true);
		testPrim('char>?', types.char, ['f', 'd', 'e'], false);
		testPrim('char>?', types.char, ['e', 'd', 'c', 'c', 'a'], false);
		testPrim('char>?', types.char, ['e', 'd', 'c', 'b', 'a'], true);
	});

runTest('char<=?',
	function() {
		testPrim('char<=?', types.char, ['A', 'a'], true);
		testPrim('char<=?', types.char, ['a', 'b'], true);
		testPrim('char<=?', types.char, ['b', 'a'], false);
		testPrim('char<=?', types.char, ['a', 'd', 'c'], false);
		testPrim('char<=?', types.char, ['a', 'b', 'b', 'd'], true);
		testPrim('char<=?', types.char, ['a', 'b', 'c', 'd', 'e'], true);
	});

runTest('char>=?',
	function() {
		testPrim('char>=?', types.char, ['A', 'a'], false);
		testPrim('char>=?', types.char, ['a', 'b'], false);
		testPrim('char>=?', types.char, ['b', 'a'], true);
		testPrim('char>=?', types.char, ['f', 'd', 'e'], false);
		testPrim('char>=?', types.char, ['e', 'd', 'c', 'c', 'a'], true);
		testPrim('char>=?', types.char, ['e', 'd', 'c', 'b', 'a'], true);
	});

runTest('char-ci<?',
	function() {
		testPrim('char-ci<?', types.char, ['A', 'a'], false);
		testPrim('char-ci<?', types.char, ['a', 'b'], true);
		testPrim('char-ci<?', types.char, ['b', 'A'], false);
		testPrim('char-ci<?', types.char, ['a', 'd', 'c'], false);
		testPrim('char-ci<?', types.char, ['a', 'b', 'b', 'd'], false);
		testPrim('char-ci<?', types.char, ['a', 'B', 'c', 'd', 'e'], true);
	});

runTest('char-ci>?',
	function() {
		testPrim('char-ci>?', types.char, ['a', 'A'], false);
		testPrim('char-ci>?', types.char, ['a', 'b'], false);
		testPrim('char-ci>?', types.char, ['b', 'A'], true);
		testPrim('char-ci>?', types.char, ['f', 'd', 'e'], false);
		testPrim('char-ci>?', types.char, ['e', 'd', 'c', 'c', 'a'], false);
		testPrim('char-ci>?', types.char, ['e', 'd', 'C', 'b', 'a'], true);
	});

runTest('char-ci<=?',
	function() {
		testPrim('char-ci<=?', types.char, ['a', 'A'], true);
		testPrim('char-ci<=?', types.char, ['a', 'B'], true);
		testPrim('char-ci<=?', types.char, ['b', 'a'], false);
		testPrim('char-ci<=?', types.char, ['a', 'd', 'c'], false);
		testPrim('char-ci<=?', types.char, ['a', 'b', 'B', 'd'], true);
		testPrim('char-ci<=?', types.char, ['a', 'b', 'C', 'd', 'e'], true);
	});

runTest('char-ci>=?',
	function() {
		testPrim('char-ci>=?', types.char, ['A', 'a'], true);
		testPrim('char-ci>=?', types.char, ['a', 'b'], false);
		testPrim('char-ci>=?', types.char, ['B', 'a'], true);
		testPrim('char-ci>=?', types.char, ['f', 'd', 'e'], false);
		testPrim('char-ci>=?', types.char, ['e', 'd', 'C', 'c', 'a'], true);
		testPrim('char-ci>=?', types.char, ['e', 'd', 'c', 'B', 'a'], true);
	});


runTest('char-alphabetic?',
	function() {
		testPrim('char-alphabetic?', types.char, ['a'], true);
		testPrim('char-alphabetic?', types.char, ['Z'], true);
		testPrim('char-alphabetic?', types.char, ['3'], false);
		testPrim('char-alphabetic?', types.char, [' '], false);
		testPrim('char-alphabetic?', types.char, ['!'], false);
		testPrim('char-alphabetic?', types.char, ['\n'], false);
	});


runTest('char-numeric?',
	function() {
		testPrim('char-numeric?', types.char, ['a'], false);
		testPrim('char-numeric?', types.char, ['Z'], false);
		testPrim('char-numeric?', types.char, ['3'], true);
		testPrim('char-numeric?', types.char, [' '], false);
		testPrim('char-numeric?', types.char, ['!'], false);
		testPrim('char-numeric?', types.char, ['\n'], false);
	});


runTest('char-whitespace?',
	function() {
		testPrim('char-whitespace?', types.char, ['a'], false);
		testPrim('char-whitespace?', types.char, ['Z'], false);
		testPrim('char-whitespace?', types.char, ['3'], false);
		testPrim('char-whitespace?', types.char, [' '], true);
		testPrim('char-whitespace?', types.char, ['!'], false);
		testPrim('char-whitespace?', types.char, ['\n'], true);
		testPrim('char-whitespace?', types.char, ['\t'], true);
	});


runTest('char-upper-case?',
	function() {
		testPrim('char-upper-case?', types.char, ['a'], false);
		testPrim('char-upper-case?', types.char, ['Z'], true);
		testPrim('char-upper-case?', types.char, ['3'], false);
		testPrim('char-upper-case?', types.char, [' '], false);
		testPrim('char-upper-case?', types.char, ['!'], false);
		testPrim('char-upper-case?', types.char, ['\n'], false);
	});


runTest('char-lower-case?',
	function() {
		testPrim('char-lower-case?', types.char, ['a'], true);
		testPrim('char-lower-case?', types.char, ['Z'], false);
		testPrim('char-lower-case?', types.char, ['3'], false);
		testPrim('char-lower-case?', types.char, [' '], false);
		testPrim('char-lower-case?', types.char, ['!'], false);
		testPrim('char-lower-case?', types.char, ['\n'], false);
	});


runTest('char->integer',
	function() {
		testPrim('char->integer', types.char, ['0'], 48);
		testPrim('char->integer', types.char, ['\n'], 10);
	});


runTest('integer->char',
	function() {
		testPrim('integer->char', id, [48], types.char('0'));
		testPrim('integer->char', id, [65], types.char('A'));
	});


runTest('char-upcase',
	function() {
		testPrim('char-upcase', types.char, ['a'], types.char('A'));
		testPrim('char-upcase', types.char, ['B'], types.char('B'));
		testPrim('char-upcase', types.char, ['2'], types.char('2'));
		testPrim('char-upcase', types.char, ['~'], types.char('~'));
	});


runTest('char-downcase',
	function() {
		testPrim('char-downcase', types.char, ['a'], types.char('a'));
		testPrim('char-downcase', types.char, ['B'], types.char('b'));
		testPrim('char-downcase', types.char, ['2'], types.char('2'));
		testPrim('char-downcase', types.char, ['~'], types.char('~'));
	});


runTest('char print formatting',
	function() {
		testPrim('format', id, ['~s', types.char('\n')], types.string('#\\newline'));
		testPrim('format', id, ['~s', types.char('\0')], types.string('#\\nul'));
		testPrim('format', id, ['~a', types.char('b')], types.string('b'));
		testPrim('format', id, ['~s', types.char('b')], types.string('#\\b'));

		var state = new StateModule.State();
		state.pushControl(makeApplication(makePrimval('format'),
						  [makeConstant('~s'),
						   makeApplication(makePrimval('integer->char'),
								   [makeConstant(24)])]));
		assert.deepEqual(run(state), types.string('#\\u0018'));

		state.pushControl(makeApplication(makePrimval('format'),
						  [makeConstant('~s'),
						   makeApplication(makePrimval('integer->char'),
								   [makeConstant(127)])]));
		assert.deepEqual(run(state), types.string('#\\rubout'));

		state.pushControl(makeApplication(makePrimval('format'),
						  [makeConstant('~s'),
						   makeApplication(makePrimval('integer->char'),
								   [makeConstant(955)])]));
		assert.deepEqual(run(state), types.string('#\\u03BB'));
	});


///////////////////////////////////////////////////////////////////////


runTest('values',
	function() {
		testPrim('values', id, [], new types.ValuesWrapper([]));
		testPrim('values', id, [1, 2, 3, 4], new types.ValuesWrapper([1, 2, 3, 4]));
		testPrim('values', id, [1], 1);
	});

runTest('call-with-values',
	function() {
		var state = new StateModule.State();
		state.pushControl(makeApplication(makePrimval('call-with-values'),
						  [makePrimval('values'),
						   makePrimval('+')]));
		assert.deepEqual(run(state), 0);

		state.pushControl(makeApplication(makePrimval('call-with-values'),
						  [makeLam(0, [], makeConstant(1)),
						   makePrimval('+')]));
		assert.deepEqual(run(state), 1);

		state.pushControl(makeApplication(makePrimval('call-with-values'),
						  [makeLam(0, [], makeApplication(makePrimval('values'),
								  		  [makeConstant(1),
										   makeConstant(2),
										   makeConstant(3)])),
						   makePrimval('+')]));
		assert.deepEqual(run(state), 6);
	});


runTest('not',
	function() {
		testPrim('not', id, [false], true);
		testPrim('not', id, [0], false);
		testPrim('not', id, [1], false);
		testPrim('not', types.char, ['0'], false);
	});


runTest('boolean?',
	function() {
		testPrim('boolean?', id, [false], true);
		testPrim('boolean?', id, [true], true);
		testPrim('boolean?', types.string, ['false'], false);
		testPrim('boolean?', id, [0], false);
		testPrim('boolean?', id, [1], false);
	});


runTest('eq?',
	function() {
		var testStr = types.string('hello');
		var testChar = types.char('H');
		testPrim('eq?', id, [1, 1], true);
		testPrim('eq?', id, [1, 2], false);
		testPrim('eq?', id, [types.rational(1, 3), types.rational(1, 3)], false);
		testPrim('eq?', types.symbol, ['a', 'a'], true);
		testPrim('eq?', types.string, ['a', 'a'], false);
		testPrim('eq?', id, [testStr, testStr], true);
		testPrim('eq?', id, [testChar, testChar], true);
		testPrim('eq?', id, [testChar, types.char('H')], true);
	});


runTest('eqv?',
	function() {
		var testStr = types.string('hello');
		var testChar = types.char('H');
		testPrim('eqv?', id, [1, 1], true);
		testPrim('eqv?', id, [1, 2], false);
		testPrim('eqv?', id, [types.rational(1, 3), types.rational(1, 3)], true);
		testPrim('eqv?', types.symbol, ['a', 'a'], true);
		testPrim('eqv?', types.string, ['a', 'a'], false);
		testPrim('eqv?', id, [testStr, testStr], true);
		testPrim('eqv?', id, [testChar, testChar], true);
		testPrim('eqv?', id, [testChar, types.char('H')], true);
	});


runTest('equal?',
	function() {
		var testStr = types.string('hello');
		var testChar = types.char('H');
		testPrim('equal?', id, [1, 1], true);
		testPrim('equal?', id, [1, 2], false);
		testPrim('equal?', id, [types.rational(1, 3), types.rational(1, 3)], true);
		testPrim('equal?', types.symbol, ['a', 'a'], true);
		testPrim('equal?', types.string, ['a', 'a'], true);
		testPrim('equal?', id, [testStr, testStr], true);
		testPrim('equal?', id, [testChar, testChar], true);
		testPrim('equal?', id, [testChar, types.char('H')], true);
	});


runTest('equal~?',
	function() {
		testPrim('equal~?', id, [types.string('h'), types.string('h'), 5], true);
		testPrim('equal~?', id, [5, 4, 0], false);
		testPrim('equal~?', id, [types.char('a'), types.char('b'), 3], false);
		testPrim('equal~?', id, [5, 3, 3], true);
		testPrim('equal~?', types.float, [5.4, 4.9, 0.5], true);
	});


runTest('struct?',
	function() {
		testPrim('struct?', types.string, ['a'], false);
		testPrim('struct?', id, [1], false);
		testPrim('struct?', id, [types.EMPTY], false);
		testPrim('struct?', types.box, [2], false);

	    var PosnType = types.makeStructureType(
		'posn', false, 2, 0, false, false);
	    testPrim('struct?', id, [PosnType.constructor(2, 4)], true);
	});


runTest('procedure-arity',
	function() {
		var state = new StateModule.State();
		state.pushControl(makeApplication(makePrimval('procedure-arity'), [makePrimval('+')]));
		assert.deepEqual(run(state), types.arityAtLeast(0));

		state.pushControl(makeApplication(makePrimval('procedure-arity'), [makePrimval('-')]));
		assert.deepEqual(run(state), types.arityAtLeast(1));

		state.pushControl(makeApplication(makePrimval('procedure-arity'), [makePrimval('equal?')]));
		assert.deepEqual(run(state), 2);

		state.pushControl(makeApplication(makePrimval('procedure-arity'), [makePrimval('random')]));
		assert.deepEqual(run(state), types.list([0, 1]));

		state.pushControl(makeApplication(makePrimval('procedure-arity'), [makePrimval('hash-ref')]));
		assert.deepEqual(run(state), types.list([2, 3]));

		var testProc = new types.CaseLambdaValue('',
			[new types.PrimProc('', 1, false, false, function() {}),
			 new types.PrimProc('', 2, true, false, function() {})]);
		state.pushControl(makeApplication(makePrimval('procedure-arity'), [makeConstant(testProc)]));
		assert.deepEqual(run(state), types.list([1, types.arityAtLeast(2)]));

		var testProc2 = new types.CaseLambdaValue('',
			[new types.PrimProc('', 1, false, false, function() {}),
			 new types.PrimProc('', 0, true, false, function() {})]);
		state.pushControl(makeApplication(makePrimval('procedure-arity'), [makeConstant(testProc2)]));
		assert.deepEqual(run(state), types.arityAtLeast(0));

		var testProc3 = new types.CaseLambdaValue('',
			[new types.PrimProc('', 1, false, false, function() {}),
			 new types.PrimProc('', 4, true, false, function() {}),
			 new types.PrimProc('', 0, false, false, function() {}),
			 new types.PrimProc('', 3, true, false, function() {}),
			 new types.PrimProc('', 3, false, false, function() {})]);
		state.pushControl(makeApplication(makePrimval('procedure-arity'), [makeConstant(testProc3)]));
		assert.deepEqual(run(state), types.list([0, 1, types.arityAtLeast(3)]));
	});


runTest('identity',
	function() {
		testPrim('identity', id, [5], 5);
		testPrim('identity', types.string, ['hello'], types.string('hello'));
	});


// runTest('make-posn',
// 	function() {
// 		testPrim('make-posn', id, [4, 5], types.posn(4, 5));
// 		testPrim('make-posn', types.char, ['a', 'B'], types.posn(types.char('a'), types.char('B')));
// 	});

// runTest('posn?',
// 	function() {
// 		testPrim('posn?', id, [4], false);
// 		testPrim('posn?', types.box, [4], false);
// 		testPrim('posn?', id, [types.posn(5, 4)], true);
// 	});

// runTest('posn-x',
// 	function() {
// 		testPrim('posn-x', id, [types.posn(5, 4)], 5);
// 		testPrim('posn-x', id, [types.posn(types.char('a'), types.char('b'))], types.char('a'));
// 	});

// runTest('posn-y',
// 	function() {	
// 		testPrim('posn-y', id, [types.posn(5, 4)], 4);
// 		testPrim('posn-y', id, [types.posn(types.char('a'), types.char('b'))], types.char('b'));
// 	});


runTest('structure equality',
	function() {
		var ParentType = types.makeStructureType('parent', false, 2, 0, false, false);
		var makeParent = ParentType.constructor;
		var ChildType = types.makeStructureType('child', ParentType, 0, 0, false, false);
		var makeChild = ChildType.constructor;

		testPrim('equal?', id, [makeParent('a', 5), makeParent('a', 5)], true);
		testPrim('equal?', id, [makeParent('a', 5), makeParent('b', 5)], false);
		testPrim('equal?', id, [makeParent('a', 5), makeChild('a', 5)], false);
		testPrim('equal?', id, [makeChild('a', 5), makeParent('a', 5)], false);
		testPrim('equal?', id, [makeParent('a', 5), types.color(4, 3, 6)], false);
	});


/***************************
 *** FFI Primitive Tests ***
 ***************************/


/*
runTest('get-js-object',
	function() {
		testPrim('get-js-object', id, ['setInterval'], types.jsObject('setInterval', setInterval));
		testPrim('get-js-object', id, [types.jsObject('types', types), 'box'],
			 types.jsObject('types.box', types.box));
		testPrim('get-js-object', types.string, ['types', 'cons'], types.jsObject('types.cons', types.cons));
		testPrim('get-js-object', id, ['world', types.string('Kernel'), 'ellipseImage'],
			 types.jsObject('world.Kernel.ellipseImage', world.Kernel.ellipseImage));
		testPrim('get-js-object', id, [types.jsObject('world', world), 'Kernel', 'isColor'],
			 types.jsObject('world.Kernel.isColor', world.Kernel.isColor));
		testPrim('get-js-object', id, [types.jsObject('world.config', world.config), 'Kernel', 'getNoneEffect'],
			 types.jsObject('world.config.Kernel.getNoneEffect', world.config.Kernel.getNoneEffect));
		testPrim('get-js-object', id, ['junk'], types.jsObject('junk', undefined));

		try {
			testPrim('get-js-object', id, ['world', 'junk', 'something'], false);
		} catch(e) {
			assert.deepEqual(e, types.schemeError(
				types.exnFailContract('get-js-object: tried to access field something of world.junk, '
					+ 'but world.junk was undefined'),
				false));
		}
	});


runTest('js-call',
	function() {
		testPrim('js-call', id, [types.jsObject('jsnums.greaterThan', jsnums.greaterThan), 4, types.rational(3, 2)], true);
		testPrim('js-call', id, [types.jsObject('types.hash', types.hash), types.EMPTY], types.hash(types.EMPTY));

		var state = new StateModule.State();
		var results = [];
		state.pushControl(makeApplication(makePrimval('js-call'),
						  [makeConstant(types.jsObject('setInterval', setInterval)),
						   makeConstant(function() { results.push('tick'); }),
						   makeConstant(500)]));
		var watchId = run(state);
		setTimeout(function() {
			clearInterval(watchId);
			assert.deepEqual(results, ['tick', 'tick', 'tick', 'tick', 'tick']);
		}, 2600);
	});
*/
		





runTest("topsyntax",
	function() {
	    sys.print("!Not implemented yet!  ");
	});




runTest("Error structure hierarchy",
	function() {
	    assert.ok(types.isExnFail(types.exnFail("hello", types.continuationMarkSet())));
	    assert.ok(types.isExnFail(types.exnFailContract("hello", types.continuationMarkSet())));
	    assert.ok(types.isExnFail(types.exnFailContractDivisionByZero("hello", types.continuationMarkSet())));
	});










/**
This next test is special and should be last.  It'll run an infinite loop, and
schedule a break.

Only after the interpreter breaks do we print "END TESTS".
*/
runTest("closure application, testing break",
	// (define (f) (f)) (begin (f)) --> infinite loop, but with bounded control stack.
	function() {
	    var state = new StateModule.State();
	    state.pushControl(makeMod(makePrefix(1), []));
	    run(state);   
	    state.pushControl(makeApplication(makeToplevel(0, 0), []));
	    state.pushControl(makeDefValues([makeToplevel(0, 0)],
					    makeLam(0, [0],
						    makeApplication(makeToplevel(0, 0),
								    []))));
	    var isTerminated = false;
	    state.onFail = function(e) {
	    	assert.ok(types.isSchemeError(e));
		assert.ok(types.isExnBreak(e.val));
		isTerminated = true;
	    };
	    interpret.run(state);
	    var waitTillBreak = function() {
		if (isTerminated) {
		    sys.print("\nEND TESTS\n")
		    return;
		} else {
		    state.breakRequested = true;
		    setTimeout(waitTillBreak, 10);
		}
	    };
	    waitTillBreak();
	});
