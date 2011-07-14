// Procedures

// For historical reasons, this module is called 'functions' instead of 'procedures'.
// This may change soon.

(function(baselib) {
    var exports = {};
    baselib.functions = exports;
    
    // Procedure types: a procedure is either a Primitive or a Closure.

    // A Primitive is a function that's expected to return.  It is not
    // allowed to call into Closures.  Its caller is expected to pop off
    // its argument stack space.
    //




    
    // coerseToJavaScript: racket function -> JavaScript function
    // Given a closure or primitive, produces an
    // asynchronous JavaScript function.
    // The function will run on the provided MACHINE.
    //
    // It assumes that it must begin its own trampoline.
    var asJavaScriptFunction = function(v, MACHINE) {
        MACHINE = MACHINE || plt.runtime.currentMachine;
        if (isPrimitiveProcedure(v)) {
            return coersePrimitiveToJavaScript(v, MACHINE);
        } else if (isClosure(v)) {
            return coerseClosureToJavaScript(v, MACHINE);
        } else {
            plt.baselib.exceptions.raise(MACHINE,
                                         plt.baselib.exceptions.makeExnFail(
                                             plt.baselib.format.format(
                                                 "Not a procedure: ~e",
                                                 v)));
        }
    };

    var coersePrimitiveToJavaScript = function(v, MACHINE) {
        return function(succ, fail) {
            try {
                succ = succ || function(){};
                fail = fail || function(){};
                var args = [];
                for (var i = 2; i < arguments.length; i++) {
                    args.push(arguments[i]);
                }

		// Check arity usage.
		if (! plt.baselib.arity.isArityMatching(v.arity, args.length)) {
		    throw new Error("arity mismatch");
		}

                var result = v.apply(null, args);
                succ(result);
            } catch (e) {
                fail(e);
            }
        }
    };


    var coerseClosureToJavaScript = function(v, MACHINE) {
        var f = function(succ, fail) {
            succ = succ || function(){};
            fail = fail || function(){};

            var args = [], i;
            for (i = 2; i < arguments.length; i++) {
                args.push(arguments[i]);
            }

	    // Check arity usage.
	    if (! plt.baselib.arity.isArityMatching(v.arity, args.length)) {
		throw new Error("arity mismatch");
	    }


            var oldVal = MACHINE.val;
            var oldArgcount = MACHINE.argcount;
            var oldProc = MACHINE.proc;

            var oldErrorHandler = MACHINE.params['currentErrorHandler'];
            var afterGoodInvoke = function(MACHINE) { 
		plt.runtime.PAUSE(
		    function(restart) {
			MACHINE.params['currentErrorHandler'] = oldErrorHandler;
			var returnValue = MACHINE.val;
			MACHINE.val = oldVal;
			MACHINE.argcount = oldArgcount;
			MACHINE.proc = oldProc;
			succ(returnValue);
		    });
            };
            afterGoodInvoke.multipleValueReturn = function(MACHINE) {
		plt.runtime.PAUSE(
		    function(restart) {
			MACHINE.params['currentErrorHandler'] = oldErrorHandler;
			var returnValues = [MACHINE.val];
			for (var i = 0; i < MACHINE.argcount - 1; i++) {
			    returnValues.push(MACHINE.env.pop());
			}
			MACHINE.val = oldVal;
			MACHINE.argcount = oldArgcount;
			MACHINE.proc = oldProc;
			succ.apply(null, returnValues);
		    });
            };

            setTimeout(
                function() {
                    MACHINE.control.push(
                        new plt.baselib.frames.CallFrame(afterGoodInvoke, null));
                    MACHINE.argcount = args.length;
                    for (var i = 0; i < args.length; i++) {
                        MACHINE.env.push(args[i]);
                    }
                    MACHINE.proc = v;
                    MACHINE.params['currentErrorHandler'] = function(MACHINE, e) {
                        MACHINE.params['currentErrorHandler'] = oldErrorHandler;
                        MACHINE.val = oldVal;
                        MACHINE.argcount = oldArgcount;
                        MACHINE.proc = oldProc;
                        fail(e);
                    };
                    plt.runtime.trampoline(MACHINE, v.label);
                },
                0);
        };
        return f;
    };



    // internallCallDuringPause: call a Racket procedure and get its results.
    // The use assumes the machine is in a running-but-paused state.
    var internalCallDuringPause = function(MACHINE, proc, success, fail) {
	if (! plt.baselib.arity.isArityMatching(proc.arity, args.length)) {
	    return fail(plt.baselib.exceptions.makeExnFailContractArity("arity mismatch"));
	}

        if (isPrimitiveProcedure(proc)) {
	    var args = [];
            for (var i = 4; i < arguments.length; i++) {
                args.push(arguments[i]);
            }
            var result = v.apply(null, args);
            succ(result);
        } else if (isClosure(v)) {

            var oldVal = MACHINE.val;
            var oldArgcount = MACHINE.argcount;
            var oldProc = MACHINE.proc;

            var oldErrorHandler = MACHINE.params['currentErrorHandler'];
            var afterGoodInvoke = function(MACHINE) { 
		plt.runtime.PAUSE(function(restart) {
                    MACHINE.params['currentErrorHandler'] = oldErrorHandler;
                    var returnValue = MACHINE.val;
                    MACHINE.val = oldVal;
                    MACHINE.argcount = oldArgcount;
                    MACHINE.proc = oldProc;
                    succ(returnValue);
		});
            };
            afterGoodInvoke.multipleValueReturn = function(MACHINE) {
		plt.runtime.PAUSE(function(restart) {
		    MACHINE.params['currentErrorHandler'] = oldErrorHandler;
                    var returnValues = [MACHINE.val];
                    for (var i = 0; i < MACHINE.argcount - 1; i++) {
			returnValues.push(MACHINE.env.pop());
                    }
                    MACHINE.val = oldVal;
                    MACHINE.argcount = oldArgcount;
                    MACHINE.proc = oldProc;
                    succ.apply(null, returnValues);
		});
            };

            MACHINE.control.push(
                new plt.baselib.frames.CallFrame(afterGoodInvoke, null));
            MACHINE.argcount = args.length;
            for (var i = 0; i < args.length; i++) {
                MACHINE.env.push(args[i]);
            }
            MACHINE.proc = v;
            MACHINE.params['currentErrorHandler'] = function(MACHINE, e) {
                MACHINE.params['currentErrorHandler'] = oldErrorHandler;
                MACHINE.val = oldVal;
                MACHINE.argcount = oldArgcount;
                MACHINE.proc = oldProc;
                fail(e);
            };
            plt.runtime.trampoline(MACHINE, v.label);
        } else {
            fail(plt.baselib.exceptions.makeExnFail(
                plt.baselib.format.format(
                    "Not a procedure: ~e",
                    v)));
        }
    };







    // A Closure is a function that takes on more responsibilities: it is
    // responsible for popping off stack space before it finishes, and it
    // is also explicitly responsible for continuing the computation by 
    // popping off the control stack and doing the jump.  Because of this,
    // closures can do pretty much anything to the machine.

    // A closure consists of its free variables as well as a label
    // into its text segment.
    var Closure = function(label, arity, closedVals, displayName) {
	this.label = label;              // (MACHINE -> void)
	this.arity = arity;              // number
	this.closedVals = closedVals;    // arrayof number
	this.displayName = displayName;  // string
    };



    // Finalize the return from a closure.  This is a helper function
    // for those who implement Closures by hand.
    //
    // If used in the body of a Closure, it must be in tail
    // position.  This finishes the closure call, and does the following:
    //
    //     * Clears out the existing arguments off the stack frame
    //     * Sets up the return value
    //     * Jumps either to the single-value return point, or the multiple-value
    //       return point.
    //
    // I'd personally love for this to be a macro and avoid the
    // extra function call here.
    var finalizeClosureCall = function(MACHINE) {
        MACHINE.callsBeforeTrampoline--;
        var frame, i, returnArgs = [].slice.call(arguments, 1);

        // clear out stack space
        // TODO: replace with a splice.
        for(i = 0; i < MACHINE.argcount; i++) {
            MACHINE.env.pop();
        }

        if (returnArgs.length === 1) {
            MACHINE.val = returnArgs[0];
	    frame = MACHINE.control.pop();
	    return frame.label(MACHINE);
        } else if (returnArgs.length === 0) {
            MACHINE.argcount = 0;
	    frame = MACHINE.control.pop();
	    return frame.label.multipleValueReturn(MACHINE);
        } else {
            MACHINE.argcount = returnArgs.length;
            MACHINE.val = returnArgs.shift();
            // TODO: replace with a splice.
            for(i = 0; i < MACHINE.argcount - 1; i++) {
                MACHINE.env.push(returnArgs.pop());
            }
	    frame = MACHINE.control.pop();
	    return frame.label.multipleValueReturn(MACHINE);
        }
    };






    var makePrimitiveProcedure = function(name, arity, f) {
        f.arity = arity;
        f.displayName = name;
        return f;
    };

    var makeClosure = function(name, arity, f, closureArgs) {
        if (! closureArgs) { closureArgs = []; }
        return new Closure(f,
                           arity,
                           closureArgs,
                           name);
    };




    var isPrimitiveProcedure = function(x) {
        return typeof(x) === 'function';
    };

    var isClosure = function(x) {
        return x instanceof Closure;
    };


    var isProcedure = function(x) {
        return (typeof(x) === 'function' ||
                x instanceof Closure);
    };



    var renameProcedure = function(f, name) {
        if (isPrimitiveProcedure(f)) {
            return makePrimitiveProcedure(
                name,
                f.arity,
                function() {
                    return f.apply(null, arguments);
                });
        } else {
            return new Closure(
                f.label,
                f.arity,
                f.closedVals,
                name);
        }
    };





    //////////////////////////////////////////////////////////////////////
    exports.Closure = Closure;
    exports.internalCallDuringPause = internalCallDuringPause;
    exports.finalizeClosureCall = finalizeClosureCall;

    exports.makePrimitiveProcedure = makePrimitiveProcedure;
    exports.makeClosure = makeClosure;

    exports.isPrimitiveProcedure = isPrimitiveProcedure;
    exports.isClosure = isClosure;

    exports.isProcedure = isProcedure;


    exports.renameProcedure = renameProcedure;


    exports.asJavaScriptFunction = asJavaScriptFunction;

})(this['plt'].baselib);