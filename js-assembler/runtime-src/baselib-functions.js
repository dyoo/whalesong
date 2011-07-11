// Functions
(function(baselib) {
    var exports = {};
    baselib.functions = exports;
    
    // Function types: a function is either a Primitive or a Closure.

    // A Primitive is a function that's expected to return.  It is not
    // allowed to call into Closures.  Its caller is expected to pop off
    // its argument stack space.
    //





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


    var isPrimitiveProcedure = function(x) {
        return typeof(x) === 'function';
    };

    var isClosure = function(x) {
        return x instanceof Closure;
    };


    var isFunction = function(x) {
        return (typeof(x) === 'function' ||
                x instanceof Closure);
    };




    //////////////////////////////////////////////////////////////////////
    exports.Closure = Closure;
    exports.finalizeClosureCall = finalizeClosureCall;
    exports.makePrimitiveProcedure = makePrimitiveProcedure;
    exports.isPrimitiveProcedure = isPrimitiveProcedure;
    exports.isClosure = isClosure;

    exports.isFunction = isFunction;

})(this['plt'].baselib);