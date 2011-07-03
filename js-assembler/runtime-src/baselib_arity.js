// Arity structure
(function(baselib) {
    var exports = {};
    baselib.arity = exports;


    // An arity is either a primitive number, an ArityAtLeast instance,
    // or a list of either primitive numbers or ArityAtLeast instances.

    var ArityAtLeast = function(n) {
	this.value = n;
    };

    // isArityMatching: arity natural -> boolean
    // Produces true if n satisfies the arity.
    var isArityMatching = function(arity, n) {
	if (typeof(arity) === 'number') {
	    return arity === n;
	} else if (arity instanceof ArityAtLeast) {
	    return n >= arity.value;
	} else {
	    while (arity !== plt.types.EMPTY) {
		if (typeof(arity.first) === 'number') {
		    if (arity.first === n) { return true; }
		} else if (arity instanceof ArityAtLeast) {
		    if (n >= arity.first.value) { return true; }
		}
		arity = arity.rest;
	    }
	    return false;
	}
    }



    exports.ArityAtLeast = ArityAtLeast;
    exports.isArityMatching = isArityMatching;


})(this['plt'].baselib);