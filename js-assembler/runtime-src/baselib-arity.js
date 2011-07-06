// Arity structure
(function(baselib) {
    var exports = {};
    baselib.arity = exports;



    var ArityAtLeast = plt.baselib.structs.makeStructureType(
        'arity-at-least', false, 1, 0, false, false);


    // An arity is either a primitive number, an ArityAtLeast instance,
    // or a list of either primitive numbers or ArityAtLeast instances.



    var isArityAtLeast = ArityAtLeast.predicate;
    var arityAtLeastValue = function(x) { 
        var val = ArityAtLeast.accessor(x, 0);
        return val;
    }


    ArityAtLeast.type.prototype.toString = function() {
        return '#<arity-at-least ' + arityAtLeastValue(this) + '>';
    };



    // isArityMatching: arity natural -> boolean
    // Produces true if n satisfies the arity.
    var isArityMatching = function(arity, n) {
	if (typeof(arity) === 'number') {
	    return arity === n;
	} else if (isArityAtLeast(arity)) {
	    return n >= arityAtLeastValue(arity);
	} else {
	    while (arity !== plt.baselib.lists.EMPTY) {
		if (typeof(arity.first) === 'number') {
		    if (arity.first === n) { return true; }
		} else if (isArityAtLeast(arity)) {
		    if (n >= arityAtLeastValue(arity.first)) { return true; }
		}
		arity = arity.rest;
	    }
	    return false;
	}
    }





    //////////////////////////////////////////////////////////////////////

    exports.ArityAtLeast = ArityAtLeast;
    exports.makeArityAtLeast = ArityAtLeast.constructor;
    exports.isArityAtLeast = isArityAtLeast;
    exports.isArityMatching = isArityMatching;
    exports.arityAtLeastValue = arityAtLeastValue;

})(this['plt'].baselib);