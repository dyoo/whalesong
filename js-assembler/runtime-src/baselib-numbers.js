// Numbers.
(function(baselib) {
    var exports = {};
    baselib.numbers = exports;



    var isNumber = jsnums.isSchemeNumber;
    var isReal = jsnums.isReal;
    var isRational = jsnums.isRational;
    var isComplex = isNumber;
    var isInteger = jsnums.isInteger;


    var isNatural = function(x) {
        return (jsnums.isExact(x) && isInteger(x) 
	        && jsnums.greaterThanOrEqual(x, 0));
    };

    var isNonNegativeReal = function(x) {
	return isReal(x) && jsnums.greaterThanOrEqual(x, 0);
    };

    var isByte = function(x) {
	return (isNatural(x) && 
		jsnums.lessThan(x, 256));
    };





    //////////////////////////////////////////////////////////////////////
    // Exports


    // We first re-export everything in jsnums.
    for (var prop in jsnums) {
        if (jsnums.hasOwnProperty(prop)) {
            exports[prop] = jsnums[prop];
        }
    }

    exports.isNumber = jsnums.isSchemeNumber;
    exports.isReal = isReal;
    exports.isRational = isRational;
    exports.isComplex = isComplex;
    exports.isInteger = isInteger;
    exports.isNatural = isNatural;
    exports.isByte = isByte;
    exports.isNonNegativeReal = isNonNegativeReal;


})(this['plt'].baselib);