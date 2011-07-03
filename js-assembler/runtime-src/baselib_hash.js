
(function(baselib) {
    var hash = {};

    baselib.hash = hash;


    
    var _eqHashCodeCounter = 0;
    var makeEqHashCode = function() {
	_eqHashCodeCounter++;
	return _eqHashCodeCounter;
    };


    // getHashCode: any -> (or fixnum string)
    // Given a value, produces a hashcode appropriate for eq.
    var getEqHashCode = function(x) {
	if (typeof(x) === 'string') {
	    return x;
	}
	if (typeof(x) === 'number') {
	    return String(x);
	}
	if (x && !x._eqHashCode) {
	    x._eqHashCode = makeEqHashCode();
	}
	if (x && x._eqHashCode) {
	    return x._eqHashCode;
	}
	return 0;
    };


    // Creates a low-level hashtable, following the interface of 
    // http://www.timdown.co.uk/jshashtable/
    //
    // Defined to use the getEqHashCode defined in baselib_hash.js.
    var makeLowLevelEqHash = function() {
	return new Hashtable(function(x) { return getEqHashCode(x); },
			     function(x, y) { return x === y; });
    };



    hash.getEqHashCode = getEqHashCode;
    hash.makeEqHashCode = makeEqHashCode;
    hash.makeLowLevelEqHash = makeLowLevelEqHash;



})(this['plt'].baselib);