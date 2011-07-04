// Equality function
(function(baselib) {
    var exports = {};
    baselib.equality = exports;


    // equals: X Y -> boolean
    // Returns true if the objects are equivalent; otherwise, returns false.
    var equals = function(x, y, aUnionFind) {
        if (x === y) { return true; }

        if (jsnums.isSchemeNumber(x) && jsnums.isSchemeNumber(y)) {
	    return jsnums.eqv(x, y);
        }

        if (baselib.strings.isString(x) && baselib.strings.isString(y)) {
	    return x.toString() === y.toString();
        }

        if (x == undefined || x == null) {
	    return (y == undefined || y == null);
        }

        if ( typeof(x) == 'object' &&
	     typeof(y) == 'object' &&
	     x.equals &&
	     y.equals) {

	    if (typeof (aUnionFind) === 'undefined') {
	        aUnionFind = new plt.baselib.UnionFind();
	    }

	    if (aUnionFind.find(x) === aUnionFind.find(y)) {
	        return true;
	    }
	    else {
	        aUnionFind.merge(x, y); 
	        return x.equals(y, aUnionFind);
	    }
        }
        return false;
    };


    exports.equals = equals;

})(this['plt'].baselib);