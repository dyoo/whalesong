/*jslint vars: true, white: true, maxerr: 50, indent: 4 */


// Equality function
/*global jsnums*/
(function (baselib, jsnums) {
    'use strict';
    var exports = {};
    baselib.equality = exports;



    var eqv = function (x, y) {
        if (x === y) { return true; }

        if (baselib.numbers.isNumber(x) && baselib.numbers.isNumber(y)) {
            return jsnums.eqv(x, y);
        } else if (baselib.chars.isChar(x) && baselib.chars.isChar(y)) {
            return x.val === y.val;
        } else {
            return false;
        }
    };




    // equals: X Y -> boolean
    // Returns true if the objects are equivalent; otherwise, returns false.
    var equals = function (x, y, aUnionFind) {
        if (x === y) { return true; }

        if (baselib.numbers.isNumber(x) && baselib.numbers.isNumber(y)) {
            return baselib.numbers.eqv(x, y);
        }

        if (baselib.strings.isString(x) && baselib.strings.isString(y)) {
            return x.toString() === y.toString();
        }

        if (x === void(0) || x === null) {
            return (y === void(0) || y === null);
        }

        if (typeof (x) === 'object' && typeof (y) === 'object' &&
            x.equals && y.equals) {
            if (aUnionFind === void(0)) {
                aUnionFind = new baselib.UnionFind();
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

    exports.eqv = eqv;
    exports.equals = equals;

}(this.plt.baselib, jsnums));