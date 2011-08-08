/*jslint unparam: true, vars: true, white: true, newcap: true, nomen: true, plusplus: true, maxerr: 50, indent: 4 */

/*global Hashtable*/


(function (baselib) {
    'use strict';
    var exports = {};

    baselib.hashes = exports;


    
    var _eqHashCodeCounter = 0;
    var makeEqHashCode = function () {
        _eqHashCodeCounter++;
        return _eqHashCodeCounter;
    };


    // getHashCode: any -> (or fixnum string)
    // Given a value, produces a hashcode appropriate for eq.
    var getEqHashCode = function (x) {
        if (typeof (x) === 'string') {
            return x;
        }
        if (typeof (x) === 'number') {
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
    var makeLowLevelEqHash = function () {
        return new Hashtable(function (x) { return getEqHashCode(x); },
                             function (x, y) { return x === y; });
    };










    //////////////////////////////////////////////////////////////////////
    // Eq Hashtables
    var EqHashTable = function (inputHash) {
        this.hash = makeLowLevelEqHash();
        this.mutable = true;

    };

    EqHashTable.prototype.toWrittenString = function (cache) {
        var keys = this.hash.keys();
        var ret = [], i;
        for (i = 0; i < keys.length; i++) {
            var keyStr = baselib.format.toWrittenString(keys[i], cache);
            var valStr = baselib.format.toWrittenString(this.hash.get(keys[i]), cache);
            ret.push('(' + keyStr + ' . ' + valStr + ')');
        }
        return ('#hasheq(' + ret.join(' ') + ')');
    };
    
    EqHashTable.prototype.toDisplayedString = function (cache) {
        var keys = this.hash.keys();
        var ret = [], i;
        for (i = 0; i < keys.length; i++) {
            var keyStr = baselib.format.toDisplayedString(keys[i], cache);
            var valStr = baselib.format.toDisplayedString(this.hash.get(keys[i]), cache);
            ret.push('(' + keyStr + ' . ' + valStr + ')');
        }
        return ('#hasheq(' + ret.join(' ') + ')');
    };

    EqHashTable.prototype.equals = function (other, aUnionFind) {
        if (!(other instanceof EqHashTable)) {
            return false; 
        }

        if (this.hash.keys().length !== other.hash.keys().length) { 
            return false;
        }

        var keys = this.hash.keys(), i;
        for (i = 0; i < keys.length; i++) {
            if (!(other.hash.containsKey(keys[i]) &&
                  baselib.equality.equals(this.hash.get(keys[i]),
                                          other.hash.get(keys[i]),
                                          aUnionFind))) {
                return false;
            }
        }
        return true;
    };



    //////////////////////////////////////////////////////////////////////
    // Equal hash tables
    var EqualHashTable = function (inputHash) {
        this.hash = new Hashtable(
            function (x) {
                return baselib.format.toWrittenString(x); 
            },
            function (x, y) {
                return baselib.equality.equals(x, y, new baselib.UnionFind()); 
            });
        this.mutable = true;
    };

    EqualHashTable.prototype.toWrittenString = function (cache) {
        var keys = this.hash.keys();
        var ret = [], i;
        for (i = 0; i < keys.length; i++) {
            var keyStr = baselib.format.toWrittenString(keys[i], cache);
            var valStr = baselib.format.toWrittenString(this.hash.get(keys[i]), cache);
            ret.push('(' + keyStr + ' . ' + valStr + ')');
        }
        return ('#hash(' + ret.join(' ') + ')');
    };
    EqualHashTable.prototype.toDisplayedString = function (cache) {
        var keys = this.hash.keys();
        var ret = [], i;
        for (i = 0; i < keys.length; i++) {
            var keyStr = baselib.format.toDisplayedString(keys[i], cache);
            var valStr = baselib.format.toDisplayedString(this.hash.get(keys[i]), cache);
            ret.push('(' + keyStr + ' . ' + valStr + ')');
        }
        return ('#hash(' + ret.join(' ') + ')');
    };

    EqualHashTable.prototype.equals = function (other, aUnionFind) {
        if ( !(other instanceof EqualHashTable) ) {
            return false; 
        }

        if (this.hash.keys().length !== other.hash.keys().length) { 
            return false;
        }

        var keys = this.hash.keys(), i;
        for (i = 0; i < keys.length; i++){
            if (! (other.hash.containsKey(keys[i]) &&
                   baselib.equality.equals(this.hash.get(keys[i]),
                                               other.hash.get(keys[i]),
                                               aUnionFind))) {
                return false;
            }
        }
        return true;
    };




    var isHash = function (x) { 
        return (x instanceof EqHashTable ||
                x instanceof EqualHashTable); 
    };


    //////////////////////////////////////////////////////////////////////

    exports.getEqHashCode = getEqHashCode;
    exports.makeEqHashCode = makeEqHashCode;
    exports.makeLowLevelEqHash = makeLowLevelEqHash;


    exports.EqualHashTable = EqualHashTable;
    exports.EqHashTable = EqHashTable;
    exports.isHash = isHash;


}(this.plt.baselib));