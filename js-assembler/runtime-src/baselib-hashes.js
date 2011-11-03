/*jslint unparam: true, vars: true, white: true, newcap: true, nomen: true, plusplus: true, maxerr: 50, indent: 4 */

/*global window,Hashtable*/

// Mutable hashtables.


(function (baselib, Hashtable) {
    'use strict';
    var exports = {};

    baselib.hashes = exports;


    
    var _eqHashCodeCounter = 0;
    var makeEqHashCode = function () {
        _eqHashCodeCounter++;
        return String(_eqHashCodeCounter);
    };


    // getHashCode: any -> string
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
        return '';
    };


    // Creates a low-level hashtable, following the interface of 
    // http://www.timdown.co.uk/jshashtable/
    //
    // Defined to use the getEqHashCode defined in baselib_hash.js.
    var makeLowLevelEqHash = function () {
        return new Hashtable(function (x) { return getEqHashCode(x); },
                             function (x, y) { return x === y; });
    };



    var makeEqHashtable = function() { 
        return new WhalesongHashtable(
            "hasheq",
            function (x) { return getEqHashCode(x); },
            function (x, y) { return x === y; });
    };

    var makeEqualHashtable = function() {
        return new WhalesongHashtable(
            "hash",
            function (x) {
                return baselib.format.toWrittenString(x); 
            },
            function (x, y) {
                return baselib.equality.equals(x, y, new baselib.UnionFind()); 
            })
    };
    
    var makeEqvHashtable = function() {
        return new WhalesongHashtable(
            "hasheqv",
            function (x) {
                return baselib.format.toWrittenString(x); 
            },
            baselib.equality.eqv);
    };



    //////////////////////////////////////////////////////////////////////
    // Whalesong's Hashtables are a thin wrapper around the mutable Hashtable
    // class to make it printable and equatable.
    var WhalesongHashtable = function (type, hash_function, equality_function) {
        this.type = type;
        this.hash_function = hash_function;
        this.equality_function = equality_function;
        this.hash = new Hashtable(hash_function, equality_function);
    };

    WhalesongHashtable.prototype.toWrittenString = function (cache) {
        var keys = this.hash.keys();
        var ret = [], i;
        for (i = 0; i < keys.length; i++) {
            var keyStr = baselib.format.toWrittenString(keys[i], cache);
            var valStr = baselib.format.toWrittenString(this.hash.get(keys[i]), cache);
            ret.push('(' + keyStr + ' . ' + valStr + ')');
        }
        return ('#' + this.type + '(' + ret.join(' ') + ')');
    };
    
    WhalesongHashtable.prototype.toDisplayedString = function (cache) {
        var keys = this.hash.keys();
        var ret = [], i;
        for (i = 0; i < keys.length; i++) {
            var keyStr = baselib.format.toDisplayedString(keys[i], cache);
            var valStr = baselib.format.toDisplayedString(this.hash.get(keys[i]), cache);
            ret.push('(' + keyStr + ' . ' + valStr + ')');
        }
        return ('#' + this.type + '(' + ret.join(' ') + ')');
    };

    WhalesongHashtable.prototype.equals = function (other, aUnionFind) {
        if (!(other instanceof WhalesongHashtable)) {
            return false; 
        }
        if (other.type !== this.type) { 
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





    WhalesongHashtable.prototype.get = function(key) {
        return this.hash.get(key);
    };

    WhalesongHashtable.prototype.put = function(key, value) {
        this.hash.put(key, value);
    };

    WhalesongHashtable.prototype.remove = function(key) {
        this.hash.remove(key);
    };

    WhalesongHashtable.prototype.containsKey = function(key) {
        return this.hash.containsKey(key);
    };

    var isHash = function (x) { 
        return (x instanceof WhalesongHashtable);
    };


    //////////////////////////////////////////////////////////////////////

    exports.getEqHashCode = getEqHashCode;
    exports.makeEqHashCode = makeEqHashCode;
    exports.makeLowLevelEqHash = makeLowLevelEqHash;

    exports.makeEqHashtable = makeEqHashtable;
    exports.makeEqvHashtable = makeEqvHashtable;
    exports.makeEqualHashtable = makeEqualHashtable;

    exports.isHash = isHash;


}(window.plt.baselib, Hashtable));