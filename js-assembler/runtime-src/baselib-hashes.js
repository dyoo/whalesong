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


    // getEqHashCode: any -> string
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


    // getEqvHashCode: any -> string
    var getEqvHashCode = function (x) {
        if (baselib.numbers.isNumber(x)) {
            return baselib.numbers.toFixnum(x);
        }
        if (baselib.chars.isChar(x)) {
            return x.val;
        } 
        return getEqHashCode(x);
    };


    var eq = function (x, y) { return x === y; };
    var eqv = baselib.equality.eqv;
    var equal = function (x, y) {
        return baselib.equality.equals(x, y, new baselib.UnionFind()); 
    };


    // Creates a low-level hashtable, following the interface of 
    // http://www.timdown.co.uk/jshashtable/
    var makeLowLevelEqHash = function () {
        return new Hashtable(getEqHashCode,
                             function (x, y) { return x === y; });
    };


    var makeEqHashtable = function() { 
        return new WhalesongHashtable(
            "hasheq",
            getEqHashCode,
            eq,
            new Hashtable(getEqHashCode, eq));
    };

    var makeEqualHashtable = function() {
        return new WhalesongHashtable(
            "hash",
            getEqualHashCode,
            equal,
            new Hashtable(getEqualHashCode, equal));
    };
    
    
    var makeEqvHashtable = function() {
        return new WhalesongHashtable(
            "hasheqv",
            getEqvHashCode,
            eqv,
            new Hashtable(getEqvHashCode, eqv));
    };


    var makeImmutableEqHashtable = function() { 
        return makeEqHashtable().toImmutable();
    };

    var makeImmutableEqualHashtable = function() {
        return makeEqualHashtable().toImmutable();
    };
        
    var makeImmutableEqvHashtable = function() {
        return makeEqvHashtable().toImmutable();
    };


    // When we need to make comparators for the immutable hash tables, use this.
    var makeComparator = function(hash, eq) {
        return function(x, y) {
            var hx = hash(x), hy = hash(y);
            if (hx < hy) { return -1; }
            if (hx > hy) { return 1; }
            
            if (eq(x, y)) { return 0; }

            hx = getEqHashCode(x);
            hy = getEqHashCode(y);
            if (hx < hy) { return -1; }
            if (hx > hy) { return 1; }
            return 0;
        }
    };


    //////////////////////////////////////////////////////////////////////
    // Whalesong's Hashtables are a thin wrapper around the mutable Hashtable
    // class to make it printable and equatable.
    var WhalesongHashtable = function (type, hash_function, equality_function, hash) {
        this.type = type;
        this.hash_function = hash_function;
        this.equality_function = equality_function;
        this.hash = hash;
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

    WhalesongHashtable.prototype.hashCode = function(depth) {
        var k = getEqualHashCode(this.type);
        var keys = this.hash.keys(), i;
        for (i = 0; i < keys.length; i++) {
            k += hashMix(getEqualHashCode(this.hash.get(keys[i]), depth));
        }
        return hashMix(k);
    };


    WhalesongHashtable.prototype.get = function(key) {
        return this.hash.get(key);
    };

    WhalesongHashtable.prototype.put = function(key, value) {
        this.hash.put(key, value);
    };

    WhalesongHashtable.prototype.functionalPut = function(key, value) {
        return this.toImmutable().functionalPut(key, value);
    };

    WhalesongHashtable.prototype.remove = function(key) {
        this.hash.remove(key);
    };

    WhalesongHashtable.prototype.functionalRemove = function(key) {
        return this.toImmutable().functionalRemove(key);
    };

    WhalesongHashtable.prototype.containsKey = function(key) {
        return this.hash.containsKey(key);
    };

    WhalesongHashtable.prototype.isImmutable = function() {
        return false;
    };

    WhalesongHashtable.prototype.toImmutable = function() {
        var keycmp = makeComparator(this.hash_function, this.equality_function)
        var immutable = new WhalesongImmutableHashtable(
            this.type,
            this.hash_function,
            this.equality_function,
            LLRBTree.makeMap(keycmp));
        var keys = this.hash.keys();
        var i;
        for (i = 0; i < keys.length; i++) {
            immutable = immutable.functionalPut(keys[i], this.hash.get(keys[i]));
        }
        return immutable;
    };


    //////////////////////////////////////////////////////////////////////
    //////////////////////////////////////////////////////////////////////
    // Whalesong's immutable hashtables are a thin wrapper around the
    // llrbtree class to make it printable and equatable.
    // llrbtree comes from: https://github.com/dyoo/js-llrbtree
    var WhalesongImmutableHashtable = function (type,
                                                hash_function,
                                                equality_function,
                                                map) {
        this.type = type;
        this.hash_function = hash_function;
        this.equality_function = equality_function;
        this.map = map;
    };

    WhalesongImmutableHashtable.prototype.toWrittenString = function (cache) {
        var items = this.map.items();
        var ret = [], i;
        for (i = 0; i < items.length; i++) {
            var keyStr = baselib.format.toWrittenString(items[i][0], cache);
            var valStr = baselib.format.toWrittenString(items[i][1], cache);
            ret.push('(' + keyStr + ' . ' + valStr + ')');
        }
        return ('#' + this.type + '(' + ret.join(' ') + ')');
    };
    
    WhalesongImmutableHashtable.prototype.toDisplayedString = function (cache) {
        var items = this.map.keys();
        var ret = [], i;
        for (i = 0; i < items.length; i++) {
            var keyStr = baselib.format.toDisplayedString(items[i][0], cache);
            var valStr = baselib.format.toDisplayedString(items[i][1], cache);
            ret.push('(' + keyStr + ' . ' + valStr + ')');
        }
        return ('#' + this.type + '(' + ret.join(' ') + ')');
    };

    WhalesongImmutableHashtable.prototype.equals = function (other, aUnionFind) {
        if (!(other instanceof WhalesongImmutableHashtable)) {
            return false; 
        }
        if (other.type !== this.type) { 
            return false;
        }
        var litems = this.map.items();
        var ritems = other.map.items();

        if (litems.length !== ritems.length) { 
            return false;
        }
        var i;
        for (i = 0; i < litems.length; i++) {
            if (!(baselib.equality.equals(litems[i][0], ritems[i][0], aUnionFind))) {
                return false;
            }
            if (!(baselib.equality.equals(litems[i][1], ritems[i][1], aUnionFind))) {
                return false;
            }
        }
        return true;
    };

    WhalesongImmutableHashtable.prototype.hashCode = function(depth) {
        var k = getEqualHashCode(this.type);
        var items = this.map.items(), i;
        for (i = 0; i < items.length; i++) {
            k = getEqualHashCode(items[i][0], depth);
            k = hashMix(k);
            k = getEqualHashCode(items[i][1], depth);
            k = hashMix(k);
        }
        return hashMix(k);
    };


    WhalesongImmutableHashtable.prototype.get = function(key) {
        return this.map.get(key);
    };

    WhalesongImmutableHashtable.prototype.put = function(key, value) {
        throw new Error();
    };

    WhalesongImmutableHashtable.prototype.functionalPut = function(key, value) {
        return new WhalesongImmutableHashtable(this.type,
                                               this.hash_function,
                                               this.equality_function,
                                               this.map.put(key, value));
    };

    WhalesongImmutableHashtable.prototype.remove = function(key) {
        throw new Error();
    };

    WhalesongImmutableHashtable.prototype.functionalRemove = function(key) {
        return new WhalesongImmutableHashtable(this.type,
                                               this.hash_function,
                                               this.equality_function,
                                               this.map.remove(key));
    };

    WhalesongImmutableHashtable.prototype.containsKey = function(key) {
        return this.map.contains(key);
    };

    WhalesongImmutableHashtable.prototype.isImmutable = function() { 
        return true;
    };
    //////////////////////////////////////////////////////////////////////    



    var isHash = function (x) { 
        return (x instanceof WhalesongHashtable || x instanceof WhalesongImmutableHashtable);
    };

    var isHashEqv = function (x) { 
        return (x instanceof WhalesongHashtable || x instanceof WhalesongImmutableHashtable) && x.type === 'eqv';
    };

    var isHashEq = function (x) { 
        return (x instanceof WhalesongHashtable || x instanceof WhalesongImmutableHashtable) && x.type === 'eq';
    };



    // Arbitrary magic number.  We have to cut off the hashing at some point.
    var MAX_HASH_DEPTH = 128;

    // Returns a JavaScript number.
    var getEqualHashCode = function (x, depth) {
        var i, t, k = 0;
        if (depth === undefined) { depth = [0]; }

        if (depth[0] > MAX_HASH_DEPTH) { return 0; }

        if (baselib.numbers.isNumber(x)) {
            return hashMix(baselib.numbers.toFixnum(x));
        }

        if (baselib.strings.isString(x)) {
            t = x.toString();
            for (i = 0; i < t.length; i++) {
                k += t.charCodeAt(i);
                k = hashMix(k);
            }
            return k;
        }

        if (x === undefined || x === null) {
            return 1;
        }

        if (typeof(x) === 'object' &&
            typeof(x.hashCode) === 'function') {
            depth[0] = depth[0] + 1;
            return x.hashCode(depth);
        }
        return 0;
    };


    // Does some weird math on k.  Grabbed from Racket's implementation of hashes.
    // References to: http://www.burtleburtle.net/bob/hash/doobs.html
    var hashMix = function(k) {
        k += (k << 10);
        k ^= (k >> 6);
        return k;
    };



    //////////////////////////////////////////////////////////////////////

    exports.getEqHashCode = getEqHashCode;
    exports.getEqualHashCode = getEqualHashCode;
    exports.getEqvHashCode = getEqvHashCode;

    exports.hashMix = hashMix;

    exports.makeEqHashCode = makeEqHashCode;
    exports.makeLowLevelEqHash = makeLowLevelEqHash;

    exports.makeEqHashtable = makeEqHashtable;
    exports.makeEqvHashtable = makeEqvHashtable;
    exports.makeEqualHashtable = makeEqualHashtable;

    exports.makeImmutableEqHashtable = makeImmutableEqHashtable;
    exports.makeImmutableEqvHashtable = makeImmutableEqvHashtable;
    exports.makeImmutableEqualHashtable = makeImmutableEqualHashtable;

    exports.isHash = isHash;
    exports.isHashEqv = isHashEqv;
    exports.isHashEq = isHashEq;
}(window.plt.baselib, Hashtable));