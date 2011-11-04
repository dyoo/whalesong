/*jslint devel: false, browser: true, unparam: true, vars: true, plusplus: true, maxerr: 500, indent: 4 */
// Structure types
(function (baselib) {
    "use strict";
    var exports = {};
    baselib.symbols = exports;


    //////////////////////////////////////////////////////////////////////
    
    // Symbols

    //////////////////////////////////////////////////////////////////////
    var Symbol = function (val) {
        this.val = val;
    };

    var symbolCache = {};
    
    // makeInstance: string -> Symbol.
    Symbol.makeInstance = function (val) {
        // To ensure that we can eq? symbols with equal values.
        if (!(symbolCache.hasOwnProperty(val))) {
            symbolCache[val] = new Symbol(val);
        }
        return symbolCache[val];
    };
    
    Symbol.prototype.equals = function (other, aUnionFind) {
        return other instanceof Symbol &&
            this.val === other.val;
    };

    Symbol.prototype.hashCode = function(depth) {
        var k = baselib.hashes.getEqualHashCode("Symbol");
        k = baselib.hashes.hashMix(k);
        k += baselib.hashes.getEqualHashCode(this.val);
        k = baselib.hashes.hashMix(k);
        return k;
    };
    

    Symbol.prototype.toString = function (cache) {
        return this.val;
    };

    Symbol.prototype.toWrittenString = function (cache) {
        return this.val;
    };

    Symbol.prototype.toDisplayedString = function (cache) {
        return this.val;
    };

    Symbol.prototype.toDomNode = function(params) {
        if (params.getMode() === 'write') {
            return $("<span/>").text(this.val).get(0);
        }
        if (params.getMode() === 'display') {
            return $("<span/>").text(this.val).get(0);
        }
        if (params.getMode() === 'print') {
            if (params.getDepth() === 0) {
                return $("<span/>").text("'" + this.val).get(0);
            } else {
                return $("<span/>").text(this.val).get(0);
            }
        }
        if (params.getMode() === 'constructor') {
            return $("<span/>").text("'" + this.val).get(0);
        }

        return $("<span/>").text(this.val).get(0);
    };
    


    var isSymbol = function (x) { return x instanceof Symbol; };

    var makeSymbol = function (s) { return Symbol.makeInstance(s); };



    //////////////////////////////////////////////////////////////////////

    exports.Symbol = Symbol;
    exports.makeSymbol = makeSymbol;
    exports.isSymbol = isSymbol;

}(this.plt.baselib));