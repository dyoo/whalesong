// Structure types

(function(baselib) {
    
    var exports = {};
    baselib.symbols = exports;


    //////////////////////////////////////////////////////////////////////
    
    // Symbols

    //////////////////////////////////////////////////////////////////////
    var Symbol = function(val) {
        this.val = val;
    };

    var symbolCache = {};
    
    // makeInstance: string -> Symbol.
    Symbol.makeInstance = function(val) {
        // To ensure that we can eq? symbols with equal values.
        if (!(val in symbolCache)) {
	    symbolCache[val] = new Symbol(val);
        } else {
        }
        return symbolCache[val];
    };
    
    Symbol.prototype.equals = function(other, aUnionFind) {
        return other instanceof Symbol &&
            this.val === other.val;
    };
    

    Symbol.prototype.toString = function(cache) {
        return this.val;
    };

    Symbol.prototype.toWrittenString = function(cache) {
        return this.val;
    };

    Symbol.prototype.toDisplayedString = function(cache) {
        return this.val;
    };


    var isSymbol = function(x) { return x instanceof Symbol; };

    var makeSymbol = function(s) { return Symbol.makeInstance(s); };



    //////////////////////////////////////////////////////////////////////

    exports.Symbol = Symbol;
    exports.makeSymbol = makeSymbol;
    exports.isSymbol = isSymbol;

})(this['plt'].baselib);