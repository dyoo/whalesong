// Structure types

(function(baselib) {



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




    baselib.Symbol = Symbol;





})(this['plt'].baselib);