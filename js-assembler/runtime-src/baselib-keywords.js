// Keywords
(function(baselib) {
    var exports = {};
    baselib.keywords = exports;


    var Keyword = function(val) {
        this.val = val;
    };

    var keywordCache = {};
    
    // makeInstance: string -> Keyword.
    Keyword.makeInstance = function(val) {
        // To ensure that we can eq? symbols with equal values.
        if (!(val in keywordCache)) {
	    keywordCache[val] = new Keyword(val);
        } else {
        }
        return keywordCache[val];
    };
    
    Keyword.prototype.equals = function(other, aUnionFind) {
        return other instanceof Keyword &&
            this.val == other.val;
    };
    

    Keyword.prototype.toString = function(cache) {
        return this.val;
    };

    Keyword.prototype.toWrittenString = function(cache) {
        return this.val;
    };

    Keyword.prototype.toDisplayedString = function(cache) {
        return this.val;
    };


    exports.Keyword = Keyword;

})(this['plt'].baselib);