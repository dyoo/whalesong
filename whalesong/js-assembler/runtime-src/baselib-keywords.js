/*jslint unparam: true, vars: true, maxerr: 50, indent: 4 */

// Keywords

(function (baselib) {
    'use strict';
    var exports = {};
    baselib.keywords = exports;


    var Keyword = function (val) {
        this.val = val;
    };

    var keywordCache = {};
    
    // makeInstance: string -> Keyword.
    Keyword.makeInstance = function (val) {
        // To ensure that we can eq? symbols with equal values.
        if (!(keywordCache.hasOwnProperty(val))) {
            keywordCache[val] = new Keyword(val);
        }
        return keywordCache[val];
    };
    
    Keyword.prototype.equals = function (other, aUnionFind) {
        return other instanceof Keyword &&
            this.val === other.val;
    };

    Keyword.prototype.hashCode = function(depth) {
        var k = baselib.hashes.getEqualHashCode("Keyword");
        k += baselib.hashes.getEqualHashCode(this.val, depth);
        k = baselib.hashes.hashMix(k);
        return k;
    };
    

    Keyword.prototype.toString = function (cache) {
        return this.val;
    };

    Keyword.prototype.toWrittenString = function (cache) {
        return this.val;
    };

    Keyword.prototype.toDisplayedString = function (cache) {
        return this.val;
    };


    exports.Keyword = Keyword;

}(this.plt.baselib));