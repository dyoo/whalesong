// Continuation marks
(function(baselib) {
    var exports = {};
    baselib.contmarks = exports;


    var ContinuationMarkSet = function(dict) {
        this.dict = dict;
    }

    ContinuationMarkSet.prototype.toDomNode = function(cache) {
        var dom = document.createElement("span");
        dom.appendChild(document.createTextNode('#<continuation-mark-set>'));
        return dom;
    };

    ContinuationMarkSet.prototype.toWrittenString = function(cache) {
        return '#<continuation-mark-set>';
    };

    ContinuationMarkSet.prototype.toDisplayedString = function(cache) {
        return '#<continuation-mark-set>';
    };

    ContinuationMarkSet.prototype.ref = function(key) {
        if ( this.dict.containsKey(key) ) {
	    return this.dict.get(key);
        }
        return [];
    };

    exports.ContinuationMarkSet = ContinuationMarkSet;


})(this['plt'].baselib);