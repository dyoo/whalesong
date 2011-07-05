// Exceptions

(function(baselib) {
    var exports = {};
    baselib.boxes = exports;


    //////////////////////////////////////////////////////////////////////
    // Boxes
    
    var Box = function(x, mutable) {
	this.val = x;
	this.mutable = mutable;
    };

    Box.prototype.ref = function() {
        return this.val;
    };

    Box.prototype.set = function(newVal) {
        if (this.mutable) {
	    this.val = newVal;
        }
    };

    Box.prototype.toString = function(cache) {
        cache.put(this, true);
        return "#&" + plt.baselib.format.toWrittenString(this.val, cache);
    };

    Box.prototype.toWrittenString = function(cache) {
        cache.put(this, true);
        return "#&" + plt.baselib.format.toWrittenString(this.val, cache);
    };

    Box.prototype.toDisplayedString = function(cache) {
        cache.put(this, true);
        return "#&" + plt.baselib.format.toDisplayedString(this.val, cache);
    };

    Box.prototype.toDomNode = function(cache) {
        cache.put(this, true);
        var parent = document.createElement("span");
        parent.appendChild(document.createTextNode('#&'));
        parent.appendChild(plt.baselib.format.toDomNode(this.val, cache));
        return parent;
    };

    Box.prototype.equals = function(other, aUnionFind) {
        return ((other instanceof Box) &&
	        plt.baselib.equality.equals(this.val, other.val, aUnionFind));
    };
    

    exports.Box = Box;



})(this['plt'].baselib);