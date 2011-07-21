// vectors
(function(baselib) {
    var exports = {};
    baselib.vectors = exports;



    Vector = function(n, initialElements) {
        this.elts = new Array(n);
        if (initialElements) {
	    for (var i = 0; i < n; i++) {
	        this.elts[i] = initialElements[i];
	    }
        } else {
	    for (var i = 0; i < n; i++) {
	        this.elts[i] = undefined;
	    }
        }
        this.mutable = true;
    };

    Vector.makeInstance = function(n, elts) {
        return new Vector(n, elts);
    }

    Vector.prototype.length = function() {
	return this.elts.length;
    };

    Vector.prototype.ref = function(k) {
        return this.elts[k];
    };

    Vector.prototype.set = function(k, v) {
        this.elts[k] = v;
    };

    Vector.prototype.equals = function(other, aUnionFind) {
        if (other != null && other != undefined && other instanceof Vector) {
	    if (other.length() != this.length()) {
	        return false
	    }
	    for (var i = 0; i <  this.length(); i++) {
	        if (! plt.baselib.equality.equals(this.elts[i], other.elts[i], aUnionFind)) {
		    return false;
	        }
	    }
	    return true;
        } else {
	    return false;
        }
    };

    Vector.prototype.toList = function() {
        var ret = plt.baselib.lists.EMPTY;
        for (var i = this.length() - 1; i >= 0; i--) {
	    ret = plt.baselib.lists.Cons.makeInstance(this.elts[i], ret);	    
        }	
        return ret;
    };

    Vector.prototype.toWrittenString = function(cache) {
        cache.put(this, true);
        var texts = [];
        for (var i = 0; i < this.length(); i++) {
	    texts.push(plt.baselib.format.toWrittenString(this.ref(i), cache));
        }
        return "#(" + texts.join(" ") + ")";
    };

    Vector.prototype.toDisplayedString = function(cache) {
        cache.put(this, true);
        var texts = [];
        for (var i = 0; i < this.length(); i++) {
	    texts.push(plt.baselib.format.toDisplayedString(this.ref(i), cache));
        }
        return "#(" + texts.join(" ") + ")";
    };

    Vector.prototype.toDomNode = function(cache) {
        cache.put(this, true);
        var node = document.createElement("span");
        node.appendChild(document.createTextNode("#("));
        for (var i = 0; i < this.length(); i++) {
	    node.appendChild(plt.baselib.format.toDomNode(this.ref(i), cache));
	    if (i !== this.length()-1) {
	        node.appendChild(document.createTextNode(" "));
	    }
        }
        node.appendChild(document.createTextNode(")"));
        return node;
    };


    var isVector = function(x) { return x instanceof Vector; };

    var makeVector = function() {
        return Vector.makeInstance(arguments.length, arguments);
    };

    var makeVectorImmutable = function() {
        var v = Vector.makeInstance(arguments.length, arguments);
        v.mutable = false;
        return v;
    };



    //////////////////////////////////////////////////////////////////////

    exports.Vector = Vector;
    exports.isVector = isVector;
    exports.makeVector = makeVector;
    exports.makeVectorImmutable = makeVectorImmutable;


})(this['plt'].baselib);