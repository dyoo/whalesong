// list structures (pairs, empty)
(function(baselib) {
    var exports = {};
    baselib.lists = exports;

    


    
    Empty = function() {
    };
    Empty.EMPTY = new Empty();


    Empty.prototype.equals = function(other, aUnionFind) {
        return other instanceof Empty;
    };

    Empty.prototype.reverse = function() {
        return this;
    };

    Empty.prototype.toWrittenString = function(cache) { return "empty"; };
    Empty.prototype.toDisplayedString = function(cache) { return "empty"; };
    Empty.prototype.toString = function(cache) { return "()"; };

    
    // Empty.append: (listof X) -> (listof X)
    Empty.prototype.append = function(b){
        return b;
    };
    



    //////////////////////////////////////////////////////////////////////

    // Cons Pairs

    var Cons = function(f, r) {
        this.first = f;
        this.rest = r;
    };

    Cons.prototype.reverse = function() {
        var lst = this;
        var ret = Empty.EMPTY;
        while (!isEmpty(lst)){
	    ret = Cons.makeInstance(lst.first, ret);
	    lst = lst.rest;
        }
        return ret;
    };
    
    Cons.makeInstance = function(f, r) {
        return new Cons(f, r);
    };

    // FIXME: can we reduce the recursion on this?
    Cons.prototype.equals = function(other, aUnionFind) {
        if (! (other instanceof Cons)) {
	    return false;
        }
        return (plt.baselib.equality.equals(this.first, other.first, aUnionFind) &&
	        plt.baselib.equality.equals(this.rest, other.rest, aUnionFind));
    };
    

    

    // Cons.append: (listof X) -> (listof X)
    Cons.prototype.append = function(b){
        if (b === Empty.EMPTY)
	    return this;
        var ret = b;
        var lst = this.reverse();
        while ( !isEmpty(lst) ) {
	    ret = Cons.makeInstance(lst.first, ret);
	    lst = lst.rest;
        }
	
        return ret;
    };
    

    Cons.prototype.toWrittenString = function(cache) {
        cache.put(this, true);
        var texts = [];
        var p = this;
        while ( p instanceof Cons ) {
	    texts.push(plt.baselib.format.toWrittenString(p.first, cache));
	    p = p.rest;
	    if (typeof(p) === 'object' && cache.containsKey(p)) {
	        break;
	    }
        }
        if ( p !== Empty.EMPTY ) {
	    texts.push('.');
	    texts.push(plt.baselib.format.toWrittenString(p, cache));
        }
        return "(" + texts.join(" ") + ")";
    };

    Cons.prototype.toString = Cons.prototype.toWrittenString;

    Cons.prototype.toDisplayedString = function(cache) {
        cache.put(this, true);
        var texts = [];
        var p = this;
        while ( p instanceof Cons ) {
	    texts.push(plt.baselib.format.toDisplayedString(p.first, cache));
	    p = p.rest;
	    if (typeof(p) === 'object' && cache.containsKey(p)) {
	        break;
	    }
        }
        if ( p !== Empty.EMPTY ) {
	    texts.push('.');
	    texts.push(plt.baselib.format.toDisplayedString(p, cache));
        }
        return "(" + texts.join(" ") + ")";
    };



    Cons.prototype.toDomNode = function(cache) {
        cache.put(this, true);
        var node = document.createElement("span");
        node.appendChild(document.createTextNode("("));
        var p = this;
        while ( p instanceof Cons ) {
	    appendChild(node, plt.baselib.format.toDomNode(p.first, cache));
	    p = p.rest;
	    if ( p !== Empty.EMPTY ) {
	        appendChild(node, document.createTextNode(" "));
	    }
	    if (typeof(p) === 'object' && cache.containsKey(p)) {
	        break;
	    }
        }
        if ( p !== Empty.EMPTY ) {
	    appendChild(node, document.createTextNode("."));
	    appendChild(node, document.createTextNode(" "));
	    appendChild(node, plt.baselib.format.toDomNode(p, cache));
        }

        node.appendChild(document.createTextNode(")"));
        return node;
    };



    exports.EMPTY = Empty.EMPTY;
    exports.Empty = Empty;
    exports.Cons = Cons;



})(this['plt'].baselib);