/*jslint browser: true, unparam: true, vars: true, plusplus: true, maxerr: 50, indent: 4 */


// list structures (pairs, empty)
(function (baselib) {
    'use strict';
    var exports = {};
    baselib.lists = exports;

    


    
    var Empty = function () {
    };
    Empty.EMPTY = new Empty();
    var EMPTY = Empty.EMPTY;



    Empty.prototype.equals = function (other, aUnionFind) {
        return other instanceof Empty;
    };

    Empty.prototype.reverse = function () {
        return this;
    };

    Empty.prototype.toWrittenString = function (cache) { return "empty"; };
    Empty.prototype.toDisplayedString = function (cache) { return "empty"; };
    Empty.prototype.toString = function (cache) { return "()"; };

    
    // Empty.append: (listof X) -> (listof X)
    Empty.prototype.append = function (b) {
        return b;
    };
    



    //////////////////////////////////////////////////////////////////////

    // Cons Pairs

    var Cons = function (first, rest) {
        this.first = first;
        this.rest = rest;
    };

    Cons.prototype.reverse = function () {
        var lst = this;
        var ret = EMPTY;
        while (lst !== EMPTY) {
            ret = Cons.makeInstance(lst.first, ret);
            lst = lst.rest;
        }
        return ret;
    };
    
    Cons.makeInstance = function (first, rest) {
        return new Cons(first, rest);
    };

    // FIXME: can we reduce the recursion on this?
    Cons.prototype.equals = function (other, aUnionFind) {
        if (!(other instanceof Cons)) {
            return false;
        }
        return (baselib.equality.equals(this.first, other.first, aUnionFind) &&
                baselib.equality.equals(this.rest, other.rest, aUnionFind));
    };
    

    

    // Cons.append: (listof X) -> (listof X)
    Cons.prototype.append = function (b) {
        if (b === EMPTY) {
            return this;
        }
        var ret = b;
        var lst = this.reverse();
        while (lst !== EMPTY) {
            ret = Cons.makeInstance(lst.first, ret);
            lst = lst.rest;
        }
        
        return ret;
    };
    

    Cons.prototype.toWrittenString = function (cache) {
        cache.put(this, true);
        var texts = [];
        var p = this;
        while (p instanceof Cons) {
            texts.push(baselib.format.toWrittenString(p.first, cache));
            p = p.rest;
            if (typeof (p) === 'object' && cache.containsKey(p)) {
                break;
            }
        }
        if (p !== EMPTY) {
            texts.push('.');
            texts.push(baselib.format.toWrittenString(p, cache));
        }
        return "(" + texts.join(" ") + ")";
    };

    Cons.prototype.toString = Cons.prototype.toWrittenString;

    Cons.prototype.toDisplayedString = function (cache) {
        cache.put(this, true);
        var texts = [];
        var p = this;
        while (p instanceof Cons) {
            texts.push(baselib.format.toDisplayedString(p.first, cache));
            p = p.rest;
            if (typeof (p) === 'object' && cache.containsKey(p)) {
                break;
            }
        }
        if (p !== Empty.EMPTY) {
            texts.push('.');
            texts.push(baselib.format.toDisplayedString(p, cache));
        }
        return "(" + texts.join(" ") + ")";
    };



    Cons.prototype.toDomNode = function (cache) {
        cache.put(this, true);
        var node = document.createElement("span");
        node.appendChild(document.createTextNode("("));
        var p = this;
        while (p instanceof Cons) {
            node.appendChild(baselib.format.toDomNode(p.first, cache));
            p = p.rest;
            if (p !== Empty.EMPTY) {
                node.appendChild(document.createTextNode(" "));
            }
            if (typeof (p) === 'object' && cache.containsKey(p)) {
                break;
            }
        }
        if (p !== Empty.EMPTY) {
            node.appendChild(document.createTextNode("."));
            node.appendChild(document.createTextNode(" "));
            node.appendChild(baselib.format.toDomNode(p, cache));
        }

        node.appendChild(document.createTextNode(")"));
        return node;
    };


    var isPair = function (x) { return x instanceof Cons; };
    var isEmpty = function (x) { return x === Empty.EMPTY; };


    var makePair = Cons.makeInstance;

    var makeList = function () {
        var result = Empty.EMPTY, i;
        for (i = arguments.length - 1; i >= 0; i--) {
            result = Cons.makeInstance(arguments[i], result);
        }
        return result;
    };


    // isList: Any -> Boolean
    // Returns true if x is a list (a chain of pairs terminated by EMPTY).
    var isList = function (x) { 
        while (x !== Empty.EMPTY) {
            if (x instanceof Cons) {
                x = x.rest;
            } else {
                return false;
            }
        }
        return true;
    };



    var reverse = function (lst) {
        var rev = EMPTY;
        while (lst !== EMPTY) {
            rev = makePair(lst.first, rev);
            lst = lst.rest;
        }
        return rev;
    };


    var length = function (lst) {
        var len = 0;
        while (lst !== EMPTY) {
            len++;
            lst = lst.rest;
        }
        return len;
    };


    var listRef = function (lst, n) {
        var i;
        for (i = 0; i < n; i++) {
            lst = lst.rest;
        }
        return lst.first;
    };



    //////////////////////////////////////////////////////////////////////

    exports.EMPTY = EMPTY;
    exports.Empty = Empty;
    exports.Cons = Cons;
    exports.isPair = isPair;
    exports.isList = isList;
    exports.isEmpty = isEmpty;
    exports.makePair = makePair;
    exports.makeList = makeList;
    exports.reverse = reverse;
    exports.length = length;
    exports.listRef = listRef;


}(this.plt.baselib));