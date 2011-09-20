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

    Empty.prototype.toDomNode = function(params) {
        if (params.getMode() === "display") {
            return $("<span/>").text("()").get(0);
        } else if (params.getMode() === "write") {
            return $("<span/>").text("()").get(0);
        } else if (params.getMode() === "print") {
            if (params.getDepth() === 0) {
                return $("<span/>").text("'()").get(0);
            } else {
                return $("<span/>").text("()").get(0);
            }
        } else if (params.getMode() === "constructor") {
            return $("<span/>").text("(list)").get(0);
        } else {
            return $("<span/>").text("()").get(0);
        }
    };

    
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

    var makePair = function (first, rest) {
        return new Cons(first, rest);
    };

    Cons.prototype.reverse = function () {
        var lst = this;
        var ret = EMPTY;
        while (lst !== EMPTY) {
            ret = makePair(lst.first, ret);
            lst = lst.rest;
        }
        return ret;
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
            ret = makePair(lst.first, ret);
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
        if (p !== EMPTY) {
            texts.push('.');
            texts.push(baselib.format.toDisplayedString(p, cache));
        }
        return "(" + texts.join(" ") + ")";
    };



    Cons.prototype.toDomNode = function (params) {
        params.put(this, true);
        var node = document.createElement("span");
        node.appendChild(document.createTextNode("("));
        var p = this;
        while (p instanceof Cons) {
            node.appendChild(params.recur(p.first));
            p = p.rest;
            if (p !== EMPTY) {
                node.appendChild(document.createTextNode(" "));
            }
            if (typeof (p) === 'object' && params.containsKey(p)) {
                break;
            }
        }
        if (p !== EMPTY) {
            node.appendChild(document.createTextNode("."));
            node.appendChild(document.createTextNode(" "));
            node.appendChild(params.recur(p));
        }

        node.appendChild(document.createTextNode(")"));
        return node;
    };


    var isPair = function (x) { return x instanceof Cons; };
    var isEmpty = function (x) { return x === EMPTY; };



    var makeList = function () {
        var result = EMPTY, i;
        for (i = arguments.length - 1; i >= 0; i--) {
            result = makePair(arguments[i], result);
        }
        return result;
    };


    // Coerse a list back into a JavaScript array.
    var listToArray = function (lst) {
        var result = [];
        while (lst !== EMPTY) {
            result.push(lst.first);
            lst = lst.rest;
        }
        return result;
    };


    // isList: Any -> Boolean
    // Returns true if x is a list (a chain of pairs terminated by EMPTY).
    var isList = function (x) { 
        var tortoise, hare;
        tortoise = hare = x;
        if (hare === EMPTY) { return true; }
        while (true) {
            if (!(hare instanceof Cons)) { return false; }
            if (tortoise instanceof Cons) { tortoise = tortoise.rest; }
            hare = hare.rest;
            if (hare instanceof Cons) { hare = hare.rest; }
            if (hare === EMPTY) { return true; }
            if (tortoise === hare) { return false; }
        }
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
    exports.listToArray = listToArray;

}(this.plt.baselib));