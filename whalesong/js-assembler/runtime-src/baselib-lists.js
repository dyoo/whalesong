/*global $*/
/*jslint browser: true, unparam: true, vars: true, plusplus: true, maxerr: 50, indent: 4 */


// list structures (pairs, empty)
(function (baselib, $) {
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

    Empty.prototype.hashCode = function(depth) {
        return baselib.hashes.getEqualHashCode("empty");
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

    Cons.prototype.hashCode = function(depth) {
        var k = baselib.hashes.getEqualHashCode("Cons");
        k += baselib.hashes.getEqualHashCode(this.first, depth);
        k = baselib.hashes.hashMix(k);
        k += baselib.hashes.getEqualHashCode(this.rest, depth);
        k = baselib.hashes.hashMix(k);
        return k;
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
        var node;

        var subelts = [], dottedPair = false, i;
        var p = this;
        while (p instanceof Cons) {
            subelts.push(params.recur(p.first));
            p = p.rest;
            if (typeof (p) === 'object' && params.containsKey(p)) {
                break;
            }
        }
        if (p !== EMPTY) {
            dottedPair = true;
            subelts.push(params.recur(p));
        }


        if (params.getMode() === 'constructor') {
            if (dottedPair) {
                node = subelts[subelts.length - 1];
                for (i = subelts.length - 2; i >= 0; i--) {
                    node = $('<span/>')
                        .append($('<span/>').text("(").addClass('lParen'))
                        .append($('<span/>').text("cons"))
                        .append(" ")
                        .append(subelts[i])
                        .append(" ")
                        .append(node)
                        .append($('<span/>').text(")").addClass('rParen'))
                        .get(0);
                }
                return node;
            } else {
                node = $("<span/>").addClass("wescheme-cons");
                node.append($('<span/>').text("(").addClass('lParen'));
                node.append($('<span/>').text("list"));
                node.append(" ");
                node.append(subelts[0]);
                for (i = 1; i < subelts.length; i++) {
                    node.append(" ");
                    node.append(subelts[i]);
                }
                node.append($('<span/>').text(')').addClass('rParen'));
                return node.get(0);
            }
        }

        node = $('<span/>').addClass("wescheme-cons");
        if (params.getMode() === 'print') {
            node.append($("<span/>").text("'"));
        }
        node.append($('<span/>').text('(').addClass('lParen'))
        node.append(subelts[0]);
        if (subelts.length > 1) {
            for (i = 1; i < subelts.length - 1; i++) {
                node.append(" ");
                node.append(subelts[i]);
            }
            if (dottedPair) {
                node.append(" ");
                node.append(".");
            }
            node.append(" ");
            node.append(subelts[subelts.length - 1]);
        }
        node.append($('<span/>').text(")").addClass('rParen'));
        return node.get(0);
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


    var arrayToList = function (arr) {
        var result = EMPTY, i;
        for (i = arr.length -1; i >= 0; i--) {
            result = makePair(arr[i], result);
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
        if (hare === EMPTY) { 
            return true; 
        }
        if (!(hare instanceof Cons)) { return false; }
        while (true) {
            // Loop invariant: at the beginning of the loop, both tortoise
            // and hare should be pointing to a cons cell.
            tortoise = tortoise.rest; 
            hare = hare.rest;
            if (hare instanceof Cons) { 
                // optimization to get amortized linear time isList:
                if (hare._isList !== void(0)) { 
                    tortoise._isList = hare._isList; return hare._isList; 
                }
                hare = hare.rest; 
                // optimization to get amortized linear time isList:
                if (hare instanceof Cons && hare._isList !== void(0)) { 
                    tortoise._isList = hare._isList; return hare._isList; 
                }
            }
            if (hare === EMPTY) { 
                // optimization to get amortized linear time isList:
                tortoise._isList = true;
                return true; 
            }
            if (tortoise === hare) {
                tortoise._isList = false;
                return false; 
            }
            if (!(hare instanceof Cons)) { 
                tortoise._isList = false;
                return false; 
            }
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
    exports.arrayToList = arrayToList;

}(this.plt.baselib, jQuery));
