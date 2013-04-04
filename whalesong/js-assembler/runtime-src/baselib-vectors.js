// vectors
/*jslint devel: false, browser: true, vars: true, plusplus: true, maxerr: 500, indent: 4 */
(function (baselib, $) {
    "use strict";
    var exports = {};
    baselib.vectors = exports;



    var Vector = function (initialElements) {
        this.elts = initialElements;
        this.mutable = true;
    };

    Vector.makeInstance = function (elts) {
        return new Vector(elts);
    };

    Vector.prototype.length = function () {
        return this.elts.length;
    };

    Vector.prototype.ref = function (k) {
        return this.elts[k];
    };

    Vector.prototype.set = function (k, v) {
        this.elts[k] = v;
    };

    Vector.prototype.equals = function (other, aUnionFind) {
        var i;
        if (other instanceof Vector) {
            if (other.length() !== this.length()) {
                return false;
            }
            for (i = 0; i <  this.length(); i++) {
                if (!(baselib.equality.equals(this.elts[i], other.elts[i], aUnionFind))) {
                    return false;
                }
            }
            return true;
        } else {
            return false;
        }
    };

    Vector.prototype.hashCode = function(depth) {
        var k = baselib.hashes.getEqualHashCode("Vector");
        var i;
        k = baselib.hashes.hashMix(k);
        for (i = 0; i < this.elts.length; i++) {
            k += baselib.hashes.getEqualHashCode(this.elts[i], depth);
            k = baselib.hashes.hashMix(k);
        }
        return k;
    };

    Vector.prototype.toList = function () {
        var ret = baselib.lists.EMPTY, i;
        for (i = this.length() - 1; i >= 0; i--) {
            ret = baselib.lists.makePair(this.elts[i], ret);           
        }       
        return ret;
    };

    Vector.prototype.toWrittenString = function (cache) {
        var texts = [], i;
        cache.put(this, true);
        for (i = 0; i < this.length(); i++) {
            texts.push(baselib.format.toWrittenString(this.ref(i), cache));
        }
        return "#(" + texts.join(" ") + ")";
    };

    Vector.prototype.toDisplayedString = function (cache) {
        var texts = [], i;
        cache.put(this, true);
        for (i = 0; i < this.length(); i++) {
            texts.push(baselib.format.toDisplayedString(this.ref(i), cache));
        }
        return "#(" + texts.join(" ") + ")";
    };

    Vector.prototype.toDomNode = function (params) {
        var node = $('<span/>'), i;
        if (params.getMode() === 'constructor') {
            node.append($('<span/>').text('(').addClass('lParen'));
            node.append($('<span/>').text('vector'));
            for (i = 0; i < this.length(); i++) {
                node.append(" ");
                node.append(params.recur(this.ref(i)));
            }
            node.append($('<span/>').text(')').addClass('rParen'));
        } else {
            node.append($('<span/>').text('#(').addClass('lParen'));
            for (i = 0; i < this.length(); i++) {
                node.append(params.recur(this.ref(i)));
                if (i !== this.length() - 1) {
                    node.append(" ");
                }
            }
            node.append($('<span/>').text(')').addClass('rParen'));
        }
        return node.get(0);
    };


    var isVector = function (x) { return x instanceof Vector; };

    // makeVector: x ... -> vector
    var makeVector = Vector.makeInstance;

    var makeVectorImmutable = function (elts) {
        var v = Vector.makeInstance(elts);
        v.mutable = false;
        return v;
    };



    //////////////////////////////////////////////////////////////////////

    exports.Vector = Vector;
    exports.isVector = isVector;
    exports.makeVector = makeVector;
    exports.makeVectorImmutable = makeVectorImmutable;


}(this.plt.baselib, jQuery));
