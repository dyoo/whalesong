// vectors
/*jslint devel: false, browser: true, vars: true, plusplus: true, maxerr: 500, indent: 4 */
(function (baselib) {
    "use strict";
    var exports = {};
    baselib.vectors = exports;



    var Vector = function (n, initialElements) {
        var i;
        this.elts = [];
        this.elts.length = n;
        if (initialElements) {
            for (i = 0; i < n; i++) {
                this.elts[i] = initialElements[i];
            }
        } else {
            for (i = 0; i < n; i++) {
                this.elts[i] = undefined;
            }
        }
        this.mutable = true;
    };

    Vector.makeInstance = function (n, elts) {
        return new Vector(n, elts);
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

    Vector.prototype.toDomNode = function (cache) {
        var node = document.createElement("span"), i;
        cache.put(this, true);
        node.appendChild(document.createTextNode("#("));
        for (i = 0; i < this.length(); i++) {
            node.appendChild(baselib.format.toDomNode(this.ref(i), cache));
            if (i !== this.length() - 1) {
                node.appendChild(document.createTextNode(" "));
            }
        }
        node.appendChild(document.createTextNode(")"));
        return node;
    };


    var isVector = function (x) { return x instanceof Vector; };

    // makeVector: x ... -> vector
    var makeVector = function (n, elts) {
        return Vector.makeInstance(n, elts);
    };

    var makeVectorImmutable = function (n, elts) {
        var v = Vector.makeInstance(n, elts);
        v.mutable = false;
        return v;
    };



    //////////////////////////////////////////////////////////////////////

    exports.Vector = Vector;
    exports.isVector = isVector;
    exports.makeVector = makeVector;
    exports.makeVectorImmutable = makeVectorImmutable;


}(this.plt.baselib));