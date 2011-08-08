// Placeholders
/*jslint browser: true, unparam: true, vars: true, maxerr: 50, indent: 4 */
(function (baselib) {
    'use strict';
    var exports = {};
    baselib.placeholders = exports;


    // Placeholders: same thing as boxes.  Distinct type just to support make-reader-graph.

    var Placeholder = function (x, mutable) {
        this.val = x;
    };

    Placeholder.prototype.ref = function () {
        return this.val;
    };

    Placeholder.prototype.set = function (newVal) {
        this.val = newVal;
    };

    Placeholder.prototype.toString = function (cache) {
        return "#<placeholder>";
    };

    Placeholder.prototype.toWrittenString = function (cache) {
        return "#<placeholder>";
    };

    Placeholder.prototype.toDisplayedString = function (cache) {
        return "#<placeholder>";
    };

    Placeholder.prototype.toDomNode = function (cache) {
        var parent = document.createElement("span");
        parent.appendChild(document.createTextNode('#<placeholder>'));
        return parent;
    };

    Placeholder.prototype.equals = function (other, aUnionFind) {
        return ((other instanceof Placeholder) &&
                baselib.equality.equals(this.val, other.val, aUnionFind));
    };


    var isPlaceholder = function (x) { 
        return x instanceof Placeholder; 
    };
    


    //////////////////////////////////////////////////////////////////////
    exports.Placeholder = Placeholder;
    exports.isPlaceholder = isPlaceholder;



}(this.plt.baselib));