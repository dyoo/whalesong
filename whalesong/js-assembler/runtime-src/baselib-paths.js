/*jslint vars: true, maxerr: 50, indent: 4 */


(function (baselib) {
    'use strict';
    var exports = {};
    baselib.paths = exports;

    // Paths

    var Path = function (p) {
        this.path = p;
    };

    Path.prototype.toString = function () {
        return "#<path:" + String(this.path) + ">";
    };


    Path.prototype.equals = function(other, aUnionFind) {
        return (other instanceof Path &&
                this.path === other.path);
    };

    Path.prototype.hashCode = function(depth) {
        var k = baselib.hashes.getEqualHashCode("path");
        k += baselib.hashes.getEqualHashCode(this.path, depth);
        k = baselib.hashes.hashMix(k);
        return k;
    };


    //////////////////////////////////////////////////////////////////////

    var makePath = function (p) {
        return new Path(p);
    };

    var isPath = baselib.makeClassPredicate(Path);



    exports.Path = Path;
    exports.makePath = makePath;
    exports.isPath = isPath;

}(this.plt.baselib));