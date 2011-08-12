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
        return String(this.path);
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