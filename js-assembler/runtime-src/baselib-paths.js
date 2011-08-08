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

    exports.Path = Path;

}(this.plt.baselib));