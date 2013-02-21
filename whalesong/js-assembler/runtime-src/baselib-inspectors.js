/*jslint vars: true, maxerr: 50, indent: 4 */

// Structure types

(function (baselib) {
    'use strict';
    var exports = {};
    baselib.inspectors = exports;


    var Inspector = function () {
    };
    var DEFAULT_INSPECTOR = new Inspector();

    Inspector.prototype.toString = function () {
        return "#<inspector>";
    };

    var isInspector = baselib.makeClassPredicate(Inspector);



    exports.Inspector = Inspector;
    exports.DEFAULT_INSPECTOR = DEFAULT_INSPECTOR;

    exports.isInspector = isInspector;


}(this.plt.baselib));