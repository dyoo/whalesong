// Structure types

(function(baselib) {
    var exports = {};
    baselib.inspectors = exports;


    var Inspector = function() {
    };
    var DEFAULT_INSPECTOR = new Inspector();

    Inspector.prototype.toString = function() {
        return "#<inspector>";
    };



    exports.Inspector = Inspector;
    exports.DEFAULT_INSPECTOR = DEFAULT_INSPECTOR;


})(this['plt'].baselib);