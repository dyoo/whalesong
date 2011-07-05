(function(baselib) {
    var exports = {};
    baselib.paths = exports;

    // Paths

    var Path = function(p) {
        this.path = p;
    };

    Path.prototype.toString = function() {
        return String(this.path);
    };

    //////////////////////////////////////////////////////////////////////

    exports.Path = Path;

})(this['plt'].baselib);