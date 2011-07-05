(function(baselib) {
    var exports = {};
    baselib.regexps = exports;


    // Regular expressions.

    var RegularExpression = function(pattern) {
        this.pattern = pattern;
    };


    var ByteRegularExpression = function(pattern) {
        this.pattern = pattern;
    };

    //////////////////////////////////////////////////////////////////////

    exports.RegularExpression = RegularExpression;
    exports.ByteRegularExpression = ByteRegularExpression;

})(this['plt'].baselib);