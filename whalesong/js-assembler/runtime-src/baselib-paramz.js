// Hardcoded parameters.
(function(baselib) {
    'use strict';
    var exports = {};
    baselib.paramz = exports;

    // The parameter keys here must be uninterned symbols, so we explicitly
    // call the symbol constructor here.
    var exceptionHandlerKey = new baselib.symbols.Symbol("exnh");
    var parameterizationKey = new baselib.symbols.Symbol("paramz");
    var breakEnabledKey = new baselib.symbols.Symbol("break-on?");

    exports.exceptionHandlerKey = exceptionHandlerKey;
    exports.parameterizationKey = parameterizationKey;
    exports.breakEnabledKey = breakEnabledKey;
    
})(this['plt'].baselib);
