/*jslint vars: true, white: true, plusplus: false, maxerr: 50, indent: 4 */
(function(baselib) {
    'use strict';

    var exports = {};
    baselib.srclocs = exports;

    // (define-struct srcloc (source line column position span))
    var srcloc = baselib.structs.makeStructureType(
        'srcloc', false, 5, 0, false, false);
    
    var makeSrcloc = srcloc.constructor;

    var isSrcloc = srcloc.predicate;
    var srclocSource = function(x) { return srcloc.accessor(x, 0); };
    var srclocLine = function(x) { return srcloc.accessor(x, 1); };
    var srclocColumn = function(x) { return srcloc.accessor(x, 2); };
    var srclocPosition = function(x) { return srcloc.accessor(x, 3); };
    var srclocSpan = function(x) { return srcloc.accessor(x, 4); };

    //////////////////////////////////////////////////////////////////////
    exports.makeSrcloc = makeSrcloc;
    exports.isSrcloc = isSrcloc;
    exports.srclocSource = srclocSource;
    exports.srclocLine = srclocLine;
    exports.srclocColumn = srclocColumn;
    exports.srclocPosition = srclocPosition;
    exports.srclocSpan = srclocSpan;

}(this.plt.baselib));