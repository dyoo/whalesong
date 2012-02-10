/*jslint vars: true, nomen: true, plusplus: true, maxerr: 50, indent: 4 */
// Arity structure
(function (baselib) {
    'use strict';
    var exports = {};
    baselib.readergraph = exports;


    var readerGraph = function (x, objectHash, n) {
        var i;
        if (typeof (x) === 'object' && objectHash.containsKey(x)) {
            return objectHash.get(x);
        }

        if (baselib.lists.isPair(x)) {
            var consPair = baselib.lists.makePair(x.first, x.rest);
            objectHash.put(x, consPair);
            consPair.first = readerGraph(x.first, objectHash, n + 1);
            consPair.rest = readerGraph(x.rest, objectHash, n + 1);
            return consPair;
        }

        if (baselib.vectors.isVector(x)) {
            var len = x.length();
            var aVector = baselib.vectors.makeVector(x.elts.slice(0));
            objectHash.put(x, aVector); 
            for (i = 0; i < len; i++) {
                aVector.elts[i] = readerGraph(aVector.elts[i], objectHash, n + 1);
            }
            return aVector;
        }

        if (baselib.boxes.isBox(x)) {
            var aBox = baselib.boxes.makeBox(x.ref());
            objectHash.put(x, aBox);
            aBox.val = readerGraph(x.ref(), objectHash, n + 1);
            return aBox;
        }

        if (baselib.hashes.isHash(x)) {
            throw new Error("make-reader-graph of hash not implemented yet");
        }

        if (baselib.structs.isStruct(x)) {
            var aStruct = baselib.clone(x);
            objectHash.put(x, aStruct);
            for (i = 0; i < x._fields.length; i++) {
                x._fields[i] = readerGraph(x._fields[i], objectHash, n + 1);
            }
            return aStruct;
        }

        if (baselib.placeholders.isPlaceholder(x)) {
            return readerGraph(x.ref(), objectHash, n + 1);
        }

        return x;
    };

    exports.readerGraph = readerGraph;

}(this.plt.baselib));