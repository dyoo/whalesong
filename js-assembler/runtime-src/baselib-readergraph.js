// Arity structure
(function(baselib) {
    var exports = {};
    baselib.readergraph = exports;


    var readerGraph = function(x, objectHash, n) {
        if (typeof(x) === 'object' && objectHash.containsKey(x)) {
	    return objectHash.get(x);
        }

        if (plt.baselib.lists.isPair(x)) {
	    var consPair = plt.baselib.lists.makePair(x.first, x.rest);
	    objectHash.put(x, consPair);
	    consPair.first = readerGraph(x.first, objectHash, n+1);
	    consPair.rest = readerGraph(x.rest, objectHash, n+1);
	    return consPair;
        }

        if (plt.baselib.vectors.isVector(x)) {
	    var len = x.length();
	    var aVector = plt.baselib.vectors.makeVector(len, x.elts);
	    objectHash.put(x, aVector);	
	    for (var i = 0; i < len; i++) {
	        aVector.elts[i] = readerGraph(aVector.elts[i], objectHash, n+1);
	    }
	    return aVector;
        }

        if (plt.baselib.boxes.isBox(x)) {
	    var aBox = plt.baselib.boxes.makeBox(x.ref());
	    objectHash.put(x, aBox);
	    aBox.val = readerGraph(x.ref(), objectHash, n+1);
	    return aBox;
        }

        if (plt.baselib.hashes.isHash(x)) {
	    throw new Error("make-reader-graph of hash not implemented yet");
        }

        if (plt.baselib.structs.isStruct(x)) {
	    var aStruct = baselib.clone(x);
	    objectHash.put(x, aStruct);
	    for(var i = 0 ;i < x._fields.length; i++) {
	        x._fields[i] = readerGraph(x._fields[i], objectHash, n+1);
	    }
	    return aStruct;
        }

        if (plt.baselib.placeholders.isPlaceholder(x)) {
	    return readerGraph(x.ref(), objectHash, n+1);
        }

        return x;
    };

    exports.readerGraph = readerGraph;

})(this['plt'].baselib);