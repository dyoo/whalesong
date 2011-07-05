// Arity structure
(function(baselib) {
    var exports = {};
    baselib.readergraph = exports;


    var readerGraph = function(x, objectHash, n) {
        if (typeof(x) === 'object' && objectHash.containsKey(x)) {
	    return objectHash.get(x);
        }

        if (types.isPair(x)) {
	    var consPair = types.cons(x.first, x.rest);
	    objectHash.put(x, consPair);
	    consPair.f = readerGraph(x.first, objectHash, n+1);
	    consPair.r = readerGraph(x.rest, objectHash, n+1);
	    return consPair;
        }

        if (types.isVector(x)) {
	    var len = x.length();
	    var aVector = types.vector(len, x.elts);
	    objectHash.put(x, aVector);	
	    for (var i = 0; i < len; i++) {
	        aVector.elts[i] = readerGraph(aVector.elts[i], objectHash, n+1);
	    }
	    return aVector;
        }

        if (types.isBox(x)) {
	    var aBox = types.box(x.ref());
	    objectHash.put(x, aBox);
	    aBox.val = readerGraph(x.ref(), objectHash, n+1);
	    return aBox;
        }

        if (types.isHash(x)) {
	    throw new Error("make-reader-graph of hash not implemented yet");
        }

        if (types.isStruct(x)) {
	    var aStruct = baselib.clone(x);
	    objectHash.put(x, aStruct);
	    for(var i = 0 ;i < x._fields.length; i++) {
	        x._fields[i] = readerGraph(x._fields[i], objectHash, n+1);
	    }
	    return aStruct;
        }

        if (types.isPlaceholder(x)) {
	    return readerGraph(x.ref(), objectHash, n+1);
        }

        return x;
    };

    exports.readerGraph = readerGraph;

})(this['plt'].baselib);