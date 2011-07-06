// Structure types

(function(baselib) {
    var exports = {};
    baselib.structs = exports;






    var StructType = function(name,             // string
                              type,             // StructType
                              numberOfArgs,     // number
                              numberOfFields,   // number
                              firstField,
		              applyGuard,
                              constructor,
                              predicate, 
                              accessor,
                              mutator) {
	this.name = name;
	this.type = type;
	this.numberOfArgs = numberOfArgs;
	this.numberOfFields = numberOfFields;
	this.firstField = firstField;

	this.applyGuard = applyGuard;
	this.constructor = constructor;
	this.predicate = predicate;
	this.accessor = accessor;
	this.mutator = mutator;
    };


    StructType.prototype.toString = function(cache) {
	return '#<struct-type:' + this.name + '>';
    };


    StructType.prototype.equals = function(other, aUnionFind) {
	return this === other;
    };





    // guard-function: array string (array -> value)





    // makeStructureType: string StructType number number boolean
    //                    guard-function -> StructType
    //
    // Creates a new structure type.

    var makeStructureType = function(theName,
                                     parentType, 
                                     initFieldCnt, 
                                     autoFieldCnt, 
                                     autoV, 
                                     guard) {

	// If no parent type given, then the parent type is Struct
        parentType = parentType || DEFAULT_PARENT_TYPE;
        guard = guard || DEFAULT_GUARD;



        // rawConstructor creates a new struct type inheriting from
        // the parent, with no guard checks.
        var rawConstructor = function(name, args) {
	    parentType.type.call(this, name, args);
	    for (var i = 0; i < initFieldCnt; i++) {
	        this._fields.push(args[i+parentType.numberOfArgs]);
	    }
	    for (var i = 0; i < autoFieldCnt; i++) {
	        this._fields.push(autoV);
	    }
        };
        rawConstructor.prototype = baselib.heir(parentType.type.prototype);



	// Set type, necessary for equality checking
        rawConstructor.prototype.type = rawConstructor;

	// The structure type consists of the name, its constructor, a
        // record of how many argument it and its parent type contains,
        // the list of autofields, the guard, and functions corresponding
        // to the constructor, the predicate, the accessor, and mutators.
	var newType = new StructType(
            theName,
	    rawConstructor,
	    initFieldCnt + parentType.numberOfArgs,
	    initFieldCnt + autoFieldCnt,
	    parentType.firstField + parentType.numberOfFields,
	    function(args, name, k) {
		return guard(args, name,
			     function(result) {
				 var parentArgs = result.slice(0, parentType.numberOfArgs);
				 var restArgs = result.slice(parentType.numberOfArgs);
				 return parentType.applyGuard(
                                     parentArgs, name,
				     function(parentRes) {
                                         return k( parentRes.concat(restArgs) ); });
			     });
	    },
            // constructor
	    function() {
		var args = [].slice.call(arguments);
		return newType.applyGuard(
                    args,
		    baselib.symbols.Symbol.makeInstance(theName),
		    function(res) { 
                        return new rawConstructor(theName, res); });
	    },

            // predicate
	    function(x) { 
		return x instanceof rawConstructor; 
	    },

            // accessor
	    function(x, i) { return x._fields[i + this.firstField]; },

            // mutator
	    function(x, i, v) { x._fields[i + this.firstField] = v; });
	return newType;
    };






    //////////////////////////////////////////////////////////////////////



    var Struct = function(constructorName, fields) {
	this._constructorName = constructorName; 
	this._fields = [];
    };

    Struct.prototype.toWrittenString = function(cache) { 
	cache.put(this, true);
	var buffer = [];
	buffer.push("(");
	buffer.push(this._constructorName);
	for(var i = 0; i < this._fields.length; i++) {
	    buffer.push(" ");
	    buffer.push(plt.baselib.format.toWrittenString(this._fields[i], cache));
	}
	buffer.push(")");
	return buffer.join("");
    };

    Struct.prototype.toDisplayedString = function(cache) {
	return plt.baselib.format.toWrittenString(this, cache); 
    };

    Struct.prototype.toDomNode = function(params) {
	params.put(this, true);
	var node = document.createElement("div");
	$(node).append(document.createTextNode("("));
	$(node).append(document.createTextNode(this._constructorName));
	for(var i = 0; i < this._fields.length; i++) {
	    $(node).append(document.createTextNode(" "));
	    $(node).append(plt.baselib.format.toDomNode(this._fields[i], params));
	}
	$(node).append(document.createTextNode(")"));
	return node;
    };


    Struct.prototype.equals = function(other, aUnionFind) {
	if ( other.type == undefined ||
	     this.type !== other.type ||
	     !(other instanceof this.type) ) {
	    return false;
	}

	for (var i = 0; i < this._fields.length; i++) {
	    if (! equals(this._fields[i],
			 other._fields[i],
			 aUnionFind)) {
		return false;
	    }
	}
	return true;
    }

    Struct.prototype.type = Struct;














//     // Struct Procedure types
//     var StructProc = function(type, name, numParams, isRest, usesState, impl) {
//         PrimProc.call(this, name, numParams, isRest, usesState, impl);
//         this.type = type;
//     };
//     StructProc.prototype = baselib.heir(PrimProc.prototype);

//     var StructConstructorProc = function() {
//         StructProc.apply(this, arguments);
//     };
//     StructConstructorProc.prototype = baselib.heir(StructProc.prototype);

//     var StructPredicateProc = function() {
//         StructProc.apply(this, arguments);
//     };
//     StructPredicateProc.prototype = baselib.heir(StructProc.prototype);

//     var StructAccessorProc = function() {
//         StructProc.apply(this, arguments);
//     };
//     StructAccessorProc.prototype = baselib.heir(StructProc.prototype);

//     var StructMutatorProc = function() {
//         StructProc.apply(this, arguments);
//     };
//     StructMutatorProc.prototype = baselib.heir(StructProc.prototype);








    // Default structure guard just calls the continuation argument.
    var DEFAULT_GUARD = function(args, name, k) { 
        return k(args); 
    };


    // The default parent type refers to the toplevel Struct.
    var DEFAULT_PARENT_TYPE = { type: Struct,
			        numberOfArgs: 0,
			        numberOfFields: 0,
			        firstField: 0,
			        applyGuard: DEFAULT_GUARD };



    var isStruct = function(x) { return x instanceof Struct; };


















    exports.StructType = StructType;
    exports.Struct = Struct;
    exports.makeStructureType = makeStructureType;
    exports.isStruct = isStruct;

//     exports.StructProc = StructProc;
//     exports.StructConstructorProc = StructConstructorProc;
//     exports.StructPredicateProc = StructPredicateProc;
//     exports.StructAccessorProc = StructAccessorProc;
//     exports.StructMutatorProc = StructMutatorProc;




    


})(this['plt'].baselib);