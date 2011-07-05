// The definitions of the basic types in Whalesong.
//
// Note: this originally came from js-vm, and as a result,
// there's quite a lot of cruft and unused code in this module.
// I need to filter and rip out the values that aren't used in Whalesong.


if (! this['plt']) { this['plt'] = {}; }

// FIXME: there's a circularity between this module and helpers, and that circularly
// should not be there!


(function (scope) {
    //////////////////////////////////////////////////////////////////////
    var types = {};
    scope['types'] = types;




    var getEqHashCode = plt.baselib.hashes.getEqHashCode;
    // makeLowLevelEqHash: -> hashtable
    // Constructs an eq hashtable that uses Moby's getEqHashCode function.
    var makeLowLevelEqHash = plt.baselib.hashes.makeLowLevelEqHash;
    var toWrittenString = plt.baselib.format.toWrittenString;
    var toDisplayedString = plt.baselib.format.toDisplayedString;
    var toDomNode = plt.baselib.format.toDomNode;



    var appendChild = function(parent, child) {
        parent.appendChild(child);
    };




    var Symbol = plt.baselib.symbols.Symbol;
    var Empty = plt.baselib.lists.Empty;
    var Cons = plt.baselib.lists.Cons;







    var isNumber = jsnums.isSchemeNumber;

    var isReal = jsnums.isReal;
    var isRational = jsnums.isRational;
    var isComplex = isNumber;
    var isInteger = jsnums.isInteger;

    var isNatural = function(x) {
        return (jsnums.isExact(x) && isInteger(x) 
	        && jsnums.greaterThanOrEqual(x, 0));
    };
    var isNonNegativeReal = function(x) {
	return isReal(x) && jsnums.greaterThanOrEqual(x, 0);
    };

    var isString = plt.baselib.strings.isString;

    var equals = plt.baselib.equality.equals;
    
    // isList: Any -> Boolean
    // Returns true if x is a list (a chain of pairs terminated by EMPTY).
    var isList = function(x) { 
	while (x !== Empty.EMPTY) {
	    if (x instanceof Cons){
		x = x.rest;
	    } else {
		return false;
	    }
	}
	return true;
    };


    var makeList = function() {
        var result = Empty.EMPTY;
        for(var i = arguments.length-1; i >= 0; i--) {
	    result = Cons.makeInstance(arguments[i], result);
        }
        return result;
    };


    var makeVector = function() {
        return Vector.makeInstance(arguments.length, arguments);
    };


    var makeVectorImmutable = function() {
        var v = Vector.makeInstance(arguments.length, arguments);
        v.mutable = false;
        return v;
    };


    var makeString = function(s) {
	if (s instanceof plt.baselib.strings.Str) {
	    return s;
	}
	else if (s instanceof Array) {
            //		for (var i = 0; i < s.length; i++) {
            //			if ( typeof s[i] !== 'string' || s[i].length != 1 ) {
            //				return undefined;
            //			}
            //		}
	    return plt.baselib.strings.Str.makeInstance(s);
	}
	else if (typeof s === 'string') {
	    return plt.baselib.strings.Str.fromString(s);
	}
	else {
	    throw types.internalError('makeString expects and array of 1-character strings or a string;' +
				      ' given ' + s.toString(),
				      false);
	}
    };


    var makeHashEq = function(lst) {
	var newHash = new plt.baselib.hashes.EqHashTable();
	while ( !isEmpty(lst) ) {
	    newHash.hash.put(lst.first.first, lst.first.rest);
	    lst = lst.rest;
	}
	return newHash;
    };


    var makeHashEqual = function(lst) {
	var newHash = new plt.baselib.hashes.EqualHashTable();
	while ( !isEmpty(lst) ) {
	    newHash.hash.put(lst.first.first, lst.first.rest);
	    lst = lst.rest;
	}
	return newHash;
    };


    var Color = plt.baselib.structs.makeStructureType(
        'color', false, 3, 0, false, false);






    //////////////////////////////////////////////////////////////////////

//     var JsValue = function(name, val) {
// 	this.name = name;
// 	this.val = val;
//     };

//     JsValue.prototype.toString = function() {
// 	return '#<js-value:' + typeof(this.val) + ':' + this.name + '>';
//     };

//     JsValue.prototype.toDomNode = function(cache) {
// 	return toDomNode(this.val, cache);
//     };

//     JsValue.prototype.equals = function(other, aUnionFind) {
// 	return (this.val === other.val);
//     };

//     // unbox: jsvalue -> any
//     // Unwraps the value out of the JsValue box.
//     JsValue.prototype.unbox = function()  {
//         return this.val;
//     };



//     var WrappedSchemeValue = function(val) {
// 	this.val = val;
//     };

//     WrappedSchemeValue.prototype.toString = function() { return toString(this.val); };
//     WrappedSchemeValue.prototype.toWrittenString = function(cache) { return toWrittenString(this.val, cache); };
//     WrappedSchemeValue.prototype.toDisplayedString = function(cache) { return toDisplayedString(this.val, cache); };


//     // unbox: jsvalue -> any
//     // Unwraps the value out of the WrappedSchemeValue box.
//     WrappedSchemeValue.prototype.unbox = function() {
//         return this.val;
//     };


    //////////////////////////////////////////////////////////////////////

//     var WorldConfig = function(startup, shutdown, startupArgs) {
// 	this.startup = startup;
// 	this.shutdown = shutdown;
//         this.startupArgs = startupArgs;
//     };

//     WorldConfig.prototype.toString = function() {
// 	return '#<world-config>';
//     };

//     WorldConfig.prototype.equals = function(other, aUnionFind) {
// 	return ( equals(this.startup, other.startup, aUnionFind) &&
// 		 equals(this.shutdown, other.shutdown, aUnionFind) &&
// 		 equals(this.shutdownArg, other.shutdownArg, aUnionFind) &&
// 		 equals(this.restartArg, other.restartArg, aUnionFind) );
//     };


//     var Effect = plt.baselib.structs.makeStructureType('effect', false, 0, 0, false, false);
//     Effect.type.prototype.invokeEffect = function() {
// 	helpers.raise(types.incompleteExn(
// 	    types.exnFail,
// 	    'effect type created without using make-effect-type',
// 	    []));
//     };


//     var makeEffectType = function(name, superType, initFieldCnt, impl, guard) {
// 	if ( !superType ) {
// 	    superType = Effect;
// 	}
	
// 	var newType = plt.baselib.structs.makeStructureType(name, superType, initFieldCnt, 0, false, guard);
// 	var lastFieldIndex = newType.firstField + newType.numberOfFields;

// 	newType.type.prototype.invokeEffect = function(aBigBang, k) {
// 	    var schemeChangeWorld = new PrimProc('update-world', 1, false, false,
// 			                         function(worldUpdater) {
// 				                     //helpers.check(worldUpdater, helpers.procArityContains(1),
// 					             //              'update-world', 'procedure (arity 1)', 1);
				                     
// 				                     return new INTERNAL_PAUSE(
// 					                 function(caller, onSuccess, onFail) {
// 						             aBigBang.changeWorld(function(w, k2) {
// 								 caller(worldUpdater,
// 									[w], k2,
// 									function(e) { throw e; },
// 									'change-world (effect)');
// 							     },
// 								                  function() { onSuccess(VOID_VALUE, 'restarting (change-world (effect))'); });
// 					                 });
// 			                         });

// 	    var args = this._fields.slice(0, lastFieldIndex);
// 	    args.unshift(schemeChangeWorld);
// 	    return aBigBang.caller(impl, args, k, function(e) { throw e; }, 'invoking effect ' + name);
// 	}

// 	return newType;
//     };


//     var RenderEffect = plt.baselib.structs.makeStructureType('render-effect', false, 0, 0, false, false);
//     RenderEffect.type.prototype.callImplementation = function(caller, k) {
// 	helpers.raise(types.incompleteExn(
// 	    types.exnFail,
// 	    'render effect created without using make-render-effect-type',
// 	    []));
//     };

//     var makeRenderEffectType = function(name, superType, initFieldCnt, impl, guard) {
// 	if ( !superType ) {
// 	    superType = RenderEffect;
// 	}
	
// 	var newType = plt.baselib.structs.makeStructureType(name, superType, initFieldCnt, 0, false, guard);
// 	var lastFieldIndex = newType.firstField + newType.numberOfFields;

// 	newType.type.prototype.callImplementation = function(caller, k) {
// 	    var args = this._fields.slice(0, lastFieldIndex);
// 	    caller(impl, args, k);
// 	}

// 	return newType;
//     };

    //////////////////////////////////////////////////////////////////////









    //////////////////////////////////////////////////////////////////////





    //////////////////////////////////////////////////////////////////////


    //////////////////////////////////////////////////////////////////////

    //////////////////////////////////////////////////////////////////////

    //////////////////////////////////////////////////////////////////////


    //////////////////////////////////////////////////////////////////////

//     var makeOptionPrimitive = function(name,
// 				       numArgs,
// 				       defaultVals,
// 				       usesState,
// 				       bodyF) {
//         var makeNthPrimitive = function(n) {
// 	    return new PrimProc(name,
// 			        numArgs + n,
// 			        false,
// 			        usesState,
// 			        function() {
// 				    var expectedNumArgs = numArgs + n + (usesState ? 1 : 0);
// 				    assert.equal(arguments.length,
// 					         expectedNumArgs);
// 				    var args = [arguments];
// 				    for (var i = 0; i < arguments.length; i++) {
// 				        args.push(arguments[i]);
// 				    }
// 				    var startDefaults = i - numArgs - (usesState ? 1 : 0);
// 				    return bodyF.apply(
// 				        bodyF,
// 				        args.concat(defaultVals.slice(startDefaults)));
// 			        });
//         };
	
//         var cases = [];
//         for (var i = 0; i <= defaultVals.length; i++) {
// 	    cases.push(makeNthPrimitive(i));
//         }
//         return new CasePrimitive(name, cases);
//     };












    //////////////////////////////////////////////////////////////////////


    // INTERNAL_CALL
    // used for interaction between the Primitives and the interpreter (callPrimitiveProcedure).
    // Don't confuse this with CallControl.
//     var INTERNAL_CALL = function(operator, operands, k) {
//         this.operator = operator;
//         this.operands = operands;
//         this.k = k;
//     };

//     // INTERNAL_PAUSE
//     // used for interaction between the Primitive functions and the
//     // interpreter.
//     // Halts the interpreter, but passing onPause the functions necessary
//     // to restart computation.
//     var INTERNAL_PAUSE = function(onPause) {
//         this.onPause = onPause;
//     };



    //////////////////////////////////////////////////////////////////////


//     // ContinuationPromptTag: symbol | false -> ContinuationPromptTag
//     var ContinuationPromptTag = function(sym) {
//         this.sym = sym;
//     };

//     var defaultContinuationPromptTag = new ContinuationPromptTag();

//     var defaultContinuationPromptTagHandler = new PrimProc(
//         'default-continuation-prompt-tag-handler',
//         1,
//         false, 
//         true,
//         function(aState, thunk) {
// 	    aState.pushControl(
// 	        new control.ApplicationControl(
// 		    new control.ConstantControl(thunk), 
// 		    []));
//         });


    //////////////////////////////////////////////////////////////////////








    //////////////////////////////////////////////////////////////////////
    // Exports



    types.symbol = Symbol.makeInstance;
    types.rational = jsnums.makeRational;
    types.floatpoint = jsnums.makeFloat;
    types.complex = jsnums.makeComplex;
    types.bignum = jsnums.makeBignum;
    types.list = makeList;
    types.vector = makeVector;
    types.vectorImmutable = makeVectorImmutable;
    types.regexp = function(p) { return new plt.baselib.regexps.RegularExpression(p) ; }
    types.byteRegexp = function(p) { return new plt.baselib.regexps.ByteRegularExpression(p) ; }
    types.character = plt.baselib.chars.Char.makeInstance;
    types['string'] = makeString;
    types.placeholder = function(x) { return new plt.baselib.placeholders.Placeholder(x); };
    types.box = function(x) { return new plt.baselib.boxes.Box(x, true); };
    types.boxImmutable = function(x) { return new plt.baselib.boxes.Box(x, false); };
    types.path = function(x) { return new plt.baselib.paths.Path(x); };
    types.bytes = function(x, mutable) { return new plt.baselib.bytes.Bytes(x, mutable); };
    types.bytesImmutable = function(x) { return new plt.baselib.bytes.Bytes(x, false); };
    types.keyword = function(k) { return new plt.baselib.keywords.Keyword(k); };
    types.pair = function(x, y) { return plt.baselib.lists.Cons.makeInstance(x, y); };
    types.hash = makeHashEqual;
    types.hashEq = makeHashEq;
//     types.jsValue = function(name, val) { return new JsValue(name, val); };
//     types.wrappedSchemeValue = function(val) { return new WrappedSchemeValue(val); };


    types.color = Color.constructor;
    types.colorRed = function(x) { return Color.accessor(x, 0); };
    types.colorGreen = function(x) { return Color.accessor(x, 1); };
    types.colorBlue = function(x) { return Color.accessor(x, 2); };



    types.FALSE = false;
    types.TRUE = true;
    types.EMPTY = Empty.EMPTY;

    types.equals = equals;
    types.isNumber = isNumber;

    types.isReal = jsnums.isReal;
    types.isRational = jsnums.isRational;
    types.isComplex = isNumber;
    types.isInteger = jsnums.isInteger;
    types.isNatural = isNatural;
    types.isNonNegativeReal = isNonNegativeReal;


    types.isSymbol = function(x) { return x instanceof Symbol; };
    types.isChar = function(x) { return x instanceof plt.baselib.chars.Char; };
    types.isString = isString;
    types.isPair = function(x) { return x instanceof Cons; };
    types.isList = isList;
    types.isEmpty = function(x) { return x === Empty.EMPTY; };
    types.isVector = function(x) { return x instanceof Vector; };
    types.isBox = function(x) { return x instanceof plt.baselib.boxes.Box; };
    types.isPlaceholder = function(x) { return x instanceof plt.baselib.placeholders.Placeholder; };
    types.isHash = function(x) { return (x instanceof plt.baselib.hashes.EqHashTable ||
				         x instanceof plt.baselib.hashes.EqualHashTable); };
    types.isByteString = function(x) { return x instanceof plt.baselib.bytes.Bytes; };
    types.isStruct = function(x) { return x instanceof Struct; };
    types.isColor = Color.predicate;

//     types.isFunction = function(x) {
// 	return (x instanceof PrimProc);
//     };


//     types.isJsValue = function(x) { return x instanceof JsValue; };
//     types.isWrappedSchemeValue = function(x) { return x instanceof WrappedSchemeValue; };

    types.cons = Cons.makeInstance;

    types.VOID = plt.baselib.constants.VOID_VALUE;
    types.EOF = plt.baselib.constants.EOF_VALUE;

//     types.ContinuationPromptTag = ContinuationPromptTag;
//     types.defaultContinuationPromptTag = defaultContinuationPromptTag;
//     types.defaultContinuationPromptTagHandler = defaultContinuationPromptTagHandler;
//     types.makeOptionPrimitive = makeOptionPrimitive;

//     types.internalCall = function(op, args, k) { return new INTERNAL_CALL(op, args, k); };
//     types.isInternalCall = function(x) { return (x instanceof INTERNAL_CALL); };
//     types.internalPause = function(onPause) { return new INTERNAL_PAUSE(onPause) };
//     types.isInternalPause = function(x) { return (x instanceof INTERNAL_PAUSE); };


    types.continuationMarkSet = function(dict) { return new plt.baselib.contmarks.ContinuationMarkSet(dict); };
    types.isContinuationMarkSet = function(x) { return x instanceof plt.baselib.contmarks.ContinuationMarkSet; };
//     types.isContinuationPromptTag = function(x) { return x instanceof ContinuationPromptTag; };


    types.Box = plt.baselib.boxes.Box;
    types.Placeholder = plt.baselib.placeholders.Placeholder;



    types.isStructType = function(x) { return x instanceof plt.baselib.structs.StructType; };

//     types.StructProc = StructProc;
//     types.StructConstructorProc = StructConstructorProc;
//     types.StructPredicateProc = StructPredicateProc;
//     types.StructAccessorProc = StructAccessorProc;
//     types.StructMutatorProc = StructMutatorProc;




    types.makeLowLevelEqHash = makeLowLevelEqHash;



    ///////////////////////////////////////
    // World-specific exports

//     // big bang info to be passed into a make-world-config startup argument
//     var BigBangInfo = plt.baselib.structs.makeStructureType('bb-info', false, 2, 0, false,
// 			                function(args, name, k) {
// 				            //helpers.check(args[0], helpers.procArityContains(1), name, 'procedure (arity 1)', 1);
// 				            //helpers.check(args[1], types.isJsValue, name, 'js-object', 2);
// 				            return k(args);
// 			                });
//     types.BigBangInfo = BigBangInfo;
//     types.makeBigBangInfo = BigBangInfo.constructor;
//     types.isBigBangInfo = BigBangInfo.predicate;
//     types.bbInfoChangeWorld = function(info) { return BigBangInfo.accessor(info, 0); };
//     types.bbInfoToplevelNode = function(info) { return BigBangInfo.accessor(info, 1); };



    // World config information for user-defined configurations
//     types.worldConfig = function(startup, shutdown, pause, restart) { return new WorldConfig(startup, shutdown, pause, restart); };
//     types.isWorldConfig = function(x) { return x instanceof WorldConfig; };


    // exporting information to create effect types
//     types.makeEffectType = makeEffectType;
//     types.isEffectType = function(x) {
// 	return ((x instanceof plt.baselib.structs.StructType)&& x.type.prototype.invokeEffect) ? true : false;
//     };

//     types.isEffect = Effect.predicate;


    // exporting functions to create render effect types
//     types.makeRenderEffectType = makeRenderEffectType;
//     types.isRenderEffectType = function(x) {
// 	return (x instanceof plt.baselib.structs.StructType && x.type.prototype.callImplementation) ? true : false;
//     };

//     types.isRenderEffect = RenderEffect.predicate;




    scope.link.announceReady('types');
})(this['plt']);

