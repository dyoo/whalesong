// Helper functions for argument checking.

(function(baselib) {
    var exports = {};
    baselib.check = exports;

    var EMPTY = plt.baselib.lists.EMPTY;
    var isPair = plt.baselib.lists.isPair;
    var makeLowLevelEqHash = plt.baselib.hashes.makeLowLevelEqHash;


    //////////////////////////////////////////////////////////////////////

    var makeCheckArgumentType = function(predicate, predicateName) {
	return function(MACHINE, callerName, position) {
	    testArgument(
		MACHINE,
		predicateName,
		predicate,
		MACHINE.env[MACHINE.env.length - 1 - position],
		position,
		callerName);
	    return MACHINE.env[MACHINE.env.length - 1 - position];
	}
    };

    var makeCheckParameterizedArgumentType = function(parameterizedPredicate, 
                                                      parameterizedPredicateName) {
	return function(MACHINE, callerName, position) {
            var args = [];
            for (var i = 3; i < arguments.length; i++) {
                args.push(arguments[i]);
            }
	    testArgument(
		MACHINE,
		parameterizedPredicateName.apply(null, args),
		function(x) {
                    return parameterizedPredicate.apply(null, [x].concat(args));
                },
		MACHINE.env[MACHINE.env.length - 1 - position],
		position,
		callerName);
	    return MACHINE.env[MACHINE.env.length - 1 - position];
	}
    };





    var makeCheckListofArgumentType = function(predicate, predicateName) {
        var listPredicate = function(x) {
            var seen = makeLowLevelEqHash();
            while (true) {
                if (x === EMPTY){
                    return true;
                }

                if (!isPair(x)) {
                    return false;
                }

                if(seen.containsKey(x)) {
                    // raise an error? we've got a cycle!
                    return false
                }

                if (! predicate(x.first)) {
                    return false;
                }
                
                seen.put(x, true);
                x = x.rest;
            }
        };
	return function(MACHINE, callerName, position) {
	    testArgument(
		MACHINE,
		'list of ' + predicateName,
		listPredicate,
		MACHINE.env[MACHINE.env.length - 1 - position],
		position,
		callerName);
	    return MACHINE.env[MACHINE.env.length - 1 - position];
	}
    };






    // testArgument: (X -> boolean) X number string string -> boolean
    // Produces true if val is true, and otherwise raises an error.
    var testArgument = function(MACHINE,
				expectedTypeName,
				predicate, 			    
				val, 
				index, 
				callerName) {
	if (predicate(val)) {
	    return true;
	} else {
	    plt.baselib.exceptions.raiseArgumentTypeError(MACHINE, 
                                                          callerName,
                                                          expectedTypeName,
				                          index,
				                          val);
	}
    };

    var testArity = function(callerName, observed, minimum, maximum) {
	if (observed < minimum || observed > maximum) {
	    plt.baselib.exceptions.raise(
                MACHINE, new Error(callerName + ": expected at least " + minimum
				   + " arguments "
				   + " but received " + observed));

	}
    };




    var checkOutputPort = makeCheckArgumentType(
        plt.baselib.ports.isOutputPort,
        'output port');

    var checkSymbol = makeCheckArgumentType(
	plt.baselib.symbols.isSymbol,
	'symbol');

    var checkString = makeCheckArgumentType(
        plt.baselib.strings.isString,
        'string');
    
    var checkProcedure = makeCheckArgumentType(
        plt.baselib.functions.isProcedure,
        'procedure');

    var checkNumber = makeCheckArgumentType(
        plt.baselib.numbers.isNumber,
        'number');

    var checkReal = makeCheckArgumentType(
        plt.baselib.numbers.isReal,
        'real');

    var checkNatural = makeCheckArgumentType(
        plt.baselib.numbers.isNatural,
        'natural');

    var checkNaturalInRange = makeCheckParameterizedArgumentType(
        function(x, a, b) {
            return plt.baselib.numbers.isNatural(x) &&
        }
        function(a, b) {
            return plt.baselib.format('natural between ~a and ~a', [a, b])
        });

    var checkInteger = makeCheckArgumentType(
        plt.baselib.numbers.isInteger,
        'integer');

    var checkRational = makeCheckArgumentType(
        plt.baselib.numbers.isRational,
        'rational');

    var checkNonNegativeReal = makeCheckArgumentType(
        plt.baselib.numbers.isNonNegativeReal,
        'non-negative real');

    var checkPair = makeCheckArgumentType(
        plt.baselib.lists.isPair,
        'pair');

    var checkList = makeCheckArgumentType(
        plt.baselib.lists.isList,
        'list');

    var checkVector = makeCheckArgumentType(
        plt.baselib.vectors.isVector,
        'vector');

    var checkBoolean = makeCheckArgumentType(
        function(x) { return x === true || x === false; },
        'boolean');

    var checkBox = makeCheckArgumentType(
        plt.baselib.boxes.isBox,
        'box');

    var checkMutableBox = makeCheckArgumentType(
        plt.baselib.boxes.isMutableBox,
        'mutable box');

    var checkInspector = makeCheckArgumentType(
        plt.baselib.inspectors.isInspector,
        'inspector');


    var checkByte = makeCheckArgumentType(
        plt.baselib.numbers.isByte,
        'byte');





    //////////////////////////////////////////////////////////////////////


    exports.testArgument = testArgument;
    exports.testArity = testArity;
    exports.makeCheckArgumentType = makeCheckArgumentType;
    exports.makeCheckParameterizedArgumentType = makeCheckParameterizedArgumentType;
    exports.makeCheckListofArgumentType = makeCheckListofArgumentType;

    exports.checkOutputPort = checkOutputPort;
    exports.checkString = checkString;
    exports.checkSymbol = checkSymbol;
    exports.checkProcedure = checkProcedure;
    exports.checkNumber = checkNumber;
    exports.checkReal = checkReal;
    exports.checkNonNegativeReal = checkNonNegativeReal;
    exports.checkNatural = checkNatural;
    exports.checkNaturalInRange = checkNaturalInRange;
    exports.checkInteger = checkInteger;
    exports.checkRational = checkRational;
    exports.checkPair = checkPair;
    exports.checkList = checkList;
    exports.checkVector = checkVector;
    exports.checkBox = checkBox;
    exports.checkMutableBox = checkMutableBox;
    exports.checkInspector = checkInspector;
    exports.checkByte = checkByte;
    exports.checkBoolean = checkBoolean;



})(this['plt'].baselib);
