// Helper functions for argument checking.

(function(baselib) {
    var exports = {};
    baselib.check = exports;


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

    var checkString = makeCheckArgumentType(
        plt.baselib.strings.isString,
        'string');
    
    var checkFunction = makeCheckArgumentType(
        plt.baselib.functions.isFunction,
        'function');

    var checkNumber = makeCheckArgumentType(
        plt.baselib.numbers.isNumber,
        'number');

    var checkReal = makeCheckArgumentType(
        plt.baselib.numbers.isReal,
        'real');

    var checkNatural = makeCheckArgumentType(
        plt.baselib.numbers.isNatural,
        'natural');

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

    var checkBox = makeCheckArgumentType(
        plt.baselib.boxes.isBox,
        'box');
    var checkMutableBox = makeCheckArgumentType(
        plt.baselib.boxes.isMutableBox,
        'mutable box');





    //////////////////////////////////////////////////////////////////////


    exports.testArgument = testArgument;
    exports.testArity = testArity;
    exports.makeCheckArgumentType = makeCheckArgumentType;

    exports.checkOutputPort = checkOutputPort;
    exports.checkString = checkString;
    exports.checkFunction = checkFunction;
    exports.checkNumber = checkNumber;
    exports.checkReal = checkReal;
    exports.checkNonNegativeReal = checkNonNegativeReal;
    exports.checkNatural = checkNatural;
    exports.checkInteger = checkInteger;
    exports.checkRational = checkRational;
    exports.checkPair = checkPair;
    exports.checkList = checkList;
    exports.checkVector = checkVector;
    exports.checkBox = checkBox;
    exports.checkMutableBox = checkMutableBox;


})(this['plt'].baselib);