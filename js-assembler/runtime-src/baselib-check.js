/*jslint vars: true, white: true, plusplus: true, maxerr: 50, indent: 4 */


// Helper functions for argument checking.

(function (baselib) {
    'use strict';
    var exports = {};
    baselib.check = exports;

    var EMPTY = baselib.lists.EMPTY;
    var isPair = baselib.lists.isPair;
    var isList = baselib.lists.isList;
    var makeLowLevelEqHash = baselib.hashes.makeLowLevelEqHash;


    //////////////////////////////////////////////////////////////////////

    // testArgument: (X -> boolean) X number string string -> boolean
    // Produces the argument value the predicate is true, and otherwise raises an error.
    var testArgument = function (MACHINE,
                                 expectedTypeName,
                                 predicate,                          
                                 val, 
                                 index, 
                                 callerName) {
        if (predicate(val)) {
            return val;
        } else {
            if (typeof(expectedTypeName) === 'function') { 
                expectedTypeName = expectedTypeName(); 
            }
            baselib.exceptions.raiseArgumentTypeError(MACHINE, 
                                                      callerName,
                                                      expectedTypeName,
                                                      index,
                                                      val);
        }
    };


    var makeCheckArgumentType = function (predicate, predicateName) {
        return function (MACHINE, callerName, position) {
            return testArgument(
                MACHINE,
                predicateName,
                predicate,
                MACHINE.e[MACHINE.e.length - 1 - position],
                position,
                callerName);
        };
    };

    var makeCheckParameterizedArgumentType = function (parameterizedPredicate, 
                                                       parameterizedPredicateName) {
        return function (MACHINE, callerName, position) {
            var args = [], i;
            for (i = 3; i < arguments.length; i++) {
                args.push(arguments[i]);
            }
            return testArgument(
                MACHINE,
                function () { return parameterizedPredicateName.apply(null, args); },
                function (x) {
                    return parameterizedPredicate.apply(null, [x].concat(args));
                },
                MACHINE.e[MACHINE.e.length - 1 - position],
                position,
                callerName);
        };
    };





    var makeCheckListofArgumentType = function (predicate, predicateName) {
        var listPredicate = function (x) {
            if (! isList(x)) { return false; }
            while (true) {
                if (x === EMPTY){
                    return true;
                }
                if (! predicate(x.first)) {
                    return false;
                }
                x = x.rest;
            }
        };
        return function (MACHINE, callerName, position) {
            return testArgument(
                MACHINE,
                'list of ' + predicateName,
                listPredicate,
                MACHINE.e[MACHINE.e.length - 1 - position],
                position,
                callerName);
        };
    };







    var testArity = function (MACHINE, callerName, observed, minimum, maximum) {
        if (observed < minimum || observed > maximum) {
            baselib.exceptions.raise(
                MACHINE, 
                baselib.exceptions.ExnFailContractArity.constructor(
                    [callerName + ": expected at least " + minimum
                     + " arguments "
                     + " but received " + observed,
                     MACHINE.captureContinuationMarks()]));
        }
    };




    var checkOutputPort = makeCheckArgumentType(
        baselib.ports.isOutputPort,
        'output port');

    var checkInputPort = makeCheckArgumentType(
        baselib.ports.isInputPort,
        'input port');

    var checkSymbol = makeCheckArgumentType(
        baselib.symbols.isSymbol,
        'symbol');

    var checkString = makeCheckArgumentType(
        baselib.strings.isString,
        'string');

    var checkSymbolOrString = makeCheckArgumentType(
        function(x) { return (baselib.symbols.isSymbol(x) || 
                              baselib.strings.isString(x)); },
        'symbol or string');

    var checkMutableString = makeCheckArgumentType(
        baselib.strings.isMutableString,
        'mutable string');

    var checkChar = makeCheckArgumentType(
        baselib.chars.isChar,
        'character');

    var checkProcedure = makeCheckArgumentType(
        baselib.functions.isProcedure,
        'procedure');

    var checkNumber = makeCheckArgumentType(
        baselib.numbers.isNumber,
        'number');

    var checkReal = makeCheckArgumentType(
        baselib.numbers.isReal,
        'real');

    var checkNatural = makeCheckArgumentType(
        baselib.numbers.isNatural,
        'natural');

    var checkByte = makeCheckArgumentType(
        baselib.numbers.isByte,
        'byte');

    var checkBytes = makeCheckArgumentType(
        baselib.bytes.isBytes,
        'bytes');

    var checkNaturalInRange = makeCheckParameterizedArgumentType(
        function (x, a, b) {
            if (! baselib.numbers.isNatural(x)) { return false; }
            return (baselib.numbers.lessThanOrEqual(a, x) &&
                    baselib.numbers.lessThan(x, b));
        },
        function (a, b) {
            return baselib.format.format('natural between ~a and ~a', [a, b]);
        });

    var checkInteger = makeCheckArgumentType(
        baselib.numbers.isInteger,
        'integer');

    var checkRational = makeCheckArgumentType(
        baselib.numbers.isRational,
        'rational');

    var checkNonNegativeReal = makeCheckArgumentType(
        baselib.numbers.isNonNegativeReal,
        'non-negative real');

    var checkPair = makeCheckArgumentType(
        baselib.lists.isPair,
        'pair');

    var checkList = makeCheckArgumentType(
        isList,
        'list');

    var checkVector = makeCheckArgumentType(
        baselib.vectors.isVector,
        'vector');

    var checkBoolean = makeCheckArgumentType(
        function (x) { return x === true || x === false; },
        'boolean');

    var checkBox = makeCheckArgumentType(
        baselib.boxes.isBox,
        'box');

    var checkMutableBox = makeCheckArgumentType(
        baselib.boxes.isMutableBox,
        'mutable box');

    var checkInspector = makeCheckArgumentType(
        baselib.inspectors.isInspector,
        'inspector');


    var checkPlaceholder = makeCheckArgumentType(
        baselib.placeholders.isPlaceholder,
        'placeholder');


    var checkSrcloc = makeCheckArgumentType(
        baselib.srclocs.isSrcloc,
        'srcloc');

    var checkContinuationMarkSet = makeCheckArgumentType(
        baselib.contmarks.isContinuationMarkSet,
        'continuation mark set');

    var checkContinuationPromptTag = makeCheckArgumentType(
        baselib.contmarks.isContinuationPromptTag,
        'continuation prompt tag');

    var checkExn = makeCheckArgumentType(
        baselib.exceptions.isExn,
        'exn');

    var checkHash = makeCheckArgumentType(
        baselib.hashes.isHash,
        'hash');
    var checkHasheq = makeCheckArgumentType(
        baselib.hashes.isHasheq,
        'hash');
    var checkHasheqv = makeCheckArgumentType(
        baselib.hashes.isHasheqv,
        'hash');
    var checkMutableHash = makeCheckArgumentType(
        function(x) { return baselib.hashes.isHash(x) && ! x.isImmutable()},
        'mutable hash');
    var checkImmutableHash = makeCheckArgumentType(
        function(x) { return baselib.hashes.isHash(x) && x.isImmutable()},
        'immutable hash');






    //////////////////////////////////////////////////////////////////////


    exports.testArgument = testArgument;
    exports.testArity = testArity;
    exports.makeCheckArgumentType = makeCheckArgumentType;
    exports.makeCheckParameterizedArgumentType = makeCheckParameterizedArgumentType;
    exports.makeCheckListofArgumentType = makeCheckListofArgumentType;
    exports.checkOutputPort = checkOutputPort;
    exports.checkInputPort = checkInputPort;
    exports.checkSymbol = checkSymbol;
    exports.checkString = checkString;
    exports.checkSymbolOrString = checkSymbolOrString;
    exports.checkMutableString = checkMutableString;
    exports.checkChar = checkChar;
    exports.checkProcedure = checkProcedure;
    exports.checkNumber = checkNumber;
    exports.checkReal = checkReal;
    exports.checkNonNegativeReal = checkNonNegativeReal;
    exports.checkNatural = checkNatural;
    exports.checkNaturalInRange = checkNaturalInRange;
    exports.checkByte = checkByte;
    exports.checkBytes = checkBytes;
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
    exports.checkPlaceholder = checkPlaceholder;
    exports.checkSrcloc = checkSrcloc;
    exports.checkContinuationMarkSet = checkContinuationMarkSet;
    exports.checkContinuationPromptTag = checkContinuationPromptTag;
    exports.checkExn = checkExn;
    exports.checkHash = checkHash;
    exports.checkImmutableHash = checkImmutableHash;
    exports.checkMutableHash = checkMutableHash;

}(this.plt.baselib));
