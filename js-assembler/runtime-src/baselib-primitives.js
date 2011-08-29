/*jslint unparam: true, sub: true, vars: true, white: true, nomen: true, plusplus: true, maxerr: 50, indent: 4 */

// Arity structure
(function (baselib) {
    'use strict';
    var exports = {};
    baselib.primitives = exports;


    //////////////////////////////////////////////////////////////////////
    //////////////////////////////////////////////////////////////////////
    // We try to isolate the effect of external modules: all the identifiers we
    // pull from external modules should be listed here, and should otherwise not
    // show up outside this section!
    var isNumber = baselib.numbers.isNumber;
    var isNatural = baselib.numbers.isNatural;
    var isPair = baselib.lists.isPair;
    var isList = baselib.lists.isList;
    var isString = baselib.strings.isString;
    var isSymbol = baselib.symbols.isSymbol;
    var equals = baselib.equality.equals;

    var NULL = baselib.lists.EMPTY;
    var VOID = baselib.constants.VOID_VALUE;

    var makeFloat = baselib.numbers.makeFloat;
    var makeComplex = baselib.numbers.makeComplex;
    var makeComplexPolar = baselib.numbers.makeComplexPolar;

    var makeSymbol = baselib.symbols.makeSymbol;

    var makeBox = baselib.boxes.makeBox;

    var makeVector = baselib.vectors.makeVector;
    var makeList = baselib.lists.makeList;
    var makePair = baselib.lists.makePair;

    var finalizeClosureCall = baselib.functions.finalizeClosureCall;
    var makePrimitiveProcedure = baselib.functions.makePrimitiveProcedure;
    var makeClosure = baselib.functions.makeClosure;


    // Other helpers
    var withArguments = baselib.withArguments;
    var toDomNode = baselib.format.toDomNode;



    // Exceptions and error handling.
    var raise = baselib.exceptions.raise;
    var raiseArgumentTypeError = baselib.exceptions.raiseArgumentTypeError;
    var raiseArityMismatchError = baselib.exceptions.raiseArityMismatchError;

    var testArgument = baselib.check.testArgument;

    var checkOutputPort = baselib.check.checkOutputPort;
    var checkString = baselib.check.checkString;
    var checkSymbolOrString = baselib.check.checkSymbolOrString;
    var checkMutableString = baselib.check.checkMutableString;
    var checkSymbol = baselib.check.checkSymbol;
    var checkByte = baselib.check.checkByte;
    var checkChar = baselib.check.checkChar;
    var checkProcedure = baselib.check.checkProcedure;
    var checkNumber = baselib.check.checkNumber;
    var checkReal = baselib.check.checkReal;
    var checkNonNegativeReal = baselib.check.checkNonNegativeReal;
    var checkNatural = baselib.check.checkNatural;
    var checkNaturalInRange = baselib.check.checkNaturalInRange;
    var checkInteger = baselib.check.checkInteger;
    var checkRational = baselib.check.checkRational;
    var checkPair = baselib.check.checkPair;
    var checkList = baselib.check.checkList;
    var checkListofChars = baselib.check.makeCheckListofArgumentType(baselib.chars.isChar,
                                                                     'character');
    var checkVector = baselib.check.checkVector;
    var checkBox = baselib.check.checkBox;
    var checkMutableBox = baselib.check.checkMutableBox;
    var checkInspector = baselib.check.checkInspector;
    //////////////////////////////////////////////////////////////////////











    // Primitives are the set of primitive values.  Not all primitives
    // are coded here; several of them (including call/cc) are injected by
    // the bootstrapping code in compiler/boostrapped-primitives.rkt
    var Primitives = {};

    var installPrimitiveProcedure = function (name, arity, f) {
        Primitives[name] = makePrimitiveProcedure(name, arity, f);
    };

    var installPrimitiveClosure = function (name, arity, f) {
        Primitives[name] = makeClosure(name, arity, f, []);
    };


    var installPrimitiveConstant = function (name, v) {
        Primitives[name] = v;
    };



    installPrimitiveConstant('pi', baselib.numbers.pi);
    installPrimitiveConstant('e', baselib.numbers.e);
    installPrimitiveConstant('null', NULL);
    installPrimitiveConstant('true', true);
    installPrimitiveConstant('false', false);


    // The parameter keys here must be uninterned symbols, so we explicitly
    // call the symbol constructor here.
    installPrimitiveConstant('exception-handler-key',
                             new baselib.symbols.Symbol("exnh"));
    installPrimitiveConstant('parameterization-key',
                             new baselib.symbols.Symbol("paramz"));
    installPrimitiveConstant('break-enabled-key',
                             new baselib.symbols.Symbol("break-on?"));


    var gensymCounter = 0;
    installPrimitiveProcedure(
        'gensym',
        makeList(0, 1),
        function(MACHINE) {
            var baseName = "g";
            if (MACHINE.argcount === 1) {
                baseName = checkSymbolOrString(MACHINE, 'gensym', 0).toString();
            }
            gensymCounter++;
            return new baselib.symbols.Symbol(baseName + gensymCounter);
        });


    installPrimitiveProcedure(
        'display',
        makeList(1, 2),
        function (MACHINE) {
            var firstArg = MACHINE.env[MACHINE.env.length - 1];
            var outputPort = MACHINE.params.currentOutputPort;
            if (MACHINE.argcount === 2) {
                outputPort = checkOutputPort(MACHINE, 'display', 1);
            }
            outputPort.writeDomNode(MACHINE, toDomNode(firstArg, 'display'));
            return VOID;
        });


    installPrimitiveProcedure(
        'write-byte', 
        makeList(1, 2),
        function (MACHINE) {
            var firstArg = checkByte(MACHINE, 'write-byte', 0);
            var outputPort = MACHINE.params.currentOutputPort;
            if (MACHINE.argcount === 2) {
                outputPort = checkOutputPort(MACHINE, 'display', 1);
            }
            outputPort.writeDomNode(MACHINE, toDomNode(String.fromCharCode(firstArg), 'display'));
            return VOID;
        });


    installPrimitiveProcedure(
        'newline', makeList(0, 1),
        function (MACHINE) {
            var outputPort = MACHINE.params.currentOutputPort;
            if (MACHINE.argcount === 1) { 
                outputPort = checkOutputPort(MACHINE, 'newline', 1);
            }
            outputPort.writeDomNode(MACHINE, toDomNode("\n", 'display'));
            return VOID;
        });

    installPrimitiveProcedure(
        'displayln',
        makeList(1, 2),
        function (MACHINE){
            var firstArg = MACHINE.env[MACHINE.env.length-1];
            var outputPort = MACHINE.params.currentOutputPort;
            if (MACHINE.argcount === 2) {
                outputPort = checkOutputPort(MACHINE, 'displayln', 1);
            }
            outputPort.writeDomNode(MACHINE, toDomNode(firstArg, 'display'));
            outputPort.writeDomNode(MACHINE, toDomNode("\n", 'display'));
            return VOID;
        });



    installPrimitiveProcedure(
        'format',
        baselib.arity.makeArityAtLeast(1),
        function (MACHINE) {
            var args = [], i, formatString;
            formatString = checkString(MACHINE, 'format', 0).toString();
            for(i = 1; i < MACHINE.argcount; i++) {
                args.push(MACHINE.env[MACHINE.env.length - 1 - i]);
            }
            return baselib.format.format(formatString, args, 'format');
        });


    installPrimitiveProcedure(
        'printf',
        baselib.arity.makeArityAtLeast(1),
        function (MACHINE) {
            var args = [], i, formatString, result, outputPort;
            formatString = checkString(MACHINE, 'printf', 0).toString();
            for(i = 1; i < MACHINE.argcount; i++) {
                args.push(MACHINE.env[MACHINE.env.length - 1 - i]);
            }
            result = baselib.format.format(formatString, args, 'format');
            outputPort = MACHINE.params.currentOutputPort;            
            outputPort.writeDomNode(MACHINE, toDomNode(result, 'display'));
            return VOID;
        });


    installPrimitiveProcedure(
        'fprintf',
        baselib.arity.makeArityAtLeast(2),
        function (MACHINE) {
            var args = [], i, formatString, outputPort, result;
            outputPort = checkOutputPort(MACHINE, 'fprintf', 0);
            formatString = checkString(MACHINE, 'fprintf', 1).toString();
            for(i = 2; i < MACHINE.argcount; i++) {
                args.push(MACHINE.env[MACHINE.env.length - 1 - i]);
            }
            result = baselib.format.format(formatString, args, 'format');
            outputPort.writeDomNode(MACHINE, toDomNode(result, 'display'));
            return VOID;
        });






    installPrimitiveProcedure(
        'current-print',
        makeList(0, 1),
        function (MACHINE) {
            if (MACHINE.argcount === 1) {
                MACHINE.params['currentPrint'] =                 
                    checkProcedure(MACHINE, 'current-print', 0);
                return VOID;
            } else {
                return MACHINE.params['currentPrint'];
            }
        });


    installPrimitiveProcedure(
        'current-output-port',
        makeList(0, 1),
        function (MACHINE) {
            if (MACHINE.argcount === 1) {
                MACHINE.params['currentOutputPort'] = 
                    checkOutputPort(MACHINE, 'current-output-port', 0);
                return VOID;
            } else {
                return MACHINE.params['currentOutputPort'];
            }
        });





    installPrimitiveProcedure(
        '=',
        baselib.arity.makeArityAtLeast(2),
        function (MACHINE) {
	    var i;
            var firstArg = checkNumber(MACHINE, '=', 0), secondArg;
            for (i = 1; i < MACHINE.argcount; i++) {
                secondArg = checkNumber(MACHINE, '=', i);
                if (! (baselib.numbers.equals(firstArg, secondArg))) {
                    return false; 
                }
            }
            return true;
        });


    
    installPrimitiveProcedure(
        '=~',
        3,
        function (MACHINE) {
            var x = checkReal(MACHINE, '=~', 0);
            var y = checkReal(MACHINE, '=~', 1);
            var range = checkNonNegativeReal(MACHINE, '=~', 2);
            return baselib.numbers.lessThanOrEqual(
                baselib.numbers.abs(baselib.numbers.subtract(x, y)), 
                range);
        });



    var makeChainingBinop = function (predicate, name) {
        return function (MACHINE) {
            var firstArg = checkNumber(MACHINE, name, 0), secondArg, i;
            for (i = 1; i < MACHINE.argcount; i++) {
                secondArg = checkNumber(MACHINE, name, i);
                if (! (predicate(firstArg, secondArg))) {
                    return false; 
                }
                firstArg = secondArg;
            }
            return true;
        };
    };

    installPrimitiveProcedure(
        '<',
        baselib.arity.makeArityAtLeast(2),
        makeChainingBinop(baselib.numbers.lessThan, '<'));


    installPrimitiveProcedure(
        '>',
        baselib.arity.makeArityAtLeast(2),
        makeChainingBinop(baselib.numbers.greaterThan, '>'));


    installPrimitiveProcedure(
        '<=',
        baselib.arity.makeArityAtLeast(2),
        makeChainingBinop(baselib.numbers.lessThanOrEqual, '<='));


    installPrimitiveProcedure(
        '>=',
        baselib.arity.makeArityAtLeast(2),
        makeChainingBinop(baselib.numbers.greaterThanOrEqual, '>='));
    

    installPrimitiveProcedure(
        '+',
        baselib.arity.makeArityAtLeast(0),
        function (MACHINE) {
            var result = 0;
            var i = 0;
            for (i = 0; i < MACHINE.argcount; i++) {
                result = baselib.numbers.add(
                    result, 
                    checkNumber(MACHINE, '+', i));
            }
            return result;
        });
    

    installPrimitiveProcedure(
        '*',
        baselib.arity.makeArityAtLeast(0),
        function (MACHINE) {
            var result = 1;
            var i = 0;
            for (i=0; i < MACHINE.argcount; i++) {
                result = baselib.numbers.multiply(
                    result, 
                    checkNumber(MACHINE, '*', i));
            }
            return result;
        });

    installPrimitiveProcedure(
        '-',
        baselib.arity.makeArityAtLeast(1),
        function (MACHINE) {
            if (MACHINE.argcount === 1) { 
                return baselib.numbers.subtract(
                    0, 
                    checkNumber(MACHINE, '-', 0));
            }
            var result = checkNumber(MACHINE, '-', 0), i;
            for (i = 1; i < MACHINE.argcount; i++) {
                result = baselib.numbers.subtract(
                    result, 
                    checkNumber(MACHINE, '-', i));
            }
            return result;
        });
    
    installPrimitiveProcedure(
        '/',
        baselib.arity.makeArityAtLeast(1),
        function (MACHINE) {
            var result = checkNumber(MACHINE, '/', 0), i;
            for (i = 1; i < MACHINE.argcount; i++) {
                result = baselib.numbers.divide(
                    result,
                    checkNumber(MACHINE, '/', i));
            }
            return result;
        });
    

    installPrimitiveProcedure(
        'add1',
        1,
        function (MACHINE) {
            var firstArg = checkNumber(MACHINE, 'add1', 0);
            return baselib.numbers.add(firstArg, 1);
        });


    installPrimitiveProcedure(
        'sub1',
        1,
        function (MACHINE) {
            var firstArg = checkNumber(MACHINE, 'sub1', 0);
            return baselib.numbers.subtract(firstArg, 1);
        });


    installPrimitiveProcedure(
        'zero?',
        1,
        function (MACHINE) {
            var firstArg = MACHINE.env[MACHINE.env.length-1];
            return baselib.numbers.equals(firstArg, 0);
        });


    installPrimitiveProcedure(
        'cons',
        2,
        function (MACHINE) {
            var firstArg = MACHINE.env[MACHINE.env.length-1];
            var secondArg = MACHINE.env[MACHINE.env.length-2];
            return makePair(firstArg, secondArg);
        });


    installPrimitiveProcedure(
        'list',
        baselib.arity.makeArityAtLeast(0),
        function (MACHINE) {
            var result = NULL, i;
            for (i = 0; i < MACHINE.argcount; i++) {
                result = makePair(MACHINE.env[MACHINE.env.length - (MACHINE.argcount - i)],
                                  result);
            }
            return result;
        });

    installPrimitiveProcedure(
        'list*',
        baselib.arity.makeArityAtLeast(1),
        function (MACHINE) {
            var result = checkList(MACHINE, 'list*', MACHINE.argcount - 1), i;
            for (i = MACHINE.argcount - 2; i >= 0; i--) {
                result = makePair(MACHINE.env[MACHINE.env.length - 1 - i],
                                  result);
            }
            return result;
        });


    installPrimitiveProcedure(
        'list-ref',
        2,
        function (MACHINE) {
            var lst = checkList(MACHINE, 'list-ref', 0);
            var index = checkNaturalInRange(MACHINE, 'list-ref', 1,
                                            0, baselib.lists.length(lst));
            return baselib.lists.listRef(lst, baselib.numbers.toFixnum(index));
        });




    installPrimitiveProcedure(
        'car',
        1,
        function (MACHINE) {
            var firstArg = checkPair(MACHINE, 'car', 0);
            return firstArg.first;
        });

    installPrimitiveProcedure(
        'cdr',
        1,
        function (MACHINE) {
            var firstArg = checkPair(MACHINE, 'cdr', 0);
            return firstArg.rest;
        });

    installPrimitiveProcedure(
        'pair?',
        1,
        function (MACHINE) {
            var firstArg = MACHINE.env[MACHINE.env.length-1];
            return isPair(firstArg);
        });


    installPrimitiveProcedure(
        'list?',
        1,
        function (MACHINE) {
            return isList(MACHINE.env[MACHINE.env.length -1]);
        });


    installPrimitiveProcedure(
        'set-car!',
        2,
        function (MACHINE) {
            var firstArg = checkPair(MACHINE, 'set-car!', 0);
            var secondArg = MACHINE.env[MACHINE.env.length-2];
            firstArg.first = secondArg;
            return VOID;
        });


    installPrimitiveProcedure(
        'set-cdr!',
        2,
        function (MACHINE) {
            var firstArg = checkPair(MACHINE, 'set-car!', 0);
            var secondArg = MACHINE.env[MACHINE.env.length-2];
            firstArg.rest = secondArg;
            return VOID;
        });

    
    installPrimitiveProcedure(
        'not',
        1,
        function (MACHINE) {
            var firstArg = MACHINE.env[MACHINE.env.length-1];
            return (firstArg === false);
        });


    installPrimitiveProcedure(
        'null?',
        1,
        function (MACHINE) {
            var firstArg = MACHINE.env[MACHINE.env.length-1];
            return firstArg === NULL;
        });


    installPrimitiveProcedure(
        'vector',
        baselib.arity.makeArityAtLeast(0),
        function (MACHINE) {
            var i;
            var result = [];
            for (i = 0; i < MACHINE.argcount; i++) {
                result.push(MACHINE.env[MACHINE.env.length-1-i]);
            }
            var newVector = makeVector.apply(null, result);
            return newVector;
        });


    installPrimitiveProcedure(
        'make-vector',
        makeList(1, 2),
        function (MACHINE) {
            var value = 0;
            var length = baselib.numbers.toFixnum(
                checkNatural(MACHINE, 'make-vector', 0));
            if (MACHINE.argcount === 2) {
                value = MACHINE.env[MACHINE.env.length - 2];
            }
            var arr = [];
	    var i;
            for(i = 0; i < length; i++) {
                arr[i] = value;
            }
            return makeVector.apply(null, arr);
        });
    

    installPrimitiveProcedure(
        'vector->list',
        1,
        function (MACHINE) {
            var elts = checkVector(MACHINE, 'vector->list', 0).elts;
            var i;
            var result = NULL;
            for (i = 0; i < elts.length; i++) {
                result = makePair(elts[elts.length - 1 - i], result);
            }
            return result;
        });

    
    installPrimitiveProcedure(
        'list->vector',
        1,
        function (MACHINE) {
            var firstArg = checkList(MACHINE, 'list->vector', 0);
            var result = [];
            while (firstArg !== NULL) {
                result.push(firstArg.first);
                firstArg = firstArg.rest;
            }
            return makeVector.apply(null, result);
        });


    installPrimitiveProcedure(
        'vector-ref',
        2,
        function (MACHINE) {
            var elts = checkVector(MACHINE, 'vector-ref', 0).elts;
            var index = MACHINE.env[MACHINE.env.length-2];
            return elts[index];
        });


    installPrimitiveProcedure(
        'vector-set!',
        3,
        function (MACHINE) {
            var elts = checkVector(MACHINE, 'vector-set!', 0).elts;
            // FIXME: check out-of-bounds vector
            var index = baselib.numbers.toFixnum(
                checkNaturalInRange(MACHINE, 'vector-set!', 1,
                                    0, elts.length));
            var val = MACHINE.env[MACHINE.env.length - 1 - 2];
            elts[index] = val;
            return VOID;
        });


    installPrimitiveProcedure(
        'vector-length',
        1,
        function (MACHINE) {
            return checkVector(MACHINE, 'vector-length', 0).elts.length;
        });



    installPrimitiveProcedure(
        'make-string',
        makeList(1, 2),
        function (MACHINE) {
            var value = String.fromCharCode(0);
            var length = baselib.numbers.toFixnum(
                checkNatural(MACHINE, 'make-string', 0));
            if (MACHINE.argcount === 2) {
                value = checkChar(MACHINE, 'make-string', 1).val;
            }
            var arr = [];
	    var i;
            for(i = 0; i < length; i++) {
                arr[i] = value;
            }
            return baselib.strings.makeMutableString(arr);
        });

    installPrimitiveProcedure(
        'substring',
        makeList(2, 3),
        function(MACHINE) {
            var str = String(checkString(MACHINE, 'substring', 0));
            var start = baselib.numbers.toFixnum(checkNatural(MACHINE, 'substring', 1));
            var end = str.length;
            if (MACHINE.argcount === 3) {
                end = baselib.numbers.toFixnum(checkNatural(MACHINE, 'substring', 2));
            }
            return str.substring(start, end);
        });


    installPrimitiveProcedure(
        'list->string',
        1,
        function (MACHINE) {
            var firstArg = checkListofChars(MACHINE, 'list->string', 0);
            var result = [];
            while (firstArg !== NULL) {
                result.push(firstArg.first.val);
                firstArg = firstArg.rest;
            }
            return result.join('');
        });


    installPrimitiveProcedure(
        'string',
        baselib.arity.makeArityAtLeast(0),
        function (MACHINE) {
            var i;
            var chars = [];
            for (i = 0; i < MACHINE.argcount; i++) {
                chars.push(checkChar(MACHINE, 'string', i).val);
            };
            return chars.join('');
        });


    installPrimitiveProcedure(
        'string->list',
        1,
        function (MACHINE) {
            var str = checkString(MACHINE, 'string->list', 0).toString();
            var i;
            var result = NULL;
            for (i = str.length - 1; i >= 0; i--) {
                result = makePair(baselib.chars.makeChar(str[i]), result);
            }
            return result;
        });



    installPrimitiveProcedure(
        'string-set!',
        3,
        function (MACHINE) {
            var str = checkMutableString(MACHINE, 'string-set!', 0);
            var k = checkNatural(MACHINE, 'string-set!', 1);
            var ch = checkChar(MACHINE, 'string-set!', 2);
	    str.set(baselib.numbers.toFixnum(k), ch.val);
            return VOID;
        });





    installPrimitiveProcedure(
        'symbol?',
        1,
        function (MACHINE) {
            var firstArg = MACHINE.env[MACHINE.env.length-1];
            return isSymbol(firstArg);
        });

    installPrimitiveProcedure(
        'symbol->string',
        1,
        function (MACHINE) {
            var firstArg = checkSymbol(MACHINE, 'symbol->string', 0);
            return firstArg.toString();
        });


    installPrimitiveProcedure(
        'string=?',
        baselib.arity.makeArityAtLeast(1),
        function (MACHINE) {
            var s = checkString(MACHINE, 'string=?', 0).toString();
	    var i;
            for (i = 1; i < MACHINE.argcount; i++) {
                if (checkString(MACHINE, 'string=?', i).toString() !== s) {
                    return false;
                }
            }
            return true;
        });


    installPrimitiveProcedure(
        'string-append',
        baselib.arity.makeArityAtLeast(0),
        function (MACHINE) {
            var buffer = [];
            var i;
            for (i = 0; i < MACHINE.argcount; i++) {
                buffer.push(checkString(MACHINE, 'string-append', i).toString());
            }
            return buffer.join('');
        });

    installPrimitiveProcedure(
        'string-length',
        1,
        function (MACHINE) {
            var firstArg = checkString(MACHINE, 'string-length', 0).toString();
            return firstArg.length;
        });


    installPrimitiveProcedure(
        'string-ref',
        2,
        function (MACHINE) {
            var firstArg = checkString(MACHINE, 'string-ref', 0).toString();
            var index = baselib.numbers.toFixnum(
                checkNaturalInRange(MACHINE, 'string-ref', 1,
                                    0, firstArg.length));
            return baselib.chars.makeChar(firstArg[index]);
        });



    installPrimitiveProcedure(
        'string?',
        1,
        function (MACHINE) {
            return isString(MACHINE.env[MACHINE.env.length - 1]);
        });


    installPrimitiveProcedure(
        'number->string',
        1,
        function (MACHINE) {
            return checkNumber(MACHINE, 'number->string', 0).toString();
        });


    installPrimitiveProcedure(
        'string->symbol',
        1,
        function (MACHINE) {
            return makeSymbol(checkString(MACHINE, 'string->symbol', 0).toString());
        });


    installPrimitiveProcedure(
        'string->number',
        1,
        function (MACHINE) {
            return baselib.numbers.fromString(
                checkString(MACHINE, 'string->number', 0).toString());
        });


    installPrimitiveProcedure(
        'boolean?',
        1,
        function(MACHINE) {
            var v = MACHINE.env[MACHINE.env.length - 1];
            return (v === true || v === false);
        });


    installPrimitiveProcedure(
        'char?',
        1,
        function(MACHINE) {
            return baselib.chars.isChar(MACHINE.env[MACHINE.env.length -1 ]);
        });


    installPrimitiveProcedure(
        'char=?',
        baselib.arity.makeArityAtLeast(2),
        function(MACHINE) {
            var s = checkChar(MACHINE, 'char=?', 0).val;
	    var i;
            for (i = 1; i < MACHINE.argcount; i++) {
                if (checkChar(MACHINE, 'char=?', i).val !== s) {
                    return false;
                }
            }
            return true;
        });


    
    installPrimitiveProcedure(
        'box',
        1,
        function (MACHINE) {
            var firstArg = MACHINE.env[MACHINE.env.length-1];
            return makeBox(firstArg);
        });

    installPrimitiveProcedure(
        'unbox',
        1,
        function (MACHINE) {
            var firstArg = checkBox(MACHINE, 'unbox', 0);
            return firstArg.ref();
        });

    installPrimitiveProcedure(
        'set-box!',
        2,
        function (MACHINE) {
            var firstArg = checkMutableBox(MACHINE, 'set-box!', 0);
            var secondArg = MACHINE.env[MACHINE.env.length-2];
            firstArg.set(secondArg);
            return VOID;
        });

    installPrimitiveProcedure(
        'void',
        baselib.arity.makeArityAtLeast(0),
        function (MACHINE) {
            return VOID;
        });


    installPrimitiveProcedure(
        'random',
        baselib.lists.makeList(0, 1),
        function (MACHINE) {
            if (MACHINE.argcount === 0) {
                return makeFloat(Math.random());
            } else {
                var n = checkNatural(MACHINE, 'random', 0);
                return Math.floor(Math.random() * baselib.numbers.toFixnum(n));
            }
        });


    installPrimitiveProcedure(
        'eq?',
        2,
        function (MACHINE) {
            var firstArg = MACHINE.env[MACHINE.env.length-1];
            var secondArg = MACHINE.env[MACHINE.env.length-2];
            return firstArg === secondArg;
        });

    installPrimitiveProcedure(
        'eqv?',
        2,
        function (MACHINE) {
            var firstArg = MACHINE.env[MACHINE.env.length-1];
            var secondArg = MACHINE.env[MACHINE.env.length-2];
            return baselib.equality.eqv(firstArg, secondArg);
        });



    installPrimitiveProcedure(
        'equal?',
        2,
        function (MACHINE) {
            var firstArg = MACHINE.env[MACHINE.env.length-1];
            var secondArg = MACHINE.env[MACHINE.env.length-2];
            return equals(firstArg, secondArg);
        });


    // This definition of apply will take precedence over the
    // implementation of apply in the boostrapped-primitives.rkt,
    // since it provides nicer error handling.
    var applyImplementation = function (MACHINE) {
        if(--MACHINE.callsBeforeTrampoline < 0) { 
            throw applyImplementation;
        }
        var proc = checkProcedure(MACHINE, 'apply', 0);
        MACHINE.env.pop();
        MACHINE.argcount--;
        checkList(MACHINE, 'apply', MACHINE.argcount - 1);
        MACHINE.spliceListIntoStack(MACHINE.argcount - 1);
        if (baselib.arity.isArityMatching(proc.racketArity, MACHINE.argcount)) {
            MACHINE.proc = proc;
            if (baselib.functions.isPrimitiveProcedure(proc)) {
                return finalizeClosureCall(MACHINE, proc(MACHINE));
            } else {
                return proc.label(MACHINE);
            }
        } else {
            raiseArityMismatchError(MACHINE, proc, proc.racketArity, MACHINE.argcount);
        }
    };
    installPrimitiveClosure(
        'apply',
        baselib.arity.makeArityAtLeast(2),
        applyImplementation);


    // FIXME: The definition of call-with-values is in
    // bootstrapped-primitives.rkt.  We may want to replace it with an
    // explicitly defined one here.





    installPrimitiveProcedure(
        'procedure?',
        1,
        function (MACHINE) {
            return baselib.functions.isProcedure(MACHINE.env[MACHINE.env.length - 1]);
        });
    
    installPrimitiveProcedure(
        'procedure-arity-includes?',
        2,
        function (MACHINE) {
            var proc = checkProcedure(MACHINE, 'procedure-arity-includes?', 0);
            var argcount = checkNatural(MACHINE, 'procedure-arity-includes?', 1);
            return baselib.arity.isArityMatching(proc.racketArity, argcount);
        });

    installPrimitiveProcedure(
        'procedure-arity',
        1,
        function (MACHINE) {
            var proc = checkProcedure(MACHINE, 'procedure-arity-includes?', 0);
            return proc.racketArity;
        });


    installPrimitiveProcedure(
        'procedure-rename',
        2,
        function (MACHINE) {
            var proc = checkProcedure(MACHINE, 'procedure-rename', 0);
            var name = checkSymbol(MACHINE, 'procedure-rename', 1);
            return baselib.functions.renameProcedure(proc, name);
        });



    installPrimitiveProcedure(
        'member',
        2,
        function (MACHINE) {
            var x = MACHINE.env[MACHINE.env.length-1];
            var lst = MACHINE.env[MACHINE.env.length-2];
            while (true) {
                if (lst === NULL) {
                    return false;
                }
                if (! isPair(lst)) {
                    raiseArgumentTypeError(MACHINE,
                                           'member',
                                           'list',
                                           1,
                                           MACHINE.env[MACHINE.env.length - 1 - 1]);
                }
                if (equals(x, (lst.first))) {
                    return lst;
                }
                lst = lst.rest;
            }   
        });
    


    installPrimitiveProcedure(
        'reverse',
        1,
        function (MACHINE) {
            var rev = NULL;
            var lst = MACHINE.env[MACHINE.env.length-1];
            while(lst !== NULL) {
                testArgument(MACHINE,
                             'pair', isPair, lst, 0, 'reverse');
                rev = makePair(lst.first, rev);
                lst = lst.rest;
            }
            return rev;
        });


    installPrimitiveProcedure(
	'number?',
	1,
	function(MACHINE) {
	    return isNumber(MACHINE.env[MACHINE.env.length - 1]);
	});


    installPrimitiveProcedure(
        'abs',
        1,
        function (MACHINE) {
            return baselib.numbers.abs(
                checkNumber(MACHINE, 'abs', 0));
        });

    installPrimitiveProcedure(
        'acos',
        1,
        function (MACHINE) {
            return baselib.numbers.acos(
                checkNumber(MACHINE, 'acos', 0));
        });


    installPrimitiveProcedure(
        'asin',
        1,
        function (MACHINE) {
            return baselib.numbers.asin(
                checkNumber(MACHINE, 'asin', 0));
        });

    installPrimitiveProcedure(
        'sin',
        1,
        function (MACHINE) {
            return baselib.numbers.sin(
                checkNumber(MACHINE, 'sin', 0));
        });



    installPrimitiveProcedure(
        'sinh',
        1,
        function (MACHINE) {
            return baselib.numbers.sinh(
                checkNumber(MACHINE, 'sinh', 0));
        });


    installPrimitiveProcedure(
        'tan',
        1,
        function (MACHINE) {
            return baselib.numbers.tan(
                checkNumber(MACHINE, 'tan', 0));
        });

    

    installPrimitiveProcedure(
        'atan',
        makeList(1, 2),
        function (MACHINE) {
            if (MACHINE.argcount === 1) {
                return baselib.numbers.atan(
                    checkNumber(MACHINE, 'atan', 0));
            } else {
                testArgument(MACHINE,
                             'number',
                             isNumber,
                             MACHINE.env[MACHINE.env.length - 1],
                             0,
                             'atan');
                testArgument(MACHINE,
                             'number',
                             isNumber,
                             MACHINE.env[MACHINE.env.length - 2],
                             1,
                             'atan');
                return makeFloat(
                    Math.atan2(
                        baselib.numbers.toFixnum(checkNumber(MACHINE, 'atan', 0)),
                        baselib.numbers.toFixnum(checkNumber(MACHINE, 'atan', 1))));
            }
        });


    installPrimitiveProcedure(
        'angle',
        1,
        function (MACHINE) {
            return baselib.numbers.angle(
                checkNumber(MACHINE, 'angle', 0));
        });

    installPrimitiveProcedure(
        'magnitude',
        1,
        function (MACHINE) {
            return baselib.numbers.magnitude(
                checkNumber(MACHINE, 'magnitude', 0));
        });

    installPrimitiveProcedure(
        'conjugate',
        1,
        function (MACHINE) {
            return baselib.numbers.conjugate(
                checkNumber(MACHINE, 'conjugate', 0));
        });




    installPrimitiveProcedure(
        'cos',
        1,
        function (MACHINE) {
            return baselib.numbers.cos(
                checkNumber(MACHINE, 'cos', 0));
        });


    installPrimitiveProcedure(
        'cosh',
        1,
        function (MACHINE) {
            return baselib.numbers.cosh(
                checkNumber(MACHINE, 'cosh', 0));
        });

    installPrimitiveProcedure(
        'gcd',
        baselib.arity.makeArityAtLeast(1),
        function (MACHINE) {
            var args = [], i, x;
            for (i = 0; i < MACHINE.argcount; i++) {
                args.push(checkNumber(MACHINE, 'gcd', i));
            }
            x = args.shift();
            return baselib.numbers.gcd(x, args);
        });

    installPrimitiveProcedure(
        'lcm',
        baselib.arity.makeArityAtLeast(1),
        function (MACHINE) {
            var args = [], i, x;
            for (i = 0; i < MACHINE.argcount; i++) {
                args.push(checkNumber(MACHINE, 'lcm', i));
            }
            x = args.shift();
            return baselib.numbers.lcm(x, args);
        });




    installPrimitiveProcedure(
        'exp',
        1,
        function (MACHINE) {
            return baselib.numbers.exp(
                checkNumber(MACHINE, 'exp', 0));
        });


    installPrimitiveProcedure(
        'expt',
        2,
        function (MACHINE) {
            return baselib.numbers.expt(
                checkNumber(MACHINE, 'expt', 0),
                checkNumber(MACHINE, 'expt', 1));
        });

    installPrimitiveProcedure(
        'exact?',
        1,
        function (MACHINE) {
            return baselib.numbers.isExact(
                checkNumber(MACHINE, 'exact?', 0));
        });


    installPrimitiveProcedure(
        'integer?',
        1,
        function (MACHINE) {
            return baselib.numbers.isInteger(MACHINE.env[MACHINE.env.length - 1]);
        });


    installPrimitiveProcedure(
        'exact-nonnegative-integer?',
        1,
        function (MACHINE) {
            return isNatural(MACHINE.env[MACHINE.env.length - 1]);
        });



    installPrimitiveProcedure(
        'imag-part',
        1,
        function (MACHINE) {
            return baselib.numbers.imaginaryPart(
                checkNumber(MACHINE, 'imag-part', 0));
        });


    installPrimitiveProcedure(
        'real-part',
        1,
        function (MACHINE) {
            return baselib.numbers.realPart(
                checkNumber(MACHINE, 'real-part', 0));
        });


    installPrimitiveProcedure(
        'make-polar',
        2,
        function (MACHINE) {
            return makeComplexPolar(
                checkReal(MACHINE, 'make-polar', 0),
                checkReal(MACHINE, 'make-polar', 1));
        });


    installPrimitiveProcedure(
        'make-rectangular',
        2,
        function (MACHINE) {
            return makeComplex(
                checkReal(MACHINE, 'make-rectangular', 0),
                checkReal(MACHINE, 'make-rectangular', 1));
        });

    installPrimitiveProcedure(
        'modulo',
        2,
        function (MACHINE) {
            return baselib.numbers.modulo(
                checkInteger(MACHINE, 'modulo', 0),
                checkInteger(MACHINE, 'modulo', 1));
        });


    installPrimitiveProcedure(
        'remainder',
        2,
        function (MACHINE) {
            return baselib.numbers.remainder(
                checkInteger(MACHINE, 'remainder', 0),
                checkInteger(MACHINE, 'remainder', 1));
        });


    installPrimitiveProcedure(
        'quotient',
        2,
        function (MACHINE) {
            return baselib.numbers.quotient(
                checkInteger(MACHINE, 'quotient', 0),
                checkInteger(MACHINE, 'quotient', 1));
        });



    installPrimitiveProcedure(
        'floor',
        1,
        function (MACHINE) {
            return baselib.numbers.floor(
                checkReal(MACHINE, 'floor', 0));
        });
    

    installPrimitiveProcedure(
        'ceiling',
        1,
        function (MACHINE) {
            return baselib.numbers.ceiling(
                checkReal(MACHINE, 'ceiling', 0));
        });
    

    installPrimitiveProcedure(
        'round',
        1,
        function (MACHINE) {
            return baselib.numbers.round(
                checkReal(MACHINE, 'round', 0));
        });
    

    installPrimitiveProcedure(
        'truncate',
        1,
        function (MACHINE) {
            var n = checkReal(MACHINE, 'truncate', 0);
            if (baselib.numbers.lessThan(n, 0)) {
                return baselib.numbers.ceiling(n);
            } else {
                return baselib.numbers.floor(n);
            }
        });
    

    installPrimitiveProcedure(
        'numerator',
        1,
        function (MACHINE) {
            return baselib.numbers.numerator(
                checkRational(MACHINE, 'numerator', 0));
        });


    installPrimitiveProcedure(
        'denominator',
        1,
        function (MACHINE) {
            return baselib.numbers.denominator(
                checkRational(MACHINE, 'denominator', 0));
        });


    installPrimitiveProcedure(
        'log',
        1,
        function (MACHINE) {
            return baselib.numbers.log(
                checkNumber(MACHINE, 'log', 0));
        });


    installPrimitiveProcedure(
        'sqr',
        1,
        function (MACHINE) {
            return baselib.numbers.sqr(
                checkNumber(MACHINE, 'sqr', 0));
        });




    installPrimitiveProcedure(
        'sqrt',
        1,
        function (MACHINE) {
            return baselib.numbers.sqrt(
                checkNumber(MACHINE, 'sqrt', 0));
        });



    installPrimitiveProcedure(
        'integer-sqrt',
        1,
        function (MACHINE) {
            return baselib.numbers.integerSqrt(
                checkInteger(MACHINE, 'integer-sqrt', 0));
        });



    installPrimitiveProcedure(
        'sgn',
        1,
        function (MACHINE) {
            return baselib.numbers.sign(
                checkInteger(MACHINE, 'sgn', 0));
        });



    installPrimitiveProcedure(
        'error',
        baselib.arity.makeArityAtLeast(1),
        function (MACHINE) {
	    var i;
            if (MACHINE.argcount === 1) {
                var sym = checkSymbol(MACHINE, 'error', 1);
                // FIXME: we should collect the current continuation marks here...
                raise(MACHINE, baselib.exceptions.makeExnFail(String(sym), undefined));
            } 
            
            if (isString(MACHINE.env[MACHINE.env.length - 1])) {
                var vs = [];
                for (i = 1; i < MACHINE.argcount; i++) {
                    vs.push(baselib.format.format("~e", [MACHINE.env[MACHINE.env.length - 1 - i]]));
                }
                raise(MACHINE, baselib.exceptions.makeExnFail(String(MACHINE.env[MACHINE.env.length - 1]) +
                                                              ": " +
                                                              vs.join(' '),
                                                              undefined));
            }

            if (isSymbol(MACHINE.env[MACHINE.env.length - 1])) {
                var fmtString = checkString(MACHINE, 'error', 1);
                var args = [MACHINE.env[MACHINE.env.length - 1]];
                for (i = 2; i < MACHINE.argcount; i++) {
                    args.push(MACHINE.env[MACHINE.env.length - 1 - i]);
                }
                raise(MACHINE, baselib.exceptions.makeExnFail(
                    baselib.format.format('~s: ' + String(fmtString),
                                          args),
                    undefined));
            }

            // Fall-through
            raiseArgumentTypeError(MACHINE, 'error', 'symbol or string', 0, MACHINE.env[MACHINE.env.length - 1]);
        });


    installPrimitiveProcedure(
        'raise-mismatch-error',
        3,
        function (MACHINE) {
            var name = checkSymbol(MACHINE, 'raise-mismatch-error', 0);
            var message = checkString(MACHINE, 'raise-mismatch-error', 0);
            var val = MACHINE.env[MACHINE.env.length - 1 - 2];
            raise(MACHINE, baselib.exceptions.makeExnFail(
		baselib.format.format("~a: ~a~e",
                                      [name,
                                       message,
                                       val]),
                undefined));
        });


    installPrimitiveProcedure(
        'raise-type-error',
        baselib.arity.makeArityAtLeast(3),
        function (MACHINE) {
            var name = checkSymbol(MACHINE, 'raise-type-error', 0);
            var expected = checkString(MACHINE, 'raise-type-error', 1);
            if (MACHINE.argcount === 3) {
                raiseArgumentTypeError(MACHINE, 
                                       name,
                                       expected,
                                       undefined,
                                       MACHINE.env[MACHINE.env.length - 1 - 2]);
            } else {
                raiseArgumentTypeError(MACHINE, 
                                       name,
                                       expected,
                                       checkNatural(MACHINE, 'raise-type-error', 2),
                                       MACHINE.env[MACHINE.env.length - 1 - 2]);
            }
        });
    



    installPrimitiveClosure(
        'make-struct-type',
        makeList(4, 5, 6, 7, 8, 9, 10, 11),
        function (MACHINE) {
            withArguments(
                MACHINE,
                4,
                [false, 
                 NULL,
                 false,
                 false,
                 NULL,
                 false,
                 false],
                function (name, 
                          superType,
                          initFieldCount,
                          autoFieldCount,
                          autoV,
                          props,  // FIXME: currently ignored
                          inspector,  // FIXME: currently ignored
                          procSpec,       // FIXME: currently ignored
                          immutables, // FIXME: currently ignored
                          guard,      // FIXME: currently ignored
                          constructorName
                         ) {

                    // FIXME: typechecks.

                    var structType = baselib.structs.makeStructureType(
                        name,
                        superType,
                        initFieldCount,
                        autoFieldCount,
                        autoV,
                        //props,
                        //inspector,
                        //procSpec,
                        //immutables,
                        guard);

                    var constructorValue = 
                        makePrimitiveProcedure(
                            constructorName,
                            baselib.numbers.toFixnum(initFieldCount),
                            function (MACHINE) {
                                var args = [];
				var i;
                                for(i = 0; i < initFieldCount; i++) {
                                    args.push(MACHINE.env[MACHINE.env.length - 1 - i]);
                                }
                                return structType.constructor.apply(null, args);
                            });

                    var predicateValue = 
                        makePrimitiveProcedure(
                            String(name) + "?",
                            1,
                            function (MACHINE) {
                                return structType.predicate(MACHINE.env[MACHINE.env.length - 1]);
                            });

                    var accessorValue = 
                        makePrimitiveProcedure(
                            String(name) + "-accessor",
                            2,
                            function (MACHINE) {
                                return structType.accessor(
                                    MACHINE.env[MACHINE.env.length - 1],
                                    baselib.numbers.toFixnum(MACHINE.env[MACHINE.env.length - 2]));
                            });
                    accessorValue.structType = structType;

                    var mutatorValue = 
                        makePrimitiveProcedure(
                            String(name) + "-mutator",
                            3,
                            function (MACHINE) {
                                return structType.mutator(
                                    MACHINE.env[MACHINE.env.length - 1],
                                    baselib.numbers.toFixnum(MACHINE.env[MACHINE.env.length - 2]),
                                    MACHINE.env[MACHINE.env.length - 3]);
                            });
                    mutatorValue.structType = structType;


                    finalizeClosureCall(MACHINE,
                                        structType,
                                        constructorValue,
                                        predicateValue,
                                        accessorValue,
                                        mutatorValue);
                });
        });
    

    installPrimitiveProcedure(
        'current-inspector',
        makeList(0, 1),
        function (MACHINE) {
            if (MACHINE.argcount === 1) {
                MACHINE.params['currentInspector'] = 
                    checkInspector(MACHINE, 'current-inspector', 0);
                return VOID;
            } else {
                return MACHINE.params['currentInspector'];
            }
        }
    ); 


    installPrimitiveProcedure(
        'make-struct-field-accessor',
        makeList(2, 3),
        function (MACHINE){
            var structType = MACHINE.env[MACHINE.env.length - 1].structType;
            var index = MACHINE.env[MACHINE.env.length - 2];
            var name;
            if (MACHINE.argcount === 3) {
                name = String(MACHINE.env[MACHINE.env.length - 3]);
            } else {
                name = 'field' + index;
            }
            var checkStruct = baselib.check.makeCheckArgumentType(structType.predicate,
                                                                  structType.name);
            return makePrimitiveProcedure(
                name,
                1,
                function (MACHINE) {
                    var aStruct = checkStruct(MACHINE, name, 0);
                    return structType.accessor(
                        aStruct,
                        baselib.numbers.toFixnum(index));
                });
            
        });


    installPrimitiveProcedure(
        'make-struct-field-mutator',
        makeList(2, 3),
        function (MACHINE){
            var structType = MACHINE.env[MACHINE.env.length - 1].structType;
            var index = MACHINE.env[MACHINE.env.length - 2];
            var name;
            if (MACHINE.argcount === 3) {
                name = String(MACHINE.env[MACHINE.env.length - 3]);
            } else {
                name = 'field' + index;
            }
            var checkStruct = baselib.check.makeCheckArgumentType(structType.predicate,
                                                                  structType.name);
            return makePrimitiveProcedure(
                name,
                2,
                function (MACHINE) {
                    var aStruct = checkStruct(MACHINE, name, 0);
                    return structType.mutator(
                        aStruct,
                        baselib.numbers.toFixnum(index),
                        MACHINE.env[MACHINE.env.length - 2]);
                });            
        });


    exports['Primitives'] = Primitives;
    exports['installPrimitiveProcedure'] = installPrimitiveProcedure; 
    exports['installPrimitiveClosure'] = installPrimitiveClosure; 
    exports['installPrimitiveConstant'] = installPrimitiveConstant; 

}(this.plt.baselib));
