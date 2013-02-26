/*jslint browser: true, undef: false, unparam: true, sub: true, vars: true, white: true, plusplus: true, maxerr: 50, indent: 4 */

// Exceptions

(function(baselib) {
    'use strict';
    var exceptions = {};
    baselib.exceptions = exceptions;



    var RacketError = function(message, racketError) {
        Error.call(this, message);
        this.message = message;
        this.racketError = racketError;
    };
    RacketError.prototype = baselib.heir(Error.prototype);
    var isRacketError = baselib.makeClassPredicate(RacketError);



    // (define-struct exn (message continuation-mark-set))
    var Exn = baselib.structs.makeStructureType(
        'exn', false, 2, 0, false, false);


    // (define-struct (exn:break exn) (continuation))
    var ExnBreak = baselib.structs.makeStructureType(
        'exn:break', Exn, 1, 0, false, false);


    var ExnFail = baselib.structs.makeStructureType(
        'exn:fail', Exn, 0, 0, false, false);

    var ExnFailContract = baselib.structs.makeStructureType(
        'exn:fail:contract', ExnFail, 0, 0, false, false);

    var ExnFailContractArity = baselib.structs.makeStructureType(
        'exn:fail:contract:arity', ExnFailContract, 0, 0, false, false);

    // exn:fail:contract (id)
    var ExnFailContractVariable = baselib.structs.makeStructureType(
        'exn:fail:contract:variable', ExnFailContract, 1, 0, false, false);

    var ExnFailContractDivisionByZero = baselib.structs.makeStructureType(
        'exn:fail:contract:divide-by-zero', ExnFailContract, 0, 0, false, false);





    var exceptionHandlerKey = new baselib.symbols.Symbol("exnh");





    //////////////////////////////////////////////////////////////////////

    // Raise error to the toplevel.

    // If the error is of an exception type, make sure e.message holds the string
    // value to allow integration with systems that don't recognize Racket error 
    // structures.
    var raise = function(MACHINE, e) { 
        if (isRacketError(e) && Exn.predicate(e.racketError)) {
            e.message = Exn.accessor(e.racketError, 0);
        } else if (Exn.predicate(e)) {
            e = new RacketError(Exn.accessor(e, 0), e);
        }

        // if (window.console !== void(0) &&
        //     typeof(window.console.log) === 'function') {
        //     window.console.log(MACHINE);
        //     if (e.stack) { window.console.log(e.stack); }
        //     else { window.console.log(e); }
        // } 

        throw e; 
    };


    var raiseFailure = function(MACHINE, msg) {
        var contMarks = MACHINE.captureContinuationMarks();
        raise(MACHINE, ExnFail.constructor([msg, contMarks]));
    };

    var raiseContractError = function(MACHINE, msg) {
        var contMarks = MACHINE.captureContinuationMarks();
        raise(MACHINE, ExnFailContract.constructor([msg, contMarks]));
    };

    var raiseDivisionByZeroError = function(MACHINE, msg) {
        var contMarks = MACHINE.captureContinuationMarks();
        raise(MACHINE, ExnFailContractDivisionByZero.constructor([msg, contMarks]));
    };


    var raiseUnboundToplevelError = function(MACHINE, name) {
        var message = baselib.format.format("Not bound: ~a", [name]);
        var contMarks = MACHINE.captureContinuationMarks();
        raise(MACHINE, 
              ExnFailContractVariable.constructor([message, 
                                                   contMarks, 
                                                   baselib.symbols.makeSymbol(name)])); 
    };


    var raiseArgumentTypeError = function(MACHINE, 
                                          callerName,
                                          expectedTypeName,
                                          argumentOffset,
                                          actualValue) {
        var message;
        var contMarks = MACHINE.captureContinuationMarks();
        if (argumentOffset !== void(0)) {
            message = baselib.format.format(
                          "~a: expected ~a as argument ~e but received ~e",
                          [callerName,
                           expectedTypeName,
                           (argumentOffset + 1),
                           actualValue]);
            raise(MACHINE, ExnFailContract.constructor([message, contMarks]));
        } else {
            message = baselib.format.format(
                          "~a: expected ~a but received ~e",
                          [callerName,
                           expectedTypeName,
                           actualValue]);
            raise(MACHINE, ExnFailContract.constructor([message, contMarks]));
        }
    };

    var raiseContextExpectedValuesError = function(MACHINE, expected) {
        var message = baselib.format.format("expected ~e values, received ~e values",
                                            [expected, MACHINE.a]);
        var contMarks = MACHINE.captureContinuationMarks();
        raise(MACHINE, ExnFailContract.constructor([message, contMarks]));
    };

    var raiseArityMismatchError = function(MACHINE, proc, received) {
        raise(MACHINE, makeArityMismatchError(MACHINE, proc, received));
    };

    var makeArityMismatchError = function(MACHINE, proc, received) {
        var message = baselib.format.format("~a: expected ~e value(s), received ~e value(s)",
                                            [proc.displayName,
                                             proc.racketArity,
                                             received]);
        var contMarks = MACHINE.captureContinuationMarks();
        return ExnFailContractArity.constructor([message, contMarks]);
    };


    var raiseOperatorApplicationError = function(MACHINE, operator) {
        var message = baselib.format.format("not a procedure: ~e",
                                            [operator]);
        var contMarks = MACHINE.captureContinuationMarks();
        raise(MACHINE, 
              ExnFailContract.constructor([message, contMarks]));
    };


    var raiseOperatorIsNotPrimitiveProcedure = function(MACHINE, operator) {
        var message = baselib.format.format("not a primitive procedure: ~e",
                                            [operator]);
        var contMarks = MACHINE.captureContinuationMarks();
        raise(MACHINE,
              ExnFailContract.constructor([message, contMarks]));
    };


    var raiseUnimplementedPrimitiveError = function(MACHINE, name) {
        var message = "unimplemented kernel procedure: " + name;
        var contMarks = MACHINE.captureContinuationMarks();
        raise(MACHINE,
              ExnFailContract.constructor([message, contMarks]));
    };


    var raiseModuleLoadingError = function(MACHINE, name) {
        var message = "unable to dynamically load module: " + name;
        var contMarks = MACHINE.captureContinuationMarks();
        raise(MACHINE,
              ExnFail.constructor([message, contMarks]));
    };








    //////////////////////////////////////////////////////////////////////
    // Exports



    exceptions.RacketError = RacketError;
    exceptions.isRacketError = isRacketError;

    exceptions.Exn = Exn;
    exceptions.makeExn = function(msg, marks) { return Exn.constructor([msg, marks]); };
    exceptions.isExn = Exn.predicate;
    exceptions.exnMessage = function(exn) { return Exn.accessor(exn, 0); };
    exceptions.exnContMarks = function(exn) { return Exn.accessor(exn, 1); };
    exceptions.exnSetContMarks = function(exn, v) { Exn.mutator(exn, 1, v); };

    exceptions.ExnBreak = ExnBreak;
    exceptions.makeExnBreak = function(msg, marks) { return ExnBreak.constructor([msg, marks]); };
    exceptions.isExnBreak = ExnBreak.predicate;
    exceptions.exnBreakContinuation = 
        function(exn) { return ExnBreak.accessor(exn, 0); };

    exceptions.ExnFail = ExnFail;
    exceptions.makeExnFail = function(msg, marks) { return ExnFail.constructor([msg, marks]); };
    exceptions.isExnFail = ExnFail.predicate;

    exceptions.ExnFailContract = ExnFailContract;
    exceptions.makeExnFailContract = function(msg, marks) { return ExnFailContract.constructor([msg, marks]); };
    exceptions.isExnFailContract = ExnFailContract.predicate;

    exceptions.ExnFailContractArity = ExnFailContractArity;
    exceptions.makeExnFailContractArity = function(msg, marks) { return ExnFailContractArity.constructor([msg, marks]); };
    exceptions.isExnFailContractArity = ExnFailContractArity.predicate;

    exceptions.ExnFailContractVariable = ExnFailContractVariable;
    exceptions.makeExnFailContractVariable = function(msg, marks) { return ExnFailContractVariable.constructor([msg, marks]); };
    exceptions.isExnFailContractVariable = ExnFailContractVariable.predicate;
    exceptions.exnFailContractVariableId = 
        function(exn) { return ExnFailContractVariable.accessor(exn, 0); };


    exceptions.ExnFailContractDivisionByZero = ExnFailContractDivisionByZero;
    exceptions.makeExnFailContractDivisionByZero = 
        function(msg, marks) { return ExnFailContractDivisionByZero.constructor([msg, marks]); };
    exceptions.isExnFailContractDivisionByZero = ExnFailContractDivisionByZero.predicate;


    exceptions.exceptionHandlerKey = exceptionHandlerKey;




    exceptions.raise = raise;
    exceptions.raiseFailure = raiseFailure;
    exceptions.raiseContractError = raiseContractError;
    exceptions.raiseDivisionByZeroError = raiseDivisionByZeroError;
    exceptions.raiseUnboundToplevelError = raiseUnboundToplevelError;
    exceptions.raiseArgumentTypeError = raiseArgumentTypeError;
    exceptions.raiseContextExpectedValuesError = raiseContextExpectedValuesError;
    exceptions.raiseArityMismatchError = raiseArityMismatchError;
    exceptions.makeArityMismatchError = makeArityMismatchError;
    exceptions.raiseOperatorApplicationError = raiseOperatorApplicationError;
    exceptions.raiseOperatorIsNotPrimitiveProcedure = raiseOperatorIsNotPrimitiveProcedure;
    exceptions.raiseUnimplementedPrimitiveError = raiseUnimplementedPrimitiveError;
    exceptions.raiseModuleLoadingError = raiseModuleLoadingError;


}(this.plt.baselib));