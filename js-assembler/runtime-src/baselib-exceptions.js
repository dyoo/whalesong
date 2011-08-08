/*jslint browser: true, undef: false, unparam: true, sub: true, vars: true, white: true, plusplus: true, maxerr: 50, indent: 4 */

// Exceptions

(function(baselib) {
    'use strict';
    var exceptions = {};
    baselib.exceptions = exceptions;



    // Error type exports
    var InternalError = function(val, contMarks) {
        this.val = val;
        this.contMarks = contMarks || false;
    };


    var SchemeError = function(val) {
        this.val = val;
    };


    var IncompleteExn = function(constructor, msg, otherArgs) {
        this.constructor = constructor;
        this.msg = msg;
        this.otherArgs = otherArgs;
    };


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
        if (Exn.predicate(e)) {
            e.message = Exn.accessor(e, 0);
        }

        if (typeof(window.console) !== 'undefined' &&
            typeof(window.console['log']) === 'function') {
            window.console.log(MACHINE);
            if (e['stack']) { window.console.log(e['stack']); }
            else { window.console.log(e); }
        } 
        throw e; 
    };




    var raiseUnboundToplevelError = function(MACHINE, name) {
        raise(MACHINE, 
              new Error(
                  baselib.format.format(
                      "Not bound: ~a",
                      [name]))); 
    };


    var raiseArgumentTypeError = function(MACHINE, 
                                          callerName,
                                          expectedTypeName,
                                          argumentOffset,
                                          actualValue) {
        if (argumentOffset !== undefined) {
            raise(MACHINE,
                  new Error(
                      baselib.format.format(
                          "~a: expected ~a as argument ~e but received ~e",
                          [callerName,
                           expectedTypeName,
                           (argumentOffset + 1),
                           actualValue])));
        } else {
            raise(MACHINE,
                  new Error(
                      baselib.format.format(
                          "~a: expected ~a but received ~e",
                          [callerName,
                           expectedTypeName,
                           actualValue])));
        }
    };

    var raiseContextExpectedValuesError = function(MACHINE, expected) {
        raise(MACHINE, 
              new Error(baselib.format.format(
                  "expected ~e values, received ~e values",
                  [expected, MACHINE.argcount])));
    };

    var raiseArityMismatchError = function(MACHINE, proc, expected, received) {
        raise(MACHINE, 
              new Error(baselib.format.format(
                  "~a: expected ~e value(s), received ~e value(s)",
                  [proc.displayName,
                   expected,
                   received])));
    };

    var raiseOperatorApplicationError = function(MACHINE, operator) {
        raise(MACHINE, 
              new Error(
                  baselib.format.format(
                      "not a procedure: ~e",
                      [operator])));
    };

    var raiseOperatorIsNotClosure = function(MACHINE, operator) {
        raise(MACHINE,
              new Error(
                  baselib.format.format(
                      "not a closure: ~e",
                      [operator])));
    };

    var raiseOperatorIsNotPrimitiveProcedure = function(MACHINE, operator) {
        raise(MACHINE,
              new Error(
                  baselib.format.format(
                      "not a primitive procedure: ~e",
                      [operator])));
    };


    var raiseUnimplementedPrimitiveError = function(MACHINE, name) {
        raise(MACHINE, 
              new Error("unimplemented kernel procedure: " + name));
    };









    //////////////////////////////////////////////////////////////////////
    // Exports

    exceptions.InternalError = InternalError;
    exceptions.internalError = function(v, contMarks) { return new InternalError(v, contMarks); };
    exceptions.isInternalError = function(x) { return x instanceof InternalError; };


    exceptions.SchemeError = SchemeError;
    exceptions.schemeError = function(v) { return new SchemeError(v); };
    exceptions.isSchemeError = function(v) { return v instanceof SchemeError; };


    exceptions.IncompleteExn = IncompleteExn;
    exceptions.makeIncompleteExn = function(constructor, msg, args) { return new IncompleteExn(constructor, msg, args); };
    exceptions.isIncompleteExn = function(x) { return x instanceof IncompleteExn; };


    exceptions.Exn = Exn;
    exceptions.makeExn = Exn.constructor;
    exceptions.isExn = Exn.predicate;
    exceptions.exnMessage = function(exn) { return Exn.accessor(exn, 0); };
    exceptions.exnContMarks = function(exn) { return Exn.accessor(exn, 1); };
    exceptions.exnSetContMarks = function(exn, v) { Exn.mutator(exn, 1, v); };

    exceptions.ExnBreak = ExnBreak;
    exceptions.makeExnBreak = ExnBreak.constructor;
    exceptions.isExnBreak = ExnBreak.predicate;
    exceptions.exnBreakContinuation = 
        function(exn) { return ExnBreak.accessor(exn, 0); };

    exceptions.ExnFail = ExnFail;
    exceptions.makeExnFail = ExnFail.constructor;
    exceptions.isExnFail = ExnFail.predicate;

    exceptions.ExnFailContract = ExnFailContract;
    exceptions.makeExnFailContract = ExnFailContract.constructor;
    exceptions.isExnFailContract = ExnFailContract.predicate;

    exceptions.ExnFailContractArity = ExnFailContractArity;
    exceptions.makeExnFailContractArity = ExnFailContractArity.constructor;
    exceptions.isExnFailContractArity = ExnFailContractArity.predicate;

    exceptions.ExnFailContractVariable = ExnFailContractVariable;
    exceptions.makeExnFailContractVariable = ExnFailContractVariable.constructor;
    exceptions.isExnFailContractVariable = ExnFailContractVariable.predicate;
    exceptions.exnFailContractVariableId = 
        function(exn) { return ExnFailContractVariable.accessor(exn, 0); };


    exceptions.ExnFailContractDivisionByZero = ExnFailContractDivisionByZero;
    exceptions.makeExnFailContractDivisionByZero = 
        ExnFailContractDivisionByZero.constructor;
    exceptions.isExnFailContractDivisionByZero = ExnFailContractDivisionByZero.predicate;


    exceptions.exceptionHandlerKey = exceptionHandlerKey;




    exceptions.raise = raise;
    exceptions.raiseUnboundToplevelError = raiseUnboundToplevelError;
    exceptions.raiseArgumentTypeError = raiseArgumentTypeError;
    exceptions.raiseContextExpectedValuesError = raiseContextExpectedValuesError;
    exceptions.raiseArityMismatchError = raiseArityMismatchError;
    exceptions.raiseOperatorApplicationError = raiseOperatorApplicationError;
    exceptions.raiseOperatorIsNotClosure = raiseOperatorIsNotClosure;
    exceptions.raiseOperatorIsNotPrimitiveProcedure = raiseOperatorIsNotPrimitiveProcedure;
    exceptions.raiseUnimplementedPrimitiveError = raiseUnimplementedPrimitiveError;


}(this.plt.baselib));