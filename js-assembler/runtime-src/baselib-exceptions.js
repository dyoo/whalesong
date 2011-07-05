// Exceptions

(function(baselib) {
    var exceptions = {};
    baselib.exceptions = exceptions;



    // Error type exports
    var InternalError = function(val, contMarks) {
	this.val = val;
	this.contMarks = (contMarks ? contMarks : false);
    }


    var SchemeError = function(val) {
	this.val = val;
    }


    var IncompleteExn = function(constructor, msg, otherArgs) {
	this.constructor = constructor;
	this.msg = msg;
	this.otherArgs = otherArgs;
    };


    // (define-struct exn (message continuation-mark-set))
    var Exn = plt.baselib.structs.makeStructureType(
        'exn', false, 2, 0, false, false);


    // (define-struct (exn:break exn) (continuation))
    var ExnBreak = plt.baselib.structs.makeStructureType(
        'exn:break', Exn, 1, 0, false, false);


    var ExnFail = plt.baselib.structs.makeStructureType(
        'exn:fail', Exn, 0, 0, false, false);

    var ExnFailContract = plt.baselib.structs.makeStructureType(
        'exn:fail:contract', ExnFail, 0, 0, false, false);

    var ExnFailContractArity = plt.baselib.structs.makeStructureType(
        'exn:fail:contract:arity', ExnFailContract, 0, 0, false, false);

    var ExnFailContractVariable = plt.baselib.structs.makeStructureType(
        'exn:fail:contract:variable', ExnFailContract, 1, 0, false, false);

    var ExnFailContractDivisionByZero = plt.baselib.structs.makeStructureType(
        'exn:fail:contract:divide-by-zero', ExnFailContract, 0, 0, false, false);





    //////////////////////////////////////////////////////////////////////

    // Exports

    exceptions.InternalError = InternalError;
    exceptions.internalError = function(v, contMarks) { return new InternalError(v, contMarks); };
    exceptions.isInternalError = function(x) { return x instanceof InternalError; };


    exceptions.SchemeError = SchemeError;
    exceptions.schemeError = function(v) { return new SchemeError(v); };
    exceptions.isSchemeError = function(v) { return v instanceof SchemeError; };


    exceptions.IncompleteExn = IncompleteExn;
    exceptions.incompleteExn = function(constructor, msg, args) { return new IncompleteExn(constructor, msg, args); };
    exceptions.isIncompleteExn = function(x) { return x instanceof IncompleteExn; };


    exceptions.Exn = Exn;
    exceptions.exn = Exn.constructor;
    exceptions.isExn = Exn.predicate;
    exceptions.exnMessage = function(exn) { return Exn.accessor(exn, 0); };
    exceptions.exnContMarks = function(exn) { return Exn.accessor(exn, 1); };
    exceptions.exnSetContMarks = function(exn, v) { Exn.mutator(exn, 1, v); };

    exceptions.ExnBreak = ExnBreak;
    exceptions.exnBreak = ExnBreak.constructor;
    exceptions.isExnBreak = ExnBreak.predicate;
    exceptions.exnBreakContinuation = 
        function(exn) { return ExnBreak.accessor(exn, 0); };

    exceptions.ExnFail = ExnFail;
    exceptions.exnFail = ExnFail.constructor;
    exceptions.isExnFail = ExnFail.predicate;

    exceptions.ExnFailContract = ExnFailContract;
    exceptions.exnFailContract = ExnFailContract.constructor;
    exceptions.isExnFailContract = ExnFailContract.predicate;

    exceptions.ExnFailContractArity = ExnFailContractArity;
    exceptions.exnFailContractArity = ExnFailContractArity.constructor;
    exceptions.isExnFailContractArity = ExnFailContractArity.predicate;

    exceptions.ExnFailContractVariable = ExnFailContractVariable;
    exceptions.exnFailContractVariable = ExnFailContractVariable.constructor;
    exceptions.isExnFailContractVariable = ExnFailContractVariable.predicate;
    exceptions.exnFailContractVariableId = 
        function(exn) { return ExnFailContractVariable.accessor(exn, 0); };


    exceptions.ExnFailContractDivisionByZero = ExnFailContractDivisionByZero;
    exceptions.exnFailContractDivisionByZero = ExnFailContractDivisionByZero.constructor;
    exceptions.isExnFailContractDivisionByZero = ExnFailContractDivisionByZero.predicate;




})(this['plt'].baselib);