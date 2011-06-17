if (! this['plt']) { this['plt'] = {}; }


/**

Note: all primitives in this file should be written so that it's easy
to syntactically pull out all of the implemented primitives.  Make
sure that any new primitive is written as:

    PRIMITIVES[name-of-primitive] = ...

That way, we can do a simple grep.

*/


(function(scope) {
    var primitives = {};
    scope.primitives = primitives;
    var PRIMITIVES = {};


    var types = scope.types;
    var helpers = scope.helpers;

    var CALL, PAUSE, PrimProc, CasePrimitive, makeOptionPrimitive, procArityContains;
    var assocListToHash, raise;
    var isList, isListOf;
    var check;
    var checkListOf;

    CALL = types.internalCall;
    PAUSE = types.internalPause;
    PrimProc = types.PrimProc;
    CasePrimitive = types.CasePrimitive;
    makeOptionPrimitive = types.makeOptionPrimitive;
    
    procArityContains = helpers.procArityContains;
    assocListToHash = helpers.assocListToHash;
    raise = helpers.raise;
    isList = helpers.isList;
    isListOf = helpers.isListOf;
    check = helpers.check;
    checkListOf = helpers.checkListOf;
    
    scope.link.ready('types', 
                     function() {
                         types = scope.types;
                         CALL = types.internalCall;
                         PAUSE = types.internalPause;
                         PrimProc = types.PrimProc;
                         CasePrimitive = types.CasePrimitive;
                         makeOptionPrimitive = types.makeOptionPrimitive;
                     });
    scope.link.ready('helpers', 
                     function() {
                         helpers = scope.helpers;
                         procArityContains = helpers.procArityContains;
                         assocListToHash = helpers.assocListToHash;
                         raise = helpers.raise;
                         isList = helpers.isList;
                         isListOf = helpers.isListOf;
                         check = helpers.check;
                         checkListOf = helpers.checkListOf;
                     });





    //////////////////////////////////////////////////////////////////////

    // Helper Functions

    var id = function(x) { return x; };

    var sub1 = function(x) {
	check(x, isNumber, 'sub1', 'number', 1, [x]);
	return jsnums.subtract(x, 1);
    }

    var add1 = function(x) {
	check(x, isNumber, 'add1', 'number', 1, [x]);
	return jsnums.add(x, 1);
    }

    var callWithValues = function(f, vals) {
	if (vals instanceof types.ValuesWrapper) {
	    return CALL(f, vals.elts, id);
	}
	else {
	    return CALL(f, [vals], id);
	}
    };


    // onSingleResult: x (x -> y) -> y
    // Applies f on x, but first checks that x is a single value.
    // If it isn't, raises an arity error.
    var onSingleResult = function(x, f) {
        if (x instanceof types.ValuesWrapper) {
	    if (x.elts.length === 1) {
	        return f(x.elts[0]);
	    } else {
	        var argsStr = helpers.map(function(x) { return "~s"; }, x.elts).join(' ');
	        raise(types.incompleteExn(
		    types.exnFailContractArity,
		    helpers.format(
		        'context expected 1 value, received ~s values: ' + argsStr,
		        [x.elts.length].concat(x.elts))));
	    }
        } else {
	    return f(x);
        }
    };


    var procedureArity = function(proc) {
	check(proc, isFunction, 'procedure-arity', 'procedure', 1, [proc]);
	
	var singleCaseArity = function(aCase) {
	    if (aCase instanceof types.ContinuationClosureValue) {
		return types.arityAtLeast(0);
	    }
	    else if (aCase.isRest) {
		return types.arityAtLeast(aCase.numParams);
	    }
	    else {
		return aCase.numParams;
	    }
	}
	
	if ( proc instanceof PrimProc ||
	     proc instanceof types.ClosureValue ||
	     proc instanceof types.ContinuationClosureValue ) {
	    return singleCaseArity(proc);
	}
	else {
	    var cases;
	    if ( proc instanceof CasePrimitive ) {
		cases = proc.cases;
	    }
	    else if ( proc instanceof types.CaseLambdaValue ) {
		cases = proc.closures;
	    }
	    else {
		throw types.internalError('procedure-arity given wrong type that passed isFunction!', false);
	    }

	    var ret = [];
	    for (var i = 0; i < cases.length; i++) {
		ret.push( singleCaseArity(cases[i]) );
	    }
	    ret = normalizeArity(ret);
	    return ret.length == 1 ? ret[0] : types.list(ret);
	}
    };

    var normalizeArity = function(arity) {
	var newArity = arity.slice(0);
	var sortFunc = function(x, y) {
	    if ( types.isArityAtLeast(x) ) {
		if ( types.isArityAtLeast(y) ) {
		    return types.arityAtLeastValue(x) - types.arityAtLeastValue(y);
		}
		else {
		    return types.arityAtLeastValue(x) - y - 0.5;
		}
	    }
	    else {
		if ( types.isArityAtLeast(y) ) {
		    return x - types.arityAtLeastValue(y) + 0.5;
		}
		else {
		    return x - y;
		}
	    }
	};
	newArity.sort(sortFunc);

	for (var i = 0; i < newArity.length-1; i++) {
	    if ( types.isArityAtLeast(newArity[i]) ) {
		return newArity.slice(0, i+1);
	    }
	}
	return newArity;
    };







    var length = function(lst) {
	checkList(lst, 'length', 1, [lst]);
	var ret = 0;
	for (; !lst.isEmpty(); lst = lst.rest) {
	    ret = ret+1;
	}
	return ret;
    }

    var append = function(initArgs) {
	if (initArgs.length == 0) {
	    return types.EMPTY;
	}
	var args = initArgs.slice(0, initArgs.length-1);
	var lastArg = initArgs[initArgs.length - 1];
	arrayEach(args, function(x, i) {checkList(x, 'append', i+1, initArgs);});

	var ret = lastArg;
	for (var i = args.length-1; i >= 0; i--) {
	    ret = args[i].append(ret);
	}
	return ret;
    }

    var foldHelp = function(f, acc, args) {
	if ( args[0].isEmpty() ) {
	    return acc;
	}

	var fArgs = [];
	var argsRest = [];
	for (var i = 0; i < args.length; i++) {
	    fArgs.push(args[i].first);
	    argsRest.push(args[i].rest);
	}
	fArgs.push(acc);
	return CALL(f, fArgs,
		    function(result) {
			return foldHelp(f, result, argsRest);
		    });
    }

    var quicksort = function(functionName) {
	return function(initList, comp) {
	    checkList(initList, functionName, 1, arguments);
	    check(comp, procArityContains(2), functionName, 'procedure (arity 2)', 2, arguments);
	    
	    var quicksortHelp = function(k) {
		return function(lst) {
		    if ( lst.isEmpty() ) {
			return k(types.EMPTY);
		    }
		    
		    var compYes = new PrimProc('compYes', 1, false, false,
					       function(x) { return CALL(comp, [x, lst.first], id); });
		    var compNo = new PrimProc('compNo', 1, false, false,
					      function(x) { return CALL(comp, [x, lst.first],
									function(res) { return !res; });
						          });

		    return CALL(PRIMITIVES['filter'],
				[compYes, lst.rest],
				quicksortHelp(function(sorted1) {
				    return CALL(PRIMITIVES['filter'],
						[compNo, lst.rest],
						quicksortHelp(function(sorted2) {
						    return k( append([sorted1,
								      types.list([lst.first]),
								      sorted2]) );
						}));
				}));
		};
	    }
	    return quicksortHelp(id)(initList);
	};
    }

    var compare = function(args, comp) {
	var curArg = args[0];
	for (var i = 1; i < args.length; i++) {
	    if ( !comp(curArg, args[i]) ) {
		return false;
	    }
	    curArg = args[i];
	}
	return true;
    }

    // isAlphabeticString: string -> boolean
    var isAlphabeticString = function(s) {
	for(var i = 0; i < s.length; i++) {
	    if (! ((s.charAt(i) >= "a" && s.charAt(i) <= "z") ||
		   (s.charAt(i) >= "A" && s.charAt(i) <= "Z"))) {
		return false;
	    }
	}
	return true;
    };


    var isMutableString = function(s) {
        return isString(s) && typeof s != 'string';
    };


    var isNumericString = function(s) {
	for (var i = 0; i < s.length; i++) {
	    if ( ! (s.charAt(i) >= '0' && s.charAt(i) <= '9') ) {
		return false;
	    }
	}
	return true;
    }

    // isWhitespaceString: string -> boolean
    var isWhitespaceString = (function() {
	var pat = new RegExp("^\\s*$");
	return function(s) {
	    return (s.match(pat) ? true : false);
	}
    }());




    var isImmutable = function(x) {
	return ((isString(x) ||
		 isByteString(x) ||
		 isVector(x) ||
		 isHash(x) ||
		 isBox(x)) &&
		!x.mutable);
    };






    // On any numeric error, throw a contract error.
    jsnums.onThrowRuntimeError = function(msg, x, y) {
	raise(types.incompleteExn(
	    types.exnFailContract,
	    helpers.format("~a: ~s ~s", [msg, x, y]),
	    []));
    };




    var checkAndGetGuard = function(funName, guard, numberOfGuardArgs) {
	if ( !guard ) {
	    return false;
	}

	// Check the number of arguments on the guard
	if ( !procArityContains(numberOfGuardArgs)(guard) ) {
	    raise(types.incompleteExn(
		types.exnFailContract,
		helpers.format(
		    '~a: guard procedure does not accept ~a arguments '
			+ '(one more than the number constructor arguments): ~s',
		    [funName, numberOfGuardArgs, guard]),
		[]));
	}
	
	// if the guard has the right number of arguments,
	// then construct a javascript function to call it
	return function(args, name, k) {
	    args = args.concat([name]);
	    return CALL(guard, args,
			function(res) {
			    if ( res instanceof types.ValuesWrapper ) {
				return k(res.elts);
			    }
			    else {
				return k([res]);
			    }
			});
	};
    };





    var getMakeStructTypeReturns = function(aStructType) {
	var name = aStructType.name;
	return new types.ValuesWrapper(
	    [aStructType,
	     (new types.StructConstructorProc(aStructType,
					      'make-'+name,
					      aStructType.numberOfArgs,
					      false,
					      false,
					      aStructType.constructor)),
	     (new types.StructPredicateProc(aStructType, name+'?', 1, false, false, aStructType.predicate)),
	     (new types.StructAccessorProc(aStructType,
					   name+'-ref',
					   2,
					   false,
					   false,
					   function(x, i) {
					       check(x, aStructType.predicate, name+'-ref', 'struct:'+name, 1, arguments);
					       check(i, isNatural, name+'-ref', 'non-negative exact integer', 2, arguments);

					       var numFields = aStructType.numberOfFields;
					       if ( jsnums.greaterThanOrEqual(i, numFields) ) {
						   var msg = (name+'-ref: slot index for <struct:'+name+'> not in ' +
							      '[0, ' + (numFields-1) + ']: ' + i);
						   raise( types.incompleteExn(types.exnFailContract, msg, []) );
					       }
					       return aStructType.accessor(x, jsnums.toFixnum(i));
					   })),
	     (new types.StructMutatorProc(aStructType,
					  name+'-set!',
					  3,
					  false,
					  false,
					  function(x, i, v) {
					      check(x, aStructType.predicate, name+'-set!', 'struct:'+name, 1, arguments);
					      check(i, isNatural, name+'-set!', 'non-negative exact integer', 2, arguments);

					      var numFields = aStructType.numberOfFields;
					      if ( jsnums.greaterThanOrEqual(i, numFields) ) {
						  var msg = (name+'-set!: slot index for <struct'+name+'> not in ' +
							     '[0, ' + (numFields-1) + ']: ' + i);
						  raise( types.incompleteExn(types.exnFailContract, msg, []) );
					      }
					      aStructType.mutator(x, jsnums.toFixnum(i), v)
					  })) ]);
    };




    //////////////////////////////////////////////////////////////////////


    var isNumber = jsnums.isSchemeNumber;
    var isReal = jsnums.isReal;
    var isRational = jsnums.isRational;
    var isComplex = isNumber;
    var isInteger = jsnums.isInteger;

    var isNatural = function(x) {
	return jsnums.isExact(x) && isInteger(x) && jsnums.greaterThanOrEqual(x, 0);
    };

    var isNonNegativeReal = function(x) {
	return isReal(x) && jsnums.greaterThanOrEqual(x, 0);
    };

    var isSymbol = types.isSymbol;
    var isChar = types.isChar;
    var isString = types.isString;
    var isPair = types.isPair;
    var isEmpty = function(x) { return x === types.EMPTY; };
    var isVector = types.isVector;
    var isBox = types.isBox;
    var isHash = types.isHash;
    var isByteString = types.isByteString;

    var isByte = function(x) {
	return (isNatural(x) &&
		jsnums.lessThanOrEqual(x, 255));
    }

    var isBoolean = function(x) {
	return (x === true || x === false);
    }

    var isFunction = types.isFunction;

    var isEqual = function(x, y) {
	return types.isEqual(x, y, new types.UnionFind());
    }

    var isEq = function(x, y) {
	return x === y;
    }

    var isEqv = function(x, y) {
	if (isNumber(x) && isNumber(y)) {
	    return jsnums.eqv(x, y);
	}
	else if (isChar(x) && isChar(y)) {
	    return x.val === y.val;
	}
	return x === y;
    }



    var isAssocList = function(x) {
	return isPair(x) && isPair(x.rest) && isEmpty(x.rest.rest);
    };


    var isCompoundEffect = function(x) {
	return ( types.isEffect(x) || isListOf(x, isCompoundEffect) );
    };

    var isJsValue = types.isJsValue;

    var isJsObject = function(x) {
	return isJsValue(x) && typeof(x.val) == 'object';
    };

    var isJsFunction = function(x) {
	return isJsValue(x) && typeof(x.val) == 'function';
    };



    var arrayEach = function(arr, f) {
	for (var i = 0; i < arr.length; i++) {
	    f.call(null, arr[i], i);
	}
    }


    var checkList = function(x, functionName, position, args) {
	if ( !isList(x) ) {
	    helpers.throwCheckError([functionName,
				     'list',
				     helpers.ordinalize(position),
				     x],
				    position,
				    args);
	}
    }

    var checkListOfLength = function(lst, n, functionName, position, args) {
	if ( !isList(lst) || (length(lst) < n) ) {
	    helpers.throwCheckError([functionName,
				     'list with ' + n + ' or more elements',
				     helpers.ordinalize(position),
				     lst],
				    position,
				    args);
	}
    }

    var checkAllSameLength = function(lists, functionName, args) {
	if (lists.length == 0)
	    return;
	
	var len = length(lists[0]);
	arrayEach(lists,
		  function(lst, i) {
		      if (length(lst) != len) {
			  var argsStr = helpers.map(function(x) { return " ~s"; }, args).join('');
			  var msg = helpers.format(functionName + ': all lists must have the same size; arguments were:' + argsStr,
						   args);
			  raise( types.incompleteExn(types.exnFailContract, msg, []) );
		      }
		  });
    }


    //////////////////////////////////////////////////////////////////////


    // Special moby-specific primitives

    PRIMITIVES['verify-boolean-branch-value'] =
	new PrimProc('verify-boolean-branch-value',
		     2,
		     false,
		     false,
		     function(x, aLoc) { 
			 if (x !== true && x !== false) {
			     // FIXME: should throw structure
			     // make-moby-error-type:branch-value-not-boolean
			     // instead.
			     throw new Error("the value " + sys.inspect(x) + " is not boolean type at " + aLoc);
			 }
			 return x;
		     })

    PRIMITIVES['throw-cond-exhausted-error'] = 
	new PrimProc('throw-cond-exhausted-error',
		     1,
		     false,
		     false,
		     function(aLoc) {
			 // FIXME: should throw structure
			 // make-moby-error-type:conditional-exhausted
			 // instead.
			 throw types.schemeError(types.incompleteExn(types.exnFail, "cond: all question results were false", []));
		     });


    PRIMITIVES['print-values'] = 
        new PrimProc('print-values',
		     0,
		     true,
		     true,
		     function(state, values) {
		         var printed = false;
		         for (var i = 0; i < values.length; i++) {
			     if (values[i] !== types.VOID) {
			         if (printed) {
				     state.getDisplayHook()("\n");
			         }
			         state.getPrintHook()(values[i]);
			         printed = true;
			     }
		         }
		         if (printed) {
			     state.getDisplayHook()("\n");
		         }
		         state.v = types.VOID;
		     });





    //////////////////////////////////////////////////////////////////////

    var defaultPrint = 
        new PrimProc('print', 
		     1, 
		     false, 
		     true, 
		     function(state, x) {
		         state.getPrintHook()(helpers.toDisplayedString(x));
		         state.v = types.VOID;
		     });


    PRIMITIVES['write'] =
        new CasePrimitive('write',
	                  [new PrimProc('write', 1, false, true, function(aState, x) {
			      aState.getPrintHook()(x);
			      aState.v = types.VOID;
		          }),
	                   new PrimProc('write', 2, false, true, function(aState, x, port) {
		 	       throw types.internalError('write to a port not implemented yet.', false);
		           }) ]);



    PRIMITIVES['display'] = 
	new CasePrimitive('display',
		          [new PrimProc('display', 1, false, true, function(state, x) {
			      state.getDisplayHook()(x);
			      state.v = types.VOID;
	                  }),
			   new PrimProc('display', 2, false, true, function(state, x, port) {
	                       // FIXME
	                       throw types.internalError("display to a port not implemented yet.", false);
	                   } )]);



    PRIMITIVES['newline'] = 
	new CasePrimitive('newline',
	                  [new PrimProc('newline', 0, false, true, function(state) {
		              state.getDisplayHook()('\n');
	                      state.v = types.VOID;
	                  }),
	                   new PrimProc('newline', 1, false, false, function(port) {
	                       // FIXME
	                       throw types.internalError("newline to a port not implemented yet.", false);
	                   } )]);



    PRIMITIVES['current-print'] =
        new PrimProc('current-print', 
		     0, 
		     false, false,
		     function() {
		         return defaultPrint;
		     });


    PRIMITIVES['current-continuation-marks'] =
        // FIXME: should be CasePrimitive taking either 0 or 1 arguments
        new PrimProc('current-continuation-marks',
		     0,
		     false, true,
		     function(aState) {
		         aState.v = state.captureCurrentContinuationMarks(aState);
		     });

    PRIMITIVES['continuation-mark-set?'] =
	new PrimProc('continuation-mark-set?',
		     1,
		     false,
		     false,
		     types.isContinuationMarkSet);

    PRIMITIVES['continuation-mark-set->list'] = 
        new PrimProc('continuation-mark-set->list',
		     2,
		     false,
		     true,
		     function(state, markSet, keyV) {
		         check(markSet, 
			       types.isContinuationMarkSet, 
			       'continuation-mark-set->list',
			       'continuation-mark-set',
			       1,
			       [markSet, keyV]);
		         state.v = types.list(markSet.ref(keyV));
		     });



    PRIMITIVES['for-each'] =
        new PrimProc('for-each', 
		     2, 
		     true, false,
		     function(f, firstArg, arglists) {
		 	 var allArgs = [f, firstArg].concat(arglists);
		 	 arglists.unshift(firstArg);
			 check(f, isFunction, 'for-each', 'procedure', 1, allArgs);
			 arrayEach(arglists, function(lst, i) {checkList(lst, 'for-each', i+2, allArgs);});
			 checkAllSameLength(arglists, 'for-each', allArgs);
		         check(f, procArityContains(arglists.length), 'for-each', 'procedure (arity ' + arglists.length + ')', 1, allArgs);

			 var forEachHelp = function(args) {
			     if (args[0].isEmpty()) {
				 return types.VOID;
			     }

			     var argsFirst = [];
			     var argsRest = [];
			     for (var i = 0; i < args.length; i++) {
				 argsFirst.push(args[i].first);
				 argsRest.push(args[i].rest);
			     }

			     return CALL(f, argsFirst,
					 function(result) { return forEachHelp(argsRest); });
			 }

			 return forEachHelp(arglists);
		     });


    PRIMITIVES['make-thread-cell'] = 
	new CasePrimitive('make-thread-cell', [
	    new PrimProc("make-thread-cell",
		         1, false, false,
		         function(x) {
			     return new types.ThreadCell(x, false);
		         }
		        ),
	    new PrimProc("make-thread-cell",
		         2, false, false,
		         function(x, y) {
			     return new types.ThreadCell(x, y);
		         }
		        )]);



    PRIMITIVES['make-continuation-prompt-tag'] = 
	new CasePrimitive('make-continuation-prompt-tag', 
			  [
	                      new PrimProc("make-continuation-prompt-tag",
		                           0, false, false,
		                           function() {
			                       return new types.ContinuationPromptTag();
		                           }
		                          ),
	                      new PrimProc("make-continuation-prompt-tag",
		                           1, false, false,
		                           function(x) {
			                       check(x, isSymbol, 'make-continuation-prompt-tag',
			                             'symbol', 1, arguments);
			                       return new types.ContinuationPromptTag(x);
		                           }
		                          )]);



    PRIMITIVES['call-with-continuation-prompt'] =
	new PrimProc('call-with-continuation-prompt',
		     1,
		     true, true,
		     function(aState, proc, args) {

			 // First check that proc is a procedure.
			 var allArgs = [proc].concat(args);
			 check(proc, isFunction, 'call-with-continuation-prompt', 'procedure', 1, allArgs);

			 // Do other argument parsing stuff...
			 var promptTag;
			 var handler;
			 var procArgs;
			 if (args.length === 0) {
			     promptTag = types.defaultContinuationPromptTag;
			     handler = types.defaultContinuationPromptTagHandler;
			     procArgs = args.slice(0);
			 } else if (args.length === 1) {
			     promptTag = args[0];
			     handler = types.defaultContinuationPromptTagHandler;
			     procArgs = args.slice(1);
			     
			 } else if (args.length >= 2) {
			     promptTag = args[0];
			     handler = args[1];
			     procArgs = args.slice(2);
			 }

			 // If the handler is false, default to ()
			 if (handler === false) {
			     handler = defaultCallWithContinuationPromptHandler;
			 }
			 
			 // Add the prompt.
			 aState.pushControl(new control.PromptControl(aState.vstack.length,
								      promptTag,
								      handler));
			 // Within the context of the prompt, do the procedure application.
			 aState.pushControl(
			     new control.ApplicationControl(
				 new control.ConstantControl(proc), 
				 helpers.map(function(op) {
				     return new control.ConstantControl(op)},
					     procArgs)));
		     });

    PRIMITIVES['default-continuation-prompt-tag'] =
	new PrimProc('default-continuation-prompt-tag',
		     0,
		     false, false,
		     function() {
			 return types.defaultContinuationPromptTag;
		     });


    PRIMITIVES['continuation-prompt-tag?'] =
	new PrimProc('continuation-prompt-tag?',
		     1,
		     false, false,
		     types.isContinuationPromptTag);





    // Implements the default handler for a continuation prompt, if one isn't provided
    // by call-with-continuation-prompt.
    var defaultCallWithContinuationPromptHandler =
	new PrimProc('default-call-with-continuation-prompt-handler',
		     1,
		     false,
		     true,
		     function(aState, abortThunk) {
			 // The default handler accepts a single abort thunk
			 // argument, and then re-installs the prompt and continues
			 // with the abort thunk.
			 // (call-with-continuation-prompt abort-thunk prompt-tag #f)
			 aState.pushControl(
			     new control.ApplicationControl(
				 new control.ConstantControl(PRIMITIVES['call-with-continuation-prompt']), 
				 helpers.map(function(op) {
				     return new control.ConstantControl(op)},
					     [abortThunk, promptTag, false])));
		     });


    PRIMITIVES['abort-current-continuation'] =
	new PrimProc('abort-current-continuation',
 		     1,
		     true, true,
		     function(aState, promptTag, args) {
			 control.setupAbortToPrompt(aState, promptTag, args);
		     });
    











    PRIMITIVES['make-struct-type'] =
	makeOptionPrimitive(
	    'make-struct-type',
	    4,
	    [false, 
	     types.EMPTY,
	     false,
	     false,
	     types.EMPTY,
	     false,
	     false],
	    true,
	    function(userArgs,
		     aState,
		     name,
		     superType,
		     initFieldCnt,
		     autoFieldCnt,
		     autoV,
		     props,	 // FIXME: currently ignored
		     inspector,  // FIXME: currently ignored
		     procSpec,	 // FIXME: currently ignored
		     immutables, // FIXME: currently ignored
		     guard,
		     constructorName // FIXME: currently ignored
		    ) {
		check(name, isSymbol, 'make-struct-type', 'symbol', 1, userArgs);
		check(superType, function(x) { return x === false || types.isStructType(x); },
		      'make-struct-type', 'struct-type or #f', 2, userArgs);
		check(initFieldCnt, isNatural, 'make-struct-type', 'exact non-negative integer', 3, userArgs);
		check(autoFieldCnt, isNatural, 'make-struct-type', 'exact non-negative integer', 4, userArgs);
		// TODO: check props
		// TODO: check inspector
		// TODO: check procSpect
		checkListOf(immutables, isNatural, 'make-struct-type', 'exact non-negative integer', 9, userArgs);
		check(guard, function(x) { return x === false || isFunction(x); },
		      'make-struct-type', 'procedure or #f', 10, userArgs);

		var numberOfGuardArgs = initFieldCnt + 1 + (superType ? superType.numberOfArgs : 0);
		var aStructType = 
		    types.makeStructureType(name.toString(),
					    superType,
					    jsnums.toFixnum(initFieldCnt),
					    jsnums.toFixnum(autoFieldCnt),
					    autoV,
					    checkAndGetGuard('make-struct-type', guard, numberOfGuardArgs));

		aState.v = getMakeStructTypeReturns(aStructType);
	    });
    
    
    PRIMITIVES['make-struct-field-accessor'] =
	makeOptionPrimitive(
	    'make-struct-field-accessor',
	    2,
	    [false],
	    false,
	    function(userArgs, accessor, fieldPos, fieldName) {
	    	check(accessor, function(x) { return x instanceof types.StructAccessorProc && x.numParams > 1; },
		      'make-struct-field-accessor', 'accessor procedure that requires a field index', 1, userArgs);
		check(fieldPos, isNatural, 'make-struct-field-accessor', 'exact non-negative integer', 2, userArgs);
		check(fieldName, function(x) { return x === false || isSymbol(x); },
		      'make-struct-field-accessor', 'symbol or #f', 3, userArgs);

	    	var procName = accessor.type.name + '-'
		    + (fieldName ? fieldName.toString() : 'field' + fieldPos.toString());

		return new types.StructAccessorProc(accessor.type, procName, 1, false, false,
					            function(x) {
						        check(x, accessor.type.predicate, procName, 'struct:'+accessor.type.name, 1);
						        return accessor.impl(x, fieldPos);
					            });
	    });



    PRIMITIVES['make-struct-field-mutator'] =
	makeOptionPrimitive(
	    'make-struct-field-mutator',
	    2,
	    [false],
	    false,
	    function(userArgs, mutator, fieldPos, fieldName) {
	    	check(mutator, function(x) { return x instanceof types.StructMutatorProc && x.numParams > 1; },
		      'make-struct-field-mutator', 'mutator procedure that requires a field index', 1, userArgs);
		check(fieldPos, isNatural, 'make-struct-field-mutator', 'exact non-negative integer', 2, userArgs);
		check(fieldName, function(x) { return x === false || isSymbol(x); },
		      'make-struct-field-mutator', 'symbol or #f', 3, userArgs);

	    	var procName = mutator.type.name + '-'
		    + (fieldName ? fieldName.toString() : 'field' + fieldPos.toString());

		return new types.StructMutatorProc(mutator.type, procName, 2, false, false,
					           function(x, v) {
						       check(x, mutator.type.predicate, procName, 'struct:'+mutator.type.name, 1, arguments);
						       return mutator.impl(x, fieldPos, v);
					           });
	    });


    PRIMITIVES['struct-type?'] = 
	new PrimProc('struct-type?', 1, false, false, types.isStructType);

    PRIMITIVES['struct-constructor-procedure?'] =
        new PrimProc('struct-constructor-procedure?', 1, false, false,
		     function(x) {
		         return x instanceof types.StructConstructorProc; });

    PRIMITIVES['struct-predicate-procedure?'] =
        new PrimProc('struct-predicate-procedure?', 1, false, false,
		     function(x) { 
		         return x instanceof types.StructPredicateProc; });

    PRIMITIVES['struct-accessor-procedure?'] =
        new PrimProc('struct-accessor-procedure?', 1, false, false,
		     function(x) { 
		         return x instanceof types.StructAccessorProc; });

    PRIMITIVES['struct-mutator-procedure?'] =
        new PrimProc('struct-mutator-procedure?', 1, false, false,
		     function(x) {
		         return (x instanceof types.StructMutatorProc); });



    PRIMITIVES['procedure-arity'] = new PrimProc('procedure-arity', 1, false, false, procedureArity);


    PRIMITIVES['procedure-arity-includes?'] = 
	new PrimProc('procedure-arity-includes?',
		     2,
		     false,
		     false,
		     function(proc, k) {
			 check(proc, isFunction, 'procedure-arity-includes?', 'procedure', 1, [proc, k]);
			 check(k, isNatural, 'procedure-arity-includes?', 'exact non-negative integer', 2, [proc, k]);
			 return helpers.procArityContains(k)(proc);
		     });


    PRIMITIVES['make-arity-at-least'] =
	new PrimProc('make-arity-at-least',
		     1,
		     false,
		     false,
		     types.arityAtLeast);

    PRIMITIVES['arity-at-least?'] = 
	new PrimProc('arity-at-least?',
		     1,
		     false, false,
		     function(x) {
			 return types.isArityAtLeast(x);
		     });

    PRIMITIVES['arity-at-least-value'] = 
	new PrimProc('arity-at-least-value',
		     1,
		     false, false,
		     function(x) {
		 	 check(x, types.isArityAtLeast, 'arity-at-least-value', 
			       'arity-at-least', 1, [x]);
			 return types.arityAtLeastValue(x);
		     });


    PRIMITIVES['apply'] =
        new PrimProc('apply',
		     2,
		     true, false,
		     function(f, firstArg, args) {
		 	 var allArgs = [f, firstArg].concat(args);
		 	 check(f, isFunction, 'apply', 'procedure', 1, allArgs);
		 	 args.unshift(firstArg);

			 var lastArg = args.pop();
			 checkList(lastArg, 'apply', args.length+2, allArgs);
			 var args = args.concat(helpers.schemeListToArray(lastArg));

			 return CALL(f, args, id);
		     });


    PRIMITIVES['values'] =
        new PrimProc('values',
		     0,
		     true, false,
		     function(args) {
		 	 if (args.length === 1) {
			     return args[0];
			 }
		 	 return new types.ValuesWrapper(args);
		     });


    PRIMITIVES['call-with-values'] =
        new PrimProc('call-with-values',
		     2,
		     false, false,
		     function(g, r) {
		 	 check(g, procArityContains(0), 'call-with-values', 'procedure (arity 0)', 1, arguments);
			 check(r, isFunction, 'call-with-values', 'procedure', 2, arguments);

			 return CALL(g, [],
				     function(res) {
					 return callWithValues(r, res);
				     });
		     });


    PRIMITIVES['compose'] =
        new PrimProc('compose',
		     0,
		     true, false,
		     function(procs) {
		 	 arrayEach(procs, function(p, i) {check(p, isFunction, 'compose', 'procedure', i+1, procs);});

			 if (procs.length == 0) {
			     return PRIMITIVES['values'];
			 }
			 var funList = types.list(procs).reverse();
			 
			 var composeHelp = function(x, fList) {
			     if ( fList.isEmpty() ) {
				 return x;
			     }

			     return CALL(new PrimProc('', 1, false, false,
						      function(args) {
							  return callWithValues(fList.first, args);
						      }),
					 [x],
					 function(result) {
					     return composeHelp(result, fList.rest);
					 });
			 }
			 return new PrimProc('', 0, true, false,
					     function(args) {
						 if (args.length === 1) {
						     return composeHelp(args[0], funList);
						 }
					         return composeHelp(new types.ValuesWrapper(args), funList);
					     });
		     });


    PRIMITIVES['current-inexact-milliseconds'] =
        new PrimProc('current-inexact-milliseconds',
		     0,
		     false, false,
		     function() {
			 return jsnums.makeFloat((new Date()).valueOf());
		     });


    PRIMITIVES['current-seconds'] =
        new PrimProc('current-seconds',
		     0,
		     false, false,
		     function() {
		 	 return Math.floor( (new Date()).getTime() / 1000 );
		     });


    PRIMITIVES['current-inspector'] = 
        new PrimProc('current-inspector',
		     0,
		     false, false,
		     function() {
		         return false;
		     });



    PRIMITIVES['not'] =
        new PrimProc('not',
		     1,
		     false, false,
		     function(x) {
		 	 return x === false;
		     });


    PRIMITIVES['void'] =
        new PrimProc('void', 0, true, false,
		     function(args) {
		 	 return types.VOID;
		     });


    PRIMITIVES['random'] =
	new CasePrimitive('random',
	                  [new PrimProc('random', 0, false, false,
		                        function() {return types.floatpoint(Math.random());}),
	                   new PrimProc('random', 1, false, false,
		                        function(n) {
			                    check(n, isNatural, 'random', 'non-negative exact integer', 1, arguments);
			                    return Math.floor(Math.random() * jsnums.toFixnum(n));
		                        }) ]);


    PRIMITIVES['sleep'] =
        new CasePrimitive('sleep',
	                  [new PrimProc('sleep', 0, false, false, function() { return types.VOID; }),
	                   new PrimProc('sleep',
		                        1,
		                        false, false,
		                        function(secs) {
			                    check(secs, isNonNegativeReal, 'sleep', 'non-negative real number', 1);
			                    
			                    var millisecs = jsnums.toFixnum( jsnums.multiply(secs, 1000) );
			                    return PAUSE(function(caller, success, fail) {
				                setTimeout(function() { success(types.VOID); },
					                   millisecs);
			                    });
		                        }) ]);


    PRIMITIVES['identity'] = new PrimProc('identity', 1, false, false, id);


    PRIMITIVES['raise'] = 
	new PrimProc('raise', 
		     1, 
		     false, 
		     false,
		     raise);

    PRIMITIVES['error'] =
        new PrimProc('error',
		     1,
		     true, false,
		     function(arg1, args) {
		 	 var allArgs = [arg1].concat(args);
		 	 check(arg1, function(x) {return isSymbol(x) || isString(x);},
			       'error', 'symbol or string', 1, allArgs);

			 if ( isSymbol(arg1) ) {
			     if ( args.length === 0 ) {
				 raise( types.incompleteExn(types.exnFail, "error: " + arg1.val, []) );
			     }
			     var formatStr = args.shift();
			     check(formatStr, isString, 'error', 'string', 2, allArgs);

			     args.unshift(arg1);
			     raise( types.incompleteExn(types.exnFail, helpers.format('~s: '+formatStr.toString(), args), []) );
			 }
			 else {
			     var msgBuffer = [arg1.toString()];
			     for (var i = 0; i < args.length; i++) {
				 msgBuffer.push( helpers.toDisplayedString(args[i]) );
			     }
			     raise( types.incompleteExn(types.exnFail, msgBuffer.join(''), []) );
			 }
		     });



    PRIMITIVES['make-exn'] = new PrimProc('make-exn', 2, false, false, types.exn);

    PRIMITIVES['exn-message'] =
        new PrimProc('exn-message',
		     1,
		     false, false,
		     function(exn) {
		 	 check(exn, types.isExn, 'exn-message', 'exn', 1, [exn]);
			 return types.exnMessage(exn);
		     });


    PRIMITIVES['exn-continuation-marks'] =
        new PrimProc('exn-continuation-marks',
		     1,
		     false, false,
		     function(exn) {
		 	 check(exn, types.isExn, 'exn-continuation-marks', 'exn', 1, [exn]);
			 return types.exnContMarks(exn);
		     });


    PRIMITIVES['make-exn:fail'] = new PrimProc('make-exn:fail', 2, false, false, types.exnFail);


    PRIMITIVES['make-exn:fail:contract'] = new PrimProc('make-exn:fail:contract', 2, false, false, types.exnFailContract);

    PRIMITIVES['make-exn:fail:contract:arity'] =
	new PrimProc('make-exn:fail:contract:arity',
		     2,
		     false,
		     false,
		     types.exnFailContractArity);


    PRIMITIVES['make-exn:fail:contract:variable'] =
	new PrimProc('make-exn:fail:contract:variable',
		     3,
		     false,
		     false,
		     types.exnFailContractVariable);



    PRIMITIVES['make-exn:fail:contract:divide-by-zero'] =
        new PrimProc('make-exn:fail:contract:divide-by-zero', 
		     2,
		     false, 
		     false,
		     types.exnFailContractDivisionByZero);


    PRIMITIVES['exn?'] = 
	new PrimProc('exn?',
		     1,
		     false,
		     false,
		     types.isExn);


    PRIMITIVES['exn:fail?'] = 
	new PrimProc('exn:fail?',
		     1,
		     false,
		     false,
		     types.isExnFail);


    PRIMITIVES['exn:fail:contract?'] = 
	new PrimProc('exn:fail:contract?',
		     1,
		     false,
		     false,
		     types.isExnFailContract);


    PRIMITIVES['exn:fail:contract:arity?'] = 
	new PrimProc('exn:fail:contract:arity?',
		     1,
		     false,
		     false,
		     types.isExnFailContractArity);


    PRIMITIVES['exn:fail:contract:variable?'] = 
	new PrimProc('exn:fail:contract:variable?',
		     1,
		     false,
		     false,
		     types.isExnFailContractVariable);


    PRIMITIVES['exn:fail:contract:divide-by-zero?'] = 
	new PrimProc('exn:fail:contract:divide-by-zero?',
		     1,
		     false,
		     false,
		     types.isExnFailContractDivisionByZero);




    /***********************
 *** Math Primitives ***
 ***********************/


    PRIMITIVES['*'] = 
        new PrimProc('*',
		     0,
		     true, false,
		     function(args) {
		         arrayEach(args, function(x, i) {check(x, isNumber, '*', 'number', i+1, args);});

		         var result = types.rational(1);
		         for(var i = 0; i < args.length; i++) {
			     result = jsnums.multiply(args[i], result);
		         }
		         return result;
		     });



    PRIMITIVES['-'] = 
        new PrimProc("-",
		     1,
		     true, false,
		     function(x, args) {
		         var allArgs = [x].concat(args);
		         check(x, isNumber, '-', 'number', 1, allArgs);
		         arrayEach(args, function(y, i) {check(y, isNumber, '-', 'number', i+2, allArgs);});

		         if (args.length == 0) { 
			     return jsnums.subtract(0, x);
		         }
		         var result = x;
		         for (var i = 0; i < args.length; i++) {
			     result = jsnums.subtract(result, args[i]);
		         }
		         return result;
		     });


    PRIMITIVES['+'] = 
        new PrimProc("+",
		     0,
		     true, false,
		     function(args) {
		         arrayEach(args, function(x, i) {check(x, isNumber, '+', 'number', i+1, args);});

		         if (args.length == 0) { 
			     return 0;
		         }
		         var result = args[0];
		         for (var i = 1; i < args.length; i++) {
			     result = jsnums.add(result, args[i]);
		         }
		         return result;
		     });


    PRIMITIVES['='] = 
        new PrimProc("=",
		     2,
		     true, false,
		     function(x, y, args) {
		 	 args.unshift(y);
		 	 args.unshift(x);
		 	 arrayEach(args, function(z, i) {check(z, isNumber, '=', 'number', i+1, args);});

		 	 return compare(args, jsnums.equals);
		     });


    PRIMITIVES['=~'] =
        new PrimProc('=~',
		     3,
		     false, false,
		     function(x, y, range) {
		 	 check(x, isReal, '=~', 'real', 1, arguments);
			 check(y, isReal, '=~', 'real', 2, arguments);
			 check(range, isNonNegativeReal, '=~', 'non-negative-real', 3, arguments);

			 return jsnums.lessThanOrEqual(jsnums.abs(jsnums.subtract(x, y)), range);
		     });


    PRIMITIVES['/'] =
        new PrimProc('/',
		     1,
		     true, false,
		     function(x, args) {
		 	 var allArgs = [x].concat(args);
		 	 check(x, isNumber, '/', 'number', 1, allArgs);
		 	 arrayEach(args, function(y, i) {check(y, isNumber, '/', 'number', i+2, allArgs);});
			 
			 if (args.length == 0) {
			     if ( jsnums.eqv(x, 0) ) {
				 raise( types.incompleteExn(types.exnFailContractDivisionByZero, '/: division by zero', []) );
			     }	
			     return jsnums.divide(1, x);
			 }

		 	 var res = x;
		 	 for (var i = 0; i < args.length; i++) {
			     if ( jsnums.eqv(args[i], 0) ) {
				 raise( types.incompleteExn(types.exnFailContractDivisionByZero, '/: division by zero', []) );
			     }	
			     res = jsnums.divide(res, args[i]);
		 	 }
		 	 return res;
		     });



    PRIMITIVES['sub1'] =
        new PrimProc("sub1",
		     1,
		     false, false,
		     sub1);

    PRIMITIVES['add1'] =
        new PrimProc("add1",
		     1,
		     false, false,
		     add1);


    PRIMITIVES['<'] = 
        new PrimProc('<',
		     2,
		     true, false,
		     function(x, y, args) {
		 	 args.unshift(y);
		 	 args.unshift(x);
		 	 arrayEach(args, function(z, i) {check(z, isNumber, '<', 'number', i+1, args);});

		 	 return compare(args, jsnums.lessThan);
		     });


    PRIMITIVES['>'] =
        new PrimProc('>',
		     2,
		     true, false,
		     function(x, y, args) {
		 	 args.unshift(y);
		 	 args.unshift(x);
		 	 arrayEach(args, function(z, i) {check(z, isNumber, '>', 'number', i+1, args);});

		 	 return compare(args, jsnums.greaterThan);
		     });


    PRIMITIVES['<='] = 
        new PrimProc('<=',
		     2,
		     true, false,
		     function(x, y, args) {
		 	 args.unshift(y);
		 	 args.unshift(x);
		 	 arrayEach(args, function(z, i) {check(z, isNumber, '<=', 'number', i+1, args);});

		 	 return compare(args, jsnums.lessThanOrEqual);
		     });


    PRIMITIVES['>='] =
        new PrimProc('>=',
		     2,
		     true, false,
		     function(x, y, args) {
		 	 args.unshift(y);
		 	 args.unshift(x);
		 	 arrayEach(args, function(z, i) {check(z, isNumber, '>=', 'number', i+1, args);});

		 	 return compare(args, jsnums.greaterThanOrEqual);
		     });




    PRIMITIVES['abs'] =
        new PrimProc('abs',
		     1,
		     false, false,
		     function(x) {
		 	 check(x, isReal, 'abs', 'real', 1);
			 return jsnums.abs(x);
		     });


    PRIMITIVES['quotient'] =
        new PrimProc('quotient',
		     2,
		     false, false,
		     function(x, y) {
		 	 check(x, isInteger, 'quotient', 'integer', 1, arguments);
			 check(y, isInteger, 'quotient', 'integer', 2, arguments);

			 return jsnums.quotient(x, y);
		     });


    PRIMITIVES['remainder'] =
        new PrimProc('remainder',
		     2,
		     false, false,
		     function(x, y) {
		 	 check(x, isInteger, 'remainder', 'integer', 1, arguments);
			 check(y, isInteger, 'remainder', 'integer', 2, arguments);

			 return jsnums.remainder(x, y);
		     });


    PRIMITIVES['modulo'] =
        new PrimProc('modulo',
		     2,
		     false, false,
		     function(x, y) {
		 	 check(x, isInteger, 'modulo', 'integer', 1, arguments);
			 check(y, isInteger, 'modulo', 'integer', 2, arguments);

			 return jsnums.modulo(x, y);
		     });


    PRIMITIVES['max'] =
        new PrimProc('max',
		     1,
		     true, false,
		     function(x, args) {
			 args.unshift(x);
                         //		 	check(x, isReal, 'max', 'real', 1, allArgs);
			 arrayEach(args, function(y, i) {check(y, isReal, 'max', 'real', i+1, args);});

			 var curMax = x;
			 for (var i = 1; i < args.length; i++) {
			     if ( jsnums.greaterThan(args[i], curMax) ) {
				 curMax = args[i];
			     }
			 }
			 return curMax;
		     });


    PRIMITIVES['min'] =
        new PrimProc('min',
		     1,
		     true, false,
		     function(x, args) {
		 	 args.unshift(x);
                         //		 	check(x, isReal, 'min', 'real', 1);
			 arrayEach(args, function(y, i) {check(y, isReal, 'min', 'real', i+1, args);});

			 var curMin = x;
			 for (var i = 1; i < args.length; i++) {
			     if ( jsnums.lessThan(args[i], curMin) ) {
				 curMin = args[i];
			     }
			 }
			 return curMin;
		     });


    PRIMITIVES['gcd'] =
        new PrimProc('gcd',
		     1,
		     true, false,
		     function(x, args) {
		 	 var allArgs = [x].concat(args);
		 	 check(x, isInteger, 'gcd', 'integer', 1, allArgs);
		 	 arrayEach(args, function(y, i) {check(y, isInteger, 'gcd', 'integer', i+2, allArgs);});

		 	 return jsnums.gcd(x, args);
		     });

    PRIMITIVES['lcm'] =
        new PrimProc('lcm',
		     1,
		     true, false,
		     function(x, args) {
		 	 var allArgs = [x].concat(args);
		 	 check(x, isInteger, 'lcm', 'integer', 1, allArgs);
		 	 arrayEach(args, function(y, i) {check(y, isInteger, 'lcm', 'integer', i+2, allArgs);});

		 	 return jsnums.lcm(x, args);
		     });


    PRIMITIVES['floor'] =
        new PrimProc('floor',
		     1,
		     false, false,
		     function(x) {
		 	 check(x, isReal, 'floor', 'real', 1);
			 return jsnums.floor(x);
		     });


    PRIMITIVES['ceiling'] =
        new PrimProc('ceiling',
		     1,
		     false, false,
		     function(x) {
		 	 check(x, isReal, 'ceiling', 'real', 1);
			 return jsnums.ceiling(x);
		     });


    PRIMITIVES['round'] =
        new PrimProc('round',
		     1,
		     false, false,
		     function(x) {
		 	 check(x, isReal, 'round', 'real', 1);
			 return jsnums.round(x);
		     });

    PRIMITIVES['truncate'] =
        new PrimProc('truncate',
		     1,
		     false, false,
		     function(x) {
		 	 check(x, isReal, 'truncate', 'real', 1);
		         if (jsnums.lessThan(x, 0)) {
			     return jsnums.ceiling(x);
		         } else {
			     return jsnums.floor(x);
		         }
		     });



    PRIMITIVES['numerator'] =
        new PrimProc('numerator',
		     1,
		     false, false,
		     function(x) {
		 	 check(x, isRational, 'numerator', 'rational number', 1);
			 return jsnums.numerator(x);
		     });


    PRIMITIVES['denominator'] =
        new PrimProc('denominator',
		     1,
		     false, false,
		     function(x) {
		 	 check(x, isRational, 'denominator', 'rational number', 1);
			 return jsnums.denominator(x);
		     });


    PRIMITIVES['expt'] = 
        new PrimProc("expt",
		     2,
		     false, false,
		     function(x, y) {
		 	 check(x, isNumber, 'expt', 'number', 1, arguments);
			 check(y, isNumber, 'expt', 'number', 2, arguments);
		 	 return jsnums.expt(x, y);
		     });


    PRIMITIVES['exp'] =
        new PrimProc('exp',
		     1,
		     false, false,
		     function(x) {
		 	 check(x, isNumber, 'exp', 'number', 1);
			 return jsnums.exp(x);
		     });


    PRIMITIVES['log'] =
        new PrimProc('log',
		     1,
		     false, false,
		     function(x) {
		 	 check(x, isNumber, 'log', 'number', 1);
			 return jsnums.log(x);
		     });


    PRIMITIVES['sin'] =
        new PrimProc('sin',
		     1,
		     false, false,
		     function(x) {
		 	 check(x, isNumber, 'sin', 'number', 1);
			 return jsnums.sin(x);
		     });


    PRIMITIVES['cos'] =
        new PrimProc('cos',
		     1,
		     false, false,
		     function(x) {
		 	 check(x, isNumber, 'cos', 'number', 1);
			 return jsnums.cos(x);
		     });


    PRIMITIVES['tan'] =
        new PrimProc('tan',
		     1,
		     false, false,
		     function(x) {
		 	 check(x, isNumber, 'tan', 'number', 1);
			 return jsnums.tan(x);
		     });


    PRIMITIVES['asin'] =
        new PrimProc('asin',
		     1,
		     false, false,
		     function(x) {
		 	 check(x, isNumber, 'asin', 'number', 1);
			 return jsnums.asin(x);
		     });


    PRIMITIVES['acos'] =
        new PrimProc('acos',
		     1,
		     false, false,
		     function(x) {
		 	 check(x, isNumber, 'acos', 'number', 1);
			 return jsnums.acos(x);
		     });


    PRIMITIVES['atan'] =
	new CasePrimitive('atan',
			  [new PrimProc('atan',
					1,
					false, false,
					function(x) {
		 			    check(x, isNumber, 'atan', 'number', 1);
					    return jsnums.atan(x);
					}),
			   new PrimProc('atan',
					2,
					false, false,
					function(x, y) {
					    check(x, isReal, 'atan', 'number', 1);
					    check(y, isReal, 'atan', 'number', 1);
					    return jsnums.makeFloat(
						Math.atan2(jsnums.toFixnum(x),
							   jsnums.toFixnum(y)));
					})]);


    PRIMITIVES['sinh'] =
        new PrimProc('sinh',
		     1,
		     false, false,
		     function(x) {
		 	 check(x, isNumber, 'sinh', 'number', 1);
			 return jsnums.sinh(x);
		     });


    PRIMITIVES['cosh'] =
        new PrimProc('cosh',
		     1,
		     false, false,
		     function(x) {
		 	 check(x, isNumber, 'cosh', 'number', 1);
			 return jsnums.cosh(x);
		     });


    PRIMITIVES['sqr'] =
        new PrimProc('sqr',
		     1,
		     false, false,
		     function(x) {
		 	 check(x, isNumber, 'sqr', 'number', 1);
			 return jsnums.sqr(x);
		     });


    PRIMITIVES['sqrt'] =
        new PrimProc('sqrt',
		     1,
		     false, false,
		     function(x) {
		 	 check(x, isNumber, 'sqrt', 'number', 1);
			 return jsnums.sqrt(x);
		     });


    PRIMITIVES['integer-sqrt'] =
        new PrimProc('integer-sqrt',
		     1,
		     false, false,
		     function(x) {
		 	 check(x, isInteger, 'integer-sqrt', 'integer', 1);
			 return jsnums.integerSqrt(x);
		     });


    PRIMITIVES['make-rectangular'] =
        new PrimProc('make-rectangular',
		     2,
		     false, false,
		     function(x, y) {
		 	 check(x, isReal, 'make-rectangular', 'real', 1, arguments);
			 check(y, isReal, 'make-rectangular', 'real', 2, arguments);
			 return types.complex(x, y);
		     });

    PRIMITIVES['make-polar'] =
        new PrimProc('make-polar',
		     2,
		     false, false,
		     function(x, y) {
		 	 check(x, isReal, 'make-polar', 'real', 1, arguments);
			 check(x, isReal, 'make-polar', 'real', 2, arguments);
			 return jsnums.makeComplexPolar(x, y);
		     });


    PRIMITIVES['real-part'] =
        new PrimProc('real-part',
		     1,
		     false, false,
		     function(x) {
		 	 check(x, isNumber, 'real-part', 'number', 1);
			 return jsnums.realPart(x);
		     });


    PRIMITIVES['imag-part'] =
        new PrimProc('imag-part',
		     1,
		     false, false,
		     function(x) {
		 	 check(x, isNumber, 'imag-part', 'number', 1);
			 return jsnums.imaginaryPart(x);
		     });


    PRIMITIVES['angle'] =
        new PrimProc('angle',
		     1,
		     false, false,
		     function(x) {
		 	 check(x, isNumber, 'angle', 'number', 1);
			 return jsnums.angle(x);
		     });


    PRIMITIVES['magnitude'] =
        new PrimProc('magnitude',
		     1,
		     false, false,
		     function(x) {
		 	 check(x, isNumber, 'magnitude', 'number', 1);
			 return jsnums.magnitude(x);
		     });


    PRIMITIVES['conjugate'] =
        new PrimProc('conjugate',
		     1,
		     false, false,
		     function(x) {
		 	 check(x, isNumber, 'conjugate', 'number', 1);
			 return jsnums.conjugate(x);
		     });


    PRIMITIVES['sgn'] =
	new PrimProc('sgn',
		     1,
		     false, false,
		     function(x) {
			 check(x, isReal, 'sgn', 'real number', 1);
			 if (jsnums.isInexact(x)) {
			     if ( jsnums.greaterThan(x, 0) ) {
				 return jsnums.makeFloat(1);
			     } else if ( jsnums.lessThan(x, 0) ) {
				 return jsnums.makeFloat(-1);
			     } else {
				 return jsnums.makeFloat(0);
			     }
			 } else {
			     if ( jsnums.greaterThan(x, 0) ) {
				 return 1;
			     } else if ( jsnums.lessThan(x, 0) ) {
				 return -1;
			     } else {
				 return 0;
			     }
			 }
		     });


    PRIMITIVES['inexact->exact'] =
        new PrimProc('inexact->exact',
		     1,
		     false, false,
		     function (x) {
		 	 check(x, isNumber, 'inexact->exact', 'number', 1);
			 try {
			     return jsnums.toExact(x);
			 } catch(e) {
			     raise( types.exnFailContract('inexact->exact: no exact representation for '
							  + helpers.toDisplayedString(x),
							  false) );
			 }
		     });


    PRIMITIVES['exact->inexact'] =
        new PrimProc('exact->inexact',
		     1,
		     false, false,
		     function (x) {
		 	 check(x, isNumber, 'exact->inexact', 'number', 1);
			 return jsnums.toInexact(x);
		     });


    PRIMITIVES['number->string'] =
        new PrimProc('number->string',
		     1,
		     false, false,
		     function(x) {
		         check(x, isNumber, 'number->string', 'number', 1);
		         return types.string(x.toString());
		     });


    PRIMITIVES['string->number'] =
        new PrimProc('string->number',
		     1,
		     false, false,
		     function(str) {
		 	 check(str, isString, 'string->number', 'string', 1);
		         return jsnums.fromString(str.toString());
		     });


    PRIMITIVES['xml->s-exp'] =
        new PrimProc('xml->s-exp',
		     1,
		     false, false,
		     function(str) {
		 	 check(str, isString, 'xml->s-exp', 'string', 1);
		         str = str.toString();
			 if (str.length == 0) {
			     return types.string('');
			 }

			 var xmlDoc;
			 try {
			     //Internet Explorer
			     xmlDoc = new ActiveXObject("Microsoft.XMLDOM");
			     xmlDoc.async = "false";
			     xmlDoc.loadXML(s);
			     // FIXME: check parse errors
			 }
			 catch(e) {
			     var parser = new DOMParser();
			     xmlDoc = parser.parseFromString(s, "text/xml");
			     // FIXME: check parse errors
			 }

			 var parseAttributes = function(attrs) {
			     var result = types.EMPTY;
			     for (var i = 0; i < attrs.length; i++) {
				 var keyValue = types.cons(types.symbol(attrs.item(i).nodeName),
							   types.cons(attrs.item(i).nodeValue,
								      types.EMPTY));
				 result = types.cons(keyValue, result);
			     }
			     return types.cons(types.symbol("@"), result).reverse();
			 };

			 var parse = function(node) {
			     if (node.nodeType == Node.ELEMENT_NODE) {
				 var result = types.EMPTY;
				 var child = node.firstChild;
				 while (child != null) {
				     var nextResult = parse(child);
				     if (isString(nextResult) && 
					 !result.isEmpty() &&
					 isString(result.first)) {
					 result = types.cons(result.first + nextResult,
							     result.rest);
				     } else {
					 result = types.cons(nextResult, result);
				     }
				     child = child.nextSibling;
				 }
				 result = result.reverse();
				 result = types.cons(parseAttributes(node.attributes),
						     result);
				 result = types.cons(
				     types.symbol(node.nodeName),
				     result);
				 return result;
			     } else if (node.nodeType == Node.TEXT_NODE) {
				 return node.textContent;
			     } else if (node.nodeType == Node.CDATA_SECTION_NODE) {
				 return node.data;
			     } else {
				 return types.EMPTY;
			     }
			 };
			 var result = parse(xmlDoc.firstChild);
			 return result;
		     });




    /******************
 *** Predicates ***
 ******************/

    PRIMITIVES['procedure?'] = new PrimProc('procedure?', 1, false, false, isFunction);

    PRIMITIVES['pair?'] = new PrimProc('pair?', 1, false, false, isPair);
    PRIMITIVES['cons?'] = new PrimProc('cons?', 1, false, false, isPair);
    PRIMITIVES['empty?'] = new PrimProc('empty?', 1, false, false, isEmpty);
    PRIMITIVES['null?'] = new PrimProc('null?', 1, false, false, isEmpty);

    PRIMITIVES['undefined?'] = new PrimProc('undefined?', 1, false, false, function(x) { return x === types.UNDEFINED; });
    PRIMITIVES['void?'] = new PrimProc('void?', 1, false, false, function(x) { return x === types.VOID; });


    PRIMITIVES['immutable?'] = new PrimProc('immutable?', 1, false, false, isImmutable);

    PRIMITIVES['symbol?'] = new PrimProc('symbol?', 1, false, false, isSymbol);
    PRIMITIVES['string?'] = new PrimProc('string?', 1, false, false, isString);
    PRIMITIVES['char?'] = new PrimProc('char?', 1, false, false, isChar);
    PRIMITIVES['boolean?'] = new PrimProc('boolean?', 1, false, false, isBoolean);
    PRIMITIVES['vector?'] = new PrimProc('vector?', 1, false, false, isVector);
    PRIMITIVES['struct?'] = new PrimProc('struct?', 1, false, false, types.isStruct);
    PRIMITIVES['eof-object?'] = new PrimProc('eof-object?', 1, false, false, function(x) { return x === types.EOF; });
    PRIMITIVES['posn?'] = new PrimProc('posn?', 1, false, false, types.isPosn);
    PRIMITIVES['bytes?'] = new PrimProc('bytes?', 1, false, false, isByteString);
    PRIMITIVES['byte?'] = new PrimProc('byte?', 1, false, false, isByte);

    PRIMITIVES['number?'] = new PrimProc('number?', 1, false, false, isNumber);
    PRIMITIVES['complex?'] = new PrimProc('complex?', 1, false, false, isComplex);
    PRIMITIVES['real?'] = new PrimProc('real?', 1, false, false, isReal);
    PRIMITIVES['rational?'] = new PrimProc('rational?', 1, false, false, isRational);
    PRIMITIVES['integer?'] = new PrimProc('integer?', 1, false, false, isInteger);

    PRIMITIVES['exact?'] =
        new PrimProc('exact?', 1, false, false,
		     function(x) {
			 check(x, isNumber, 'exact?', 'number', 1);
			 return jsnums.isExact(x);
		     });
    PRIMITIVES['inexact?'] =
        new PrimProc('inexact?', 1, false, false,
		     function(x) {
			 check(x, isNumber, 'inexact?', 'number', 1);
			 return jsnums.isInexact(x);
		     });

    PRIMITIVES['odd?'] =
        new PrimProc('odd?',
		     1,
		     false, false,
		     function(x) {
			 check(x, isInteger, 'odd?', 'integer', 1);
			 return jsnums.equals(jsnums.modulo(x, 2), 1);
		     });
    PRIMITIVES['even?'] =
        new PrimProc('even?',
		     1,
		     false, false,
		     function(x) {
		 	 check(x, isInteger, 'even?', 'integer', 1);
			 return jsnums.equals(jsnums.modulo(x, 2), 0);
		     });

    PRIMITIVES['zero?'] =
        new PrimProc("zero?",
		     1,
		     false, false,
		     function(x) {
		         return jsnums.equals(0, x)
		     });

    PRIMITIVES['positive?'] =
        new PrimProc('positive?',
		     1,
		     false, false,
		     function(x) {
		 	 check(x, isReal, 'positive?', 'real', 1);
			 return jsnums.greaterThan(x, 0);
		     });
    PRIMITIVES['negative?'] =
        new PrimProc('negative?',
		     1,
		     false, false,
		     function(x) {
		 	 check(x, isReal, 'negative?', 'real', 1);
			 return jsnums.lessThan(x, 0);
		     });

    PRIMITIVES['box?'] = new PrimProc('box?', 1, false, false, isBox);

    PRIMITIVES['hash?'] = new PrimProc('hash?', 1, false, false, isHash);


    PRIMITIVES['eq?'] = new PrimProc('eq?', 2, false, false, isEq);
    PRIMITIVES['eqv?'] = new PrimProc('eqv?', 2, false, false, isEqv);
    PRIMITIVES['equal?'] = new PrimProc('equal?', 2, false, false, isEqual);
    PRIMITIVES['equal~?'] =
        new PrimProc('equal~?',
		     3,
		     false, false,
		     function(x, y, range) {
		 	 check(range, isNonNegativeReal, 'equal~?', 'non-negative-real', 3, arguments);

			 return (isEqual(x, y) ||
				 (isReal(x) && isReal(y) &&
				  jsnums.lessThanOrEqual(jsnums.abs(jsnums.subtract(x, y)), range)));
		     });


    PRIMITIVES['false?'] = new PrimProc('false?', 1, false, false, function(x) { return x === false; });
    PRIMITIVES['boolean=?'] =
        new PrimProc('boolean=?',
		     2,
		     false, false,
		     function(x, y) {
		 	 check(x, isBoolean, 'boolean=?', 'boolean', 1, arguments);
			 check(y, isBoolean, 'boolean=?', 'boolean', 2, arguments);
			 return x === y;
		     });

    PRIMITIVES['symbol=?'] =
        new PrimProc('symbol=?',
		     2,
		     false, false,
		     function(x, y) {
		 	 check(x, isSymbol, 'symbol=?', 'symbol', 1, arguments);
			 check(y, isSymbol, 'symbol=?', 'symbol', 2, arguments);
			 return isEqual(x, y);
		     });


    PRIMITIVES['js-value?'] = new PrimProc('js-value?', 1, false, false, isJsValue);
    PRIMITIVES['js-object?'] = new PrimProc('js-object?', 1, false, false, isJsObject);
    PRIMITIVES['js-function?'] = new PrimProc('js-function?', 1, false, false, isJsFunction);


    /***********************
 *** List Primitives ***
 ***********************/

    PRIMITIVES['cons'] =
        new PrimProc('cons',
		     2,
		     false, false,
		     function(f, r) {
                         //		 	checkList(r, "cons", 2);
		 	 return types.cons(f, r);
		     });


    PRIMITIVES['car'] =
        new PrimProc('car',
		     1,
		     false, false,
		     function(lst) {
		 	 check(lst, isPair, 'car', 'pair', 1);
			 return lst.first;
		     });

    PRIMITIVES['cdr'] =
        new PrimProc('cdr',
		     1,
		     false, false,
		     function (lst) {
			 check(lst, isPair, 'cdr', 'pair', 1);
			 return lst.rest;
		     });

    PRIMITIVES['caar'] =
        new PrimProc('caar',
		     1,
		     false, false,
		     function(lst) {
		 	 check(lst, function(x) { return (isPair(x) && isPair(x.first)); },
			       'caar', 'caarable value', 1);
		 	 return lst.first.first;
		     });

    PRIMITIVES['cadr'] =
        new PrimProc('cadr',
		     1,
		     false, false,
		     function(lst) {
		 	 check(lst, function(x) { return isPair(x) && isPair(x.rest); },
			       'cadr', 'cadrable value', 1);
			 return lst.rest.first;
		     });

    PRIMITIVES['cdar'] =
        new PrimProc('cdar',
		     1,
		     false, false,
		     function(lst) {
		 	 check(lst, function(x) { return isPair(x) && isPair(x.first); },
			       'cdar', 'cdarable value', 1);
		 	 return lst.first.rest;
		     });

    PRIMITIVES['cddr'] =
        new PrimProc('cddr',
		     1,
		     false, false,
		     function(lst) {
		 	 check(lst, function(x) { return isPair(x) && isPair(x.rest); },
			       'cddr', 'cddrable value', 1);
		 	 return lst.rest.rest;
		     });

    PRIMITIVES['caaar'] =
        new PrimProc('caaar',
		     1,
		     false, false,
		     function(lst) {
		 	 check(lst, function(x) { return ( isPair(x) &&
							   isPair(x.first) &&
							   isPair(x.first.first) ); },
			       'caaar', 'caaarable value', 1);
		 	 return lst.first.first.first;
		     });

    PRIMITIVES['caadr'] =
        new PrimProc('caadr',
		     1,
		     false, false,
		     function(lst) {
		 	 check(lst, function(x) { return ( isPair(x) &&
							   isPair(x.rest) &&
							   isPair(x.rest.first) ); },
			       'caadr', 'caadrable value', 1);
		 	 return lst.rest.first.first;
		     });

    PRIMITIVES['cadar'] =
        new PrimProc('cadar',
		     1,
		     false, false,
		     function(lst) {
		 	 check(lst, function(x) { return ( isPair(x) &&
							   isPair(x.first) &&
							   isPair(x.first.rest) ); },
			       'cadar', 'cadarable value', 1);
		 	 return lst.first.rest.first;
		     });

    PRIMITIVES['cdaar'] =
        new PrimProc('cdaar',
		     1,
		     false, false,
		     function(lst) {
		 	 check(lst, function(x) { return ( isPair(x) &&
							   isPair(x.first) &&
							   isPair(x.first.first) ); },
			       'cdaar', 'cdaarable value', 1);
		 	 return lst.first.first.rest;
		     });

    PRIMITIVES['cdadr'] =
        new PrimProc('cdadr',
		     1,
		     false, false,
		     function(lst) {
		 	 check(lst, function(x) { return ( isPair(x) &&
							   isPair(x.rest) &&
							   isPair(x.rest.first) ); },
			       'cdadr', 'cdadrable value', 1);
		 	 return lst.rest.first.rest;
		     });

    PRIMITIVES['cddar'] =
        new PrimProc('cddar',
		     1,
		     false, false,
		     function(lst) {
		 	 check(lst, function(x) { return ( isPair(x) &&
							   isPair(x.first) &&
							   isPair(x.first.rest) ); },
			       'cddar', 'cddarable value', 1);
		 	 return lst.first.rest.rest;
		     });

    PRIMITIVES['caddr'] =
        new PrimProc('caddr',
		     1,
		     false, false,
		     function(lst) {
		 	 check(lst, function(x) { return ( isPair(x) &&
							   isPair(x.rest) &&
							   isPair(x.rest.rest) ); },
			       'caddr', 'caddrable value', 1);
		 	 return lst.rest.rest.first;
		     });

    PRIMITIVES['cdddr'] =
        new PrimProc('cdddr',
		     1,
		     false, false,
		     function(lst) {
		 	 check(lst, function(x) { return ( isPair(x) &&
							   isPair(x.rest) &&
							   isPair(x.rest.rest) ); },
			       'cdddr', 'cdddrable value', 1);
		 	 return lst.rest.rest.rest;
		     });

    PRIMITIVES['cadddr'] =
        new PrimProc('cadddr',
		     1,
		     false, false,
		     function(lst) {
		 	 check(lst, function(x) { return ( isPair(x) &&
							   isPair(x.rest) &&
							   isPair(x.rest.rest) &&
				       			   isPair(x.rest.rest.rest) ); },
			       'cadddr', 'cadddrable value', 1);
		 	 return lst.rest.rest.rest.first;
		     });


    PRIMITIVES['rest'] =
        new PrimProc('rest',
		     1,
		     false, false,
		     function(lst) {
		 	 check(lst, function(x) { return isList(x) && !isEmpty(x); },
			       'rest', 'non-empty list', 1);
			 return lst.rest;
		     });

    PRIMITIVES['first'] =
        new PrimProc('first',
		     1,
		     false, false,
		     function(lst) {
		 	 check(lst, function(x) { return isList(x) && !isEmpty(x); },
			       'first', 'non-empty list', 1);
			 return lst.first;
		     });

    PRIMITIVES['second'] =
        new PrimProc('second',
		     1,
		     false, false,
		     function(lst) {
			 checkListOfLength(lst, 2, 'second', 1);
			 return lst.rest.first;
		     });

    PRIMITIVES['third'] =
        new PrimProc('third',
		     1,
		     false, false,
		     function(lst) {
		 	 checkListOfLength(lst, 3, 'third', 1);
			 return lst.rest.rest.first;
		     });

    PRIMITIVES['fourth'] =
        new PrimProc('fourth',
		     1,
		     false, false,
		     function(lst) {
		 	 checkListOfLength(lst, 4, 'fourth', 1);
			 return lst.rest.rest.rest.first;
		     });

    PRIMITIVES['fifth'] =
        new PrimProc('fifth',
		     1,
		     false, false,
		     function(lst) {
		 	 checkListOfLength(lst, 5, 'fifth', 1);
		 	 return lst.rest.rest.rest.rest.first;
		     });

    PRIMITIVES['sixth'] =
        new PrimProc('sixth',
		     1,
		     false, false,
		     function(lst) {
		 	 checkListOfLength(lst, 6, 'sixth', 1);
		 	 return lst.rest.rest.rest.rest.rest.first;
		     });

    PRIMITIVES['seventh'] =
        new PrimProc(
	    'seventh',
	    1,
	    false, false,
	    function(lst) {
		checkListOfLength(lst, 7, 'seventh', 1);
		return lst.rest.rest.rest.rest.rest.rest.first;
	    });

    PRIMITIVES['eighth'] =
        new PrimProc('eighth',
		     1,
		     false, false,
		     function(lst) {
		 	 checkListOfLength(lst, 8, 'eighth', 1);
		 	 return lst.rest.rest.rest.rest.rest.rest.rest.first;
		     });


    PRIMITIVES['length'] =
        new PrimProc('length',
		     1,
		     false, false,
		     function(lst) {
		  	 return jsnums.makeRational(length(lst));
		     });


    PRIMITIVES['list?'] = new PrimProc('list?', 1, false, false, isList);


    PRIMITIVES['list'] =
        new PrimProc('list',
		     0,
		     true, false,
		     types.list);


    PRIMITIVES['list*'] =
        new PrimProc('list*',
		     1,
		     true, false,
		     function(anItem, otherItems) {
		         if (otherItems.length == 0) {
			     return anItem;
		         }
		         var allArgs = [anItem].concat(otherItems);
		         
		         var result = allArgs[allArgs.length - 1];
		         for (var i = allArgs.length - 2 ; i >= 0; i--) {
			     result = types.cons(allArgs[i], result);
		         }
		         return result;
		         
                         // 		     var lastListItem = otherItems.pop();
                         // 		     checkList(lastListItem, 'list*', otherItems.length+2, allArgs);

                         // 		     otherItems.unshift(anItem);
                         // 		     return append([types.list(otherItems), lastListItem]);
		     });


    PRIMITIVES['list-ref'] =
        new PrimProc('list-ref',
		     2,
		     false, false,
		     function(origList, num) {
		 	 check(num, isNatural, 'list-ref', 'non-negative exact integer', 2, arguments);

			 var lst = origList;
			 var n = jsnums.toFixnum(num);
		 	 for (var i = 0; i < n; i++) {
			     // According to the documentation of list-ref, we don't actually
			     // check the whole thing as a list.  We rather do it as we walk
			     // along the cons chain.
			     if (! isPair(lst) && lst !== types.EMPTY) {
				 var msg = ('list-ref: index ' + n +
					    ' is too large for list (not a proper list): ' +
					    helpers.toDisplayedString(origList));
				 raise( types.incompleteExn(types.exnFailContract,
							    msg,
							    []) );
			     }
		 	     if (lst.isEmpty()) {
				 var msg = ('list-ref: index ' + n +
					    ' is too large for list: ' +
					    helpers.toDisplayedString(origList));
				 raise( types.incompleteExn(types.exnFailContract, msg, []) );
		 	     }
	  		     lst = lst.rest;
		 	 }


		         if (! isPair(lst) && lst !== types.EMPTY) {
			     var msg = ('list-ref: index ' + n +
				        ' is too large for list (not a proper list): ' +
				        helpers.toDisplayedString(origList));
			     raise( types.incompleteExn(types.exnFailContract,
						        msg,
						        []) );
		         }
		 	 return lst.first;
		     });

    PRIMITIVES['list-tail'] =
        new PrimProc('list-tail',
		     2,
		     false, false,
		     function(origList, num) {
			 check(num, isNatural, 'list-tail', 'non-negative exact integer', 2, arguments);

			 var lst = origList;
			 var n = jsnums.toFixnum(num);
		 	 for (var i = 0; i < n; i++) {
			     // According to the documentation of list-tail, we don't actually
			     // check the whole thing as a list.  We rather do it as we walk
			     // along the cons chain.
			     if (! isPair(lst) && lst !== types.EMPTY) {
				 var msg = ('list-tail: index ' + n +
					    ' is too large for list (not a proper list): ' +
					    helpers.toDisplayedString(origList));
				 raise( types.incompleteExn(types.exnFailContract,
							    msg,
							    []) );
			     }
			     if (lst.isEmpty()) {
				 var msg = ('list-tail: index ' + n +
					    ' is too large for list: ' +
					    helpers.toDisplayedString(origList));
				 raise( types.incompleteExn(types.exnFailContract, msg, []) );
			     }
			     lst = lst.rest;
			 }
			 return lst;
		     });


    PRIMITIVES['append'] =
        new PrimProc('append',
		     0,
		     true, false,
		     append);


    PRIMITIVES['reverse'] =
        new PrimProc('reverse',
		     1,
		     false, false,
		     function(lst) {
		 	 checkList(lst, 'reverse', 1);
		 	 return lst.reverse();
		     });


    PRIMITIVES['map'] =
        new PrimProc('map',
		     2,
		     true, false,
		     function(f, lst, arglists) {
		 	 var allArgs = [f, lst].concat(arglists);
		 	 arglists.unshift(lst);
		 	 check(f, isFunction, 'map', 'procedure', 1, allArgs);
		 	 arrayEach(arglists, function(x, i) {checkList(x, 'map', i+2, allArgs);});
			 checkAllSameLength(arglists, 'map', allArgs);
			 
		         check(f, procArityContains(arglists.length), 'map', 'procedure (arity ' + arglists.length + ')', 1, allArgs);

			 var mapHelp = function(f, args, acc) {
			     if (args[0].isEmpty()) {
				 return acc.reverse();
			     }
			     
			     var argsFirst = [];
			     var argsRest = [];
			     for (var i = 0; i < args.length; i++) {
				 argsFirst.push(args[i].first);
				 argsRest.push(args[i].rest);
			     }
			     var result = CALL(f, argsFirst,
					       function(result) {
					           return onSingleResult(result,
								         function(result) {
								             return mapHelp(f, argsRest, types.cons(result, acc));
								         });
					       });
			     return result;
			 }
			 return mapHelp(f, arglists, types.EMPTY);
		     });


    PRIMITIVES['andmap'] =
        new PrimProc('andmap',
		     2,
		     true, false,
		     function(f, lst, arglists) {
		 	 var allArgs = [f, lst].concat(arglists);
		 	 arglists.unshift(lst);
		  	 check(f, isFunction, 'andmap', 'procedure', 1, allArgs);
		  	 arrayEach(arglists, function(x, i) {checkList(x, 'andmap', i+2, allArgs);});
			 checkAllSameLength(arglists, 'andmap', allArgs);
		         check(f, procArityContains(arglists.length), 'andmap', 'procedure (arity ' + arglists.length + ')', 1, allArgs);
                         
			 var andmapHelp = function(f, args) {
			     if ( args[0].isEmpty() ) {
				 return true;
			     }

			     var argsFirst = [];
			     var argsRest = [];
			     for (var i = 0; i < args.length; i++) {
				 argsFirst.push(args[i].first);
				 argsRest.push(args[i].rest);
			     }

			     return CALL(f, argsFirst,
					 function(result) {
					     if (argsRest[0].isEmpty()) {
						 return result;
					     }
					     return onSingleResult(result,
							           function(result) {

								       return result && andmapHelp(f, argsRest);
							           });
					 });
			 }
			 return andmapHelp(f, arglists);
		     });


    PRIMITIVES['ormap'] =
        new PrimProc('ormap',
		     2,
		     true, false,
		     function(f, lst, arglists) {
		 	 var allArgs = [f, lst].concat(arglists);
		 	 arglists.unshift(lst);
		  	 check(f, isFunction, 'ormap', 'procedure', 1, allArgs);
		  	 arrayEach(arglists, function(x, i) {checkList(x, 'ormap', i+2, allArgs);});
			 checkAllSameLength(arglists, 'ormap', allArgs);

		         check(f, procArityContains(arglists.length), 'ormap', 'procedure (arity ' + arglists.length + ')', 1, allArgs);

			 var ormapHelp = function(f, args) {
			     if ( args[0].isEmpty() ) {
				 return false;
			     }

			     var argsFirst = [];
			     var argsRest = [];
			     for (var i = 0; i < args.length; i++) {
				 argsFirst.push(args[i].first);
				 argsRest.push(args[i].rest);
			     }

			     return CALL(f, argsFirst,
					 function(result) {
					     if (argsRest[0].isEmpty()) {
						 return result;
					     }
					     return onSingleResult(
						 result,
						 function(result) {
						     return result || ormapHelp(f, argsRest);
						 });
					 });
			 }
			 return ormapHelp(f, arglists);
		     });


    PRIMITIVES['memq'] =
        new PrimProc('memq',
		     2,
		     false, false,
		     function(item, origList) {
		         var lst = origList;
		         if (! isPair(lst) && lst !== types.EMPTY) {
			     var msg = ('memq: not a proper list: ' +
				        helpers.toDisplayedString(origList));
			     raise( types.incompleteExn(types.exnFailContract,
							msg,
						        []) );
		         }
			 while ( !lst.isEmpty() ) {

			     if ( isEq(item, lst.first) ) {
				 return lst;
			     }
			     lst = lst.rest;
			     if (! isPair(lst) && lst !== types.EMPTY) {
				 var msg = ('memq: not a proper list: ' +
					    helpers.toDisplayedString(origList));
				 raise( types.incompleteExn(types.exnFailContract,
							    msg,
							    []) );
			     }
			 }
			 return false;
		     });


    PRIMITIVES['memv'] =
        new PrimProc('memv',
		     2,
		     false, false,
		     function(item, origList) {
		         var lst = origList;
			 if (! isPair(lst) && lst !== types.EMPTY) {
			     var msg = ('memv: not a proper list: ' +
					helpers.toDisplayedString(origList));
			     raise( types.incompleteExn(types.exnFailContract,
							msg,
							[]) );
			 }
			 while ( !lst.isEmpty() ) {
			     if ( isEqv(item, lst.first) ) {
				 return lst;
			     }
			     lst = lst.rest;
			     if (! isPair(lst) && lst !== types.EMPTY) {
				 var msg = ('memv: not a proper list: ' +
					    helpers.toDisplayedString(origList));
				 raise( types.incompleteExn(types.exnFailContract,
							    msg,
							    []) );
			     }
			 }
			 return false;
		     });


    PRIMITIVES['member'] =
        new PrimProc('member',
		     2,
		     false, false,
		     function(item, origList) {
		         var lst = origList;
		 	 //checkList(lst, 'member', 2, arguments);
		         if (! isPair(lst) && lst !== types.EMPTY) {
			     var msg = ('member: not a proper list: ' +
				        helpers.toDisplayedString(origList));
			     raise( types.incompleteExn(types.exnFailContract,
						        msg,
						        []) );
		         }
		 	 while ( !lst.isEmpty() ) {
		 	     if ( isEqual(item, lst.first) ) {
		 		 return lst;
		 	     }
		 	     lst = lst.rest;

			     if (! isPair(lst) && lst !== types.EMPTY) {
				 var msg = ('member: not a proper list: ' +
					    helpers.toDisplayedString(origList));
				 raise( types.incompleteExn(types.exnFailContract,
							    msg,
							    []) );
			     }
		 	 }
		 	 return false;
		     });


    PRIMITIVES['memf'] =
        new PrimProc('memf',
		     2,
		     false, false,
		     function(f, initList) {
		 	 check(f, isFunction, 'memf', 'procedure', 1, arguments);
			 checkList(initList, 'memf', 2, arguments);

			 var memfHelp = function(lst) {
			     if ( lst.isEmpty() ) {
				 return false;
			     }

			     return CALL(f, [lst.first],
					 function(result) {
					     if (result) {
						 return lst;
					     }
					     return memfHelp(lst.rest);
					 });
			 }
			 return memfHelp(initList);
		     });


    PRIMITIVES['assq'] =
        new PrimProc('assq',
		     2,
		     false, false,
		     function(item, origList) {
		         var lst = origList;
		         // checkListOf(lst, isPair, 'assq', 'pair', 2, arguments);
		         if (! isPair(lst) && lst !== types.EMPTY) {
			     var msg = ('assq: not a proper list: ' +
				        helpers.toDisplayedString(origList));
			     raise( types.incompleteExn(types.exnFailContract,
						        msg,
						        []) );
		         }
			 while ( !lst.isEmpty() ) {
			     if (! isPair(lst.first)) {
				 var msg = ('assq: non-pair found in list: ' +
					    helpers.toDisplayedString(lst.first) +' in  ' +
					    helpers.toDisplayedString(origList));
				 raise( types.incompleteExn(types.exnFailContract,
							    msg,
							    []) );
			     }
			     if ( isEq(item, lst.first.first) ) {
				 return lst.first;
			     }
			     lst = lst.rest;

			     if (! isPair(lst) && lst !== types.EMPTY) {
				 var msg = ('assq: not a proper list: ' +
					    helpers.toDisplayedString(origList));
				 raise( types.incompleteExn(types.exnFailContract,
							    msg,
							    []) );
			     }
			 }
			 return false;
		     });


    PRIMITIVES['assv'] =
        new PrimProc('assv',
		     2,
		     false, false,
		     function(item, origList) {
		         //checkListOf(lst, isPair, 'assv', 'pair', 2, arguments);
		         var lst = origList;
		         if (! isPair(lst) && lst !== types.EMPTY) {
			     var msg = ('assv: not a proper list: ' +
				        helpers.toDisplayedString(origList));
			     raise( types.incompleteExn(types.exnFailContract,
						        msg,
						        []) );
		         }
		         while ( !lst.isEmpty() ) {
			     if (! isPair(lst.first)) {
			         var msg = ('assv: non-pair found in list: ' +
					    helpers.toDisplayedString(lst.first) +' in  ' +
					    helpers.toDisplayedString(origList));
			         raise( types.incompleteExn(types.exnFailContract,
							    msg,
							    []) );
			     }
			     if ( isEqv(item, lst.first.first) ) {
				 return lst.first;
			     }
			     lst = lst.rest;
			     if (! isPair(lst) && lst !== types.EMPTY) {
			         var msg = ('assv: not a proper list: ' +
					    helpers.toDisplayedString(origList));
			         raise( types.incompleteExn(types.exnFailContract,
							    msg,
							    []) );
			     }
			 }
			 return false;
		     });


    PRIMITIVES['assoc'] =
        new PrimProc('assoc',
		     2,
		     false, false,
		     function(item, origList) {
		         var lst = origList;
		         //checkListOf(lst, isPair, 'assoc', 'pair', 2, arguments);
		         if (! isPair(lst) && lst !== types.EMPTY) {
			     var msg = ('assoc: not a proper list: ' +
					helpers.toDisplayedString(origList));
			     raise( types.incompleteExn(types.exnFailContract,
						        msg,
						        []) );
		         }
			 while ( !lst.isEmpty() ) {
			     if (! isPair(lst.first)) {
				 var msg = ('assoc: non-pair found in list: ' +
					    helpers.toDisplayedString(lst.first) +' in  ' +
					    helpers.toDisplayedString(origList));
				 raise( types.incompleteExn(types.exnFailContract,
							    msg,
							    []) );
			     }
			     if ( isEqual(item, lst.first.first) ) {
				 return lst.first;
			     }
			     lst = lst.rest;

			     if (! isPair(lst) && lst !== types.EMPTY) {
				 var msg = ('assoc: not a proper list: ' +
					    helpers.toDisplayedString(origList));
				 raise( types.incompleteExn(types.exnFailContract,
							    msg,
							    []) );
			     }
			 }
			 return false;
		     });


    PRIMITIVES['remove'] =
        new PrimProc('remove',
		     2,
		     false, false,
		     function(item, lst) {
		 	 checkList(lst, 'remove', 2, arguments);
		 	 var originalLst = lst;
		 	 var result = types.EMPTY;
		 	 while ( !lst.isEmpty() ) {
		 	     if ( isEqual(item, lst.first) ) {
		 		 return append([result.reverse(), lst.rest]);
		 	     } else {
		 		 result = types.cons(lst.first, result);
		 		 lst = lst.rest;
		 	     }
		 	 }
		 	 return originalLst;
		     });


    PRIMITIVES['filter'] =
        new PrimProc('filter',
		     2,
		     false, false,
		     function(f, lst) {
		 	 check(f, procArityContains(1), 'filter', 'procedure (arity 1)', 1, arguments);
			 checkList(lst, 'filter', 2);

			 var filterHelp = function(f, lst, acc) {
			     if ( lst.isEmpty() ) {
				 return acc.reverse();
			     }

			     return CALL(f, [lst.first],
					 function(result) {
					     if (result) {
						 return filterHelp(f, lst.rest,
								   types.cons(lst.first, acc));
					     }
					     else {
						 return filterHelp(f, lst.rest, acc);
					     }
					 });
			 }
			 return filterHelp(f, lst, types.EMPTY);
		     });

    PRIMITIVES['foldl'] =
        new PrimProc('foldl',
		     3,
		     true, false,
		     function(f, initAcc, lst, arglists) {
		 	 arglists.unshift(lst);
			 var allArgs = [f, initAcc].concat(arglists);
		 	 check(f, isFunction, 'foldl', 'procedure', 1, allArgs);
			 arrayEach(arglists, function(x, i) {checkList(x, 'foldl', i+3, allArgs);});
			 checkAllSameLength(arglists, 'foldl', allArgs);
	                 
			 return foldHelp(f, initAcc, arglists);
		     });

    PRIMITIVES['foldr'] =
        new PrimProc('foldr',
		     3,
		     true, false,
		     function(f, initAcc, lst, arglists) {
		 	 arglists.unshift(lst);
			 var allArgs = [f, initAcc].concat(arglists);
		 	 check(f, isFunction, 'foldr', 'procedure', 1, allArgs);
			 arrayEach(arglists, function(x, i) {checkList(x, 'foldr', i+3, allArgs);});
			 checkAllSameLength(arglists, 'foldr', allArgs);

			 for (var i = 0; i < arglists.length; i++) {
			     arglists[i] = arglists[i].reverse();
			 }
			 
			 return foldHelp(f, initAcc, arglists);
		     });


    PRIMITIVES['quicksort'] = new PrimProc('quicksort', 2, false, false, quicksort('quicksort'));
    PRIMITIVES['sort'] = new PrimProc('sort', 2, false, false, quicksort('sort'));



    PRIMITIVES['argmax'] =
        new PrimProc('argmax',
		     2,
		     false, false,
		     function(f, initList) {
		 	 var args = arguments
		 	 check(f, isFunction, 'argmax', 'procedure', 1, args);
			 check(initList, isPair, 'argmax', 'non-empty list', 2, args);

			 var argmaxHelp = function(lst, curMaxVal, curMaxElt) {
			     if ( lst.isEmpty() ) {
				 return curMaxElt;
			     }

			     return CALL(f, [lst.first],
					 function(result) {
					     check(result, isReal, 'argmax',
						   'procedure that returns real numbers', 1, args);
					     if (jsnums.greaterThan(result, curMaxVal)) {
						 return argmaxHelp(lst.rest, result, lst.first);
					     }
					     else {
						 return argmaxHelp(lst.rest, curMaxVal, curMaxElt);
					     }
					 });
			 }
			 return CALL(f, [initList.first],
				     function(result) {
					 check(result, isReal, 'argmax', 'procedure that returns real numbers', 1, args);
					 return argmaxHelp(initList.rest, result, initList.first);
				     });
		     });


    PRIMITIVES['argmin'] =
        new PrimProc('argmin',
		     2,
		     false, false,
		     function(f, initList) {
		 	 var args = arguments;
		 	 check(f, isFunction, 'argmin', 'procedure', 1, args);
			 check(initList, isPair, 'argmin', 'non-empty list', 2, args);

			 var argminHelp = function(lst, curMaxVal, curMaxElt) {
			     if ( lst.isEmpty() ) {
				 return curMaxElt;
			     }

			     return CALL(f, [lst.first],
					 function(result) {
					     check(result, isReal, 'argmin',
						   'procedure that returns real numbers', 1, args);
					     if (jsnums.lessThan(result, curMaxVal)) {
						 return argminHelp(lst.rest, result, lst.first);
					     }
					     else {
						 return argminHelp(lst.rest, curMaxVal, curMaxElt);
					     }
					 });
			 }
			 return CALL(f, [initList.first],
				     function(result) {
					 check(result, isReal, 'argmin', 'procedure that returns real numbers', 1, args);
					 return argminHelp(initList.rest, result, initList.first);
				     });
		     });


    PRIMITIVES['build-list'] =
        new PrimProc('build-list',
		     2,
		     false, false,
		     function(num, f) {
		 	 check(num, isNatural, 'build-list', 'non-negative exact integer', 1, arguments);
			 check(f, isFunction, 'build-list', 'procedure', 2, arguments);

			 var buildListHelp = function(n, acc) {
			     if ( jsnums.greaterThanOrEqual(n, num) ) {
				 return acc.reverse();
			     }

			     return CALL(f, [n],
					 function (result) {
					     return buildListHelp(n+1, types.cons(result, acc));
					 });
			 }
			 return buildListHelp(0, types.EMPTY);
		     });


    /**********************
 *** Box Primitives ***
 **********************/


    PRIMITIVES['box'] = new PrimProc('box', 1, false, false, types.box);

    PRIMITIVES['box-immutable'] = new PrimProc('box-immutable', 1, false, false, types.boxImmutable);

    PRIMITIVES['unbox'] =
        new PrimProc('unbox',
		     1,
		     false, false,
		     function(box) {
		 	 check(box, isBox, 'unbox', 'box', 1);
			 return box.ref();
		     });


    PRIMITIVES['set-box!'] =
        new PrimProc('set-box!',
		     2,
		     false, false,
		     function(box, newVal) {
		 	 check(box, function(x) { return isBox(x) && x.mutable; }, 'set-box!', 'mutable box', 1, arguments);
			 box.set(newVal);
			 return types.VOID;
		     });



    /****************************
 *** Hashtable Primitives ***
 ****************************/


    PRIMITIVES['make-hash'] =
	new CasePrimitive('make-hash', 
	                  [new PrimProc('make-hash', 0, false, false, function() { return types.hash(types.EMPTY); }),
	                   new PrimProc('make-hash',
		                        1,
		                        false, false,
		                        function(lst) {
			                    checkListOf(lst, isPair, 'make-hash', 'list of pairs', 1);
			                    return types.hash(lst);
		                        }) ]);

    PRIMITIVES['make-hasheq'] =
	new CasePrimitive('make-hasheq',
	                  [new PrimProc('make-hasheq', 0, false, false, function() { return types.hashEq(types.EMPTY); }),
	                   new PrimProc('make-hasheq',
		                        1,
		                        false, false,
		                        function(lst) {
			                    checkListOf(lst, isPair, 'make-hasheq', 'list of pairs', 1);
			                    return types.hashEq(lst);
		                        }) ]);

    PRIMITIVES['hash-set!'] =
        new PrimProc('hash-set!',
		     3,
		     false, false,
		     function(obj, key, val) {
		 	 check(obj, isHash, 'hash-set!', 'hash', 1, arguments);
			 obj.hash.put(key, val);
			 return types.VOID;
		     });

    PRIMITIVES['hash-ref'] =
	new CasePrimitive('hash-ref',
	                  [new PrimProc('hash-ref',
		                        2,
		                        false, false,
		                        function(obj, key) {
			                    check(obj, isHash, 'hash-ref', 'hash', 1, arguments);

			                    if ( !obj.hash.containsKey(key) ) {
			  	                var msg = 'hash-ref: no value found for key: ' + helpers.toDisplayedString(key);
			  	                raise( types.incompleteExn(types.exnFailContract, msg, []) );
			                    }
			                    return obj.hash.get(key);
		                        }),
	                   new PrimProc('hash-ref',
		                        3,
		                        false, false,
		                        function(obj, key, defaultVal) {
			                    check(obj, isHash, 'hash-ref', 'hash', 1, arguments);

			                    if (obj.hash.containsKey(key)) {
				                return obj.hash.get(key);
			                    }
			                    else {
				                if (isFunction(defaultVal)) {
					            return CALL(defaultVal, [], id);
				                }
				                return defaultVal;
			                    }
		                        }) ]);

    PRIMITIVES['hash-remove!'] =
        new PrimProc('hash-remove',
		     2,
		     false, false,
		     function(obj, key) {
		 	 check(obj, isHash, 'hash-remove!', 'hash', 1, arguments);
			 obj.hash.remove(key);
			 return types.VOID;
		     });

    PRIMITIVES['hash-map'] =
        new PrimProc('hash-map',
		     2,
		     false, false,
		     function(ht, f) {
		 	 check(ht, isHash, 'hash-map', 'hash', 1, arguments);
			 check(f, isFunction, 'hash-map', 'procedure', 2, arguments);
			 
			 var keys = ht.hash.keys();
			 var hashMapHelp = function(i, acc) {
			     if (i >= keys.length) {
				 return acc;
			     }

			     var val = ht.hash.get(keys[i]);
			     return CALL(f, [keys[i], val],
					 function(result) {
					     return hashMapHelp(i+1, types.cons(result, acc));
					 });
			 }
			 return hashMapHelp(0, types.EMPTY);
		     });


    PRIMITIVES['hash-for-each'] =
        new PrimProc('hash-for-each',
		     2,
		     false, false,
		     function(ht, f) {
		 	 check(ht, isHash, 'hash-for-each', 'hash', 1, arguments);
			 check(f, isFunction, 'hash-for-each', 'procedure', 2, arguments);
		 	 
		 	 var keys = ht.hash.keys();
		 	 var hashForEachHelp = function(i) {
		 	     if (i >= keys.length) {
				 return types.VOID;
			     }

			     var val = ht.hash.get(keys[i]);
			     return CALL(f, [keys[i], val],
					 function(result) {
					     return hashForEachHelp(i+1);
					 });
			 }
			 return hashForEachHelp(0);
		     });



    /*************************
 *** String Primitives ***
 *************************/


    var makeStringImpl = function(n, c) {
        check(n, isNatural, 'make-string', 'non-negative exact integer', 1, arguments);
        check(c, isChar, 'make-string', 'char', 2, arguments);
        var ret = [];
        for (var i = 0; jsnums.lessThan(i, n); i++) {
	    ret.push(c.val);
        }
        return types.string(ret);
    };

    PRIMITIVES['make-string'] =
	new CasePrimitive(
	    'make-string',
	    [new PrimProc('make-string',
			  2,
			  false, false,
			  makeStringImpl),
	     new PrimProc('make-string',
			  1,
			  false, false,
			  function(n) {
			      return makeStringImpl(n, types.character(String.fromCharCode(0)));
			  })]);
    
    

    PRIMITIVES['replicate'] =
        new PrimProc('replicate',
		     2,
		     false, false,
		     function(n, str) {
		 	 check(n, isNatural, 'replicate', 'non-negative exact integer', 1, arguments);
			 check(str, isString, 'replicate', 'string', 2, arguments);

			 var ret = "";
			 var primStr = str.toString();
			 for (var i = 0; jsnums.lessThan(i, n); i++) {
			     ret += primStr;
			 }
			 return types.string(ret);
		     });


    PRIMITIVES['string'] =
        new PrimProc('string',
		     0,
		     true, false,
		     function(chars) {
			 arrayEach(chars, function(c, i) {check(c, isChar, 'string', 'char', i+1, chars);});

			 var ret = [];
			 for (var i = 0; i < chars.length; i++) {
			     ret.push(chars[i].val);
			 }
			 return types.string(ret);
		     });


    PRIMITIVES['string-length'] =
        new PrimProc('string-length', 1, false, false,
		     function(str) {
		         check(str, isString, 'string-length', 'string', 1);
		         return str.toString().length;
		     });


    PRIMITIVES['string-ref'] =
        new PrimProc('string-ref',
		     2,
		     false, false,
		     function(str, num) {
		 	 check(str, isString, 'string-ref', 'string', 1, arguments);
			 check(num, isNatural, 'string-ref', 'non-negative exact integer', 2, arguments);

		         str = str.toString();
			 var n = jsnums.toFixnum(num);
			 if (n >= str.length) {
			     var msg = ('string-ref: index ' + n + ' out of range ' +
					'[0, ' + (str.length-1) + '] for string: ' +
					helpers.toDisplayedString(str));
			     raise( types.incompleteExn(types.exnFailContract, msg, []) );
			 }
			 return types.character(str.charAt(n));
		     });


    PRIMITIVES['string=?'] =
        new PrimProc('string=?',
		     2,
		     true, false,
		     function(str1, str2, strs) {
		 	 strs.unshift(str2);
		 	 strs.unshift(str1);
		 	 arrayEach(strs, function(str, i) {check(str, isString, 'string=?', 'string', i+1, strs);});
		 	 
			 return compare(strs, function(strA, strB) {return strA.toString() === strB.toString();});
		     });


    PRIMITIVES['string-ci=?'] =
        new PrimProc('string-ci=?',
		     2,
		     true, false,
		     function(str1, str2, strs) {
		 	 strs.unshift(str2);
			 strs.unshift(str1);

			 for(var i = 0; i < strs.length; i++) {
			     check(strs[i], isString, 'string-ci=?', 'string', i+1, strs);
			     strs[i] = strs[i].toString().toLowerCase();
			 }

			 return compare(strs, function(strA, strB) {return strA === strB;});
		     });


    PRIMITIVES['string<?'] =
        new PrimProc('string<?',
		     2,
		     true, false,
		     function(str1, str2, strs) {
		 	 strs.unshift(str2);
			 strs.unshift(str1);
			 arrayEach(strs, function(str, i) {check(str, isString, 'string<?', 'string', i+1, strs);});

			 return compare(strs, function(strA, strB) {return strA.toString() < strB.toString();});
		     });


    PRIMITIVES['string>?'] =
        new PrimProc('string>?',
		     2,
		     true, false,
		     function(str1, str2, strs) {
		 	 strs.unshift(str2);
			 strs.unshift(str1);
			 arrayEach(strs, function(str, i) {check(str, isString, 'string>?', 'string', i+1, strs);});

			 return compare(strs, function(strA, strB) {return strA.toString() > strB.toString();});
		     });


    PRIMITIVES['string<=?'] =
        new PrimProc('string<=?',
		     2,
		     true, false,
		     function(str1, str2, strs) {
		 	 strs.unshift(str2);
			 strs.unshift(str1);
			 arrayEach(strs, function(str, i) {check(str, isString, 'string<=?', 'string', i+1, strs);});

			 return compare(strs, function(strA, strB) {return strA.toString() <= strB.toString();});
		     });


    PRIMITIVES['string>=?'] =
        new PrimProc('string>=?',
		     2,
		     true, false,
		     function(str1, str2, strs) {
		 	 strs.unshift(str2);
			 strs.unshift(str1);
			 arrayEach(strs, function(str, i) {check(str, isString, 'string>=?', 'string', i+1, strs);});

			 return compare(strs, function(strA, strB) {return strA.toString() >= strB.toString();});
		     });


    PRIMITIVES['string-ci<?'] =
        new PrimProc('string-ci<?',
		     2,
		     true, false,
		     function(str1, str2, strs) {
		 	 strs.unshift(str2);
			 strs.unshift(str1);

			 for (var i = 0; i < strs.length; i++) {
			     check(strs[i], isString, 'string-ci<?', 'string', i+1, strs);
			     strs[i] = strs[i].toString().toLowerCase();
			 }

			 return compare(strs, function(strA, strB) {return strA < strB;});
		     });


    PRIMITIVES['string-ci>?'] =
        new PrimProc('string-ci>?',
		     2,
		     true, false,
		     function(str1, str2, strs) {
		 	 strs.unshift(str2);
			 strs.unshift(str1);

			 for (var i = 0; i < strs.length; i++) {
			     check(strs[i], isString, 'string-ci>?', 'string', i+1, strs);
			     strs[i] = strs[i].toString().toLowerCase();
			 }

			 return compare(strs, function(strA, strB) {return strA > strB;});
		     });


    PRIMITIVES['string-ci<=?'] =
        new PrimProc('string-ci<=?',
		     2,
		     true, false,
		     function(str1, str2, strs) {
		 	 strs.unshift(str2);
			 strs.unshift(str1);

			 for (var i = 0; i < strs.length; i++) {
			     check(strs[i], isString, 'string-ci<=?', 'string', i+1, strs);
			     strs[i] = strs[i].toString().toLowerCase();
			 }

			 return compare(strs, function(strA, strB) {return strA <= strB;});
		     });


    PRIMITIVES['string-ci>=?'] =
        new PrimProc('string-ci>=?',
		     2,
		     true, false,
		     function(str1, str2, strs) {
		 	 strs.unshift(str2);
			 strs.unshift(str1);

			 for (var i = 0; i < strs.length; i++) {
			     check(strs[i], isString, 'string-ci>=?', 'string', i+1, strs);
			     strs[i] = strs[i].toString().toLowerCase();
			 }

			 return compare(strs, function(strA, strB) {return strA >= strB;});
		     });


    PRIMITIVES['substring'] =
	new CasePrimitive('substring', 
	                  [new PrimProc('substring',
		                        2,
		                        false, false,
		                        function(str, theStart) {
			                    check(str, isString, 'substring', 'string', 1, arguments);
			                    check(theStart, isNatural, 'substring', 'non-negative exact integer', 2, arguments);
			                    str = str.toString();
			                    var start = jsnums.toFixnum(theStart);
			                    if (start > str.length) {
			   	                var msg = ('substring: starting index ' + start + ' out of range ' +
					                   '[0, ' + str.length + '] for string: ' + helpers.toDisplayedString(str));
				                raise( types.incompleteExn(types.exnFailContract, msg, []) );
			                    }
			                    else {
			  	                return types.string( str.substring(jsnums.toFixnum(start)) );
			                    }
		                        }),
	                   new PrimProc('substring',
		                        3,
		                        false, false,
		                        function(str, theStart, theEnd) {
			                    check(str, isString, 'substring', 'string', 1, arguments);
			                    check(theStart, isNatural, 'substring', 'non-negative exact integer', 2, arguments);
			                    check(theEnd, isNatural, 'substring', 'non-negative exact integer', 3, arguments);
			                    str = str.toString();
			                    var start = jsnums.toFixnum(theStart);
			                    var end = jsnums.toFixnum(theEnd);
			                    if (start > str.length) {
			   	                var msg = ('substring: starting index ' + start + ' out of range ' +
					                   '[0, ' + str.length + '] for string: ' + helpers.toDisplayedString(str));
				                raise( types.incompleteExn(types.exnFailContract, msg, []) );
			                    }
			                    if (end < start || end > str.length) {
			   	                var msg = ('substring: ending index ' + end + ' out of range ' + '[' + start +
					                   ', ' + str.length + '] for string: ' + helpers.toDisplayedString(str));
				                raise( types.incompleteExn(types.exnFailContract, msg, []) );
			                    }
			                    return types.string( str.substring(start, end) );
		                        }) ]);


    PRIMITIVES['string-append'] = 
        new PrimProc("string-append",
		     0,
		     true, false,
		     function(args) {
		 	 arrayEach(args,
				   function(str, i) {
				       check(str, isString, 'string-append', 'string', i+1, args);
				   });
			 
			 for (var i = 0; i < args.length; i++) {
			     args[i] = args[i].toString();
			 }
			 return types.string(args.join(""));
		     });


    PRIMITIVES['string->list'] =
        new PrimProc('string->list',
		     1,
		     false, false,
		     function(str) {
		 	 check(str, isString, 'string->list', 'string', 1);
		         str = str.toString();
			 var lst = types.EMPTY;
			 for (var i = str.length-1; i >= 0; i--) {
			     lst = types.cons(types.character(str.charAt(i)), lst);
			 }
			 return lst;
		     });


    PRIMITIVES['list->string'] =
        new PrimProc('list->string',
		     1,
		     false, false,
		     function(lst) {
		 	 checkListOf(lst, isChar, 'list->string', 'char', 1);

			 var ret = [];
			 while( !lst.isEmpty() ) {
			     ret.push(lst.first.val);
			     lst = lst.rest;
			 }
			 return types.string(ret);
		     });


    PRIMITIVES['string-copy'] =
        new PrimProc('string-copy',
		     1,
		     false, false,
		     function(str) {
		 	 check(str, isString, 'string-copy', 'string', 1);
			 return types.string(str.toString());
		     });



    PRIMITIVES['string->symbol'] =
        new PrimProc('string->symbol',
		     1,
		     false, false,
		     function(str) {
		 	 check(str, isString, 'string->symbol', 'string', 1);
			 return types.symbol(str.toString());
		     });


    PRIMITIVES['symbol->string'] =
        new PrimProc('symbol->string',
		     1,
		     false, false,
		     function(symb) {
		 	 check(symb, isSymbol, 'symbol->string', 'symbol', 1);
			 return types.string(symb.toString());
		     });


    PRIMITIVES['format'] =
        new PrimProc('format', 1, true, false,
		     function(formatStr, args) {
		 	 check(formatStr, isString, 'format', 'string', 1, [formatStr].concat(args));
		         formatStr = formatStr.toString();
			 return types.string( helpers.format(formatStr, args, 'format') );
		     });


    PRIMITIVES['printf'] =
        new PrimProc('printf', 1, true, true,
		     function(state, formatStr, args) {
		 	 check(formatStr, isString, 'printf', 'string', 1, [formatStr].concat(args));
		         formatStr = formatStr.toString();
			 var msg = helpers.format(formatStr, args, 'printf');
			 state.getDisplayHook()(msg);
			 state.v = types.VOID;
		     });


    PRIMITIVES['string->int'] =
        new PrimProc('string->int',
		     1,
		     false, false,
		     function(str) {
		 	 check(str, function(s) {return isString(s) && s.length == 1;},
			       'string->int', '1-letter string', 1);
		         str = str.toString();
			 return str.charCodeAt(0);
		     });


    PRIMITIVES['int->string'] =
        new PrimProc('int->string',
		     1,
		     false, false,
		     function(num) {
		 	 check(num, function(x) {
			     if ( !isInteger(x) ) {
				 return false;
			     }
			     var n = jsnums.toFixnum(x);
			     return ((n >= 0 && n < 55296) ||
				     (n > 57343 && n <= 1114111));
			 },
			       'int->string',
			       'exact integer in [0,55295] or [57344,1114111]',
			       1);

			 return types.string( String.fromCharCode(jsnums.toFixnum(num)) );
		     });


    PRIMITIVES['explode'] =
        new PrimProc('explode',
		     1,
		     false, false,
		     function(str) {
		 	 check(str, isString, 'explode', 'string', 1);
		         str = str.toString();
			 var ret = types.EMPTY;
			 for (var i = str.length-1; i >= 0; i--) {
			     ret = types.cons( types.string(str.charAt(i)), ret );
			 }
			 return ret;
		     });

    PRIMITIVES['implode'] =
        new PrimProc('implode',
		     1,
		     false, false,
		     function(lst) {
		 	 checkListOf(lst, function(x) { return isString(x) && x.length == 1; },
				     'implode', 'list of 1-letter strings', 1);
			 var ret = [];
			 while ( !lst.isEmpty() ) {
			     ret.push( lst.first.toString() );
			     lst = lst.rest;
			 }
			 return types.string(ret);
		     });


    PRIMITIVES['string-alphabetic?'] =
        new PrimProc('string-alphabetic?',
		     1,
		     false, false,
		     function(str) {
		 	 check(str, isString, 'string-alphabetic?', 'string', 1);
		         str = str.toString();
			 return isAlphabeticString(str);
		     });


    PRIMITIVES['string-ith'] =
        new PrimProc('string-ith',
		     2,
		     false, false,
		     function(str, num) {
		 	 check(str, isString, 'string-ith', 'string', 1, arguments);
			 check(num, function(x) { return isNatural(x) && jsnums.lessThan(x, str.length); }, 'string-ith',
			       'exact integer in [0, length of the given string minus 1 (' + (str.length-1) + ')]', 2, arguments);
		         str = str.toString();
			 return types.string( str.charAt(jsnums.toFixnum(num)) );
		     });


    PRIMITIVES['string-lower-case?'] =
        new PrimProc('string-lower-case?',
		     1,
		     false, false,
		     function(str) {
		 	 check(str, isString, 'string-lower-case?', 'string', 1);
			 var primStr = str.toString();
			 return isAlphabeticString(str) && primStr.toLowerCase() === primStr;
		     });


    PRIMITIVES['string-numeric?'] =
        new PrimProc('string-numeric?',
		     1,
		     false, false,
		     function(str) {
		 	 check(str, isString, 'string-numeric?', 'string', 1);
		         str = str.toString();
			 return isNumericString(str);
		     });


    PRIMITIVES['string-upper-case?'] =
        new PrimProc('string-upper-case?',
		     1,
		     false, false,
		     function(str) {
		 	 check(str, isString, 'string-upper-case?', 'string', 1);
			 var primStr = str.toString();
			 return isAlphabeticString(str) && primStr.toUpperCase() === primStr;
		     });


    PRIMITIVES['string-whitespace?'] =
        new PrimProc('string-whitespace?',
		     1,
		     false, false,
		     function(str) {
		 	 check(str, isString, 'string-whitespace?', 'string', 1);
		         str = str.toString();
			 return isWhitespaceString(str);
		     });


    PRIMITIVES['build-string'] =
        new PrimProc('build-string',
		     2,
		     false, false,
		     function(num, f) {
		 	 check(num, isNatural, 'build-string', 'non-negative exact integer', 1, arguments);
			 check(f, isFunction, 'build-string', 'procedure', 2, arguments);

			 var buildStringHelp = function(n, acc) {
			     if ( jsnums.greaterThanOrEqual(n, num) ) {
				 return types.string(acc);
			     }

			     return CALL(f, [n],
					 function(res) {
					     check(res, isChar, 'build-string',
						   'procedure that returns a char', 2);
					     acc.push(res.val)
					     return buildStringHelp(n+1, acc);
					 });
			 }
			 return buildStringHelp(0, []);
		     });


    PRIMITIVES['string->immutable-string'] =
        new PrimProc('string->immutable-string',
		     1,
		     false, false,
		     function(str) {
		 	 check(str, isString, 'string->immutable-string', 'string', 1);
			 return str.toString();
		     });


    PRIMITIVES['string-set!'] =
        new PrimProc('string-set!',
		     3,
		     false, false,
		     function(str, k, c) {
		         check(str, function(x) { return isMutableString(x); },
			       'string-set!', 'mutable string', 1, arguments);
			 check(k, isNatural, 'string-set!', 'non-negative exact integer', 2, arguments);
			 check(c, isChar, 'string-set!', 'char', 3, arguments);

			 if ( jsnums.greaterThanOrEqual(k, str.length) ) {
			     var msg = ('string-set!: index ' + k + ' out of range ' +
					'[0, ' + (str.length-1) + '] for string: ' +
					helpers.toDisplayedString(str));
			     raise( types.incompleteExn(types.exnFailContract, msg, []) );
			 }
			 str.set(jsnums.toFixnum(k), c.val);
			 return types.VOID;
		     });


    PRIMITIVES['string-fill!'] =
        new PrimProc('string-fill!',
		     2,
		     false, false,
		     function(str, c) {
		         check(str, function(x) { return isMutableString(x); },
			       'string-fill!', 'mutable string', 1, arguments);
			 check(c, isChar, 'string-fill!', 'char', 2, arguments);
		         
			 for (var i = 0; i < str.length; i++) {
			     str.set(i, c.val);
			 }
			 return types.VOID;
		     });



    //////////////////////////////////////////////////////////////////////
    //  Immutable cyclic data
    PRIMITIVES['make-reader-graph'] = 
	new PrimProc('make-reader-graph', 1, false, false,
		     function(x) {
			 var result = types.readerGraph(x, types.makeLowLevelEqHash(), 0);
			 return result;
		     });


    PRIMITIVES['make-placeholder'] =
	new PrimProc('make-placeholder', 1, false, false,
		     function(x) { return types.placeholder(x); });


    PRIMITIVES['placeholder-set!'] = 
	new PrimProc('placeholder-set!', 2, false, false,
		     function(pl, x) {
			 check(pl, types.isPlaceholder,
			       "placeholder-set!", "placeholder", 1);
			 pl.set(x);
			 return types.VOID;
		     });

    PRIMITIVES['placeholder-get'] =
	new PrimProc('placeholder-get', 1, false, false,
		     function(pl) {
			 check(pl, types.isPlaceholder, 
			       "placeholder-get", "placeholder", 1);
			 return pl.get();
		     });




    //////////////////////////////////////////////////////////////////////




    /******************************
 *** Byte String Primitives ***
 ******************************/


    PRIMITIVES['make-bytes'] =
	new CasePrimitive('make-bytes',
	                  [new PrimProc('make-bytes',
		                        1,
		                        false, false,
		                        function(k) {
			                    check(k, isNatural, 'make-bytes', 'non-negative exact integer', 1);
			                    
			                    var ret = [];
			                    for (var i = 0; i < jsnums.toFixnum(k); i++) {
			  	                ret.push(0);
			                    }
			                    return types.bytes(ret, true);
		                        }),
	                   new PrimProc('make-bytes',
		                        2,
		                        false, false,
		                        function(k, b) {
			                    check(k, isNatural, 'make-bytes', 'non-negative exact integer', 1, arguments);
			                    check(b, isByte, 'make-bytes', 'byte', 2, arguments);

			                    var ret = [];
			                    for (var i = 0; i < jsnums.toFixnum(k); i++) {
			  	                ret.push(b);
			                    }
			                    return types.bytes(ret, true);
		                        }) ]);


    PRIMITIVES['bytes'] =
        new PrimProc('bytes',
		     0,
		     true, false,
		     function(args) {
		 	 arrayEach(args, function(b, i) {check(b, isByte, 'bytes', 'byte', i+1, args);});
			 return types.bytes(args, true);
		     });


    PRIMITIVES['bytes->immutable-bytes'] =
        new PrimProc('bytes->immutable-bytes',
		     1,
		     false, false,
		     function(bstr) {
		 	 check(bstr, isByteString, 'bytes->immutable-bytes', 'byte string', 1);
			 if ( bstr.mutable ) {
			     return bstr.copy(false);
			 }
			 else {
			     return bstr;
			 }
		     });


    PRIMITIVES['bytes-length'] =
        new PrimProc('bytes-length',
		     1,
		     false, false,
		     function(bstr) {
		 	 check(bstr, isByteString, 'bytes-length', 'byte string', 1);
			 return bstr.length();
		     });


    PRIMITIVES['bytes-ref'] =
        new PrimProc('bytes-ref',
		     2,
		     false, false,
		     function(bstr, num) {
		 	 check(bstr, isByteString, 'bytes-ref', 'byte string', 1, arguments);
			 check(num, isNatural, 'bytes-ref', 'non-negative exact integer', 2, arguments);

			 var n = jsnums.toFixnum(num);
			 if ( n >= bstr.length() ) {
			     var msg = ('bytes-ref: index ' + n + ' out of range ' +
					'[0, ' + (bstr.length-1) + '] for byte-string: ' +
					helpers.toDisplayedString(bstr));
			     raise( types.incompleteExn(types.exnFailContract, msg, []) );
			 }
			 return bstr.get(n);
		     });


    PRIMITIVES['bytes-set!'] =
        new PrimProc('bytes-set!',
		     3,
		     false, false,
		     function(bstr, num, b) {
		 	 check(bstr, function(x) { return isByteString(x) && x.mutable; },
			       'bytes-set!', 'mutable byte string', 1, arguments);
			 check(num, isNatural, 'bytes-set!', 'non-negative exact integer', 2, arguments);
			 check(b, isByte, 'bytes-set!', 'byte', 3, arguments);

			 var n = jsnums.toFixnum(num);
			 if ( n >= bstr.length() ) {
			     var msg = ('bytes-set!: index ' + n + ' out of range ' +
					'[0, ' + (bstr.length-1) + '] for byte-string: ' +
					helpers.toDisplayedString(bstr));
			     raise( types.incompleteExn(types.exnFailContract, msg, []) );
			 }
			 bstr.set(n, b);
			 return types.VOID;
		     });


    PRIMITIVES['subbytes'] =
	new CasePrimitive('subbytes',
	                  [new PrimProc('subbytes',
		                        2,
		                        false, false,
		                        function(bstr, theStart) {
		                            check(bstr, isByteString, 'subbytes', 'bytes string', 1, arguments);
			                    check(theStart, isNatural, 'subbytes', 'non-negative exact integer', 2, arguments);
			                    
			                    var start = jsnums.toFixnum(theStart);
			                    if (start > bstr.length()) {
			   	                var msg = ('subbytes: starting index ' + start + ' out of range ' +
					                   '[0, ' + bstr.length + '] for byte-string: ' +
					                   helpers.toDisplayedString(bstr));
				                raise( types.incompleteExn(types.exnFailContract, msg, []) );
			                    }
			                    else {
			  	                return bstr.subbytes(jsnums.toFixnum(start));
			                    }
		                        }),
	                   new PrimProc('subbytes',
		                        3,
		                        false, false,
		                        function(bstr, theStart, theEnd) {
		                            check(bstr, isByteString, 'subbytes', 'byte string', 1, arguments);
			                    check(theStart, isNatural, 'subbytes', 'non-negative exact integer', 2, arguments);
			                    check(theEnd, isNatural, 'subbytes', 'non-negative exact integer', 3, arguments);

			                    var start = jsnums.toFixnum(theStart);
			                    var end = jsnums.toFixnum(theEnd);
			                    if (start > bstr.length()) {
			   	                var msg = ('subbytes: starting index ' + start + ' out of range ' +
					                   '[0, ' + bstr.length() + '] for byte-string: ' +
					                   helpers.toDisplayedString(bstr));
				                raise( types.incompleteExn(types.exnFailContract, msg, []) );
			                    }
			                    if (end < start || end > bstr.length()) {
			   	                var msg = ('subbytes: ending index ' + end + ' out of range ' + '[' + start +
					                   ', ' + bstr.length() + '] for byte-string: ' +
					                   helpers.toDisplayedString(bstr));
				                raise( types.incompleteExn(types.exnFailContract, msg, []) );
			                    }
			                    else {
			  	                return bstr.subbytes(start, end);
			                    }
		                        }) ]);


    PRIMITIVES['bytes-copy'] =
        new PrimProc('bytes-copy',
		     1,
		     false, false,
		     function(bstr) {
		 	 check(bstr, isByteString, 'bytes-copy', 'byte string', 1);
			 return bstr.copy(true);
		     });


    PRIMITIVES['bytes-fill!'] =
        new PrimProc('bytes-fill!',
		     2,
		     false, false,
		     function(bstr, b) {
		 	 check(bstr, function(x) { return isByteString(x) && x.mutable; },
			       'bytes-fill!', 'mutable byte string', 1, arguments);
			 check(b, isByte, 'bytes-fill!', 'byte', 2, arguments);
			 
			 for (var i = 0; i < bstr.length(); i++) {
			     bstr.set(i, b);
			 }
			 return types.VOID;
		     });


    PRIMITIVES['bytes-append'] =
        new PrimProc('bytes-append',
		     0,
		     true, false,
		     function(args) {
		  	 arrayEach(args, function(x, i) { check(x, isByteString, 'bytes-append', 'byte string', i+1, args); });

			 var ret = [];
			 for (var i = 0; i < args.length; i++) {
			     ret = ret.concat(args[i].bytes);
			 }
			 return types.bytes(ret, true);
		     });


    PRIMITIVES['bytes->list'] =
        new PrimProc('bytes->list',
		     1,
		     false, false,
		     function(bstr) {
		 	 check(bstr, isByteString, 'bytes->list', 'byte string', 1);

			 var ret = types.EMPTY;
			 for (var i = bstr.length()-1; i >= 0; i--) {
			     ret = types.cons(bstr.get(i), ret);
			 }
			 return ret;
		     });


    PRIMITIVES['list->bytes'] =
        new PrimProc('list->bytes',
		     1,
		     false, false,
		     function(lst) {
		 	 checkListOf(lst, isByte, 'list->bytes', 'byte', 1);

			 var ret = [];
			 while ( !lst.isEmpty() ) {
			     ret.push(lst.first);
			     lst = lst.rest;
			 }
			 return types.bytes(ret, true);
		     });


    PRIMITIVES['bytes=?'] =
        new PrimProc('bytes=?',
		     2,
		     true, false,
		     function(bstr1, bstr2, bstrs) {
		 	 bstrs.unshift(bstr2);
			 bstrs.unshift(bstr1);
			 arrayEach(bstrs, function(x, i) { check(x, isByteString, 'bytes=?', 'byte string', i+1, bstrs); });

			 return compare(bstrs, function(bstrA, bstrB) { return bstrA.toString() === bstrB.toString(); });
		     });


    PRIMITIVES['bytes<?'] =
        new PrimProc('bytes<?',
		     2,
		     true, false,
		     function(bstr1, bstr2, bstrs) {
		 	 bstrs.unshift(bstr2);
			 bstrs.unshift(bstr1);
			 arrayEach(bstrs, function(x, i) { check(x, isByteString, 'bytes<?', 'byte string', i+1, bstrs); });

			 return compare(bstrs, function(bstrA, bstrB) { return bstrA.toString() < bstrB.toString(); });
		     });


    PRIMITIVES['bytes>?'] =
        new PrimProc('bytes>?',
		     2,
		     true, false,
		     function(bstr1, bstr2, bstrs) {
		 	 bstrs.unshift(bstr2);
			 bstrs.unshift(bstr1);
			 arrayEach(bstrs, function(x, i) { check(x, isByteString, 'bytes>?', 'byte string', i+1, bstrs); });

			 return compare(bstrs, function(bstrA, bstrB) { return bstrA.toString() > bstrB.toString(); });
		     });




    /*************************
 *** Vector Primitives ***
 *************************/

    var makeVectorImpl = function(size, content) {
        check(size, isNatural, 'make-vector', 'non-negative exact integer', 1, arguments);
        var s = jsnums.toFixnum(size);
        var ret = [];
        for (var i = 0; i < s; i++) {
	    ret.push(content);
        }
        return types.vector(ret);
    };

    PRIMITIVES['make-vector'] = new CasePrimitive
    ("make-vector",
     [new PrimProc('make-vector',
		   2,
		   false, false,
		   makeVectorImpl),
      new PrimProc('make-vector',
		   1,
		   false, false,
		   function(size) { return makeVectorImpl(size, jsnums.fromFixnum(0)); })]);


    PRIMITIVES['vector'] =
        new PrimProc('vector',
		     0,
		     true, false,
		     function(args) {
		 	 return types.vector(args);
		     });


    PRIMITIVES['vector-length'] =
        new PrimProc('vector-length',
		     1,
		     false, false,
		     function(vec) {
		 	 check(vec, isVector, 'vector-length', 'vector', 1);
			 return vec.length();
		     });


    PRIMITIVES['vector-ref'] =
        new PrimProc('vector-ref',
		     2,
		     false, false,
		     function(vec, index) {
		 	 check(vec, isVector, 'vector-ref', 'vector', 1, arguments);
			 check(index, isNatural, 'vector-ref', 'non-negative exact integer', 2, arguments);

			 var i = jsnums.toFixnum(index);
			 if (i >= vec.length()) {
			     var msg = ('vector-ref: index ' + i + ' out of range ' +
					'[0, ' + (vec.length()-1) + '] for vector: ' +
					helpers.toDisplayedString(vec));
			     raise( types.incompleteExn(types.exnFailContract, msg, []) );
			 }
			 return vec.ref(i);
		     });


    PRIMITIVES['vector-set!'] =
        new PrimProc('vector-set!',
		     3,
		     false, false,
		     function(vec, index, val) {
		 	 check(vec, isVector, 'vector-set!', 'vector', 1, arguments);
			 check(index, isNatural, 'vector-set!', 'non-negative exact integer', 2, arguments);

			 var i = jsnums.toFixnum(index);
			 if (i >= vec.length()) {
			     var msg = ('vector-set!: index ' + i + ' out of range ' +
					'[0, ' + (vec.length()-1) + '] for vector: ' +
					helpers.toDisplayedString(vec));
			     raise( types.incompleteExn(types.exnFailContract, msg, []) );
			 }
			 vec.set(i, val);
			 return types.VOID;
		     });


    PRIMITIVES['vector->list'] =
        new PrimProc('vector->list',
		     1,
		     false, false,
		     function(vec) {
		 	 check(vec, isVector, 'vector->list', 'vector', 1);
			 return vec.toList();
		     });


    PRIMITIVES['list->vector'] =
        new PrimProc('list->vector',
		     1,
		     false, false,
		     function(lst) {
		 	 checkList(lst, 'list->vector', 1);
			 return types.vector( helpers.schemeListToArray(lst) );
		     });


    PRIMITIVES['build-vector'] =
        new PrimProc('build-vector',
		     2,
		     false, false,
		     function(num, f) {
		 	 check(num, isNatural, 'build-vector', 'non-negative exact integer', 1, arguments);
			 check(f, isFunction, 'build-vector', 'procedure', 2, arguments);

			 var buildVectorHelp = function(n, acc) {
			     if ( jsnums.greaterThanOrEqual(n, num) ) {
				 return types.vector(acc);
			     }

			     return CALL(f, [n],
					 function (result) {
					     acc.push(result)
					     return buildVectorHelp(n+1, acc);
					 });
			 }
			 return buildVectorHelp(0, []);
		     });



    /***********************
 *** Char Primitives ***
 ***********************/


    PRIMITIVES['char=?'] =
        new PrimProc('char=?',
		     2,
		     true, false,
		     function(char1, char2, chars) {
		 	 chars.unshift(char2);
			 chars.unshift(char1);
			 arrayEach(chars, function(c, i) {check(c, isChar, 'char=?', 'char', i+1, chars);});

			 return compare(chars, function(c1, c2) {return c1.val === c2.val;});
		     });


    PRIMITIVES['char<?'] =
        new PrimProc('char<?',
		     2,
		     true, false,
		     function(char1, char2, chars) {
		 	 chars.unshift(char2);
			 chars.unshift(char1);
			 arrayEach(chars, function(c, i) {check(c, isChar, 'char<?', 'char', i+1, chars);});

			 return compare(chars, function(c1, c2) {return c1.val < c2.val;});
		     });


    PRIMITIVES['char>?'] =
        new PrimProc('char>?',
		     2,
		     true, false,
		     function(char1, char2, chars) {
		 	 chars.unshift(char2);
			 chars.unshift(char1);
			 arrayEach(chars, function(c, i) {check(c, isChar, 'char>?', 'char', i+1, chars);});

			 return compare(chars, function(c1, c2) {return c1.val > c2.val;});
		     });


    PRIMITIVES['char<=?'] =
        new PrimProc('char<=?',
		     2,
		     true, false,
		     function(char1, char2, chars) {
		 	 chars.unshift(char2);
			 chars.unshift(char1);
			 arrayEach(chars, function(c, i) {check(c, isChar, 'char<=?', 'char', i+1, chars);});

			 return compare(chars, function(c1, c2) {return c1.val <= c2.val;});
		     });


    PRIMITIVES['char>=?'] =
        new PrimProc('char>=?',
		     2,
		     true, false,
		     function(char1, char2, chars) {
		 	 chars.unshift(char2);
			 chars.unshift(char1);
			 arrayEach(chars, function(c, i) {check(c, isChar, 'char>=?', 'char', i+1, chars);});

			 return compare(chars, function(c1, c2) {return c1.val >= c2.val;});
		     });


    PRIMITIVES['char-ci=?'] =
        new PrimProc('char-ci=?',
		     2,
		     true, false,
		     function(char1, char2, chars) {
		 	 chars.unshift(char2);
			 chars.unshift(char1);
			 arrayEach(chars, function(c, i) {check(c, isChar, 'char-ci=?', 'char', i+1, chars);});

			 return compare(chars,
				        function(c1, c2) {
					    return c1.val.toLowerCase() === c2.val.toLowerCase();
				        });
		     });


    PRIMITIVES['char-ci<?'] =
        new PrimProc('char-ci<?',
		     2,
		     true, false,
		     function(char1, char2, chars) {
		 	 chars.unshift(char2);
			 chars.unshift(char1);
			 arrayEach(chars, function(c, i) {check(c, isChar, 'char-ci<?', 'char', i+1, chars);});

			 return compare(chars,
				        function(c1, c2) {
					    return c1.val.toLowerCase() < c2.val.toLowerCase();
				        });
		     });


    PRIMITIVES['char-ci>?'] =
        new PrimProc('char-ci>?',
		     2,
		     true, false,
		     function(char1, char2, chars) {
		 	 chars.unshift(char2);
			 chars.unshift(char1);
			 arrayEach(chars, function(c, i) {check(c, isChar, 'char-ci>?', 'char', i+1, chars);});

			 return compare(chars,
				        function(c1, c2) {
					    return c1.val.toLowerCase() > c2.val.toLowerCase();
				        });
		     });


    PRIMITIVES['char-ci<=?'] =
        new PrimProc('char-ci<=?',
		     2,
		     true, false,
		     function(char1, char2, chars) {
		 	 chars.unshift(char2);
			 chars.unshift(char1);
			 arrayEach(chars, function(c, i) {check(c, isChar, 'char-ci<=?', 'char', i+1, chars);});

			 return compare(chars,
				        function(c1, c2) {
					    return c1.val.toLowerCase() <= c2.val.toLowerCase();
				        });
		     });


    PRIMITIVES['char-ci>=?'] =
        new PrimProc('char-ci>=?',
		     2,
		     true, false,
		     function(char1, char2, chars) {
		 	 chars.unshift(char2);
			 chars.unshift(char1);
			 arrayEach(chars, function(c, i) {check(c, isChar, 'char-ci>=?', 'char', i+1, chars);});

			 return compare(chars,
				        function(c1, c2) {
					    return c1.val.toLowerCase() >= c2.val.toLowerCase();
				        });
		     });


    PRIMITIVES['char-alphabetic?'] =
        new PrimProc('char-alphabetic?',
		     1,
		     false, false,
		     function(c) {
		 	 check(c, isChar, 'char-alphabetic?', 'char', 1);
			 return isAlphabeticString(c.val);
		     });


    PRIMITIVES['char-numeric?'] =
        new PrimProc('char-numeric?',
		     1,
		     false, false,
		     function(c) {
		 	 check(c, isChar, 'char-numeric?', 'char', 1);
			 return (c.val >= '0' && c.val <= '9');
		     });


    PRIMITIVES['char-whitespace?'] =
        new PrimProc('char-whitespace?',
		     1,
		     false, false,
		     function(c) {
		 	 check(c, isChar, 'char-whitespace?', 'char', 1);
			 return isWhitespaceString(c.val);
		     });


    PRIMITIVES['char-upper-case?'] =
        new PrimProc('char-upper-case?',
		     1,
		     false, false,
		     function(c) {
		 	 check(c, isChar, 'char-upper-case?', 'char', 1);
			 return (isAlphabeticString(c.val) && c.val.toUpperCase() === c.val);
		     });


    PRIMITIVES['char-lower-case?'] =
        new PrimProc('char-lower-case?',
		     1,
		     false, false,
		     function(c) {
		 	 check(c, isChar, 'char-lower-case?', 'char', 1);
			 return (isAlphabeticString(c.val) && c.val.toLowerCase() === c.val);
		     });


    PRIMITIVES['char->integer'] =
        new PrimProc('char->integer',
		     1,
		     false, false,
		     function(c) {
		 	 check(c, isChar, 'char->integer', 'char', 1);
			 return c.val.charCodeAt(0);
		     });


    PRIMITIVES['integer->char'] =
        new PrimProc('integer->char',
		     1,
		     false, false,
		     function(num) {
		 	 check(num, function(x) {
			     if ( !isNatural(x) ) {
				 return false;
			     }
			     var n = jsnums.toFixnum(x);
			     return ((n >= 0 && n < 55296) ||
				     (n > 57343 && n <= 1114111));
			 },
			       'integer->char',
			       'exact integer in [0,#x10FFFF], not in [#xD800,#xDFFF]',
			       1);

			 return types.character( String.fromCharCode(jsnums.toFixnum(num)) );
		     });


    PRIMITIVES['char-upcase'] =
        new PrimProc('char-upcase',
		     1,
		     false, false,
		     function(c) {
		 	 check(c, isChar, 'char-upcase', 'char', 1);
			 return types.character( c.val.toUpperCase() );
		     });


    PRIMITIVES['char-downcase'] =
        new PrimProc('char-downcase',
		     1,
		     false, false,
		     function(c) {
		 	 check(c, isChar, 'char-downcase', 'char', 1);
			 return types.character( c.val.toLowerCase() );
		     });





    var callCCPrim = new types.PrimProc('call/cc',
				        1,
				        false, true,
				        function(aState, f) {
					    var continuationClosure = 
					        state.captureContinuationClosure(aState);
					    aState.pushValue(continuationClosure);
					    aState.v = f;
					    aState.pushControl(
					        new control.CallControl(1));
				        });
    
    PRIMITIVES['call/cc'] = callCCPrim;
    PRIMITIVES['call-with-current-continuation'] = callCCPrim;




    //////////////////////////////////////////////////////////////////////








    //////////////////////////////////////////////////////////////////////

    var GENSYM_COUNTER = 0;
    
    var gensymImpl = function(x) {
        check(x,
	      function(x) { return isString(x) || isSymbol(x) },
	      'gensym', 'symbol or string', 
	      1);
        return types.symbol(x.toString() + '' + (GENSYM_COUNTER++));
    };

    PRIMITIVES['gensym'] = 
	new CasePrimitive(
	    'gensym',
	    [new PrimProc('gensym',
			  1,
			  false,
			  false,
			  gensymImpl),
	     new PrimProc('gensym',
			  0,
			  false,
			  false,
			  function() { return gensymImpl('g') })]);











    /***************************
 *** Primitive Constants ***
 ***************************/


    PRIMITIVES['eof'] = types.EOF;
    PRIMITIVES['e'] = jsnums.e;
    PRIMITIVES['empty'] = types.EMPTY;
    PRIMITIVES['false'] = false;
    PRIMITIVES['true'] = true;
    PRIMITIVES['pi'] = jsnums.pi;
    PRIMITIVES['null'] = types.EMPTY;






    //////////////////////////////////////////////////////////////////////
    /** Parameters **/
    var PARAMZ = {};

    PARAMZ['exception-handler-key'] = types.exceptionHandlerKey;



    ///////////////////////////////////////////////////////////////

    // getPrimitive: string (string | undefined) -> scheme-value
    primitives.getPrimitive = function(name, resolvedModuleName) {
        if (resolvedModuleName === undefined) {
	    return PRIMITIVES[name];
        }

        if (resolvedModuleName === types.symbol("moby/kernel")) {
	    return PRIMITIVES[name];
        }

        if (resolvedModuleName === types.symbol("moby/paramz")) {
	    return PARAMZ[name];
        }

        if (types.isEqual(resolvedModuleName,
		          types.list([types.symbol("quote"), types.symbol("#%kernel")]))) {
	    return PRIMITIVES[name];
        }

        if (types.isEqual(resolvedModuleName,
		          types.list([types.symbol("quote"), types.symbol("#%paramz")]))) {
	    return PARAMZ[name];
        }

        // FIXME: if we get to this point, this should be treated as an internal error...
        return PRIMITIVES[name];
    };

    primitives.isPrimitive = function(x) {
        return x instanceof PrimProc;
    };





    scope.link.announceReady('primitives');
})(this['plt']);

