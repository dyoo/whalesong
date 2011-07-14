
/************************
 *** World Primitives ***
 ************************/

var PrimProc = types.PrimProc;
var CasePrimitive = types.CasePrimitive;
var makeOptionPrimitive = types.makeOptionPrimitive;
var checkListOf = helpers.checkListOf;
var procArityContains = helpers.procArityContains;
var raise = helpers.raise;


var makeCaller = function(aState) {
    return function(operator, operands, k, callSite) {
	interpret.call(aState, operator, operands, k, aState.onFail, callSite);
    };
};




// Every world configuration function (on-tick, stop-when, ...)
// produces a WorldConfigOption instance.
var WorldConfigOption = function(name) {
    this.name = name;	    
};

WorldConfigOption.prototype.configure = function(config) {
    raise(types.incompleteExn(
	types.exnFailContract,
	'unimplemented WorldConfigOption',
	[]));
};

WorldConfigOption.prototype.toDomNode = function(cache) {  
    var div = document.createElement('div');
    div.appendChild(document.createTextNode("(" + this.name + " ...)"));
    return div;
};

WorldConfigOption.prototype.toWrittenString = function(cache) {
    return "(" + this.name + " ...)";
};

WorldConfigOption.prototype.toDisplayedString = function(cache) {
    return "(" + this.name + " ...)";
};



var isWorldConfigOption = function(x) { return x instanceof WorldConfigOption; };





// convertAttribList: (listof (list string (or string boolean))) -> (hashof string string)
var convertAttribList = function(attribList) {
    var newList = types.EMPTY;
    var nextElt;
    var key, val;
    while (!types.isEmpty(attribList)) {
	nextElt = attribList.first();

	key = nextElt.first();
	val = nextElt.rest().first();

	key = String(key);

	if (types.isString(val)) {
	    val = String(val);
	} else if (types.isBoolean(val)) {
	    // do nothing: the representation is the same.
	} else if (types.isSymbol(val)) {
	    if (String(val) === 'true') {
		val = true;
	    } else if (String(val) === 'false') {
		val = false;
	    } else {
		val = String(val);
	    }
	} else {
	    // raise error: neither string nor boolean
	    raise(types.incompleteExn(
		types.exnFailContract,		  
		helpers.format(
		    "attribute value ~s neither a string nor a boolean",
		    [val]),
		[]));
	}
	// ensure each element in the hash are primitive strings
	newList = types.cons(types.list([key, val]),
			     newList);
	attribList = attribList.rest();
    }
    return helpers.assocListToHash(newList);
}




//////////////////////////////////////////////////////////////////////




EXPORTS['key=?'] =
    new PrimProc('key=?',
		 2,
		 false, false,
		 function(key1, key2) {
		     return (String(key1).toLowerCase() === 
			     String(key2).toLowerCase());
		 });





var OnTickBang = function(handler, effectHandler, aDelay) {
    WorldConfigOption.call(this, 'on-tick');
    this.handler = handler;
    this.effectHandler = effectHandler;
    this.aDelay = aDelay;
};

OnTickBang.prototype = helpers.heir(WorldConfigOption.prototype);

OnTickBang.prototype.configure = function(config) {
    var newVals = { 
	onTick: this.handler,
	onTickEffect: this.effectHandler,
	tickDelay: jsnums.toFixnum(jsnums.multiply(1000, this.aDelay))
    };
    return config.updateAll(newVals);
};




// The default tick delay is 28 times a second.
var DEFAULT_TICK_DELAY = types.rational(1, 28);

EXPORTS['on-tick'] =
	new CasePrimitive(
	    'on-tick',
	    [new PrimProc('on-tick',
			  1,
			  false, false,
			  function(f) {
			      check(f, isFunction, "on-tick", "procedure", 1);
			      return new OnTickBang(f,
						    new PrimProc('', 1, false, false,
								 function(w) { return types.effectDoNothing(); }),
						    DEFAULT_TICK_DELAY);
			  }),
	     new PrimProc('on-tick',
			  2,
			  false, false,
			  function(f, aDelay) {
			      check(f, isFunction, "on-tick", "procedure", 1, arguments);
			      check(aDelay, isNumber, "on-tick", "number", 2, arguments);
			      return new OnTickBang(f,
						    new PrimProc('', 1, false, false,
								 function(w) { return types.effectDoNothing(); }),
						    aDelay);
			  }) ]);



EXPORTS['on-tick!'] =
    new CasePrimitive('on-tick!',
	[new PrimProc('on-tick!',
		      2,
		      false, false,
		      function(handler, effectHandler) {
			  check(handler, isFunction, "on-tick!", "procedure", 1, arguments);
			  check(effectHandler, isFunction, "on-tick!","procedure", 2, arguments);
			  return new OnTickBang(handler, effectHandler, DEFAULT_TICK_DELAY);
		      }),
	 new PrimProc('on-tick!',
		      3,
		      false, false,
		      function(handler, effectHandler, aDelay)  {
			  check(handler, isFunction, "on-tick!", "procedure", 1, arguments);
			  check(effectHandler, isFunction, "on-tick!","procedure", 2, arguments);
			  check(aDelay, isNumber, "on-tick!", "number", 3, arguments);
			  return new OnTickBang(handler, effectHandler, aDelay);
		      }) ]);



var onEvent = function(funName, inConfigName, numArgs) {
    return function(handler) {
	return onEventBang(funName, inConfigName)(handler,
						  new PrimProc('', numArgs, false, false, function() { return types.EMPTY; }));
    };
};


var onEventBang = function(funName, inConfigName) {

    var CustomConfigOption = function(handler, effectHandler) {
	WorldConfigOption.call(this, funName);
	this.handler = handler;
	this.effectHandler = effectHandler;
    };
    CustomConfigOption.prototype = helpers.heir(WorldConfigOption.prototype);

    CustomConfigOption.prototype.configure =function(config) {
	var newHash = {};
	newHash[inConfigName] = this.handler;
	newHash[inConfigName+'Effect'] = this.effectHandler;
	return config.updateAll(newHash);
    }

    return function(handler, effectHandler) {
	check(handler, isFunction, funName, 'procedure', 1, arguments);
	check(effectHandler, isFunction, funName, 'procedure', 2, arguments);
	return new CustomConfigOption(handler, effectHandler);
    };
};


EXPORTS['on-key'] = new PrimProc('on-key', 1, false, false, onEvent('on-key', 'onKey', 2));
EXPORTS['on-key!'] = new PrimProc('on-key!', 2, false, false, onEventBang('on-key!', 'onKey'));


EXPORTS['stop-when'] = new PrimProc('stop-when', 1, false, false,
				       onEvent('stop-when', 'stopWhen', 1));
EXPORTS['stop-when!'] = new PrimProc('stop-when!', 2, false, false,
					onEventBang('stop-when!', 'stopWhen'));





var DrawConfigOption = function(f) {
    WorldConfigOption.call(this, 'to-draw');
    this.f = f;
};

DrawConfigOption.prototype = helpers.heir(WorldConfigOption.prototype);

DrawConfigOption.prototype.configure = function(config) {
    return config.updateAll({'onRedraw': this.f});
};


EXPORTS['to-draw'] =
    new PrimProc('to-draw',
		 1,
		 false, false,
		 function(f) {
		     check(f, isFunction, 'to-draw', 'procedure', 1);
		     return new DrawConfigOption(f);
		 });


var DrawPageOption = function(domHandler) {
    WorldConfigOption.call(this, 'to-draw-page');
    this.domHandler = domHandler;
};
DrawPageOption.prototype = helpers.heir(WorldConfigOption.prototype);
DrawPageOption.prototype.configure = function(config) {
    return config.updateAll({'onDraw': this.domHandler});
};


var DrawPageAndCssOption = function(domHandler, styleHandler) {
    WorldConfigOption.call(this, 'to-draw-page');
    this.domHandler = domHandler;
    this.styleHandler = styleHandler;
};
DrawPageAndCssOption.prototype = helpers.heir(WorldConfigOption.prototype);
DrawPageAndCssOption.prototype.configure = function(config) {
    return config.updateAll({'onDraw': this.domHandler,
			     'onDrawCss' : this.styleHandler});
};




EXPORTS['to-draw-page'] =
    new CasePrimitive('to-draw-page',
	[new PrimProc('to-draw-page',
		      1,
		      false, false,
		      function(domHandler) {
			  check(domHandler, isFunction, 'to-draw-page', 'procedure', 1);
			  return new DrawPageOption(domHandler);
		      }),
	 new PrimProc('to-draw-page',
		      2,
		      false, false,
		      function(domHandler, styleHandler) {
		 	  check(domHandler, isFunction, 'to-draw-page', 'procedure', 1, arguments);
			  check(styleHandler, isFunction, 'to-draw-page', 'procedure', 2, arguments);
			  return new DrawPageAndCssOption(domHandler, styleHandler);		      }) ]);


var InitialEffectOption = function(effect) {
    WorldConfigOption.call(this, 'initial-effect');
    this.effect = effect;
};
InitialEffectOption.prototype = helpers.heir(WorldConfigOption.prototype);
InitialEffectOption.prototype.configure = function(config) {
    return config.updateAll({'initialEffect': this.effect});
};


EXPORTS['initial-effect'] =
    new PrimProc('initial-effect',
		 1,
		 false, false,
		 function(effect) {
		     return new InitialEffectOption(effect);
		 });



/**************************
 *** Jsworld Primitives ***
 **************************/


var jsp = function(attribList) {
	checkListOf(attribList, function(x) { return isList(x) && length(x) == 2; },
		    'js-p', 'list of (list of X Y)', 1);
	var attribs = convertAttribList(attribList);
	var node = jsworld.MobyJsworld.p(attribs);
	node.toWrittenString = function(cache) { return "(js-p)"; };
	node.toDisplayedString = node.toWrittenString;
	node.toDomNode = function(cache) { return node; };
	return helpers.wrapJsValue(node);
};
EXPORTS['js-p'] =
    new CasePrimitive('js-p',
	[new PrimProc('js-p', 0, false, false, function() { return jsp(types.EMPTY); }),
	 new PrimProc('js-p', 1, false, false, jsp)]);


var jsdiv = function(attribList) {
	checkListOf(attribList, isAssocList, 'js-div', '(listof X Y)', 1);

	var attribs = convertAttribList(attribList);
	var node = jsworld.MobyJsworld.div(attribs);
	
	node.toWrittenString = function(cache) { return "(js-div)"; };
	node.toDisplayedString = node.toWrittenString;
	node.toDomNode = function(cache) { return node; };
	return helpers.wrapJsValue(node);
};

EXPORTS['js-div'] =
    new CasePrimitive('js-div',
		      [new PrimProc('js-div', 0, false, false, function() {
			  return jsdiv(types.EMPTY); 
		      }),
		       new PrimProc('js-div', 1, false, false, jsdiv)
		      ]);


var jsButtonBang = function(funName) {
	return function(worldUpdateF, effectF, attribList) {
		check(worldUpdateF, isFunction, funName, 'procedure', 1);
		check(effectF, isFunction, funName, 'procedure', 2);
		checkListOf(attribList, isAssocList, funName, '(listof X Y)', 3);

		var attribs = attribList ? convertAttribList(attribList) : {};
		var node = jsworld.MobyJsworld.buttonBang(worldUpdateF, effectF, attribs);

		node.toWrittenString = function(cache) { return '(' + funName + ' ...)'; };
		node.toDisplayedString = node.toWrittenString;
		node.toDomNode = function(cache) { return node; };
		return helpers.wrapJsValue(node);
	}
};
var jsButton = function(updateWorldF, attribList) {
	var noneF = new types.PrimProc('', 1, false, false, function(w) { return types.EMPTY; });
	return jsButtonBang('js-button')(updateWorldF, noneF, attribList);
};
EXPORTS['js-button'] =
    new CasePrimitive('js-button',
	[new PrimProc('js-button', 1, false, false,
		      function(updateWorldF) {
			  return jsButton(updateWorldF, types.EMPTY)}),
	 new PrimProc('js-button', 2, false, false, jsButton)]);


EXPORTS['js-button!'] =
    new CasePrimitive('js-button!',
	[new PrimProc('js-button!', 2, false, false, 
		      function(worldUpdateF, effectF) {
			  return jsButtonBang('js-button!')(worldUpdateF, effectF, types.EMPTY);
		      }),
	 new PrimProc('js-button!', 3, false, false, 
		      jsButtonBang('js-button!'))]);



var jsInput = function(type, updateF, attribList) {
    check(type, isString, 'js-input', 'string', 1);
    check(updateF, isFunction, 'js-input', 'procedure', 2);
    checkListOf(attribList, isAssocList, 'js-input', '(listof X Y)', 3);

    var attribs = attribList ? convertAttribList(attribList) : {};
    var node = jsworld.MobyJsworld.input(String(type), 
					 updateF, attribs);

    node.toWrittenString = function(cache) { return "(js-input ...)"; }
    node.toDisplayedString = node.toWrittenString;
    node.toDomNode = function(cache) { return node; }
    return helpers.wrapJsValue(node);
};

EXPORTS['js-input'] =
	new CasePrimitive('js-input', 
	[new PrimProc('js-input', 2, false, false, 
		      function(type, updateF) {
			  return jsInput(type, updateF, types.EMPTY)}),
	 new PrimProc('js-input', 3, false, false, jsInput)]);



var jsImg = function(src, attribList) {
    check(src, isString, "js-img", "string", 1);
    checkListOf(attribList, isAssocList, 'js-img', '(listof X Y)', 2);

    var attribs = convertAttribList(attribList);
    var node = jsworld.MobyJsworld.img(String(src), attribs);

    node.toWrittenString = function(cache) { return "(js-img ...)"; }
    node.toDisplayedString = node.toWrittenString;
    node.toDomNode = function(cache) { return node; }
    return helpers.wrapJsValue(node);
};



EXPORTS['js-img'] =
    new CasePrimitive('js-img',
	[new PrimProc('js-img', 1, false, false, 
		      function(src) { return jsImg(src, types.EMPTY); }),
	 new PrimProc('js-img', 2, false, false, jsImg)]);



EXPORTS['js-text'] =
    new PrimProc('js-text',
		 1,
		 false, false,
		 function(s) {
		     check(s, isString, 'js-text', 'string', 1);

		     var node = jsworld.MobyJsworld.text(String(s), []);
		     node.toWrittenString = function(cache) { return "(js-text ...)"; }
		     node.toDisplayedString = node.toWrittenString;
		     node.toDomNode = function(cache) { return node; }
		     return helpers.wrapJsValue(node);
		 });


var jsSelect = function(optionList, updateF, attribList) {
    checkListOf(optionList, isString, 'js-select', 'listof string', 1);
    check(updateF, isFunction, 'js-select', 'procedure', 2);
    checkListOf(attribList, isAssocList, 'js-select', '(listof X Y)', 3);

    var attribs = attribList ? convertAttribList(attribList) : {};
    var options = helpers.deepListToArray(optionList);
    for (var i = 0 ; i < options.length; i++) {
	options[i] = String(options[i]);
    }
    var node = jsworld.MobyJsworld.select(options, updateF, attribs);

    node.toWrittenString = function(cache) { return '(js-select ...)'; };
    node.toDisplayedString = node.toWrittenString;
    node.toDomNode = function(cache) { return node; };
    return helpers.wrapJsValue(node);
};


EXPORTS['js-select'] =
    new CasePrimitive(
	'js-select',
	[new PrimProc('js-select', 2, false, false, 
		      function(optionList, updateF) {
			  return jsSelect(optionList, updateF,
					  types.EMPTY)
		      }),
	 new PrimProc('js-select', 3, false, false,
		      jsSelect)]);




EXPORTS['big-bang'] =
    new PrimProc('big-bang',
		 1,
		 true, true,
		 function(state, initW, handlers) {
		     arrayEach(handlers,
			       function(x, i) {
				   check(x, function(y) { return isWorldConfigOption(y) || isList(y) || types.isWorldConfig(y); },
					 'js-big-bang', 'handler or attribute list', i+2);
			       });
		     var unwrappedConfigs = 
			 helpers.map(function(x) {
			     if ( isWorldConfigOption(x) ) {
				 return function(config) { return x.configure(config); };
			     }
			     else {
				 return x;
			     }
			 },
				     handlers);
		     return types.internalPause(function(caller, restarter, onFail) {
			 var bigBangController;
			 var onBreak = function() {
			     bigBangController.breaker();
			 }
			 state.addBreakRequestedListener(onBreak);
			 bigBangController = jsworld.MobyJsworld.bigBang(
			     initW, 
			     state.getToplevelNodeHook()(),
			     unwrappedConfigs,
			     caller,
			     function(v) {
				 state.removeBreakRequestedListener(onBreak);
				 restarter(v);
			     },
 			     onFail);
		     })
		 });


//////////////////////////////////////////////////////////////////////


var emptyPage = function(attribList) {
    checkListOf(attribList, isAssocList, 'empty-page', '(listof X Y)', 1);

    var attribs = convertAttribList(attribList);
    var node = jsworld.MobyJsworld.emptyPage(attribs);
    
    // 	node.toWrittenString = function(cache) { return "(js-div)"; };
    // 	node.toDisplayedString = node.toWrittenString;
    // 	node.toDomNode = function(cache) { return node; };
    // 	return helpers.wrapJsValue(node);
    return node;
};

EXPORTS['empty-page'] =
    new CasePrimitive('empty-page',
		      [new PrimProc('empty-page', 0, false, false, 
				    function() {  return emptyPage(types.EMPTY); }),
		       new PrimProc('empty-page', 1, false, false, emptyPage)]);


EXPORTS['place-on-page'] = 
    new PrimProc('empty-page',
		 4,
		 false, false,
		 function(elt, left, top, page) {
		     // FIXME: add type checking
		     check(left, isReal, 'place-on-page', 'real', 2);
		     check(top, isReal, 'place-on-page', 'real', 3);
		     return jsworld.MobyJsworld.placeOnPage(
			 elt, jsnums.toFixnum(left), jsnums.toFixnum(top), page);
		 });





//////////////////////////////////////////////////////////////////////





EXPORTS['make-world-config'] =
    new PrimProc('make-world-config',
		 2,
		 true, false,
		 function(startup, shutdown, startupArgs) {
		 	var allArgs = [startup, shutdown].concat(startupArgs);
		 	check(startup, isFunction, 'make-world-config', 'procedure', 1, allArgs);
			check(shutdown, procArityContains(1), 'make-world-config', 'procedure (arity 1)', 2, allArgs);
			arrayEach(startupArgs, function(x, i) { check(x, isFunction, 'make-world-config', 'handler', i+3, allArgs); });

			if ( !procArityContains(startupArgs.length)(startup) ) {
				raise( types.incompleteExn(
					types.exnFailContract,
					'make-world-config: 1st argument must have arity equal to '
					+ 'the number of arguments after the second',
					[]) );
			}

			return types.worldConfig(startup, shutdown, startupArgs);
		 });


EXPORTS['make-effect-type'] =
	makeOptionPrimitive(
	    'make-effect-type',
	    4,
	    [false],
	    true,
	    function(userArgs, aState, name, superType, fieldCnt, impl, guard) {
		check(name, isSymbol, 'make-effect-type', 'string', 1, userArgs);
		check(superType, function(x) { return x === false || types.isEffectType(x) },
		      'make-effect-type', 'effect type or #f', 2, userArgs);
		check(fieldCnt, isNatural, 'make-effect-type', 'exact non-negative integer', 3, userArgs);
		check(impl, isFunction, 'make-effect-type', 'procedure', 4, userArgs);
//		checkListOf(handlerIndices, isNatural, 'make-effect-type', 'exact non-negative integer', 5);
		check(guard, function(x) { return x === false || isFunction(x); }, 'make-effect-type', 'procedure or #f', 6, userArgs);
		// Check the number of arguments on the guard
		var numberOfGuardArgs = fieldCnt + 1 + (superType ? superType.numberOfArgs : 0);
		if ( guard && !procArityContains(numberOfGuardArgs)(guard) ) {
			raise(types.incompleteExn(
				types.exnFailContract,
				helpers.format(
					'make-effect-type: guard procedure does not accept ~a arguments '
					+ '(one more than the number constructor arguments): ~s',
					[numberOfGuardArgs, guard]),
				[]));
		}

//		var jsImpl = schemeProcToJs(aState, impl);
		var jsGuard = (guard ? schemeProcToJs(aState, guard) : false);
//		var handlerIndices_js = helpers.map(jsnums.toFixnum, helpers.schemeListToArray(handlerIndices));

//		var caller = makeCaller(aState);
//		var wrapHandler = function(handler, changeWorld) {
//			return types.jsObject('function', function() {
//				var externalArgs = arguments;
//				changeWorld(function(w, k) {
//					var args = [w];
//					for (var i = 0; i < externalArgs.length; i++) {
//						args.push( helpers.wrapJsValue(externalArgs[i]) );
//					}
//					caller(handler, args, k);
//				});
//			});
//		}

		var anEffectType = types.makeEffectType(String(name),
							superType,
							fieldCnt,
							impl,
//							handlerIndices_js,
							jsGuard,
							makeCaller(aState));
		aState.v = getMakeStructTypeReturns(anEffectType);
	    });


EXPORTS['effect-type?'] = new PrimProc('effect-type?', 1, false, false, types.isEffectType);
EXPORTS['effect?'] = new PrimProc('effect?', 1, false, false, types.isEffect);

//EXPORTS['make-effect:do-nothing'] = new PrimProc('make-effect:do-nothing', 0, false, false, types.EffectDoNothing.constructor);
//EXPORTS['effect:do-nothing?'] = new PrimProc('effect:do-nothing?', 1, false, false, types.EffectDoNothing.predicate);


EXPORTS['make-render-effect-type'] =
	makeOptionPrimitive(
	    'make-render-effect-type',
	    4,
	    [false],
	    true,
	    function(userArgs, aState, name, superType, fieldCnt, impl, guard) {
		check(name, isSymbol, 'make-render-effect-type', 'string', 1, userArgs);
		check(superType, function(x) { return x === false || types.isEffectType(x) },
		      'make-render-effect-type', 'effect type or #f', 2, userArgs);
		check(fieldCnt, isNatural, 'make-render-effect-type', 'exact non-negative integer', 3, userArgs);
		check(impl, isFunction, 'make-render-effect-type', 'procedure', 4, userArgs);
		check(guard, function(x) { return x === false || isFunction(x); }, 'make-render-effect-type', 'procedure or #f', 6, userArgs);
		// Check the number of arguments on the guard
		var numberOfGuardArgs = fieldCnt + 1 + (superType ? superType.numberOfArgs : 0);
		if ( guard && !procArityContains(numberOfGuardArgs)(guard) ) {
			raise(types.incompleteExn(
				types.exnFailContract,
				helpers.format(
					'make-effect-type: guard procedure does not accept ~a arguments '
					+ '(one more than the number constructor arguments): ~s',
					[numberOfGuardArgs, guard]),
				[]));
		}
		var jsGuard = (guard ? schemeProcToJs(aState, guard) : false);

		var aRenderEffectType = types.makeRenderEffectType(String(name),
								   superType,
								   fieldCnt,
								   impl,
								   jsGuard);
		aState.v = getMakeStructTypeReturns(aRenderEffectType);
	    });


EXPORTS['render-effect-type?'] = new PrimProc('render-effect-type?', 1, false, false, types.isRenderEffectType);
EXPORTS['render-effect?'] = new PrimProc('render-effect?', 1, false, false, types.isRenderEffect);


EXPORTS['world-with-effects'] =
    new PrimProc('world-with-effects',
		 2,
		 false, false,
		 function(effects, w) {
		 	check(effects, isCompoundEffect, 'world-with-effects', 'compound effect', 1, arguments);

			return jsworld.Jsworld.with_multiple_effects(w, helpers.flattenSchemeListToArray(effects));
		 });



EXPORTS['make-render-effect'] = new PrimProc('make-render-effect', 2, false, false, types.makeRenderEffect);

EXPORTS['render-effect?'] = new PrimProc('render-effect?', 1, false, false, types.isRenderEffect);

EXPORTS['render-effect-dom-node'] =
    new PrimProc('render-effect-dom-node',
		 1,
		 false, false,
		 function(effect) {
		 	check(effect, types.isRenderEffect, 'render-effect-dom-node', 'render-effect', 1);
			return types.renderEffectDomNode(effect);
		 });

EXPORTS['render-effect-effects'] =
    new PrimProc('render-effect-effects',
		 1,
		 false, false,
		 function(effect) {
		 	check(effect, types.isRenderEffect, 'render-effect-effects', 'render-effect', 1);
			return types.renderEffectEffects(effect);
		 });


















//////////////////////////////////////////////////////////////////////

// Helper Functions








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


var length = function(lst) {
    checkList(lst, 'length', 1, [lst]);
    var ret = 0;
    for (; !isEmpty(lst); lst = lst.rest()) {
	ret = ret+1;
    }
    return ret;
}

















var getMakeStructTypeReturns = function(aStructType) {
	var name = aStructType.name;
	return new types.ValuesWrapper(
		[aStructType,
		 (new types.StructConstructorProc(name,
					    'make-'+name,
					    aStructType.numberOfArgs,
					    false,
					    false,
					    aStructType.constructor)),
		 (new types.StructPredicateProc(name, name+'?', 1, false, false, aStructType.predicate)),
		 (new types.StructAccessorProc(name,
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
		 (new types.StructMutatorProc(name,
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
var isList = helpers.isList;
var isListOf = helpers.isListOf;

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






var isStyle = function(x) {
	return ((isString(x) || isSymbol(x)) &&
		(String(x).toLowerCase() == "solid" ||
		 String(x).toLowerCase() == "outline"));
};


var isAssocList = function(x) {
	return isPair(x) && isPair(x.rest()) && isEmpty(x.rest().rest());
};


var isCompoundEffect = function(x) {
	return ( types.isEffect(x) || isListOf(x, isCompoundEffect) );
};

var isJsValue = types.isJsValue;
var isJsFunction = function(x) {
    return isJsValue(x) && typeof(x.unbox()) == 'function';
};



var arrayEach = function(arr, f) {
	for (var i = 0; i < arr.length; i++) {
		f.call(null, arr[i], i);
	}
}

//var throwCheckError = helpers.throwCheckError;
var check = helpers.check;














//////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////





/*



PRIMITIVES['js-p'] =
    makeOptionPrimitive('js-p',
			0,
			[types.EMPTY],
			false,
			function(userArgs, attribList) {
				checkListOf(attribList, function(x) { return isList(x) && length(x) == 2; },
					    'js-p', 'list of (list of X Y)', 1, userArgs);

				var attribs = assocListToHash(attribList);
				var node = helpers.wrapJsValue( jsworld.Jsworld.p(attribs) );

				node.toWrittenString = function(cache) { return "(js-p)"; };
				node.toDisplayedString = node.toWrittenString;
			//	node.toDomNode = function(cache) { return node; };
				return node;
			});


PRIMITIVES['js-div'] =
    makeOptionPrimitive('js-div',
			0,
			[types.EMPTY],
			false,
			function(userArgs, attribList) {
				checkListOf(attribList, isAssocList, 'js-div', '(listof X Y)', 1, userArgs);

				var attribs = assocListToHash(attribList);
				var node = helpers.wrapJsValue( jsworld.Jsworld.div(attribs) );
				
				node.toWrittenString = function(cache) { return "(js-div)"; };
				node.toDisplayedString = node.toWrittenString;
			//	node.toDomNode = function(cache) { return node; };
				return node;
			});


var jsButtonBang = function(funName, worldUpdateF, effectF, attribList) {
	var attribs = assocListToHash(attribList);
	var node = helpers.wrapJsValue( jsworld.Jsworld.buttonBang(worldUpdateF, effectF, attribs) );

	node.toWrittenString = function(cache) { return '(' + funName + ' ...)'; };
	node.toDisplayedString = node.toWrittenString;
//	node.toDomNode = function(cache) { return node; };
	return node;
};
PRIMITIVES['js-button'] =
    makeOptionPrimitive('js-button',
			1,
			[types.EMPTY],
			false,
			function(userArgs, updateWorldF, attribList) {
				check(updateWorldF, isFunction, 'js-button', 'procedure', 1, userArgs);
				checkListOf(attribList, isAssocList, 'js-button', '(listof X Y)', 2, userArgs);

				var noneF = new types.PrimProc('', 1, false, false, function(w) { return types.EMPTY; });
				return jsButtonBang('js-button', updateWorldF, noneF, attribList);
			});

PRIMITIVES['js-button!'] =
    makeOptionPrimitive('js-button!',
			2,
			[types.EMPTY],
			false,
			function(userArgs, updateWorldF, effectF, attribList) {
				check(worldUpdateF, isFunction, funName, 'procedure', 1, userArgs);
				check(effectF, isFunction, funName, 'procedure', 2, userArgs);
				checkListOf(attribList, isAssocList, funName, '(listof X Y)', 3, userArgs);

				return jsButtonBang('js-button!', updateWorldF, effectF, attribList);
			});


PRIMITIVES['js-input'] =
    makeOptionPrimitive('js-input',
			2,
			[types.EMPTY],
			false,
			function(userArgs, type, updateF, attribList) {
				check(type, isString, 'js-input', 'string', 1, userArgs);
				check(updateF, isFunction, 'js-input', 'procedure', 2, userArgs);
				checkListOf(attribList, isAssocList, 'js-input', '(listof X Y)', 3, userArgs);

				var attribs = assocListToHash(attribList);
				var node = helpers.wrapJsValue( jsworld.Jsworld.input(type.toString(), updateF, attribs) );

				node.toWrittenString = function(cache) { return "(js-input ...)"; }
				node.toDisplayedString = node.toWrittenString;
			//	node.toDomNode = function(cache) { return node; }
				return node;
			});


PRIMITIVES['js-img'] =
    makeOptionPrimitive('js-img',
			1,
			[types.EMPTY],
			false,
			function(userArgs, src, attribList) {
				check(src, isString, "js-img", "string", 1, userArgs);
				checkListOf(attribList, isAssocList, 'js-img', '(listof X Y)', 2, userArgs);

				var attribs = assocListToHash(attribList);
			        var node = helpers.wrapJsValue( jsworld.Jsworld.img(src.toString(), attribs) );

				node.toWrittenString = function(cache) { return "(js-img ...)"; }
				node.toDisplayedString = node.toWrittenString;
			//	node.toDomNode = function(cache) { return node; }
				return node;
			});


PRIMITIVES['js-text'] =
    new PrimProc('js-text',
		 1,
		 false, false,
		 function(s) {
		 	check(s, isString, 'js-text', 'string', 1);

		        var node = helpers.wrapJsValue( jsworld.Jsworld.text(s.toString(), []) );
			node.toWrittenString = function(cache) { return "(js-text ...)"; }
			node.toDisplayedString = node.toWrittenString;
//			node.toDomNode = function(cache) { return node; }
			return node;
		 });


PRIMITIVES['js-select'] =
    makeOptionPrimitive('js-select',
			2,
			[types.EMPTY],
			false,
			function(userArgs, optionList, updateF, attribList) {
				checkListOf(optionList, isString, 'js-select', 'listof string', 1, userArgs);
				check(updateF, isFunction, 'js-select', 'procedure', 2, userArgs);
				checkListOf(attribList, isAssocList, 'js-select', '(listof X Y)', 3, userArgs);

				var attribs = assocListToHash(attribList);
				var options = helpers.deepListToArray(optionList);
			        for (var i = 0; i < options.length; i++) {
				    options[i] = options[i].toString();
				}
				var node = helpers.wrapJsValue( jsworld.Jsworld.select(options, updateF, attribs) );

				node.toWrittenString = function(cache) { return '(js-select ...)'; };
				node.toDisplayedString = node.toWrittenString;
			//	node.toDomNode = function(cache) { return node; };
				return node;
			});



PRIMITIVES['js-big-bang'] =
    new PrimProc('js-big-bang',
		 1,
		 true, true,
		 function(aState, initW, configs) {
		 	arrayEach(configs,
				  function(x, i) {
				  	check(x, function(y) { return (types.isWorldConfig(y) ||
									jsworld.Jsworld.isBuiltInConfig(y)); },
					      'js-big-bang', 'world configuration', i+2);
				  });

			return PAUSE(function(caller, onSuccess, onFail) {
				var bigBangController = {};
				var onBreak = function() {
					bigBangController.breaker(aState);
				}
				aState.addBreakRequestedListener(onBreak);
				aState.onSuccess = function(v) {
					aState.removeBreakRequestedListener(onBreak);
					onSuccess(v);
				};
				jsworld.Jsworld.bigBang(initW,
//							aState.getToplevelNodeHook()(),
							configs,
							aState,
							caller,
							bigBangController);
//							caller,
//							function(v) {
//								aState.removeBreakRequestedListener(onBreak);
//								onSuccess(v);
//							},
//							onFail,
//							bigBangController);
			});
		 });


//////////////////////////////////////////////////////////////////////


    var emptyPage = function(attribList) {
	checkListOf(attribList, isAssocList, 'empty-page', '(listof X Y)', 1);

	var attribs = assocListToHash(attribList);
	var node = jsworld.MobyJsworld.emptyPage(attribs);
	
// 	node.toWrittenString = function(cache) { return "(js-div)"; };
// 	node.toDisplayedString = node.toWrittenString;
// 	node.toDomNode = function(cache) { return node; };
// 	return helpers.wrapJsValue(node);
	return node;
    };

    PRIMITIVES['empty-page'] =
	new CasePrimitive('empty-page',
			  [new PrimProc('empty-page', 0, false, false, 
					function() {  return emptyPage(types.EMPTY); }),
			   new PrimProc('empty-page', 1, false, false, emptyPage)]);

    
    PRIMITIVES['place-on-page'] = 
	new PrimProc('empty-page',
		     4,
		     false, false,
		     function(elt, left, top, page) {
			 // FIXME: add type checking
			 return jsworld.MobyJsworld.placeOnPage(
			     elt, left, top, page);
		     });
					    




//////////////////////////////////////////////////////////////////////





PRIMITIVES['make-world-config'] =
    makeOptionPrimitive('make-world-config',
			2,
			[false, false],
			false,
			function(userArgs, startup, shutdown, pause, restart) {
				check(startup, procArityContains(1), 'make-world-config', 'procedure', 1, userArgs);
				check(shutdown, procArityContains(1), 'make-world-config', 'procedure (arity 1)', 2, userArgs);
				check(pause, function(x) { return (x === false || procArityContains(1)(x)); },
				      'make-world-config', 'procedure (arity 1) or #f', 3, userArgs);
				check(restart, function(x) { return (x === false || procArityContains(2)(x)); },
				      'make-world-config', 'procedure (arity 2) or #f', 4, userArgs);

				return types.worldConfig(startup, shutdown, pause, restart);
			});

PRIMITIVES['bb-info'] = types.BigBangInfo;
PRIMITIVES['make-bb-info'] = new PrimProc('make-bb-info', 2, false, false, types.makeBigBangInfo);
PRIMITIVES['bb-info?'] = new PrimProc('bb-info?', 1, false, false, types.isBigBangInfo);

PRIMITIVES['bb-info-change-world'] =
    new PrimProc('bb-info-change-world',
		 1,
		 false, false, 
		 function(bbInfo) {
		 	check(bbInfo, types.isBigBangInfo, 'bb-info-change-world', 'bb-info', 1);
			return types.bbInfoChangeWorld(bbInfo);
		 });

PRIMITIVES['bb-info-toplevel-node'] =
    new PrimProc('bb-info-toplevel-node',
		 1,
		 false, false, 
		 function(bbInfo) {
		 	check(bbInfo, types.isBigBangInfo, 'bb-info-toplevel-node', 'bb-info', 1);
			return types.bbInfoToplevelNode(bbInfo);
		 });


PRIMITIVES['make-effect-type'] =
	makeOptionPrimitive(
	    'make-effect-type',
	    4,
	    [false],
	    true,
	    function(userArgs, aState, name, superType, fieldCnt, impl, guard) {
		check(name, isSymbol, 'make-effect-type', 'string', 1, userArgs);
		check(superType, function(x) { return x === false || types.isEffectType(x) },
		      'make-effect-type', 'effect type or #f', 2, userArgs);
		check(fieldCnt, isNatural, 'make-effect-type', 'exact non-negative integer', 3, userArgs);
		check(impl, isFunction, 'make-effect-type', 'procedure', 4, userArgs);
		check(guard, function(x) { return x === false || isFunction(x); }, 'make-effect-type', 'procedure or #f', 6, userArgs);

		var numberOfGuardArgs = fieldCnt + 1 + (superType ? superType.numberOfArgs : 0);
		var anEffectType = types.makeEffectType(name.toString(),
							superType,
							fieldCnt,
							impl,
							checkAndGetGuard('make-effect-type',
									 guard,
									 numberOfGuardArgs));
		aState.v = getMakeStructTypeReturns(anEffectType);
	    });


PRIMITIVES['effect-type?'] = new PrimProc('effect-type?', 1, false, false, types.isEffectType);
PRIMITIVES['effect?'] = new PrimProc('effect?', 1, false, false, types.isEffect);

//PRIMITIVES['make-effect:do-nothing'] = new PrimProc('make-effect:do-nothing', 0, false, false, types.EffectDoNothing.constructor);
//PRIMITIVES['effect:do-nothing?'] = new PrimProc('effect:do-nothing?', 1, false, false, types.EffectDoNothing.predicate);


PRIMITIVES['make-render-effect-type'] =
	makeOptionPrimitive(
	    'make-render-effect-type',
	    4,
	    [false],
	    true,
	    function(userArgs, aState, name, superType, fieldCnt, impl, guard) {
		check(name, isSymbol, 'make-render-effect-type', 'string', 1, userArgs);
		check(superType, function(x) { return x === false || types.isEffectType(x) },
		      'make-render-effect-type', 'effect type or #f', 2, userArgs);
		check(fieldCnt, isNatural, 'make-render-effect-type', 'exact non-negative integer', 3, userArgs);
		check(impl, isFunction, 'make-render-effect-type', 'procedure', 4, userArgs);
		check(guard, function(x) { return x === false || isFunction(x); }, 'make-render-effect-type', 'procedure or #f', 6, userArgs);

		var numberOfGuardArgs = fieldCnt + 1 + (superType ? superType.numberOfArgs : 0);
		var aRenderEffectType =
			types.makeRenderEffectType(name.toString(),
						   superType,
						   fieldCnt,
						   impl,
						   checkAndGetGuard('make-render-effect-type',
								    guard,
								    numberOfGuardArgs));
		aState.v = getMakeStructTypeReturns(aRenderEffectType);
	    });


PRIMITIVES['render-effect-type?'] = new PrimProc('render-effect-type?', 1, false, false, types.isRenderEffectType);
PRIMITIVES['render-effect?'] = new PrimProc('render-effect?', 1, false, false, types.isRenderEffect);


PRIMITIVES['world-with-effects'] =
    new PrimProc('world-with-effects',
		 2,
		 false, false,
		 function(effects, w) {
		 	check(effects, isCompoundEffect, 'world-with-effects', 'compound effect', 1, arguments);

			return jsworld.Jsworld.worldWithEffects(helpers.flattenSchemeListToArray(effects), w);
		 });



PRIMITIVES['make-render-effect'] = new PrimProc('make-render-effect', 2, false, false, types.makeRenderEffect);

PRIMITIVES['render-effect?'] = new PrimProc('render-effect?', 1, false, false, types.isRenderEffect);

PRIMITIVES['render-effect-dom-node'] =
    new PrimProc('render-effect-dom-node',
		 1,
		 false, false,
		 function(effect) {
		 	check(effect, types.isRenderEffect, 'render-effect-dom-node', 'render-effect', 1);
			return types.renderEffectDomNode(effect);
		 });

PRIMITIVES['render-effect-effects'] =
    new PrimProc('render-effect-effects',
		 1,
		 false, false,
		 function(effect) {
		 	check(effect, types.isRenderEffect, 'render-effect-effects', 'render-effect', 1);
			return types.renderEffectEffects(effect);
		 });








PRIMITIVES['stop-when'] =
    new PrimProc('stop-when', 1, false, false,
		 function(test) {
		 	check(test, isFunction, 'stop-when', 'procedure', 1);
			return jsworld.Jsworld.stopWhenConfig(test);
		 });
//PRIMITIVES['stop-when!'] = new PrimProc('stop-when!', 2, false, false,
//					onEventBang('stop-when!', 'stopWhen'));


PRIMITIVES['to-draw'] =
    new PrimProc('to-draw',
		 1,
		 false, false,
		 function(f) {
		     check(f, isFunction, 'to-draw', 'procedure', 1);
		     return jsworld.Jsworld.onDrawSceneConfig(f);

		 });


PRIMITIVES['to-draw-page'] =
    new CasePrimitive('to-draw-page',
	[new PrimProc('to-draw-page',
		      1,
		      false, false,
		      function(domHandler) {
			  check(domHandler, isFunction, 'to-draw-page', 'procedure', 1);
			  return jsworld.Jsworld.onDrawPageConfig(domHandler);
		      }),
	 new PrimProc('to-draw-page',
		      2,
		      false, false,
		      function(domHandler, styleHandler) {
		 	  check(domHandler, isFunction, 'to-draw-page', 'procedure', 1, arguments);
			  check(styleHandler, isFunction, 'to-draw-page', 'procedure', 2, arguments);
			  return jsworld.Jsworld.onDrawPageConfig(domHandler, styleHandler);
		      }) ]);



*/