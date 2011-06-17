if (! this['plt']) { this['plt'] = {}; }

// Helpers library: includes a bunch of helper functions that will be used
//
//
// FIXME: there's a circularity between this module and types, and that circularly
// should not be there!


//////////////////////////////////////////////////////////////

// File of helper functions for primitives and world.


(function(scope) {
    var helpers = {};
    scope.helpers = helpers;


    // types refers to plt.types, and will be initialized later.
    var types = scope['types'];
    scope.link.ready('types', 
                     function() { 
                         types = scope['types'];
                     });





    var format = function(formatStr, args, functionName) {
	var throwFormatError = function() {
	    functionName = functionName || '#<procedure>';
	    var matches = formatStr.match(new RegExp('~[sSaA]', 'g'));
	    var expectedNumberOfArgs = matches == null ? 0 : matches.length;
	    var errorStrBuffer = [functionName + ': format string requires ' + expectedNumberOfArgs
				  + ' arguments, given ' + args.length + '; arguments were:',
				  toWrittenString(formatStr)];
	    for (var i = 0; i < args.length; i++) {
		errorStrBuffer.push( toWrittenString(args[i]) );
	    }

	    raise( types.incompleteExn(types.exnFailContract, errorStrBuffer.join(' '), []) );
	}

	var pattern = new RegExp("~[sSaAneE%~]", "g");
	var buffer = args.slice(0);;
	function f(s) {
	    if (s == "~~") {
		return "~";
	    } else if (s == '~n' || s == '~%') {
		return "\n";
	    } else if (s == '~s' || s == "~S") {
		if (buffer.length == 0) {
		    throwFormatError();
		}
		return toWrittenString(buffer.shift());
	    } else if (s == '~e' || s == "~E") {
		// FIXME: we don't yet have support for the error-print
		// handler, and currently treat ~e just like ~s.
		if (buffer.length == 0) {
		    throwFormatError();
		}
		return toWrittenString(buffer.shift());
	    } else if (s == '~a' || s == "~A") {
		if (buffer.length == 0) {
		    throwFormatError();
		}
		return toDisplayedString(buffer.shift());
	    } else {
		throw types.internalError('format: string.replace matched invalid regexp', false);
	    }
	}
	var result = formatStr.replace(pattern, f);
	if (buffer.length > 0) {
	    throwFormatError();
	}
	return result;
    };


    // forEachK: CPS( array CPS(array -> void) (error -> void) -> void )
    // Iterates through an array and applies f to each element using CPS
    // If an error is thrown, it catches the error and calls f_error on it
    var forEachK = function(a, f, f_error, k) {
	var forEachHelp = function(i) {
	    if( i >= a.length ) {
		if (k) {
		    return k();
		} else {
		    return;
		}
	    }

	    try {
		return f(a[i], function() { return forEachHelp(i+1); });
	    } catch (e) {
		f_error(e);
	    }
	};
	return forEachHelp(0);
    };


    // reportError: (or exception string) -> void
    // Reports an error to the user, either at the console
    // if the console exists, or as alerts otherwise.
    var reportError = function(e) {
	var reporter;
	if (typeof(console) != 'undefined' && 
	    typeof(console.log) != 'undefined') {
	    reporter = (function(x) { console.log(x); });
	} else {
	    reporter = (function(x) { alert(x); });
	}
	if (typeof e == 'string') {
	    reporter(e);
	} else if ( types.isSchemeError(e) ) {
	    if ( types.isExn(e.val) ) {
		reporter( types.exnMessage(e.val) );
	    }
	    else {
		reporter(e.val);
	    }
	} else if ( types.isInternalError(e) ) {
	    reporter(e.val);
	} else if (e.message) {
	    reporter(e.message);
	} else {
	    reporter(e.toString());
	}
        //		if (plt.Kernel.lastLoc) {
        //			var loc = plt.Kernel.lastLoc;
        //			if (typeof(loc) === 'string') {
        //			reporter("Error was raised around " + loc);
        //			} else if (typeof(loc) !== 'undefined' &&
        //				   typeof(loc.line) !== 'undefined') {
        //			reporter("Error was raised around: "
        //				 + plt.Kernel.locToString(loc));
        //			}
        //		}
    };


    var raise = function(v) {
	throw types.schemeError(v);
    };


    var procArityContains = function(n) {
	return function(proc) {
	    var singleCase = function(aCase) {
		if ( aCase instanceof types.ContinuationClosureValue ) {
		    return true;
		}
		return (aCase.numParams == n ||
			(aCase.isRest && aCase.numParams <= n));
	    };

	    var cases = [];
	    if ( proc instanceof types.ContinuationClosureValue ||
		 proc instanceof types.ClosureValue ||
		 proc instanceof types.PrimProc ) {
		return singleCase(proc);
	    }
	    else if (proc instanceof types.CasePrimitive) {
		cases = proc.cases;
	    }
	    else if (proc instanceof types.CaseLambdaValue) {
		cases = proc.closures;
	    }

	    for (var i = 0; i < cases.length; i++) {
		if ( singleCase(cases[i]) )
		    return true;
	    }
	    return false;
	}
    };

    var throwCheckError = function(details, pos, args) {
	var errorFormatStr;
	if (args && args.length > 1) {
	    var errorFormatStrBuffer = ['~a: expects type <~a> as ~a arguments, given: ~s; other arguments were:'];
	    for (var i = 0; i < args.length; i++) {
		if ( i != pos-1 ) {
		    errorFormatStrBuffer.push(toWrittenString(args[i]));
		}
	    }
	    errorFormatStr = errorFormatStrBuffer.join(' ');
	}
	else {
	    errorFormatStr = "~a: expects argument of type <~a>, given: ~s";
	    details.splice(2, 1);
	}

	raise( types.incompleteExn(types.exnFailContract,
				   helpers.format(errorFormatStr, details),
				   []) );
    };

    var check = function(x, f, functionName, typeName, position, args) {
	if ( !f(x) ) {
	    throwCheckError([functionName,
			     typeName,
			     helpers.ordinalize(position),
			     x],
			    position,
			    args);
	}
    };

    var isList = function(x) {
	var seenPairs = makeLowLevelEqHash();
	while (true) {
	    if (seenPairs.containsKey(x)) {
		return true;
	    } else if (x === types.EMPTY) {
		return true;
	    } else if (types.isPair(x)) {
		seenPairs.put(x, true);
		x = x.rest();
	    } else {
		return false;
	    }
	}
    };

    var isListOf = function(x, f) {
	var seenPairs = makeLowLevelEqHash();
	while (true) {
	    if (seenPairs.containsKey(x)) {
		return true;
	    } else if (x === types.EMPTY) {
		return true;
	    } else if (types.isPair(x)) {
		seenPairs.put(x, true);
		if (f(x.first())) {
		    x = x.rest();
		} else {
		    return false;
		}
	    } else {
		return false;
	    }
	}
    };

    var checkListOf = function(lst, f, functionName, typeName, position, args) {
	if ( !isListOf(lst, f) ) {
	    helpers.throwCheckError([functionName,
				     'list of ' + typeName,
				     helpers.ordinalize(position),
				     lst],
				    position,
				    args);
	}
    };


    //	// remove: array any -> array
    //	// removes the first instance of v in a
    //	// or returns a copy of a if v does not exist
    //	var remove = function(a, v) {
    //		for (var i = 0; i < a.length; i++) {
    //			if (a[i] === v) {
    //				return a.slice(0, i).concat( a.slice(i+1, a.length) );
    //			}
    //		}
    //		return a.slice(0);
    //	};

    // map: array (any -> any) -> array
    // applies f to each element of a and returns the result
    // as a new array
    var map = function(f, a) {
	var b = new Array(a.length);
	for (var i = 0; i < a.length; i++) {
	    b[i] = f(a[i]);
	}
	return b;
    };


    var concatMap = function(f, a) {
	var b = [];
	for (var i = 0; i < a.length; i++) {
	    b = b.concat( f(a[i]) );
	}
	return b;
    };


    var schemeListToArray = function(lst) {
	var result = [];
	while ( !lst.isEmpty() ) {
	    result.push(lst.first());
	    lst = lst.rest();
	}
	return result;
    }

    // deepListToArray: any -> any
    // Converts list structure to array structure.
    var deepListToArray = function(x) {
	var thing = x;
	if (thing === types.EMPTY) {
	    return [];
	} else if (types.isPair(thing)) {
	    var result = [];
	    while (!thing.isEmpty()) {
		result.push(deepListToArray(thing.first()));
		thing = thing.rest();
	    }
	    return result;
	} else {
	    return x;
	}
    }


    var flattenSchemeListToArray = function(x) {
	if ( !isList(x) ) {
	    return [x];
	}

	var ret = [];
	while ( !x.isEmpty() ) {
	    ret = ret.concat( flattenSchemeListToArray(x.first()) );
	    x = x.rest();
	}
	return ret;
    };


    // assocListToHash: (listof (list X Y)) -> (hashof X Y)
    var assocListToHash = function(lst) {
	var result = {};
	while ( !lst.isEmpty() ) {
	    var key = lst.first().first();
	    var val = lst.first().rest().first();
	    result[key] = val;
	    lst = lst.rest();
	}
	return result;
    };


    var ordinalize = function(n) {
	// special case for 11th:
	if ( n % 100 == 11 ) {
	    return n + 'th';
	}
	var res = n;
	switch( n % 10 ) {
	case 1: res += 'st'; break;
	case 2: res += 'nd'; break;
	case 3: res += 'rd'; break;
	default: res += 'th'; break;
	}
	return res;
    }


    var wrapJsValue = function(x) {
	if (x === undefined) {
	    return types.jsValue('undefined', x);
	}
	else if (x === null) {
	    return types.jsValue('null', x);
	}
	else if (typeof(x) == 'function') {
	    return types.jsValue('function', x);
	}
	else if ( x instanceof Array ) {
	    return types.jsValue('array', x);
	}
	else if ( typeof(x) == 'string' ) {
	    return types.jsValue("'" + x.toString() + "'", x);
	}
	else {
	    return types.jsValue(x.toString(), x);
	}
    };


    var getKeyCodeName = function(e) {
	var code = e.charCode || e.keyCode;
	var keyname;
	switch(code) {
	case 16: keyname = "shift"; break;
	case 17: keyname = "control"; break;
	case 19: keyname = "pause"; break;
	case 27: keyname = "escape"; break;
	case 33: keyname = "prior"; break;
	case 34: keyname = "next"; break;
	case 35: keyname = "end"; break;
	case 36: keyname = "home"; break;
	case 37: keyname = "left"; break;
	case 38: keyname = "up"; break;
	case 39: keyname = "right"; break;
	case 40: keyname = "down"; break;
	case 42: keyname = "print"; break;
	case 45: keyname = "insert"; break;
	case 46: keyname = String.fromCharCode(127); break;
	case 106: keyname = "*"; break;
	case 107: keyname = "+"; break;
	case 109: keyname = "-"; break;
	case 110: keyname = "."; break;
	case 111: keyname = "/"; break;
	case 144: keyname = "numlock"; break;
	case 145: keyname = "scroll"; break;
	case 186: keyname = ";"; break;
	case 187: keyname = "="; break;
	case 188: keyname = ","; break;
	case 189: keyname = "-"; break;
	case 190: keyname = "."; break;
	case 191: keyname = "/"; break;
	case 192: keyname = "`"; break;
	case 219: keyname = "["; break;
	case 220: keyname = "\\"; break;
	case 221: keyname = "]"; break;
	case 222: keyname = "'"; break;
	default: if (code >= 96 && code <= 105) {
	    keyname = (code - 96).toString();
	}
	    else if (code >= 112 && code <= 123) {
		keyname = "f" + (code - 111);
	    }
	    else {
		keyname = String.fromCharCode(code).toLowerCase();
	    }
	    break;
	}
	return keyname;
    };





    // maybeCallAfterAttach: dom-node -> void
    // walk the tree rooted at aNode, and call afterAttach if the element has
    // such a method.
    var maybeCallAfterAttach = function(aNode) {
	var stack = [aNode];
	while (stack.length !== 0) {
	    var nextNode = stack.pop();
	    if (nextNode.afterAttach) {
		nextNode.afterAttach(nextNode);
	    }
	    if (nextNode.hasChildNodes && nextNode.hasChildNodes()) {
		var children = nextNode.childNodes;
		for (var i = 0; i < children.length; i++) {
		    stack.push(children[i]);
		}
	    }
	}
    };








    // makeLocationDom: location -> dom
    // Dom type that has special support in the editor through the print hook.
    // The print hook is expected to look at the printing of dom values with
    // this particular structure.  In the context of WeScheme, the environment
    // will rewrite these to be clickable links.
    var makeLocationDom = function(aLocation) {
	var locationSpan = document.createElement("span");
	var idSpan = document.createElement("span");
	var offsetSpan = document.createElement("span");
	var lineSpan = document.createElement("span");
	var columnSpan = document.createElement("span");
	var spanSpan = document.createElement("span");

	locationSpan['className'] = 'location-reference';
	idSpan['className'] = 'location-id';
	offsetSpan['className'] = 'location-offset';
	lineSpan['className'] = 'location-line';
	columnSpan['className'] = 'location-column';
	spanSpan['className'] = 'location-span';

	idSpan.appendChild(document.createTextNode(String(aLocation.id)));
	offsetSpan.appendChild(document.createTextNode(String(aLocation.offset)));
	lineSpan.appendChild(document.createTextNode(String(aLocation.line)));
	columnSpan.appendChild(document.createTextNode(String(aLocation.column)));
	spanSpan.appendChild(document.createTextNode(String(aLocation.span)));

	locationSpan.appendChild(idSpan);
	locationSpan.appendChild(offsetSpan);
	locationSpan.appendChild(lineSpan);
	locationSpan.appendChild(columnSpan);   
	locationSpan.appendChild(spanSpan);

	return locationSpan;
    };


    var isLocationDom = function(thing) {
	return (thing
		&&
		(thing.nodeType === Node.TEXT_NODE ||
		 thing.nodeType === Node.ELEMENT_NODE)
		&&
		thing['className'] === 'location-reference');
    };











    var _eqHashCodeCounter = 0;
    makeEqHashCode = function() {
	_eqHashCodeCounter++;
	return _eqHashCodeCounter;
    };

    
    // getHashCode: any -> (or fixnum string)
    // Produces a hashcode appropriate for eq.
    getEqHashCode = function(x) {
	if (typeof(x) === 'string') {
	    return x;
	}
	if (typeof(x) === 'number') {
	    return String(x);
	}
	if (x && !x._eqHashCode) {
	    x._eqHashCode = makeEqHashCode();
	}
	if (x && x._eqHashCode) {
	    return x._eqHashCode;
	}
	return 0;
    };




    var makeLowLevelEqHash = function() {
	return new Hashtable(function(x) { return getEqHashCode(x); },
			     function(x, y) { return x === y; });
    };



    // Inheritance.
    var heir = function(parentPrototype) {
	var f = function() {}
	f.prototype = parentPrototype;
	return new f();
    };



 
    // toWrittenString: Any Hashtable -> String
    var toWrittenString = function(x, cache) {
        if (! cache) { 
     	    cache = makeLowLevelEqHash();
        }
        if (x === null) {
            return "null";
        }
        if (x === true) { return "true"; }
        if (x === false) { return "false"; }
        if (typeof(x) === 'object') {
	    if (cache.containsKey(x)) {
		return "...";
	    }
        }
        if (x == undefined) {
	    return "#<undefined>";
        }
        if (typeof(x) == 'string') {
	    return escapeString(x.toString());
        }
        if (typeof(x) != 'object' && typeof(x) != 'function') {
	    return x.toString();
        }

        var returnVal;
        if (typeof(x.toWrittenString) !== 'undefined') {
	    returnVal = x.toWrittenString(cache);
        } else if (typeof(x.toDisplayedString) !== 'undefined') {
	    returnVal = x.toDisplayedString(cache);
        } else {
	    returnVal = x.toString();
        }
        cache.remove(x);
        return returnVal;
    };



    // toDisplayedString: Any Hashtable -> String
    var toDisplayedString = function(x, cache) {
        if (! cache) {
    	    cache = makeLowLevelEqHash();
        }
        if (x === null) {
            return "null";
        }
        if (x === true) { return "true"; }
        if (x === false) { return "false"; }
        if (typeof(x) === 'object') {
	    if (cache.containsKey(x)) {
		return "...";
	    }
        }
        if (x == undefined || x == null) {
	    return "#<undefined>";
        }
        if (typeof(x) == 'string') {
	    return x;
        }
        if (typeof(x) != 'object' && typeof(x) != 'function') {
	    return x.toString();
        }

        var returnVal;
        if (typeof(x.toDisplayedString) !== 'undefined') {
	    returnVal = x.toDisplayedString(cache);
        } else if (typeof(x.toWrittenString) !== 'undefined') {
	    returnVal = x.toWrittenString(cache);
        } else {
	    returnVal = x.toString();
        }
        cache.remove(x);
        return returnVal;
    };




    var ToDomNodeParameters = function(params) {
        if (! params) { params = {}; }
        this.cache = makeLowLevelEqHash();
        for (var k in params) {
            if (params.hasOwnProperty(k)) {
                this[k] = params[k];
            }
        }
        this.objectCounter = 0;
    };

    // getMode: -> (U "print" "display" "write")
    ToDomNodeParameters.prototype.getMode = function() {
        if (this.mode) { 
            return this.mode; 
        }
        return 'print';
    };

    ToDomNodeParameters.prototype.containsKey = function(x) {
        return this.cache.containsKey(x);
    };

    ToDomNodeParameters.prototype.get = function(x) {
        return this.cache.get(x);
    };

    ToDomNodeParameters.prototype.remove = function(x) {
        return this.cache.remove(x);
    };

    ToDomNodeParameters.prototype.put = function(x) {
        this.objectCounter++;
        return this.cache.put(x, this.objectCounter);
    };


    // toDomNode: scheme-value -> dom-node
    var toDomNode = function(x, params) {
        if (params === 'write') {
            params = new ToDomNodeParameters({'mode' : 'write'});
        } else if (params === 'print') {
            params = new ToDomNodeParameters({'mode' : 'print'});
        } else if (params === 'display') {
            params = new ToDomNodeParameters({'mode' : 'display'});
        } else {
            params = params || new ToDomNodeParameters({'mode' : 'display'});
        } 

        if (jsnums.isSchemeNumber(x)) {
	    var node = numberToDomNode(x, params);
            $(node).addClass("number");
            return node;
        }
        
        if (x === null) {
	    var node = document.createElement("span");
	    node.appendChild(document.createTextNode("null"));
            $(node).addClass("null");
	    return node;
        }

        if (x === true) {
	    var node = document.createElement("span");
	    node.appendChild(document.createTextNode("true"));
            $(node).addClass("boolean");
	    return node;
        }

        if (x === false) {
	    var node = document.createElement("span");
	    node.appendChild(document.createTextNode("false"));
            $(node).addClass("boolean");
	    return node;
        }

        if (typeof(x) == 'object') {
	    if (params.containsKey(x)) {
		var node = document.createElement("span");
		node.appendChild(document.createTextNode("#" + params.get(x)));
		return node;
	    }
        }
        if (x === undefined || x == null) {
	    var node = document.createElement("span");
	    node.appendChild(document.createTextNode("#<undefined>"));
	    return node;
        }

        if (typeof(x) == 'string') {
	    var wrapper = document.createElement("span");
            wrapper.style["white-space"] = "pre";
	    var node;
            if (params.getMode() === 'write' || params.getMode() === 'print') {
                node = document.createTextNode(toWrittenString(x));
            } else {
                node = document.createTextNode(toDisplayedString(x));
            }
	    wrapper.appendChild(node);
            $(wrapper).addClass("string");
	    return wrapper;
        }

        if (typeof(x) != 'object' && typeof(x) != 'function') {
	    var node = document.createElement("span");
	    node.appendChild(document.createTextNode(x.toString()));
            $(node).addClass("procedure");
	    return node;
        }

        var returnVal;
        if (x.nodeType) {
	    returnVal =  x;
        } else if (typeof(x.toDomNode) !== 'undefined') {
	    returnVal =  x.toDomNode(params);
        } else if (params.getMode() === 'write' && 
                   typeof(x.toWrittenString) !== 'undefined') {
	    var node = document.createElement("span");
	    node.appendChild(document.createTextNode(x.toWrittenString(params)));
	    returnVal =  node;
        } else if (params.getMode() === 'display' &&
                   typeof(x.toDisplayedString) !== 'undefined') {
	    var node = document.createElement("span");
	    node.appendChild(document.createTextNode(x.toDisplayedString(params)));
	    returnVal =  node;
        } else {
	    var node = document.createElement("span");
	    node.appendChild(document.createTextNode(x.toString()));
	    returnVal =  node;
        }
        params.remove(x);
        return returnVal;
    };



    // numberToDomNode: jsnum -> dom
    // Given a jsnum, produces a dom-node representation.
    var numberToDomNode = function(n, params) {
        var node;
        if (jsnums.isExact(n)) {
	    if (jsnums.isInteger(n)) {
	        node = document.createElement("span");
	        node.appendChild(document.createTextNode(n.toString()));
	        return node;
	    } else if (jsnums.isRational(n)) {
	        return rationalToDomNode(n);
	    } else if (jsnums.isComplex(n)) {
	        node = document.createElement("span");
	        node.appendChild(document.createTextNode(n.toString()));
	        return node;
	    } else {
	        node = document.createElement("span");
	        node.appendChild(document.createTextNode(n.toString()));
	        return node;
	    }
        } else {
	    node = document.createElement("span");
	    node.appendChild(document.createTextNode(n.toString()));
	    return node;
        }
    };

    // rationalToDomNode: rational -> dom-node
    var rationalToDomNode = function(n) {
        var repeatingDecimalNode = document.createElement("span");
        var chunks = jsnums.toRepeatingDecimal(jsnums.numerator(n),
                                               jsnums.denominator(n),
                                               {limit: 25});
        repeatingDecimalNode.appendChild(document.createTextNode(chunks[0] + '.'))
        repeatingDecimalNode.appendChild(document.createTextNode(chunks[1]));
        if (chunks[2] === '...') {
            repeatingDecimalNode.appendChild(
                document.createTextNode(chunks[2]));
        } else if (chunks[2] !== '0') {
            var overlineSpan = document.createElement("span");
            overlineSpan.style.textDecoration = 'overline';
            overlineSpan.appendChild(document.createTextNode(chunks[2]));
            repeatingDecimalNode.appendChild(overlineSpan);
        }


        var fractionalNode = document.createElement("span");
        var numeratorNode = document.createElement("sup");
        numeratorNode.appendChild(document.createTextNode(String(jsnums.numerator(n))));
        var denominatorNode = document.createElement("sub");
        denominatorNode.appendChild(document.createTextNode(String(jsnums.denominator(n))));
        fractionalNode.appendChild(numeratorNode);
        fractionalNode.appendChild(document.createTextNode("/"));
        fractionalNode.appendChild(denominatorNode);

        
        var numberNode = document.createElement("span");
        numberNode.appendChild(repeatingDecimalNode);
        numberNode.appendChild(fractionalNode);
        fractionalNode.style['display'] = 'none';

        var showingRepeating = true;

        numberNode.onclick = function(e) {
            showingRepeating = !showingRepeating;
            repeatingDecimalNode.style['display'] = 
                (showingRepeating ? 'inline' : 'none')
            fractionalNode.style['display'] = 
                (!showingRepeating ? 'inline' : 'none')
        };
        numberNode.style['cursor'] = 'pointer';
        return numberNode;
    }






    var escapeString = function(s) {
        return '"' + replaceUnprintableStringChars(s) + '"';
    };

    var replaceUnprintableStringChars = function(s) {
	var ret = [];
	for (var i = 0; i < s.length; i++) {
	    var val = s.charCodeAt(i);
	    switch(val) {
	    case 7: ret.push('\\a'); break;
	    case 8: ret.push('\\b'); break;
	    case 9: ret.push('\\t'); break;
	    case 10: ret.push('\\n'); break;
	    case 11: ret.push('\\v'); break;
	    case 12: ret.push('\\f'); break;
	    case 13: ret.push('\\r'); break;
	    case 34: ret.push('\\"'); break;
	    case 92: ret.push('\\\\'); break;
	    default: if (val >= 32 && val <= 126) {
		ret.push( s.charAt(i) );
	    }
		else {
		    var numStr = val.toString(16).toUpperCase();
		    while (numStr.length < 4) {
			numStr = '0' + numStr;
		    }
		    ret.push('\\u' + numStr);
		}
		break;
	    }
	}
	return ret.join('');
    };






    // clone: object -> object
    // Copies an object.  The new object should respond like the old
    // object, including to things like instanceof
    var clone = function(obj) {
        var C = function() {}
        C.prototype = obj;
        var c = new C();
        for (property in obj) {
	    if (obj.hasOwnProperty(property)) {
	        c[property] = obj[property];
	    }
        }
        return c;
    };






    ////////////////////////////////////////////////

    helpers.format = format;
    helpers.forEachK = forEachK;
    helpers.reportError = reportError;
    helpers.raise = raise;

    helpers.procArityContains = procArityContains;
    helpers.throwCheckError = throwCheckError;
    helpers.isList = isList;
    helpers.isListOf = isListOf;
    helpers.check = check;
    helpers.checkListOf = checkListOf;
    
    //	helpers.remove = remove;
    helpers.map = map;
    helpers.concatMap = concatMap;
    helpers.schemeListToArray = schemeListToArray;
    helpers.deepListToArray = deepListToArray;
    helpers.flattenSchemeListToArray = flattenSchemeListToArray;
    helpers.assocListToHash = assocListToHash;

    helpers.ordinalize = ordinalize;
    helpers.wrapJsValue = wrapJsValue;

    helpers.getKeyCodeName = getKeyCodeName;

    helpers.maybeCallAfterAttach = maybeCallAfterAttach;

    helpers.makeLocationDom = makeLocationDom;
    helpers.isLocationDom = isLocationDom;


    helpers.getEqHashCode = getEqHashCode;
    helpers.makeLowLevelEqHash = makeLowLevelEqHash;

    helpers.heir = heir;
    helpers.escapeString = escapeString;
    helpers.toWrittenString = toWrittenString;
    helpers.toDisplayedString = toDisplayedString;


    helpers.toDomNode = toDomNode;
    helpers.ToDomNodeParameters = ToDomNodeParameters;

    helpers.clone = clone;


    scope.link.announceReady('helpers');
})(this['plt']);

/////////////////////////////////////////////////////////////////
