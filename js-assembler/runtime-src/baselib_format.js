// Formatting library.
// Produces string and DOM representations of values.
//
(function(baselib) {
    var exports = {};
    baselib.format = exports;


    
    // format: string [X ...] string -> string
    // String formatting.  If an exception occurs, throws
    // a plain Error whose message describes the formatting error.
    var format = function(formatStr, args, functionName) {
	var throwFormatError = function() {
	    functionName = functionName || 'format';
	    var matches = formatStr.match(new RegExp('~[sSaA]', 'g'));
	    var expectedNumberOfArgs = (matches === null ? 0 : matches.length);
	    var errorStrBuffer = [functionName + ': format string requires ' + expectedNumberOfArgs
				  + ' arguments, given ' + args.length + '; arguments were:',
				  toWrittenString(formatStr)];
	    for (var i = 0; i < args.length; i++) {
		errorStrBuffer.push( toWrittenString(args[i]) );
	    }

	    throw new Error(errorStrBuffer.join(' '));
	}

	var pattern = new RegExp("~[sSaAnevE%~]", "g");
	var buffer = args.slice(0);
	var onTemplate = function(s) {
	    if (s === "~~") {
		return "~";
	    } else if (s === '~n' || s === '~%') {
		return "\n";
	    } else if (s === '~s' || s === "~S") {
		if (buffer.length === 0) {
		    throwFormatError();
		}
		return toWrittenString(buffer.shift());
	    } else if (s === '~e' || s === "~E") {
		// FIXME: we don't yet have support for the error-print
		// handler, and currently treat ~e just like ~s.
		if (buffer.length === 0) {
		    throwFormatError();
		}
		return toWrittenString(buffer.shift()); 
            }
            else if (s === '~v') {
		if (buffer.length === 0) {
		    throwFormatError();
		}
                // fprintf must do something more interesting here by
                // printing the dom representation directly...
		return toWrittenString(buffer.shift());
	    } else if (s === '~a' || s === "~A") {
		if (buffer.length === 0) {
		    throwFormatError();
		}
		return toDisplayedString(buffer.shift());
	    } else {
		throw new Error(functionName + 
                                ': string.replace matched invalid regexp');
	    }
	}
	var result = formatStr.replace(pattern, onTemplate);
	if (buffer.length > 0) {
	    throwFormatError();
	}
	return result;
    };


    // toWrittenString: Any Hashtable -> String
    var toWrittenString = function(x, cache) {
        if (! cache) { 
     	    cache = plt.baselib.hash.makeLowLevelEqHash();
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
    	    cache = plt.baselib.hash.makeLowLevelEqHash();
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
        this.cache = plt.baselib.hash.makeLowLevelEqHash();
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










    //////////////////////////////////////////////////////////////////////


    exports.ToDomNodeParameters = ToDomNodeParameters;

    exports.format = format;
    exports.toWrittenString = toWrittenString;
    exports.toDisplayedString = toDisplayedString;
    exports.toDomNode = toDomNode;

    exports.escapeString = escapeString;
})(this['plt'].baselib);