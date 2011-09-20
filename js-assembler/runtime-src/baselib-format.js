/*jslint browser: true, undef: false, unparam: true, sub: true, vars: true, white: true, plusplus: true, maxerr: 50, indent: 4 */

// Formatting library.
// Produces string and DOM representations of values.
//
/*global $*/
(function(baselib, $) {
    'use strict';
    var exports = {};
    baselib.format = exports;


    var replaceUnprintableStringChars = function(s) {
        var ret = [], i;
        for (i = 0; i < s.length; i++) {
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

    var escapeString = function(s) {
        return '"' + replaceUnprintableStringChars(s) + '"';
    };


    // toWrittenString: Any Hashtable -> String
    var toWrittenString = function(x, cache) {
        if (! cache) { 
            cache = baselib.hashes.makeLowLevelEqHash();
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
        if (x === undefined) {
            return "#<undefined>";
        }
        if (typeof(x) === 'string') {
            return escapeString(x.toString());
        }

        if (baselib.functions.isProcedure(x)) {
            return '#<procedure:' + x.displayName + '>';
        }

        if (typeof(x) !== 'object' && typeof(x) !== 'function') {
            return x.toString();
        }

        var returnVal;
        if (x.toWrittenString) {
            returnVal = x.toWrittenString(cache);
        } else if (x.toDisplayedString) {
            returnVal = x.toDisplayedString(cache);
        } else {
            returnVal = x.toString();
        }
        return returnVal;
    };



    // toDisplayedString: Any Hashtable -> String
    var toDisplayedString = function(x, cache) {
        if (! cache) {
            cache = baselib.hashes.makeLowLevelEqHash();
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
        if (x === undefined || x === null) {
            return "#<undefined>";
        }
        if (typeof(x) === 'string') {
            return x;
        }

        if (baselib.functions.isProcedure(x)) {
            return '#<procedure:' + x.displayName + '>';
        }

        if (typeof(x) !== 'object' && typeof(x) !== 'function') {
            return x.toString();
        }

        var returnVal;
        if (x.toDisplayedString) {
            returnVal = x.toDisplayedString(cache);
        } else if (x.toWrittenString) {
            returnVal = x.toWrittenString(cache);
        } else {
            returnVal = x.toString();
        }
        return returnVal;
    };



    var formatRegexp1 = new RegExp('~[sSaA]', 'g');
    var formatRegexp2 = new RegExp("~[sSaAnevE%~]", "g");
    
    // format: string [X ...] string -> string
    // String formatting.  If an exception occurs, throws
    // a plain Error whose message describes the formatting error.
    var format = function(formatStr, args, functionName) {
        var throwFormatError = function() {
            functionName = functionName || 'format';
            var matches = formatStr.match(formatRegexp1);
            var expectedNumberOfArgs = (matches === null ? 0 : matches.length);
            var errorStrBuffer = [functionName + ': format string requires ' + expectedNumberOfArgs
                                  + ' arguments, given ' + args.length + '; arguments were:',
                                  toWrittenString(formatStr)];
            var i;
            for (i = 0; i < args.length; i++) {
                errorStrBuffer.push( toWrittenString(args[i]) );
            }

            throw new Error(errorStrBuffer.join(' '));
        };


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
        };
        var result = formatStr.replace(formatRegexp2, onTemplate);
        if (buffer.length > 0) {
            throwFormatError();
        }
        return result;
    };
    



    var ToDomNodeParameters = function(params) {
        if (! params) { params = {}; }
        this.cache = baselib.hashes.makeLowLevelEqHash();
        var k;
        for (k in params) {
            if (params.hasOwnProperty(k)) {
                this[k] = params[k];
            }
        }
        if (this.depth === undefined) {
            this.depth = 0;
        }
        if (this.objectCounter === undefined) {
            this.objectCounter = 0;
        }
    };


    ToDomNodeParameters.prototype.incrementDepth = function() {
        return new ToDomNodeParameters({ mode : this.mode,
                                         depth: this.depth + 1,
                                         objectCounter: this.objectCounter });
    };
    

    // getMode: -> (U "print" "display" "write" "constructor")
    ToDomNodeParameters.prototype.getMode = function() {
        if (this.mode) { 
            return this.mode; 
        }
        return 'print';
    };

    ToDomNodeParameters.prototype.getDepth = function(x) {
        return this.depth;
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

    ToDomNodeParameters.prototype.recur = function(x) {
        return toDomNode(x, this.incrementDepth());
    };



    // rationalToDomNode: rational -> dom-node
    var rationalToDomNode = function(n) {
        var repeatingDecimalNode = document.createElement("span");
        var chunks = baselib.numbers.toRepeatingDecimal(baselib.numbers.numerator(n),
                                                        baselib.numbers.denominator(n),
                                                        {limit: 25});
        repeatingDecimalNode.appendChild(document.createTextNode(chunks[0] + '.'));
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
        numeratorNode.appendChild(document.createTextNode(String(baselib.numbers.numerator(n))));
        var denominatorNode = document.createElement("sub");
        denominatorNode.appendChild(document.createTextNode(String(baselib.numbers.denominator(n))));
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
                (showingRepeating ? 'inline' : 'none');
            fractionalNode.style['display'] = 
                (!showingRepeating ? 'inline' : 'none');
        };
        numberNode.style['cursor'] = 'pointer';
        return numberNode;
    };


    // numberToDomNode: jsnum -> dom
    // Given a jsnum, produces a dom-node representation.
    var numberToDomNode = function(n, params) {
        var node;
        if (baselib.numbers.isExact(n)) {
            if (baselib.numbers.isInteger(n)) {
                node = document.createElement("span");
                node.appendChild(document.createTextNode(n.toString()));
                return node;
            } else if (baselib.numbers.isRational(n)) {
                return rationalToDomNode(n);
            } else if (baselib.numbers.isComplex(n)) {
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


    var coerseToParams = function(params) {
        if (params === 'write') {
            params = new ToDomNodeParameters({'mode' : 'write'});
        } else if (params === 'print') {
            params = new ToDomNodeParameters({'mode' : 'print'});
        } else if (params === 'display') {
            params = new ToDomNodeParameters({'mode' : 'display'});
        } else if (params === 'constructor') {
            params = new ToDomNodeParameters({'mode' : 'constructor'});
        } else {
            params = params || new ToDomNodeParameters({'mode' : 'display'});
        } 
        return params;
    };


    // toDomNode: scheme-value -> dom-node
    var toDomNode = function(x, params) {
        var node;
        params = coerseToParams(params);

        if (baselib.numbers.isSchemeNumber(x)) {
            node = numberToDomNode(x, params);
            $(node).addClass("number");
            return node;
        }

        if (typeof(x) === 'string') {
            var wrapper = document.createElement("span");
            wrapper.style["white-space"] = "pre";
            if (params.getMode() === 'write' || params.getMode() === 'print') {
                node = document.createTextNode(toWrittenString(x));
            } else {
                node = document.createTextNode(toDisplayedString(x));
            }
            wrapper.appendChild(node);
            $(wrapper).addClass("string");
            return wrapper;
        }

        if (x === true || x === false) {
            node = document.createElement("span");
            node.appendChild(document.createTextNode(x ? "true" : "false"));
            $(node).addClass("boolean");
            return node;
        }

        if (x === null) {
            node = document.createElement("span");
            node.appendChild(document.createTextNode("#<null>"));
            $(node).addClass("null");
            return node;
        }

        if (x === undefined) {
            node = document.createElement("span");
            node.appendChild(document.createTextNode("#<undefined>"));
            $(node).addClass("undefined");
            return node;
        }

        if (baselib.functions.isProcedure(x)) {
            node = document.createElement("span");
            node.appendChild(document.createTextNode('#<procedure: ' + x.displayName + '>'));
            $(node).addClass("procedure");
            return node;
        }

        if (typeof(x) !== 'object') {
            node = document.createElement("span");
            node.appendChild(document.createTextNode(x.toString()));
            return node;
        }

        // Otherwise, we know the value is an object.
        if (params.containsKey(x)) {
            node = document.createElement("span");
            node.appendChild(document.createTextNode("#" + params.get(x)));
            return node;
        }
        var returnVal;
        if (x.nodeType) {
            returnVal = x;
        } else if (x.toDomNode) {
            returnVal = x.toDomNode(params);
        } else if (params.getMode() === 'write' && x.toWrittenString) {
            node = document.createElement("span");
            node.appendChild(document.createTextNode(x.toWrittenString(params)));
            returnVal = node;
        } else if (params.getMode() === 'display' && x.toDisplayedString) {
            node = document.createElement("span");
            node.appendChild(document.createTextNode(x.toDisplayedString(params)));
            returnVal = node;
        } else {
            node = document.createElement("span");
            node.appendChild(document.createTextNode(x.toString()));
            returnVal = node;
        }
        return returnVal;
    };



    //////////////////////////////////////////////////////////////////////


    exports.ToDomNodeParameters = ToDomNodeParameters;

    exports.format = format;
    exports.toWrittenString = toWrittenString;
    exports.toDisplayedString = toDisplayedString;
    exports.toDomNode = toDomNode;

    exports.escapeString = escapeString;
}(this.plt.baselib, $));