/*jslint browser: false, unparam: true, vars: true, white: true, nomen: true, plusplus: true, maxerr: 50, indent: 4 */


// Strings

// Strings are either mutable or immutable.  immutable strings are represented
// as regular JavaScript strings.  Mutable ones are represented as instances
// of the Str class.

(function (baselib) {
    'use strict';
    var exports = {};

    baselib.strings = exports;


    // chars: arrayof string
    // Precondition: each string must only be 1 character long or bad things
    // happen.
    var Str = function (chars) {
	this.chars = chars;
	this.length = chars.length;
	this.mutable = true;
    };

    Str.makeInstance = function (chars) {
	return new Str(chars);
    };

    Str.fromString = function (s) {
	return Str.makeInstance(s.split(""));
    };

    Str.prototype.toString = function () {
	return this.chars.join("");
    };

    var replaceUnprintableStringChars = function (s) {
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
	    default: 
                if (val >= 32 && val <= 126) {
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

    var escapeString = function (s) {
        return '"' + replaceUnprintableStringChars(s) + '"';
    };

    Str.prototype.toWrittenString = function (cache) {
        return escapeString(this.toString());
    };

    Str.prototype.toDisplayedString = Str.prototype.toString;

    Str.prototype.toDomNode = function(params) {
        return $("<span/>")
            .text(escapeString(this.toString()))
            .addClass('wescheme-string')
            .get(0);
    };


    Str.prototype.copy = function () {
	return Str.makeInstance(this.chars.slice(0));
    };

    Str.prototype.substring = function (start, end) {
	if (end === null || end === void(0)) {
	    end = this.length;
	}
	return Str.makeInstance( this.chars.slice(start, end) );
    };

    Str.prototype.charAt = function (index) {
	return this.chars[index];
    };

    Str.prototype.charCodeAt = function (index) {
	return this.chars[index].charCodeAt(0);
    };

    Str.prototype.replace = function (expr, newStr) {
	return Str.fromString(this.toString().replace(expr, newStr) );
    };


    Str.prototype.equals = function (other, aUnionFind) {
	if ( !(other instanceof Str || typeof(other) === 'string') ) {
	    return false;
	}
	return this.toString() === other.toString();
    };

    Str.prototype.hashCode = function(depth) {
        return baselib.hashes.getEqualHashCode(this.toString());
    };


    Str.prototype.set = function (i, c) {
	this.chars[i] = c;
    };

    Str.prototype.toUpperCase = function () {
	return Str.fromString(this.chars.join("").toUpperCase() );
    };

    Str.prototype.toLowerCase = function () {
	return Str.fromString(this.chars.join("").toLowerCase() );
    };

    Str.prototype.match = function (regexpr) {
	return this.toString().match(regexpr);
    };



    var isString = function (s) {
	return (typeof s === 'string' || 
                s instanceof Str);
    };

    var isMutableString = baselib.makeClassPredicate(Str);



    exports.Str = Str;
    exports.escapeString = escapeString;
    exports.isString = isString;
    exports.isMutableString = isMutableString;
    exports.makeMutableString = Str.makeInstance;

}(this.plt.baselib));
