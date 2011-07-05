// Strings

// Strings are either mutable or immutable.  immutable strings are represented
// as regular JavaScript strings.  Mutable ones are represented as instances
// of the Str class.

(function(baselib) {
    var exports = {};

    baselib.strings = exports;



    var isString = function(s) {
	return (typeof s === 'string' || 
                s instanceof Str);
    };





    // Now using mutable strings
    var Str = function(chars) {
	this.chars = chars;
	this.length = chars.length;
	this.mutable = true;
    }

    Str.makeInstance = function(chars) {
	return new Str(chars);
    }

    Str.fromString = function(s) {
	return Str.makeInstance(s.split(""));
    }

    Str.prototype.toString = function() {
	return this.chars.join("");
    }

    Str.prototype.toWrittenString = function(cache) {
        return escapeString(this.toString());
    }

    Str.prototype.toDisplayedString = Str.prototype.toString;

    Str.prototype.copy = function() {
	return Str.makeInstance(this.chars.slice(0));
    }

    Str.prototype.substring = function(start, end) {
	if (end == null || end == undefined) {
	    end = this.length;
	}
	
	return Str.makeInstance( this.chars.slice(start, end) );
    }

    Str.prototype.charAt = function(index) {
	return this.chars[index];
    }

    Str.prototype.charCodeAt = function(index) {
	return this.chars[index].charCodeAt(0);
    }

    Str.prototype.replace = function(expr, newStr) {
	return Str.fromString( this.toString().replace(expr, newStr) );
    }


    Str.prototype.equals = function(other, aUnionFind) {
	if ( !(other instanceof Str || typeof(other) == 'string') ) {
	    return false;
	}
	return this.toString() === other.toString();
    }


    Str.prototype.set = function(i, c) {
	this.chars[i] = c;
    }

    Str.prototype.toUpperCase = function() {
	return Str.fromString( this.chars.join("").toUpperCase() );
    }

    Str.prototype.toLowerCase = function() {
	return Str.fromString( this.chars.join("").toLowerCase() );
    }

    Str.prototype.match = function(regexpr) {
	return this.toString().match(regexpr);
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


    /*
// Strings
// For the moment, we just reuse Javascript strings.
String = String;
String.makeInstance = function(s) {
    return s.valueOf();
};
    
    
// WARNING
// WARNING: we are extending the built-in Javascript string class here!
// WARNING
String.prototype.equals = function(other, aUnionFind){
    return this == other;
};
    
var _quoteReplacingRegexp = new RegExp("[\"\\\\]", "g");
String.prototype.toWrittenString = function(cache) {
    return '"' + this.replace(_quoteReplacingRegexp,
			      function(match, submatch, index) {
				  return "\\" + match;
			      }) + '"';
};

String.prototype.toDisplayedString = function(cache) {
    return this;
};
*/


    //////////////////////////////////////////////////////////////////////



    exports.Str = Str;
    exports.escapeString = escapeString;
    exports.isString = isString;


})(this['plt'].baselib);