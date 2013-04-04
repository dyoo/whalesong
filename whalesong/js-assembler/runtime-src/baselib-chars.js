// Single characters
(function(baselib, $) {
    var exports = {};
    baselib.chars = exports;


    // Chars
    // Char: string -> Char
    var Char = function(val){
        this.val = val;
    };
    // The characters less than 256 must be eq?, according to the
    // documentation:
    // http://docs.racket-lang.org/reference/characters.html
    var _CharCache = {};
    for (var i = 0; i < 256; i++) {
        _CharCache[String.fromCharCode(i)] = new Char(String.fromCharCode(i));
    }
    
    // makeInstance: 1-character string -> Char  
    Char.makeInstance = function(val){
        if (_CharCache[val]) {
	    return _CharCache[val];
        }
        return new Char(val);
    };

    Char.prototype.toString = function(cache) {
	var code = this.val.charCodeAt(0);
	var returnVal;
	switch (code) {
	case 0: returnVal = '#\\nul'; break;
	case 8: returnVal = '#\\backspace'; break;
	case 9: returnVal = '#\\tab'; break;
	case 10: returnVal = '#\\newline'; break;
	case 11: returnVal = '#\\vtab'; break;
	case 12: returnVal = '#\\page'; break;
	case 13: returnVal = '#\\return'; break;
	case 20: returnVal = '#\\space'; break;
	case 127: returnVal = '#\\rubout'; break;
	default: if (code >= 32 && code <= 126) {
	    returnVal = ("#\\" + this.val);
	}
	    else {
		var numStr = code.toString(16).toUpperCase();
		while (numStr.length < 4) {
		    numStr = '0' + numStr;
		}
		returnVal = ('#\\u' + numStr);
	    }
	    break;
	}
	return returnVal;
    };

    Char.prototype.toWrittenString = Char.prototype.toString;

    Char.prototype.toDisplayedString = function (cache) {
        return this.val;
    };

    Char.prototype.toDomNode = function(params) {
        return $('<span/>')
            .text(this.toString())
            .addClass('wescheme-character')
            .get(0);
    };

    Char.prototype.getValue = function() {
        return this.val;
    };

    Char.prototype.equals = function(other, aUnionFind){
        return other instanceof Char && this.val == other.val;
    };

    Char.prototype.hashCode = function(depth) {
        var k = baselib.hashes.getEqualHashCode('Char');
        k += this.val.charCodeAt(0);
        k = baselib.hashes.hashMix(k);
        return k;
    };


    exports.Char = Char;
    exports.makeChar = Char.makeInstance;
    exports.isChar = plt.baselib.makeClassPredicate(Char);


})(this['plt'].baselib, jQuery);
