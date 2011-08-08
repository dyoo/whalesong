/*jslint unparam: true, vars: true, white: true, plusplus: true, maxerr: 50, indent: 4 */



(function(baselib) {
    'use strict';
    var exports = {};
    baselib.bytes = exports;

    // Bytes

    var Bytes = function(bts, mutable) {
        // bytes: arrayof [0-255]
        this.bytes = bts;
        this.mutable = (mutable === undefined) ? false : mutable;
    };

    Bytes.prototype.get = function(i) {
	return this.bytes[i];
    };

    Bytes.prototype.set = function(i, b) {
	if (this.mutable) {
	    this.bytes[i] = b;
	}
    };

    Bytes.prototype.length = function() {
	return this.bytes.length;
    };

    Bytes.prototype.copy = function(mutable) {
	return new Bytes(this.bytes.slice(0), mutable);
    };

    Bytes.prototype.subbytes = function(start, end) {
	if (end === null || end === undefined) {
	    end = this.bytes.length;
	}
	
	return new Bytes( this.bytes.slice(start, end), true );
    };


    Bytes.prototype.equals = function(other) {
        if (! (other instanceof Bytes)) {
	    return false;
        }
        if (this.bytes.length !== other.bytes.length) {
	    return false;
        }
        var A = this.bytes;
        var B = other.bytes;
        var n = this.bytes.length;
        var i;
        for (i = 0; i < n; i++) {
	    if (A[i] !== B[i]) {
	        return false;
            }
        }
        return true;
    };


    Bytes.prototype.toString = function(cache) {
	var ret = '', i;
	for (i = 0; i < this.bytes.length; i++) {
	    ret += String.fromCharCode(this.bytes[i]);
	}

	return ret;
    };

    Bytes.prototype.toDisplayedString = Bytes.prototype.toString;

    var escapeByte = function(aByte) {
	var ret = [];
	var returnVal;
	switch(aByte) {
	case 7: returnVal = '\\a'; break;
	case 8: returnVal = '\\b'; break;
	case 9: returnVal = '\\t'; break;
	case 10: returnVal = '\\n'; break;
	case 11: returnVal = '\\v'; break;
	case 12: returnVal = '\\f'; break;
	case 13: returnVal = '\\r'; break;
	case 34: returnVal = '\\"'; break;
	case 92: returnVal = '\\\\'; break;
	default: if (aByte >= 32 && aByte <= 126) {
	    returnVal = String.fromCharCode(aByte);
	}
	    else {
		ret.push( '\\' + aByte.toString(8) );
	    }
	    break;
	}
	return returnVal;
    };

    Bytes.prototype.toWrittenString = function() {
	var ret = ['#"'], i;
	for (i = 0; i < this.bytes.length; i++) {
	    ret.push(escapeByte(this.bytes[i]));
	}
	ret.push('"');
	return ret.join('');
    };




    exports.Bytes = Bytes;

}(this.plt.baselib));