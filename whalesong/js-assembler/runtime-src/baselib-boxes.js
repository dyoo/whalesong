/*jslint browser: true, unparam: true, vars: true, white: true, plusplus: true, maxerr: 50, indent: 4 */


// Exceptions

(function(baselib, $) {
    'use strict';
    var exports = {};
    baselib.boxes = exports;


    //////////////////////////////////////////////////////////////////////
    // Boxes
    
    var Box = function(x, mutable) {
	this.val = x;
	this.mutable = mutable;
    };

    Box.prototype.ref = function() {
        return this.val;
    };

    Box.prototype.set = function(newVal) {
        if (this.mutable) {
	    this.val = newVal;
        }
    };

    Box.prototype.toString = function(cache) {
        cache.put(this, true);
        return "#&" + baselib.format.toWrittenString(this.val, cache);
    };

    Box.prototype.toWrittenString = function(cache) {
        cache.put(this, true);
        return "#&" + baselib.format.toWrittenString(this.val, cache);
    };

    Box.prototype.toDisplayedString = function(cache) {
        cache.put(this, true);
        return "#&" + baselib.format.toDisplayedString(this.val, cache);
    };

    Box.prototype.toDomNode = function(params) {
        var node = $('<span/>');
        if (params.getMode() === 'constructor') {
            node.append($('<span/>').text('(').addClass('lParen'));
            node.append($('<span/>').text('box'));
            node.append(" ");
            node.append(params.recur(this.val));
            node.append($('<span/>').text(')').addClass('rParen'));
        } else {
            node.append($('<span/>').text('#&'));
            node.append(params.recur(this.val));
        }
        return node.get(0);
    };

    Box.prototype.equals = function(other, aUnionFind) {
        return ((other instanceof Box) &&
	        baselib.equality.equals(this.val, other.val, aUnionFind));
    };

    Box.prototype.hashCode = function(depth) {
        var k = baselib.hashes.getEqualHashCode("Box");
        k = baselib.hashes.hashMix(k);
        k += baselib.hashes.getEqualHashCode(this.val, depth);
        k = baselib.hashes.hashMix(k);
        return k;
    };

    
    var makeBox = function(x) { 
        return new Box(x, true); 
    };

    var makeImmutableBox = function(x) {
        return new Box(x, false); 
    };

    var isBox = function(x) {
        return x instanceof Box;
    };

    var isMutableBox = function(x) { 
        return (x instanceof Box && x.mutable); 
    };

    var isImmutableBox = function(x) { 
        return (x instanceof Box && (!x.mutable));
    };




    //////////////////////////////////////////////////////////////////////
    exports.Box = Box;
    exports.isBox = isBox;
    exports.isMutableBox = isMutableBox;
    exports.isImmutableBox = isImmutableBox;
    exports.makeBox = makeBox;
    exports.makeImmutableBox = makeImmutableBox;


}(this.plt.baselib, jQuery));
