/**
 *
 *  Originally grabbed from:
 *  Base64 encode / decode
 *  http://www.webtoolkit.info/
 *
 *  dyoo: modified to work with arrays of bytes rather than assume
 *  the bytes are strings.
 **/
var Base64 = (function() {
    'use strict';

    // private property
    var _keyStr = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/=";

    var Base64 = {        
        // public method for encoding
        encode : function (inputBytes) {
	    var output = [];
	    var chr1, chr2, chr3, enc1, enc2, enc3, enc4;
	    var i = 0;
	    while (i < inputBytes.length) {
                
	        chr1 = inputBytes[i++];
	        chr2 = inputBytes[i++];
	        chr3 = inputBytes[i++];
                
	        enc1 = chr1 >> 2;
	        enc2 = ((chr1 & 3) << 4) | (chr2 >> 4);
	        enc3 = ((chr2 & 15) << 2) | (chr3 >> 6);
	        enc4 = chr3 & 63;
                
	        if (isNaN(chr2)) {
		    enc3 = enc4 = 64;
	        } else if (isNaN(chr3)) {
		    enc4 = 64;
	        }
	        output.push(_keyStr.charAt(enc1));
                output.push(_keyStr.charAt(enc2));
		output.push(_keyStr.charAt(enc3));
                output.push(_keyStr.charAt(enc4));
	    }
	    return output.join('');
        },
        
        // public method for decoding
        decode : function (inputString) {
	    var outputBytes = [];
	    var chr1, chr2, chr3;
	    var enc1, enc2, enc3, enc4;
	    var i = 0;
            
	    inputString = inputString.replace(/[^A-Za-z0-9\+\/\=]/g, "");
            
	    while (i < inputString.length) {
	        enc1 = _keyStr.indexOf(inputString.charAt(i++));
	        enc2 = _keyStr.indexOf(inputString.charAt(i++));
	        enc3 = _keyStr.indexOf(inputString.charAt(i++));
	        enc4 = _keyStr.indexOf(inputString.charAt(i++));
                
	        chr1 = (enc1 << 2) | (enc2 >> 4);
	        chr2 = ((enc2 & 15) << 4) | (enc3 >> 2);
	        chr3 = ((enc3 & 3) << 6) | enc4;
                
	        outputBytes.push(chr1);
                
	        if (enc3 !== 64) {
		    outputBytes.push(chr2);
	        }
	        if (enc4 !== 64) {
		    outputBytes.push(chr3);
	        }
	    }
	    return outputBytes;            
        }
    };
    return Base64;
}());