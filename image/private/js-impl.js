
var makePrimitiveProcedure = plt.baselib.functions.makePrimitiveProcedure;
var makeClosure = plt.baselib.functions.makeClosure;
var finalizeClosureCall = plt.baselib.functions.finalizeClosureCall;
var PAUSE = plt.runtime.PAUSE;


var isString = plt.baselib.strings.isString;
var isSymbol = plt.baselib.symbols.isSymbol;


var isFontFamily = function(x){
    return ((isString(x) || isSymbol(x)) &&
	    (x.toString().toLowerCase() == "default" ||
	     x.toString().toLowerCase() == "decorative" ||
	     x.toString().toLowerCase() == "roman" ||
	     x.toString().toLowerCase() == "script" ||
	     x.toString().toLowerCase() == "swiss" ||
	     x.toString().toLowerCase() == "modern" ||
	     x.toString().toLowerCase() == "symbol" ||
	     x.toString().toLowerCase() == "system"))
	|| (x === false);		// false is also acceptable
};
var isFontStyle = function(x){
    return ((isString(x) || isSymbol(x)) &&
	    (x.toString().toLowerCase() == "normal" ||
	     x.toString().toLowerCase() == "italic" ||
	     x.toString().toLowerCase() == "slant"))
	|| (x === false);		// false is also acceptable
};
var isFontWeight = function(x){
    return ((isString(x) || isSymbol(x)) &&
	    (x.toString().toLowerCase() == "normal" ||
	     x.toString().toLowerCase() == "bold" ||
	     x.toString().toLowerCase() == "light"))
	|| (x === false);		// false is also acceptable
};
var isMode = function(x) {
    return ((isString(x) || isSymbol(x)) &&
	    (x.toString().toLowerCase() == "solid" ||
	     x.toString().toLowerCase() == "outline"));
};

var isPlaceX = function(x) {
    return ((isString(x) || isSymbol(x)) &&
	    (x.toString().toLowerCase() == "left"  ||
	     x.toString().toLowerCase() == "right" ||
	     x.toString().toLowerCase() == "center" ||
	     x.toString().toLowerCase() == "middle"));
};

var isPlaceY = function(x) {
    return ((isString(x) || isSymbol(x)) &&
	    (x.toString().toLowerCase() == "top"	  ||
	     x.toString().toLowerCase() == "bottom"   ||
	     x.toString().toLowerCase() == "baseline" ||
	     x.toString().toLowerCase() == "center"   ||
	     x.toString().toLowerCase() == "middle"));
};

var isStyle = function(x) {
    return ((isString(x) || isSymbol(x)) &&
	    (x.toString().toLowerCase() == "solid" ||
	     x.toString().toLowerCase() == "outline"));
};






var checkString = plt.baselib.check.checkString;
var checkByte = plt.baselib.check.checkByte;
var checkReal = plt.baselib.check.checkReal;
var checkBoolean = plt.baselib.check.checkBoolean;

var _checkColor = plt.baselib.check.makeCheckArgumentType(
    isColorOrColorString,
    'color');

var checkColor = function(MACHINE, functionName, position) {
    var aColor = _checkColor(MACHINE, functionName, position);
    if (colorDb.get(aColor)) {
	aColor = colorDb.get(aColor);
    }
    return aColor;
};

var checkImage = plt.baselib.check.makeCheckArgumentType(
    isImage,
    'image');


var checkFontFamily = plt.baselib.check.makeCheckArgumentType(
    isFontFamily,
    'font family');

var checkFontStyle = plt.baselib.check.makeCheckArgumentType(
    isFontStyle,
    'font style');

var checkFontWeight = plt.baselib.check.makeCheckArgumentType(
    isFontWeight,
    'font weight');

var checkPlaceX = plt.baselib.check.makeCheckArgumentType(
    isPlaceX,
    'x-place');

var checkPlaceY = plt.baselib.check.makeCheckArgumentType(
    isPlaceX,
    'y-place');










//////////////////////////////////////////////////////////////////////


EXPORTS['image-color?'] =
    makePrimitiveProcedure(
        'image-color?',
        1,
        function(MACHINE) {
            var elt = MACHINE.env[MACHINE.env.length - 1];
            return (isColorOrColorString(elt));
        });



EXPORTS['text'] =
    makePrimitiveProcedure(
        'text',
        3,
        function(MACHINE) {
	    var aString = checkString(MACHINE,'text', 0);
	    var aSize = checkByte(MACHINE, 'text', 1); 
	    var aColor = checkColor(MACHINE, 'text', 2);
	    return makeTextImage(aString.toString(), 
                                 jsnums.toFixnum(aSize),
                                 aColor,
				 "normal",
                                 "Optimer",
                                 "",
                                 "",
                                 false);
        });


EXPORTS['text/font'] = 
    makePrimitiveProcedure(
        'text/font',
        8,
        function(MACHINE) {
            var aString = checkString(MACHINE, "text/font", 0);
	    var aSize = checkByte(MACHINE, "text/font", 1);
	    var aColor = checkColor(MACHINE, "text/font", 2);
	    var aFace = checkStringOrFalse(MACHINE, "text/font", 3);
	    var aFamily = checkFontFamily(MACHINE, "text/font", 4);
	    var aStyle = checkFontStyle(MACHINE, "text/font", 5);
	    var aWeight = checkFontWeight(MACHINE, "text/font", 6);
	    var aUnderline = checkBoolean(MACHINE, "text/font", 7);
	    return makeTextImage(aString.toString(),
                                 jsnums.toFixnum(aSize),
                                 aColor,
				 aFace.toString(),
                                 aFamily.toString(),
                                 aStyle.toString(),
				 aWeight.toString(),
                                 aUnderline);
        });


EXPORTS['image-url'] = 
    makeClosure(
        'image-url',
        1,
        function(MACHINE) {
            var url = checkString(MACHINE, 'image-url', 0);
            PAUSE(
                function(restart) {
                    var rawImage = new Image();
                    rawImage.onload = function() {
                        restart(function(MACHINE) {
                                        finalizeClosureCall(
                                            MACHINE, 
                                            makeFileImage(url.toString(),
                                                          rawImage));
                        });
                    };
                    rawImage.onerror = function(e) {
                        restart(function(MACHINE) {
                            plt.baselib.exceptions.raise(
                                MACHINE, 
                                new Error(plt.baselib.format.format(
                                    "unable to load ~a: ~a",
                                    url,
                                    e.message)));
                        });
                    }
                    rawImage.src = url.toString();
                }
            );
        });


EXPORTS['open-image-url'] = 
    plt.baselib.functions.renameProcedure(EXPORTS['image-url'],
                                          'open-image-url');


EXPORTS['overlay'] = 
    makePrimitiveProcedure(
        'overlay',
        plt.baselib.arity.makeArityAtLeast(2),
        function(MACHINE) {
	    var img1 = checkImage(MACHINE, "overlay", 0);
	    var img2 = checkImage(MACHINE, "overlay" 1);
            var restImages = [];
	    for (var i = 2; i < MACHINE.argcount; i++) {
		restImages.push(checkImage(MACHINE, "overlay", i));
	    }
            
	    var img = makeOverlayImage(img1, img2, "middle", "middle");
	    for (var i = 0; i < restImages.length; i++) {
		img = makeOverlayImage(img, restImages[i], "middle", "middle");
	    }
	    return img;
        });



EXPORTS['overlay/xy'] = 
    makePrimitiveProcedure(
        'overlay/xy',
        4,
        function(MACHINE) {
	    var img1 = checkImage(MACHINE, "overlay/xy", 0);
	    var deltaX = checkReal(MACHINE, "overlay/xy", 1);
	    var deltaY = checkReal(MACHINE, "overlay/xy", 2);
	    var img2 = checkImage(MACHINE, "overlay/xy", 3);
	    return makeOverlayImage(img1.updatePinhole(0, 0),
				    img2.updatePinhole(0, 0),
				    jsnums.toFixnum(deltaX),
				    jsnums.toFixnum(deltaY));
        });



 EXPORTS['overlay/align'] = 
     makePrimitiveProcedure(
         'overlay/align',
         plt.baselib.arity.makeArityAtLeast(4),
         function(MACHINE) {
	     var placeX = checkPlaceX(MACHINE, "overlay/align", 0);
	     var placeY = checkPlaceY(MACHINE, "overlay/align", 1);
	     var img1 = checkImage(MACHINE, "overlay/align", 2);
	     var img2 = checkImage(MACHINE, "overlay/align", 3);
             var restImages = [];
	     for (var i = 4; i < MACHINE.argcount; i++) {
                 restImages.push(checkImage(MACHINE, "overlay/align", i));
             }
	     var img = makeOverlayImage(img1,
					img2,
					placeX.toString(),
					placeY.toString());
	     for (var i = 0; i < restImages.length; i++)
		 img = makeOverlayImage(img,
					restImages[i], 
					placeX.toString(), 
					placeY.toString());
	     return img;
         });





EXPORTS['underlay'] = 
    makePrimitiveProcedure(
        'underlay',
        plt.baselib.arity.makeArityAtLeast(2),
        function(MACHINE) {
	    var img1 = checkImage(MACHINE, "underlay", 0);
	    var img2 = checkImage(MACHINE, "underlay", 1);
	    var restImages = [];
	    for (var i = 2; i < MACHINE.argcount; i++) {
		restImages.push(checkImage(MACHINE, "underlay", i));
	    }

	    var img = makeOverlayImage(img2, img1, 0, 0);
	    for (var i = 0; i < restImages.length; i++) {
		img = makeOverlayImage(restImages[i], img, 0, 0);
	    }
	    return img;
        });


EXPORTS['underlay/xy'] = 
    makePrimitiveProcedure(
        'underlay/xy',
        4,
        function(MACHINE) {
	    var img1 = checkImage(MACHINE, "underlay/xy", 0);
	    var deltaX = checkReal(MACHINE, "underlay/xy", 1);
	    var deltaY = checkReal(MACHINE, "underlay/xy", 2);
	    var img2 = checkImage(MACHINE, "underlay/xy", 3);
	    return makeOverlayImage(img2.updatePinhole(0, 0), 
				    img1.updatePinhole(0, 0),
				    -(jsnums.toFixnum(deltaX)),
				    -(jsnums.toFixnum(deltaY)));
        });

EXPORTS['underlay/align'] = 
    makePrimitiveProcedure(
        'underlay/align',
        plt.baselib.arity.makeArityAtLeast(4),
        function(MACHINE) {
	    var placeX = checkPlaceX(MACHINE, "underlay/align", 0);
	    var placeY = checkPlaceY(MACHINE, "underlay/align", 1);
	    var img1 = checkImage(MACHINE, "underlay/align", 2);
	    var img2 = checkImage(MACHINE, "underlay/align", 3);
            var restImages = [];
            for (var i = 4; i < MACHINE.argcount; i++) {
                restImages.push(checkImage(MACHINE, "underlay/align", i));
            }
	    
	    var img = makeOverlayImage(img2,
				       img1,
				       placeX.toString(),
				       placeY.toString());
	    
	    for (var i = 0; i < restImages.length; i++)
		img = makeOverlayImage(restImages[i], 
				       img,
				       placeX.toString(), 
				       placeY.toString());
	    return img;
        });



// EXPORTS['beside'] = 
//     makePrimitiveProcedure(
//         'beside',
//             ???,
//         function(MACHINE) {
//             ...
//         });

// EXPORTS['beside/align'] = 
//     makePrimitiveProcedure(
//         'beside/align',
//             ???,
//         function(MACHINE) {
//             ...
//         });

// EXPORTS['above'] = 
//     makePrimitiveProcedure(
//         'above',
//             ???,
//         function(MACHINE) {
//             ...
//         });

// EXPORTS['above/align'] = 
//     makePrimitiveProcedure(
//         'above/align',
//             ???,
//         function(MACHINE) {
//             ...
//         });

// EXPORTS['place-image/align'] = 
//     makePrimitiveProcedure(
//         'place-image/align',
//             ???,
//         function(MACHINE) {
//             ...
//         });

EXPORTS['rotate'] = 
    makePrimitiveProcedure(
        'rotate',
        2,
        function(MACHINE) {
	    var angle = checkReal(MACHINE, "rotate",  0);
	    var img = checkImage(MACHINE, "rotate", 1);
	    return makeRotateImage(jsnums.toFixnum(angle), img);
        });

EXPORTS['scale'] = 
    makePrimitiveProcedure(
        'scale',
        2,
        function(MACHINE) {
	    var factor = checkReal(MACHINE, "scale", 0);
	    var img = checkImage(MACHINE, "scale", 1);
	    return makeScaleImage(jsnums.toFixnum(factor),
				  jsnums.toFixnum(factor),
				  img);
        });

EXPORTS['scale/xy'] = 
    makePrimitiveProcedure(
        'scale/xy',
        3,
        function(MACHINE) {
	    var xFactor = checkReal(MACHINE, "scale/xy", 0);
	    var yFactor = checkReal(MACHINE, "scale/xy", 1);
	    var img = checkImage(MACHINE, "scale/xy", 2);
	    return makeScaleImage(jsnums.toFixnum(xFactor), 
					   jsnums.toFixnum(yFactor),
					   img);
	    
        });

// EXPORTS['flip-horizontal'] = 
//     makePrimitiveProcedure(
//         'flip-horizontal',
//             ???,
//         function(MACHINE) {
//             ...
//         });

// EXPORTS['flip-vertical'] = 
//     makePrimitiveProcedure(
//         'flip-vertical',
//             ???,
//         function(MACHINE) {
//             ...
//         });

// EXPORTS['frame'] = 
//     makePrimitiveProcedure(
//         'frame',
//             ???,
//         function(MACHINE) {
//             ...
//         });

// EXPORTS['crop'] = 
//     makePrimitiveProcedure(
//         'crop',
//             ???,
//         function(MACHINE) {
//             ...
//         });

EXPORTS['line'] = 
    makePrimitiveProcedure(
        'line',
        3,
        function(MACHINE) {
	    var x = checkReal(MACHINE, "line", 0);
	    var y = checkReal(MACHINE,  "line", 1);
	    var c = checkColor(MACHINE, "line", 2);
	    if (colorDb.get(c)) {
		c = colorDb.get(c);
	    }
	    var line = makeLineImage(jsnums.toFixnum(x),
				     jsnums.toFixnum(y),
				     c);
	    return line;
        });




// EXPORTS['add-line'] = 
//     makePrimitiveProcedure(
//         'add-line',
//             ???,
//         function(MACHINE) {
//             ...
//         });

// EXPORTS['scene+line'] = 
//     makePrimitiveProcedure(
//         'scene+line',
//             ???,
//         function(MACHINE) {
//             ...
//         });

// EXPORTS['circle'] = 
//     makePrimitiveProcedure(
//         'circle',
//             ???,
//         function(MACHINE) {
//             ...
//         });

// EXPORTS['square'] = 
//     makePrimitiveProcedure(
//         'square',
//             ???,
//         function(MACHINE) {
//             ...
//         });

// EXPORTS['rectangle'] = 
//     makePrimitiveProcedure(
//         'rectangle',
//             ???,
//         function(MACHINE) {
//             ...
//         });

// EXPORTS['regular-polygon'] = 
//     makePrimitiveProcedure(
//         'regular-polygon',
//             ???,
//         function(MACHINE) {
//             ...
//         });

// EXPORTS['ellipse'] = 
//     makePrimitiveProcedure(
//         'ellipse',
//             ???,
//         function(MACHINE) {
//             ...
//         });

// EXPORTS['triangle'] = 
//     makePrimitiveProcedure(
//         'triangle',
//             ???,
//         function(MACHINE) {
//             ...
//         });

// EXPORTS['right-triangle'] = 
//     makePrimitiveProcedure(
//         'right-triangle',
//             ???,
//         function(MACHINE) {
//             ...
//         });

// EXPORTS['isosceles-triangle'] = 
//     makePrimitiveProcedure(
//         'isosceles-triangle',
//             ???,
//         function(MACHINE) {
//             ...
//         });

// EXPORTS['star'] = 
//     makePrimitiveProcedure(
//         'star',
//             ???,
//         function(MACHINE) {
//             ...
//         });

// EXPORTS['radial-star'] = 
//     makePrimitiveProcedure(
//         'radial-star',
//             ???,
//         function(MACHINE) {
//             ...
//         });

// EXPORTS['star-polygon'] = 
//     makePrimitiveProcedure(
//         'star-polygon',
//             ???,
//         function(MACHINE) {
//             ...
//         });

// EXPORTS['rhombus'] = 
//     makePrimitiveProcedure(
//         'rhombus',
//             ???,
//         function(MACHINE) {
//             ...
//         });

// EXPORTS['image->color-list'] = 
//     makePrimitiveProcedure(
//         'image->color-list',
//             ???,
//         function(MACHINE) {
//             ...
//         });

// EXPORTS['color-list->image'] = 
//     makePrimitiveProcedure(
//         'color-list->image',
//             ???,
//         function(MACHINE) {
//             ...
//         });

// EXPORTS['image-width'] = 
//     makePrimitiveProcedure(
//         'image-width',
//             ???,
//         function(MACHINE) {
//             ...
//         });

// EXPORTS['image-height'] = 
//     makePrimitiveProcedure(
//         'image-height',
//             ???,
//         function(MACHINE) {
//             ...
//         });

// EXPORTS['image-baseline'] = 
//     makePrimitiveProcedure(
//         'image-baseline',
//             ???,
//         function(MACHINE) {
//             ...
//         });

// EXPORTS['image-color?'] = 
//     makePrimitiveProcedure(
//         'image-color?',
//             ???,
//         function(MACHINE) {
//             ...
//         });

// EXPORTS['mode?'] = 
//     makePrimitiveProcedure(
//         'mode?',
//             ???,
//         function(MACHINE) {
//             ...
//         });

// EXPORTS['x-place?'] = 
//     makePrimitiveProcedure(
//         'x-place?',
//             ???,
//         function(MACHINE) {
//             ...
//         });

// EXPORTS['y-place?'] = 
//     makePrimitiveProcedure(
//         'y-place?',
//             ???,
//         function(MACHINE) {
//             ...
//         });

// EXPORTS['angle?'] = 
//     makePrimitiveProcedure(
//         'angle?',
//             ???,
//         function(MACHINE) {
//             ...
//         });

// EXPORTS['side-count?'] = 
//     makePrimitiveProcedure(
//         'side-count?',
//             ???,
//         function(MACHINE) {
//             ...
//         });

// EXPORTS['image-url'] = 
//     makePrimitiveProcedure(
//         'image-url',
//             ???,
//         function(MACHINE) {
//             ...
//         });
// EXPORTS['open-image-url'] = 
//     makePrimitiveProcedure(
//         'open-image-url',
//             ???,
//         function(MACHINE) {
//             ...
//         });
// EXPORTS['color-list->image'] = 
//     makePrimitiveProcedure(
//         'color-list->image',
//             ???,
//         function(MACHINE) {
//             ...
//         });


// EXPORTS['step-count?'] = 
//     makePrimitiveProcedure(
//         'step-count?',
//             ???,
//         function(MACHINE) {
//             ...
//         });
