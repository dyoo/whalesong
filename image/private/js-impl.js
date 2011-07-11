var checkString = plt.baselib.check.makeCheckArgumentType(
    plt.baselib.strings.isString,
    'string');

var checkByte = plt.baselib.check.makeCheckArgumentType(
    plt.baselib.numbers.isByte,
    'byte');

var checkColor = plt.baselib.check.makeCheckArgumentType(
    isColorOrColorString,
    'color');

var checkImage = plt.baselib.check.makeCheckArgumentType(
    isImage,
    'image');

var checkReal = plt.baselib.check.makeCheckArgumentType(
    plt.baselib.numbers.isReal,
    'real');



//////////////////////////////////////////////////////////////////////


EXPORTS['image-color?'] =
    plt.baselib.functions.makePrimitiveProcedure(
        'image-color?',
        1,
        function(MACHINE) {
            var elt = MACHINE.env[MACHINE.env.length - 1];
            return (isColorOrColorString(elt));
        });




EXPORTS['text'] =
    plt.baselib.functions.makePrimitiveProcedure(
        'text',
        3,
        function(MACHINE) {
	    var aString = checkString(MACHINE,'text', 0);
	    var aSize = checkByte(MACHINE, 'text', 1); 
	    var aColor = checkColor(MACHINE, 'text', 2);
	    if (colorDb.get(aColor)) {
		aColor = colorDb.get(aColor);
	    }
	    return makeTextImage(aString.toString(), 
				 jsnums.toFixnum(aSize),
				 aColor);
        });

// FIXME
// EXPORTS['text/font'] = 
//     plt.baselib.functions.makePrimitiveProcedure(
//         'text/font',
//             ???,
//         function(MACHINE) {
//             ...
//         });

// FIXME
// EXPORTS['image-url'] = 
//     plt.baselib.functions.makePrimitiveProcedure(
//         'image-url',
//             ???,
//         function(MACHINE) {
//             ...
//         });

// FIXME
// EXPORTS['open-image-url'] = 
//     plt.baselib.functions.makePrimitiveProcedure(
//         'open-image-url',
//             ???,
//         function(MACHINE) {
//             ...
//         });

EXPORTS['overlay'] = 
    plt.baselib.functions.makePrimitiveProcedure(
        'overlay',
        plt.baselib.arity.makeArityAtLeast(2),
        function(MACHINE) {

	    var img1 = checkImage(MACHINE, "overlay", 0);
	    var img2 = checkImage(MACHINE, "overlay", 1);
	    var restImages;
	    for (var i = 2; i < MACHINE.argcount; i++) {
		restImages.push(checkImage(MACHINE, "overlay", i));
	    }

	    var img = makeOverlayImage(img1, img2, 0, 0);
	    for (var i = 0; i < restImages.length; i++) {
		img = makeOverlayImage(img, restImages[i], 0, 0);
	    }
	    return img;
        });

EXPORTS['overlay/xy'] = 
    plt.baselib.functions.makePrimitiveProcedure(
        'overlay/xy',
        4,
        function(MACHINE) {
	    var img1 = checkImage("overlay/xy", 0);
	    var deltaX = checkReal("overlay/xy", 1);
	    var deltaY = checkReal("overlay/xy", 2);
	    var img2 = checkImage("overlay/xy", 2);
	    return makeOverlayImage(img1.updatePinhole(0, 0),
				img2.updatePinhole(0, 0),
				jsnums.toFixnum(deltaX),
				jsnums.toFixnum(deltaY));
        });

// FIXME
// EXPORTS['overlay/align'] = 
//     plt.baselib.functions.makePrimitiveProcedure(
//         'overlay/align',
//             ???,
//         function(MACHINE) {
//             ...
//         });

EXPORTS['underlay'] = 
    plt.baselib.functions.makePrimitiveProcedure(
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
    plt.baselib.functions.makePrimitiveProcedure(
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

// EXPORTS['underlay/align'] = 
//     plt.baselib.functions.makePrimitiveProcedure(
//         'underlay/align',
//             ???,
//         function(MACHINE) {
//             ...
//         });

// EXPORTS['beside'] = 
//     plt.baselib.functions.makePrimitiveProcedure(
//         'beside',
//             ???,
//         function(MACHINE) {
//             ...
//         });

// EXPORTS['beside/align'] = 
//     plt.baselib.functions.makePrimitiveProcedure(
//         'beside/align',
//             ???,
//         function(MACHINE) {
//             ...
//         });

// EXPORTS['above'] = 
//     plt.baselib.functions.makePrimitiveProcedure(
//         'above',
//             ???,
//         function(MACHINE) {
//             ...
//         });

// EXPORTS['above/align'] = 
//     plt.baselib.functions.makePrimitiveProcedure(
//         'above/align',
//             ???,
//         function(MACHINE) {
//             ...
//         });

// EXPORTS['place-image/align'] = 
//     plt.baselib.functions.makePrimitiveProcedure(
//         'place-image/align',
//             ???,
//         function(MACHINE) {
//             ...
//         });

EXPORTS['rotate'] = 
    plt.baselib.functions.makePrimitiveProcedure(
        'rotate',
        2,
        function(MACHINE) {
	    var angle = checkReal(MACHINE, "rotate",  0);
	    var img = checkImage(MACHINE, "rotate", 1);
	    return makeRotateImage(jsnums.toFixnum(angle), img);
        });

EXPORTS['scale'] = 
    plt.baselib.functions.makePrimitiveProcedure(
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
    plt.baselib.functions.makePrimitiveProcedure(
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
//     plt.baselib.functions.makePrimitiveProcedure(
//         'flip-horizontal',
//             ???,
//         function(MACHINE) {
//             ...
//         });

// EXPORTS['flip-vertical'] = 
//     plt.baselib.functions.makePrimitiveProcedure(
//         'flip-vertical',
//             ???,
//         function(MACHINE) {
//             ...
//         });

// EXPORTS['frame'] = 
//     plt.baselib.functions.makePrimitiveProcedure(
//         'frame',
//             ???,
//         function(MACHINE) {
//             ...
//         });

// EXPORTS['crop'] = 
//     plt.baselib.functions.makePrimitiveProcedure(
//         'crop',
//             ???,
//         function(MACHINE) {
//             ...
//         });

EXPORTS['line'] = 
    plt.baselib.functions.makePrimitiveProcedure(
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
//     plt.baselib.functions.makePrimitiveProcedure(
//         'add-line',
//             ???,
//         function(MACHINE) {
//             ...
//         });

// EXPORTS['scene+line'] = 
//     plt.baselib.functions.makePrimitiveProcedure(
//         'scene+line',
//             ???,
//         function(MACHINE) {
//             ...
//         });

// EXPORTS['circle'] = 
//     plt.baselib.functions.makePrimitiveProcedure(
//         'circle',
//             ???,
//         function(MACHINE) {
//             ...
//         });

// EXPORTS['square'] = 
//     plt.baselib.functions.makePrimitiveProcedure(
//         'square',
//             ???,
//         function(MACHINE) {
//             ...
//         });

// EXPORTS['rectangle'] = 
//     plt.baselib.functions.makePrimitiveProcedure(
//         'rectangle',
//             ???,
//         function(MACHINE) {
//             ...
//         });

// EXPORTS['regular-polygon'] = 
//     plt.baselib.functions.makePrimitiveProcedure(
//         'regular-polygon',
//             ???,
//         function(MACHINE) {
//             ...
//         });

// EXPORTS['ellipse'] = 
//     plt.baselib.functions.makePrimitiveProcedure(
//         'ellipse',
//             ???,
//         function(MACHINE) {
//             ...
//         });

// EXPORTS['triangle'] = 
//     plt.baselib.functions.makePrimitiveProcedure(
//         'triangle',
//             ???,
//         function(MACHINE) {
//             ...
//         });

// EXPORTS['right-triangle'] = 
//     plt.baselib.functions.makePrimitiveProcedure(
//         'right-triangle',
//             ???,
//         function(MACHINE) {
//             ...
//         });

// EXPORTS['isosceles-triangle'] = 
//     plt.baselib.functions.makePrimitiveProcedure(
//         'isosceles-triangle',
//             ???,
//         function(MACHINE) {
//             ...
//         });

// EXPORTS['star'] = 
//     plt.baselib.functions.makePrimitiveProcedure(
//         'star',
//             ???,
//         function(MACHINE) {
//             ...
//         });

// EXPORTS['radial-star'] = 
//     plt.baselib.functions.makePrimitiveProcedure(
//         'radial-star',
//             ???,
//         function(MACHINE) {
//             ...
//         });

// EXPORTS['star-polygon'] = 
//     plt.baselib.functions.makePrimitiveProcedure(
//         'star-polygon',
//             ???,
//         function(MACHINE) {
//             ...
//         });

// EXPORTS['rhombus'] = 
//     plt.baselib.functions.makePrimitiveProcedure(
//         'rhombus',
//             ???,
//         function(MACHINE) {
//             ...
//         });

// EXPORTS['image->color-list'] = 
//     plt.baselib.functions.makePrimitiveProcedure(
//         'image->color-list',
//             ???,
//         function(MACHINE) {
//             ...
//         });

// EXPORTS['color-list->image'] = 
//     plt.baselib.functions.makePrimitiveProcedure(
//         'color-list->image',
//             ???,
//         function(MACHINE) {
//             ...
//         });

// EXPORTS['image-width'] = 
//     plt.baselib.functions.makePrimitiveProcedure(
//         'image-width',
//             ???,
//         function(MACHINE) {
//             ...
//         });

// EXPORTS['image-height'] = 
//     plt.baselib.functions.makePrimitiveProcedure(
//         'image-height',
//             ???,
//         function(MACHINE) {
//             ...
//         });

// EXPORTS['image-baseline'] = 
//     plt.baselib.functions.makePrimitiveProcedure(
//         'image-baseline',
//             ???,
//         function(MACHINE) {
//             ...
//         });

// EXPORTS['image-color?'] = 
//     plt.baselib.functions.makePrimitiveProcedure(
//         'image-color?',
//             ???,
//         function(MACHINE) {
//             ...
//         });

// EXPORTS['mode?'] = 
//     plt.baselib.functions.makePrimitiveProcedure(
//         'mode?',
//             ???,
//         function(MACHINE) {
//             ...
//         });

// EXPORTS['x-place?'] = 
//     plt.baselib.functions.makePrimitiveProcedure(
//         'x-place?',
//             ???,
//         function(MACHINE) {
//             ...
//         });

// EXPORTS['y-place?'] = 
//     plt.baselib.functions.makePrimitiveProcedure(
//         'y-place?',
//             ???,
//         function(MACHINE) {
//             ...
//         });

// EXPORTS['angle?'] = 
//     plt.baselib.functions.makePrimitiveProcedure(
//         'angle?',
//             ???,
//         function(MACHINE) {
//             ...
//         });

// EXPORTS['side-count?'] = 
//     plt.baselib.functions.makePrimitiveProcedure(
//         'side-count?',
//             ???,
//         function(MACHINE) {
//             ...
//         });

// EXPORTS['image-url'] = 
//     plt.baselib.functions.makePrimitiveProcedure(
//         'image-url',
//             ???,
//         function(MACHINE) {
//             ...
//         });
// EXPORTS['open-image-url'] = 
//     plt.baselib.functions.makePrimitiveProcedure(
//         'open-image-url',
//             ???,
//         function(MACHINE) {
//             ...
//         });
// EXPORTS['color-list->image'] = 
//     plt.baselib.functions.makePrimitiveProcedure(
//         'color-list->image',
//             ???,
//         function(MACHINE) {
//             ...
//         });


// EXPORTS['step-count?'] = 
//     plt.baselib.functions.makePrimitiveProcedure(
//         'step-count?',
//             ???,
//         function(MACHINE) {
//             ...
//         });
