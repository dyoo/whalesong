var checkString = plt.runtime.makeCheckArgumentType(
    plt.runtime.strings.isString,
    'string');

var checkByte = plt.runtime.makeCheckArgumentType(
    plt.runtime.numbers.isByte,
    'byte');

var checkColor = plt.runtime.makeCheckArgumentType(
    isColorOrColorString,
    'color');

var checkImage = plt.runtime.makeCheckArgumentType(
    isImage,
    'image');

var checkReal = plt.runtime.makeCheckArgumentType(
    plt.runtime.numbers.isReal,
    'real');



//////////////////////////////////////////////////////////////////////


EXPORTS['image-color?'] =
    plt.runtime.makePrimitiveProcedure(
        'image-color?',
        1,
        function(MACHINE) {
            var elt = MACHINE.env[MACHINE.env.length - 1];
            return (isColorOrColorString(elt));
        });




EXPORTS['text'] =
    plt.runtime.makePrimitiveProcedure(
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
//     plt.runtime.makePrimitiveProcedure(
//         'text/font',
//             ???,
//         function(MACHINE) {
//             ...
//         });

// FIXME
// EXPORTS['image-url'] = 
//     plt.runtime.makePrimitiveProcedure(
//         'image-url',
//             ???,
//         function(MACHINE) {
//             ...
//         });

// FIXME
// EXPORTS['open-image-url'] = 
//     plt.runtime.makePrimitiveProcedure(
//         'open-image-url',
//             ???,
//         function(MACHINE) {
//             ...
//         });

EXPORTS['overlay'] = 
    plt.runtime.makePrimitiveProcedure(
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
    plt.runtime.makePrimitiveProcedure(
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
//     plt.runtime.makePrimitiveProcedure(
//         'overlay/align',
//             ???,
//         function(MACHINE) {
//             ...
//         });

EXPORTS['underlay'] = 
    plt.runtime.makePrimitiveProcedure(
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
    plt.runtime.makePrimitiveProcedure(
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
//     plt.runtime.makePrimitiveProcedure(
//         'underlay/align',
//             ???,
//         function(MACHINE) {
//             ...
//         });

// EXPORTS['beside'] = 
//     plt.runtime.makePrimitiveProcedure(
//         'beside',
//             ???,
//         function(MACHINE) {
//             ...
//         });

// EXPORTS['beside/align'] = 
//     plt.runtime.makePrimitiveProcedure(
//         'beside/align',
//             ???,
//         function(MACHINE) {
//             ...
//         });

// EXPORTS['above'] = 
//     plt.runtime.makePrimitiveProcedure(
//         'above',
//             ???,
//         function(MACHINE) {
//             ...
//         });

// EXPORTS['above/align'] = 
//     plt.runtime.makePrimitiveProcedure(
//         'above/align',
//             ???,
//         function(MACHINE) {
//             ...
//         });

// EXPORTS['place-image/align'] = 
//     plt.runtime.makePrimitiveProcedure(
//         'place-image/align',
//             ???,
//         function(MACHINE) {
//             ...
//         });

EXPORTS['rotate'] = 
    plt.runtime.makePrimitiveProcedure(
        'rotate',
        2,
        function(MACHINE) {
	    var angle = checkReal(MACHINE, "rotate",  0);
	    var img = checkImage(MACHINE, "rotate", 1);
	    return makeRotateImage(jsnums.toFixnum(angle), img);
        });

EXPORTS['scale'] = 
    plt.runtime.makePrimitiveProcedure(
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
    plt.runtime.makePrimitiveProcedure(
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
//     plt.runtime.makePrimitiveProcedure(
//         'flip-horizontal',
//             ???,
//         function(MACHINE) {
//             ...
//         });

// EXPORTS['flip-vertical'] = 
//     plt.runtime.makePrimitiveProcedure(
//         'flip-vertical',
//             ???,
//         function(MACHINE) {
//             ...
//         });

// EXPORTS['frame'] = 
//     plt.runtime.makePrimitiveProcedure(
//         'frame',
//             ???,
//         function(MACHINE) {
//             ...
//         });

// EXPORTS['crop'] = 
//     plt.runtime.makePrimitiveProcedure(
//         'crop',
//             ???,
//         function(MACHINE) {
//             ...
//         });

EXPORTS['line'] = 
    plt.runtime.makePrimitiveProcedure(
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

EXPORTS['add-line'] = 
    plt.runtime.makePrimitiveProcedure(
        'add-line',
            ???,
        function(MACHINE) {
            ...
        });

EXPORTS['scene+line'] = 
    plt.runtime.makePrimitiveProcedure(
        'scene+line',
            ???,
        function(MACHINE) {
            ...
        });

EXPORTS['circle'] = 
    plt.runtime.makePrimitiveProcedure(
        'circle',
            ???,
        function(MACHINE) {
            ...
        });

EXPORTS['square'] = 
    plt.runtime.makePrimitiveProcedure(
        'square',
            ???,
        function(MACHINE) {
            ...
        });

EXPORTS['rectangle'] = 
    plt.runtime.makePrimitiveProcedure(
        'rectangle',
            ???,
        function(MACHINE) {
            ...
        });

EXPORTS['regular-polygon'] = 
    plt.runtime.makePrimitiveProcedure(
        'regular-polygon',
            ???,
        function(MACHINE) {
            ...
        });

EXPORTS['ellipse'] = 
    plt.runtime.makePrimitiveProcedure(
        'ellipse',
            ???,
        function(MACHINE) {
            ...
        });

EXPORTS['triangle'] = 
    plt.runtime.makePrimitiveProcedure(
        'triangle',
            ???,
        function(MACHINE) {
            ...
        });

EXPORTS['right-triangle'] = 
    plt.runtime.makePrimitiveProcedure(
        'right-triangle',
            ???,
        function(MACHINE) {
            ...
        });

EXPORTS['isosceles-triangle'] = 
    plt.runtime.makePrimitiveProcedure(
        'isosceles-triangle',
            ???,
        function(MACHINE) {
            ...
        });

EXPORTS['star'] = 
    plt.runtime.makePrimitiveProcedure(
        'star',
            ???,
        function(MACHINE) {
            ...
        });

EXPORTS['radial-star'] = 
    plt.runtime.makePrimitiveProcedure(
        'radial-star',
            ???,
        function(MACHINE) {
            ...
        });

EXPORTS['star-polygon'] = 
    plt.runtime.makePrimitiveProcedure(
        'star-polygon',
            ???,
        function(MACHINE) {
            ...
        });

EXPORTS['rhombus'] = 
    plt.runtime.makePrimitiveProcedure(
        'rhombus',
            ???,
        function(MACHINE) {
            ...
        });

EXPORTS['image->color-list'] = 
    plt.runtime.makePrimitiveProcedure(
        'image->color-list',
            ???,
        function(MACHINE) {
            ...
        });

EXPORTS['color-list->image'] = 
    plt.runtime.makePrimitiveProcedure(
        'color-list->image',
            ???,
        function(MACHINE) {
            ...
        });

EXPORTS['image-width'] = 
    plt.runtime.makePrimitiveProcedure(
        'image-width',
            ???,
        function(MACHINE) {
            ...
        });

EXPORTS['image-height'] = 
    plt.runtime.makePrimitiveProcedure(
        'image-height',
            ???,
        function(MACHINE) {
            ...
        });

EXPORTS['image-baseline'] = 
    plt.runtime.makePrimitiveProcedure(
        'image-baseline',
            ???,
        function(MACHINE) {
            ...
        });

EXPORTS['image-color?'] = 
    plt.runtime.makePrimitiveProcedure(
        'image-color?',
            ???,
        function(MACHINE) {
            ...
        });

EXPORTS['mode?'] = 
    plt.runtime.makePrimitiveProcedure(
        'mode?',
            ???,
        function(MACHINE) {
            ...
        });

EXPORTS['x-place?'] = 
    plt.runtime.makePrimitiveProcedure(
        'x-place?',
            ???,
        function(MACHINE) {
            ...
        });

EXPORTS['y-place?'] = 
    plt.runtime.makePrimitiveProcedure(
        'y-place?',
            ???,
        function(MACHINE) {
            ...
        });

EXPORTS['angle?'] = 
    plt.runtime.makePrimitiveProcedure(
        'angle?',
            ???,
        function(MACHINE) {
            ...
        });

EXPORTS['side-count?'] = 
    plt.runtime.makePrimitiveProcedure(
        'side-count?',
            ???,
        function(MACHINE) {
            ...
        });

EXPORTS['image-url'] = 
    plt.runtime.makePrimitiveProcedure(
        'image-url',
            ???,
        function(MACHINE) {
            ...
        });
EXPORTS['open-image-url'] = 
    plt.runtime.makePrimitiveProcedure(
        'open-image-url',
            ???,
        function(MACHINE) {
            ...
        });
EXPORTS['color-list->image'] = 
    plt.runtime.makePrimitiveProcedure(
        'color-list->image',
            ???,
        function(MACHINE) {
            ...
        });


EXPORTS['step-count?'] = 
    plt.runtime.makePrimitiveProcedure(
        'step-count?',
            ???,
        function(MACHINE) {
            ...
        });