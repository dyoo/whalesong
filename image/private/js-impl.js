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
            ???,
        function(MACHINE) {
            ...
        });

EXPORTS['text/font'] = 
    plt.runtime.makePrimitiveProcedure(
        'text/font',
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

EXPORTS['overlay'] = 
    plt.runtime.makePrimitiveProcedure(
        'overlay',
            ???,
        function(MACHINE) {
            ...
        });

EXPORTS['overlay/xy'] = 
    plt.runtime.makePrimitiveProcedure(
        'overlay/xy',
            ???,
        function(MACHINE) {
            ...
        });

EXPORTS['overlay/align'] = 
    plt.runtime.makePrimitiveProcedure(
        'overlay/align',
            ???,
        function(MACHINE) {
            ...
        });

EXPORTS['underlay'] = 
    plt.runtime.makePrimitiveProcedure(
        'underlay',
            ???,
        function(MACHINE) {
            ...
        });

EXPORTS['underlay/xy'] = 
    plt.runtime.makePrimitiveProcedure(
        'underlay/xy',
            ???,
        function(MACHINE) {
            ...
        });

EXPORTS['underlay/align'] = 
    plt.runtime.makePrimitiveProcedure(
        'underlay/align',
            ???,
        function(MACHINE) {
            ...
        });

EXPORTS['beside'] = 
    plt.runtime.makePrimitiveProcedure(
        'beside',
            ???,
        function(MACHINE) {
            ...
        });

EXPORTS['beside/align'] = 
    plt.runtime.makePrimitiveProcedure(
        'beside/align',
            ???,
        function(MACHINE) {
            ...
        });

EXPORTS['above'] = 
    plt.runtime.makePrimitiveProcedure(
        'above',
            ???,
        function(MACHINE) {
            ...
        });

EXPORTS['above/align'] = 
    plt.runtime.makePrimitiveProcedure(
        'above/align',
            ???,
        function(MACHINE) {
            ...
        });

EXPORTS['place-image/align'] = 
    plt.runtime.makePrimitiveProcedure(
        'place-image/align',
            ???,
        function(MACHINE) {
            ...
        });

EXPORTS['rotate'] = 
    plt.runtime.makePrimitiveProcedure(
        'rotate',
            ???,
        function(MACHINE) {
            ...
        });

EXPORTS['scale'] = 
    plt.runtime.makePrimitiveProcedure(
        'scale',
            ???,
        function(MACHINE) {
            ...
        });

EXPORTS['scale/xy'] = 
    plt.runtime.makePrimitiveProcedure(
        'scale/xy',
            ???,
        function(MACHINE) {
            ...
        });

EXPORTS['flip-horizontal'] = 
    plt.runtime.makePrimitiveProcedure(
        'flip-horizontal',
            ???,
        function(MACHINE) {
            ...
        });

EXPORTS['flip-vertical'] = 
    plt.runtime.makePrimitiveProcedure(
        'flip-vertical',
            ???,
        function(MACHINE) {
            ...
        });

EXPORTS['frame'] = 
    plt.runtime.makePrimitiveProcedure(
        'frame',
            ???,
        function(MACHINE) {
            ...
        });

EXPORTS['crop'] = 
    plt.runtime.makePrimitiveProcedure(
        'crop',
            ???,
        function(MACHINE) {
            ...
        });

EXPORTS['line'] = 
    plt.runtime.makePrimitiveProcedure(
        'line',
            ???,
        function(MACHINE) {
            ...
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