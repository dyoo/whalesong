var resourceType = MACHINE.modules['whalesong/resource/structs.rkt'].getNamespace()['struct:resource'];

var makeClosure = plt.baselib.functions.makeClosure;
var finalizeClosureCall = plt.baselib.functions.finalizeClosureCall;

var checkResource = plt.baselib.check.makeCheckArgumentType(
    resourceType.predicate,
    "resource");

var getResourcePath = function(r) { return resourceType.accessor(r, 0); };
var getResourceKey = function(r) { return resourceType.accessor(r, 1); };

var PAUSE = plt.runtime.PAUSE;




var imageExtensions = { 'png' : true,
                        'gif' : true,
                        'jpeg' : true,
                        'jpg' : true,
                        'bmp' : true };

var isImagePath = function(s) {
    var extensionMatch = s.toLowerCase().match(/\.(\w+)$/);
    if (extensionMatch !== null && imageExtensions[extensionMatch[1]])
        return true;
    return false;
};



// A lot of this comes from image/private/kernel.js
var injectImageMethods = function(r, img) {
    r.img = img;
    r.getHeight = function() { return img.width; };
    r.getWidth = function() { return img.height; };
    r.getBaseline = function() { return img.height; };
    r.updatePinhole = function() {
        var aCopy = plt.baselib.clone(this);
        aCopy.pinholeX = x;
        aCopy.pinholeY = y;
        return aCopy;
    };
    r.render = function(ctx, x, y) {
        installHackToSupportAnimatedGifs(r);
        ctx.drawImage(r.animationHackImg, x, y);
    };
    r.toDomNode = function(params) {
        return img.cloneNode(true);
    };
};

var installHackToSupportAnimatedGifs = function(r) {
    if (r.animationHackImg) { return; }
    r.animationHackImg = r.img.cloneNode(true);
    document.body.appendChild(r.animationHackImg);
    r.animationHackImg.width = 0;
    r.animationHackImg.height = 0;
};







// If the resource is an image, decorate the value with the image properties.
EXPORTS['specialize!'] = makeClosure(
    'specialize!',
    1,
    function(MACHINE) {
        var resource = checkResource(MACHINE, 'specialize!', 0);

        if (isImagePath(getResourceKey(resource).toString())) {
            return PAUSE(
                function(restart) {
                    var rawImage = new Image();
                    rawImage.onload = function() {
                        delete(rawImage.onload);
                        delete(rawImage.onerror);
                        injectImageMethods(resource, rawImage);
                        restart(function(MACHINE) {
                            return finalizeClosureCall(MACHINE, resource);
                        });
                    };
                    rawImage.onerror = function(e) {
                        delete(rawImage.onload);
                        delete(rawImage.onerror);
                        // on any kind of image-loading error, fail out and
                        // just return the resource unchanged.
                        restart(function(MACHINE) {
                            return finalizeClosureCall(MACHINE, resource);
                        });
                    }
                    rawImage.src = getResourceKey(resource).toString();
                });
        } else {
            return finalizeClosureCall(MACHINE, resource);
        }
    });
