var resourceType = MACHINE.modules['whalesong/resource/structs.rkt'].getExternalNamespace().get('struct:resource');

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
var injectImageMethods = function(r, img, after) {
    r.img = img;
    r.getWidth = function() { return img.width; };
    r.getHeight = function() { return img.height; };
    r.getBaseline = function() { return img.height; };
    r.updatePinhole = function() {
        var aCopy = plt.baselib.clone(this);
        aCopy.pinholeX = x;
        aCopy.pinholeY = y;
        return aCopy;
    };
    r.render = function(ctx, x, y) {
        ctx.drawImage(r.animationHackImg, x, y);
    };
    r.toDomNode = function(params) {
        return img.cloneNode(true);
    };

    installHackToSupportAnimatedGifs(r, after);
};

var installHackToSupportAnimatedGifs = function(r, after) {
    r.animationHackImg = r.img.cloneNode(true);
    document.body.appendChild(r.animationHackImg);
    r.animationHackImg.width = 0;
    r.animationHackImg.height = 0;
    
    if (r.animationHackImg.complete) {
        after();
    } else {
        r.animationHackImg.onload = function() {
            delete (r.animationHackImg.onload);
            after();
        };
    }
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
                        var after = function() {
                            restart(function(MACHINE) {
                                return finalizeClosureCall(MACHINE, resource);
                            });
                        };
                        injectImageMethods(resource, rawImage, after);
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
