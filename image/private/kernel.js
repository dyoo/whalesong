var world = {};
world.Kernel = {};

EXPORTS['_kernel'] = world.Kernel;

var types = plt.types;




var worldListeners = [];
var stopped;
var timerInterval = false;


// Inheritance from pg 168: Javascript, the Definitive Guide.
var heir = function(p) {
    var f = function() {}
    f.prototype = p;
    return new f();
}


// clone: object -> object
// Copies an object.  The new object should respond like the old
// object, including to things like instanceof
var clone = function(obj) {
    var C = function() {}
    C.prototype = obj;
    var c = new C();
    for (property in obj) {
	if (obj.hasOwnProperty(property)) {
	    c[property] = obj[property];
	}
    }
    return c;
};




var announceListeners = [];
world.Kernel.addAnnounceListener = function(listener) {
    announceListeners.push(listener);
};
world.Kernel.removeAnnounceListener = function(listener) {
    var idx = announceListeners.indexOf(listener);
    if (idx != -1) {
	announceListeners.splice(idx, 1);
    }
};
world.Kernel.announce = function(eventName, vals) {
    for (var i = 0; i < announceListeners.length; i++) {
	try {
	    announceListeners[i](eventName, vals);
	} catch (e) {}
    }
};










// changeWorld: world -> void
// Changes the current world to newWorld.
var changeWorld = function(newWorld) {
    world = newWorld;
    notifyWorldListeners();
}


// updateWorld: (world -> world) -> void
// Public function: update the world, given the old state of the
// world.
world.Kernel.updateWorld = function(updater) {
    var newWorld = updater(world);
    changeWorld(newWorld);
}


world.Kernel.shutdownWorld = function() {
    stopped = true;
};


// notifyWorldListeners: -> void
// Tells all of the world listeners that the world has changed.
var notifyWorldListeners = function() {
    var i;
    for (i = 0; i < worldListeners.length; i++) {
	worldListeners[i](world);
    }
}

// addWorldListener: (world -> void) -> void
// Adds a new world listener: whenever the world is changed, the aListener
// will be called with that new world.
var addWorldListener = function(aListener) {
    worldListeners.push(aListener);
}


// getKeyCodeName: keyEvent -> String
// Given an event, try to get the name of the key.
var getKeyCodeName = function(e) {
    var code = e.charCode || e.keyCode;
    var keyname;
    if (code == 37) {
	keyname = "left";
    } else if (code == 38) {
	keyname = "up";
    } else if (code == 39) {
	keyname = "right";
    } else if (code == 40) {
	keyname = "down";
    } else {
	keyname = String.fromCharCode(code); 
    }
    return keyname;
}


// resetWorld: -> void
// Resets all of the world global values.
var resetWorld = function() {
    if (timerInterval) {
	clearInterval(timerInterval);
	timerInterval = false;
    }
    stopped = false;
    worldListeners = [];
}


var getBigBangWindow = function(width, height) {
    if (window.document.getElementById("canvas") != undefined) {
	return window;
    }

    var newWindow = window.open(
	"big-bang.html",
	"big-bang");
    //"toolbar=false,location=false,directories=false,status=false,menubar=false,width="+width+",height="+height);
    if (newWindow == null) { 
        throw new Error("Error: Not allowed to create a new window."); }

    return newWindow;
}



// scheduleTimerTick: -> void
// Repeatedly schedules an evaluation of the onTick until the program has stopped.
var scheduleTimerTick = function(window, config) {
    timerInterval = window.setInterval(
	function() {
	    if (stopped) {
		window.clearTimeout(timerInterval);
		timerInterval = false;
	    }
	    else {
		world.Kernel.stimuli.onTick();
	    }
	},
	config.lookup('tickDelay'));
}




// Base class for all images.
var BaseImage = function(pinholeX, pinholeY) {
    this.pinholeX = pinholeX;
    this.pinholeY = pinholeY;
}
world.Kernel.BaseImage = BaseImage;


var isImage = function(thing) {
    return (thing !== null &&
	    thing !== undefined &&
	    thing instanceof BaseImage);
}



BaseImage.prototype.updatePinhole = function(x, y) {
    var aCopy = clone(this);
    aCopy.pinholeX = x;
    aCopy.pinholeY = y;
    return aCopy;
};



// render: context fixnum fixnum: -> void
// Render the image, where the upper-left corner of the image is drawn at
// (x, y).
// NOTE: the rendering should be oblivous to the pinhole.
BaseImage.prototype.render = function(ctx, x, y) {
    throw new Error('BaseImage.render unimplemented!');
    //	plt.Kernel.throwMobyError(
    //	    false, 
    //	    "make-moby-error-type:generic-runtime-error", 
    //	    "Unimplemented method render");
};


// makeCanvas: number number -> canvas
// Constructs a canvas object of a particular width and height.
world.Kernel.makeCanvas = function(width, height) {
    var canvas = document.createElement("canvas");
    canvas.width = width;
    canvas.height = height;

    canvas.style.width = canvas.width + "px";
    canvas.style.height = canvas.height + "px";
    
    // KLUDGE: IE compatibility uses /js/excanvas.js, and dynamic
    // elements must be marked this way.
    if (window && typeof window.G_vmlCanvasManager != 'undefined') {
	canvas = window.G_vmlCanvasManager.initElement(canvas);
    }
    return canvas;
};



var withIeHack = function(canvas, f) {
    // 	canvas.style.display = 'none';
    // 	document.body.appendChild(canvas);
    // 	try {
    var result = f(canvas);
    // 	} catch(e) {
    // 	    document.body.removeChild(canvas);
    // 	    canvas.style.display = '';
    // 	    throw e;
    // 	}
    // 	document.body.removeChild(canvas);
    // 	canvas.style.display = '';
    return result;
};


BaseImage.prototype.toDomNode = function(cache) {
    var that = this;
    var width = that.getWidth();
    var height = that.getHeight();
    var canvas = world.Kernel.makeCanvas(width, height);

    // KLUDGE: on IE, the canvas rendering functions depend on a
    // context where the canvas is attached to the DOM tree.

    // We initialize an afterAttach hook; the client's responsible
    // for calling this after the dom node is attached to the
    // document.
    canvas.afterAttach = function() {
	var ctx = canvas.getContext("2d");
	that.render(ctx, 0, 0);
    };

    return canvas;
};




BaseImage.prototype.toWrittenString = function(cache) { return "<image>"; }
BaseImage.prototype.toDisplayedString = function(cache) { return "<image>"; }

BaseImage.prototype.isEqual = function(other, aUnionFind) {
    return (this.pinholeX == other.pinholeX &&
	    this.pinholeY == other.pinholeY);
};




// isScene: any -> boolean
// Produces true when x is a scene.
var isScene = function(x) {
    return ((x != undefined) && (x != null) && (x instanceof SceneImage));
};

// SceneImage: primitive-number primitive-number (listof image) -> Scene
var SceneImage = function(width, height, children, withBorder) {
    BaseImage.call(this, 0, 0);
    this.width = width;
    this.height = height;
    this.children = children; // arrayof [image, number, number]
    this.withBorder = withBorder;
}
SceneImage.prototype = heir(BaseImage.prototype);


// add: image primitive-number primitive-number -> Scene
SceneImage.prototype.add = function(anImage, x, y) {
    return new SceneImage(this.width, 
			  this.height,
			  this.children.concat([[anImage, 
						 x - anImage.pinholeX, 
						 y - anImage.pinholeY]]),
			  this.withBorder);
};

// render: 2d-context primitive-number primitive-number -> void
SceneImage.prototype.render = function(ctx, x, y) {
    var i;
    var childImage, childX, childY;
    // Clear the scene.
    ctx.clearRect(x, y, this.width, this.height);
    // Then ask every object to render itself.
    for(i = 0; i < this.children.length; i++) {
	childImage = this.children[i][0];
	childX = this.children[i][1];
	childY = this.children[i][2];
	ctx.save();
	childImage.render(ctx, childX + x, childY + y);
	ctx.restore();


    }
    // Finally, draw the black border if withBorder is true
    if (this.withBorder) {
	ctx.strokeStyle = 'black';
	ctx.strokeRect(x, y, this.width, this.height);
    }
};

SceneImage.prototype.getWidth = function() {
    return this.width;
};

SceneImage.prototype.getHeight = function() {
    return this.height;
};

SceneImage.prototype.isEqual = function(other, aUnionFind) {
    if (!(other instanceof SceneImage)) {
	return false;
    } 

    if (this.pinholeX != other.pinholeX ||
	this.pinholeY != other.pinholeY ||
	this.width != other.width ||
	this.height != other.height ||
	this.children.length != other.children.length) {
	return false;
    }

    for (var i = 0; i < this.children.length; i++) {
	var rec1 = this.children[i];
	var rec2 = other.children[i];
	if (rec1[1] !== rec2[1] ||
	    rec1[2] !== rec2[2] ||
	    !types.isEqual(rec1[0], 
			   rec2[0],
			   aUnionFind)) {
	    return false;
 	}
    }
    return true;
};


//////////////////////////////////////////////////////////////////////


var FileImage = function(src, rawImage) {
    BaseImage.call(this, 0, 0);
    var self = this;
    this.src = src;
    this.isLoaded = false;
    if (rawImage && rawImage.complete) { 
	this.img = rawImage;
	this.isLoaded = true;
	this.pinholeX = self.img.width / 2;
	this.pinholeY = self.img.height / 2;
    } else {
	// fixme: we may want to do something blocking here for
	// onload, since we don't know at this time what the file size
	// should be, nor will drawImage do the right thing until the
	// file is loaded.
	this.img = new Image();
	this.img.onload = function() {
	    self.isLoaded = true;
	    self.pinholeX = self.img.width / 2;
	    self.pinholeY = self.img.height / 2;
	};
	this.img.onerror = function(e) {
	    self.img.onerror = "";
	    self.img.src = "http://www.wescheme.org/images/broken.png";
	}
	this.img.src = src;
    }
}
FileImage.prototype = heir(BaseImage.prototype);
//    world.Kernel.FileImage = FileImage;


var imageCache = {};
FileImage.makeInstance = function(path, rawImage) {
    if (! (path in imageCache)) {
	imageCache[path] = new FileImage(path, rawImage);
    } 
    return imageCache[path];
};

FileImage.installInstance = function(path, rawImage) {
    imageCache[path] = new FileImage(path, rawImage);
};

FileImage.installBrokenImage = function(path) {
    imageCache[path] = new TextImage("Unable to load " + path, 10, 
				     colorDb.get("red"));
};



FileImage.prototype.render = function(ctx, x, y) {
    ctx.drawImage(this.img, x, y);
};


FileImage.prototype.getWidth = function() {
    return this.img.width;
};


FileImage.prototype.getHeight = function() {
    return this.img.height;
};

// Override toDomNode: we don't need a full-fledged canvas here.
FileImage.prototype.toDomNode = function(cache) {
    return this.img.cloneNode(true);
};

FileImage.prototype.isEqual = function(other, aUnionFind) {
    return (other instanceof FileImage &&
	    this.pinholeX == other.pinholeX &&
	    this.pinholeY == other.pinholeY &&
	    this.src == other.src);
    //		    types.isEqual(this.img, other.img, aUnionFind));
};


//////////////////////////////////////////////////////////////////////


// OverlayImage: image image -> image
// Creates an image that overlays img1 on top of the
// other image.  shiftX and shiftY are deltas off the first
// image's pinhole.
var OverlayImage = function(img1, img2, shiftX, shiftY) {
    var deltaX = img1.pinholeX - img2.pinholeX + shiftX;
    var deltaY = img1.pinholeY - img2.pinholeY + shiftY;
    var left = Math.min(0, deltaX);
    var top = Math.min(0, deltaY);
    var right = Math.max(deltaX + img2.getWidth(), 
			 img1.getWidth());
    var bottom = Math.max(deltaY + img2.getHeight(),
			  img1.getHeight());

    BaseImage.call(this, 
		   Math.floor((right-left) / 2),
		   Math.floor((bottom-top) / 2));
    this.img1 = img1;
    this.img2 = img2;
    this.width = right - left;
    this.height = bottom - top;

    this.img1Dx = -left;
    this.img1Dy = -top;
    this.img2Dx = deltaX - left;	
    this.img2Dy = deltaY - top;
};

OverlayImage.prototype = heir(BaseImage.prototype);


OverlayImage.prototype.render = function(ctx, x, y) {
    this.img2.render(ctx, x + this.img2Dx, y + this.img2Dy);
    this.img1.render(ctx, x + this.img1Dx, y + this.img1Dy);
};


OverlayImage.prototype.getWidth = function() {
    return this.width;
};

OverlayImage.prototype.getHeight = function() {
    return this.height;
};

OverlayImage.prototype.isEqual = function(other, aUnionFind) {
    return ( other instanceof OverlayImage &&
	     this.pinholeX == other.pinholeX &&
	     this.pinholeY == other.pinholeY &&
	     this.width == other.width &&
	     this.height == other.height &&
	     this.img1Dx == other.img1Dx &&
	     this.img1Dy == other.img1Dy &&
	     this.img2Dx == other.img2Dx &&
	     this.img2Dy == other.img2Dy &&
	     types.isEqual(this.img1, other.img1, aUnionFind) &&
	     types.isEqual(this.img2, other.img2, aUnionFind) );
};


//////////////////////////////////////////////////////////////////////


// rotate: angle image -> image
// Rotates image by angle degrees in a counter-clockwise direction.
// based on http://stackoverflow.com/questions/3276467/adjusting-div-width-and-height-after-rotated
var RotateImage = function(angle, img) {
    var sin   = Math.sin(angle * Math.PI / 180),
		cos   = Math.cos(angle * Math.PI / 180);
	
	// (w,0) rotation
	var x1 = Math.floor(cos * img.getWidth()),
		y1 = Math.floor(sin * img.getWidth());
	
	// (0,h) rotation
	var x2 = Math.floor(-sin * img.getHeight()),
		y2 = Math.floor( cos * img.getHeight());
	
	// (w,h) rotation
	var x3 = Math.floor(cos * img.getWidth() - sin * img.getHeight()),
		y3 = Math.floor(sin * img.getWidth() + cos * img.getHeight());
	
	var minX = Math.min(0, x1, x2, x3),
		maxX = Math.max(0, x1, x2, x3),
		minY = Math.min(0, y1, y2, y3),
		maxY = Math.max(0, y1, y2, y3);
	
	var rotatedWidth  = maxX - minX,
		rotatedHeight = maxY - minY;
	
	// resize the image
    BaseImage.call(this, 
				   Math.floor(rotatedWidth / 2),
				   Math.floor(rotatedHeight / 2));
		
	this.img	= img;
	this.width	= rotatedWidth;
	this.height = rotatedHeight;
    this.angle	= angle;
	this.translateX = -minX;
	this.translateY = -minY;
};

RotateImage.prototype = heir(BaseImage.prototype);


// translate drawing point, so that this.img appears in the UL corner. Then rotate and render this.img.
RotateImage.prototype.render = function(ctx, x, y) {
	ctx.translate(this.translateX, this.translateY);
	ctx.rotate(this.angle * Math.PI / 180);
    this.img.render(ctx, x, y);
	ctx.restore();
};


RotateImage.prototype.getWidth = function() {
    return this.width;
};

RotateImage.prototype.getHeight = function() {
    return this.height;
};

RotateImage.prototype.isEqual = function(other, aUnionFind) {
    return ( other instanceof RotateImage &&
			this.pinholeX == other.pinholeX &&
			this.pinholeY == other.pinholeY &&
			this.width == other.width &&
			this.height == other.height &&
			this.angle == other.angle &&
			this.translateX == other.translateX &&
			this.translateY == other.translateY &&
			types.isEqual(this.img, other.img, aUnionFind) );
};

//////////////////////////////////////////////////////////////////////


// ScaleImage: factor factor image -> image
// Scale an image
var ScaleImage = function(xFactor, yFactor, img) {
    
    // resize the image
    BaseImage.call(this, 
		   Math.floor((img.getWidth() * xFactor) / 2),
		   Math.floor((img.getHeight() * yFactor) / 2));
    
    this.img	= img;
    this.width	= img.getWidth() * xFactor;
    this.height = img.getHeight() * yFactor;
    this.xFactor = xFactor;
    this.yFactor = yFactor;
};

ScaleImage.prototype = heir(BaseImage.prototype);


// scale the context, and pass it to the image's render function
ScaleImage.prototype.render = function(ctx, x, y) {
    ctx.save();
    ctx.scale(this.xFactor, this.yFactor);
    this.img.render(ctx, x, y);
    ctx.restore();
};


ScaleImage.prototype.getWidth = function() {
    return this.width;
};

ScaleImage.prototype.getHeight = function() {
    return this.height;
};

ScaleImage.prototype.isEqual = function(other, aUnionFind) {
    return ( other instanceof ScaleImage &&
	     this.pinholeX == other.pinholeX &&
	     this.pinholeY == other.pinholeY &&
	     this.width == other.width &&
	     this.height == other.height &&
	     this.xFactor == other.xFactor &&
	     this.yFactor == other.yFactor &&
	     types.isEqual(this.img, other.img, aUnionFind) );
};

//////////////////////////////////////////////////////////////////////



var colorString = function(aColor) {
    return ("rgb(" + 
	    types.colorRed(aColor) + "," +
	    types.colorGreen(aColor) + ", " + 
	    types.colorBlue(aColor) + ")");
};



var RectangleImage = function(width, height, style, color) {
    BaseImage.call(this, width/2, height/2);
    this.width = width;
    this.height = height;
    this.style = style;
    this.color = color;
};
RectangleImage.prototype = heir(BaseImage.prototype);


RectangleImage.prototype.render = function(ctx, x, y) {
    if (this.style.toString().toLowerCase() == "outline") {
	ctx.save();
	ctx.beginPath();
	ctx.strokeStyle = colorString(this.color);
	ctx.strokeRect(x, y, this.width, this.height);
	ctx.closePath();
	ctx.restore();
    } else {
	ctx.save();
	ctx.beginPath();

	ctx.fillStyle = colorString(this.color);
	ctx.fillRect(x, y, this.width, this.height);

	ctx.closePath();
	ctx.restore();
    }
};

RectangleImage.prototype.getWidth = function() {
    return this.width;
};


RectangleImage.prototype.getHeight = function() {
    return this.height;
};

RectangleImage.prototype.isEqual = function(other, aUnionFind) {
    return (other instanceof RectangleImage &&
	    this.pinholeX == other.pinholeX &&
	    this.pinholeY == other.pinholeY &&
	    this.width == other.width &&
	    this.height == other.height &&
	    this.style == other.style &&
	    types.isEqual(this.color, other.color, aUnionFind));
};


//////////////////////////////////////////////////////////////////////

var TextImage = function(msg, size, color) {
    BaseImage.call(this, 0, 0);
    this.msg = msg;
    this.size = size;
    this.color = color;
    this.font = this.size + "px Optimer";

    
    var canvas = world.Kernel.makeCanvas(0, 0);
    var ctx = canvas.getContext("2d");
    ctx.font = this.font;
    var metrics = ctx.measureText(msg);

    this.width = metrics.width;
    // KLUDGE: I don't know how to get at the height.
    this.height = ctx.measureText("m").width + 20;

}

TextImage.prototype = heir(BaseImage.prototype);

TextImage.prototype.render = function(ctx, x, y) {
    ctx.save();
    ctx.font = this.font;
    ctx.textAlign = 'left';
    ctx.textBaseline = 'top';
    ctx.fillStyle = colorString(this.color);
    ctx.fillText(this.msg, x, y);
    ctx.restore();
};

TextImage.prototype.getWidth = function() {
    return this.width;
};


TextImage.prototype.getHeight = function() {
    return this.height;
};

TextImage.prototype.isEqual = function(other, aUnionFind) {
    return (other instanceof TextImage &&
	    this.pinholeX == other.pinholeX &&
	    this.pinholeY == other.pinholeY &&
	    this.msg == other.msg &&
	    this.size == other.size &&
	    types.isEqual(this.color, other.color, aUnionFind) &&
	    this.font == other.font);
};


//////////////////////////////////////////////////////////////////////

var CircleImage = function(radius, style, color) {
    BaseImage.call(this, radius, radius);
    this.radius = radius;
    this.style = style;
    this.color = color;
}
CircleImage.prototype = heir(BaseImage.prototype);

CircleImage.prototype.render = function(ctx, x, y) {
    ctx.save();
    ctx.beginPath();
    ctx.arc(x + this.radius,
	    y + this.radius,
	    this.radius, 0, 2*Math.PI, false);
    ctx.closePath();
    if (this.style.toString().toLowerCase() == "outline") {
	ctx.strokeStyle = colorString(this.color);
	ctx.stroke();
    } else {
	ctx.fillStyle = colorString(this.color);
	ctx.fill();
    }

    ctx.restore();
};

CircleImage.prototype.getWidth = function() {
    return this.radius * 2;
};

CircleImage.prototype.getHeight = function() {
    return this.radius * 2;
};

CircleImage.prototype.isEqual = function(other, aUnionFind) {
    return (other instanceof CircleImage &&
	    this.pinholeX == other.pinholeX &&
	    this.pinholeY == other.pinholeY &&
	    this.radius == other.radius &&
	    this.style == other.style &&
	    types.isEqual(this.color, other.color, aUnionFind));
};



//////////////////////////////////////////////////////////////////////


// StarImage: fixnum fixnum fixnum color -> image
var StarImage = function(points, outer, inner, style, color) {
    BaseImage.call(this,
		   Math.max(outer, inner),
		   Math.max(outer, inner));
    this.points = points;
    this.outer = outer;
    this.inner = inner;
    this.style = style;
    this.color = color;

    this.radius = Math.max(this.inner, this.outer);
};

StarImage.prototype = heir(BaseImage.prototype);

var oneDegreeAsRadian = Math.PI / 180;

// render: context fixnum fixnum -> void
// Draws a star on the given context.
// Most of this code here adapted from the Canvas tutorial at:
// http://developer.apple.com/safari/articles/makinggraphicswithcanvas.html
StarImage.prototype.render = function(ctx, x, y) {
    ctx.save();
    ctx.beginPath();
    for( var pt = 0; pt < (this.points * 2) + 1; pt++ ) {
	var rads = ( ( 360 / (2 * this.points) ) * pt ) * oneDegreeAsRadian - 0.5;
	var radius = ( pt % 2 == 1 ) ? this.outer : this.inner;
	ctx.lineTo(x + this.radius + ( Math.sin( rads ) * radius ), 
		   y + this.radius + ( Math.cos( rads ) * radius ) );
    }
    ctx.closePath();
    if (this.style.toString().toLowerCase() == "outline") {
	ctx.strokeStyle = colorString(this.color);
	ctx.stroke();
    } else {
	ctx.fillStyle = colorString(this.color);
	ctx.fill();
    }

    ctx.restore();
};

// getWidth: -> fixnum
StarImage.prototype.getWidth = function() {
    return this.radius * 2;
};


// getHeight: -> fixnum
StarImage.prototype.getHeight = function() {
    return this.radius * 2;
};

StarImage.prototype.isEqual = function(other, aUnionFind) {
    return (other instanceof StarImage &&
	    this.pinholeX == other.pinholeX &&
	    this.pinholeY == other.pinholeY &&
	    this.points == other.points &&
	    this.outer == other.outer &&
	    this.inner == other.inner &&
	    this.style == other.style &&
	    types.isEqual(this.color, other.color, aUnionFind));
};




//////////////////////////////////////////////////////////////////////
//Triangle
///////
var TriangleImage = function(side, style, color) {
    this.width = side;
    this.height = Math.ceil(side * Math.sqrt(3) / 2);

    BaseImage.call(this, Math.floor(this.width/2), Math.floor(this.height/2));
    this.side = side;
    this.style = style;
    this.color = color;
}
TriangleImage.prototype = heir(BaseImage.prototype);


TriangleImage.prototype.render = function(ctx, x, y) {
    var width = this.getWidth();
    var height = this.getHeight();
    ctx.save();
    ctx.beginPath();
    ctx.moveTo(x + this.side/2, y);
    ctx.lineTo(x + width, y + height);
    ctx.lineTo(x, y + height);
    ctx.closePath();

    if (this.style.toString().toLowerCase() == "outline") {
	ctx.strokeStyle = colorString(this.color);
	ctx.stroke();
    }
    else {
	ctx.fillStyle = colorString(this.color);
	ctx.fill();
    }
    ctx.restore();
};



TriangleImage.prototype.getWidth = function() {
    return this.width;
};

TriangleImage.prototype.getHeight = function() {
    return this.height;
};

TriangleImage.prototype.isEqual = function(other, aUnionFind) {
    return (other instanceof TriangleImage &&
	    this.pinholeX == other.pinholeX &&
	    this.pinholeY == other.pinholeY &&
	    this.side == other.side &&
	    this.style == other.style &&
	    types.isEqual(this.color, other.color, aUnionFind));
};



//////////////////////////////////////////////////////////////////////
//Ellipse 
var EllipseImage = function(width, height, style, color) {
    BaseImage.call(this, Math.floor(width/2), Math.floor(height/2));
    this.width = width;
    this.height = height;
    this.style = style;
    this.color = color;
};

EllipseImage.prototype = heir(BaseImage.prototype);


EllipseImage.prototype.render = function(ctx, aX, aY) {
    ctx.save();
    ctx.beginPath();

    // Most of this code is taken from:
    // http://webreflection.blogspot.com/2009/01/ellipse-and-circle-for-canvas-2d.html
    var hB = (this.width / 2) * .5522848,
    vB = (this.height / 2) * .5522848,
    eX = aX + this.width,
    eY = aY + this.height,
    mX = aX + this.width / 2,
    mY = aY + this.height / 2;
    ctx.moveTo(aX, mY);
    ctx.bezierCurveTo(aX, mY - vB, mX - hB, aY, mX, aY);
    ctx.bezierCurveTo(mX + hB, aY, eX, mY - vB, eX, mY);
    ctx.bezierCurveTo(eX, mY + vB, mX + hB, eY, mX, eY);
    ctx.bezierCurveTo(mX - hB, eY, aX, mY + vB, aX, mY);
    ctx.closePath();
    if (this.style.toString().toLowerCase() == "outline") {
 	ctx.strokeStyle = colorString(this.color);
	ctx.stroke();
    }
    else {
 	ctx.fillStyle = colorString(this.color);
	ctx.fill();
    }


    ctx.restore();
};

EllipseImage.prototype.getWidth = function() {
    return this.width;
};

EllipseImage.prototype.getHeight = function() {
    return this.height;
};

EllipseImage.prototype.isEqual = function(other, aUnionFind) {
    return (other instanceof EllipseImage &&
	    this.pinholeX == other.pinholeX &&
	    this.pinholeY == other.pinholeY &&
	    this.width == other.width &&
	    this.height == other.height &&
	    this.style == other.style &&
	    types.isEqual(this.color, other.color, aUnionFind));
};


//////////////////////////////////////////////////////////////////////
//Line
var LineImage = function(x, y, color) {
    if (x >= 0) {
	if (y >= 0) {
	    BaseImage.call(this, 0, 0);
	} else {
	    BaseImage.call(this, 0, -y);
	}
    } else {
	if (y >= 0) {
	    BaseImage.call(this, -x, 0);
	} else {
	    BaseImage.call(this, -x, -y);
	}
    }


    this.x = x;
    this.y = y;
    this.color = color;
    this.width = Math.abs(x) + 1;
    this.height = Math.abs(y) + 1;
}

LineImage.prototype = heir(BaseImage.prototype);


LineImage.prototype.render = function(ctx, xstart, ystart) {
    ctx.save();

    if (this.x >= 0) {
	if (this.y >= 0) {
	    ctx.moveTo(xstart, ystart);
	    ctx.lineTo((xstart + this.x),
		       (ystart + this.y));
	} else {
	    ctx.moveTo(xstart, ystart + (-this.y));
	    ctx.lineTo(xstart + this.x, ystart);
	}
    } else {
	if (this.y >= 0) {
	    ctx.moveTo(xstart + (-this.x), ystart);
	    ctx.lineTo(xstart,
		       (ystart + this.y));		
	} else {
	    ctx.moveTo(xstart + (-this.x), ystart + (-this.y));
	    ctx.lineTo(xstart, ystart);
	}
    }
    ctx.strokeStyle = colorString(this.color);
    ctx.stroke();
    ctx.restore();
};


LineImage.prototype.getWidth = function() {
    return this.width;
};


LineImage.prototype.getHeight = function() {
    return this.height;
};

LineImage.prototype.isEqual = function(other, aUnionFind) {
    return (other instanceof LineImage &&
	    this.pinholeX == other.pinholeX &&
	    this.pinholeY == other.pinholeY &&
	    this.x == other.x &&
	    this.y == other.y &&
	    types.isEqual(this.color, other.color, aUnionFind));
};





//////////////////////////////////////////////////////////////////////
// Effects

/**
     * applyEffect: compound-effect -> (arrayof (world -> world))

     applyEffect applies all of the effects

     @param aCompEffect a compound effect is either a scheme list of
     compound effects or a single primitive effect */
world.Kernel.applyEffect = function(aCompEffect) {
    if ( types.isEmpty(aCompEffect) ) {
    	// Do Nothing
    } else if ( types.isPair(aCompEffect) ) {
    	var results = world.Kernel.applyEffect(aCompEffect.first());
    	return results.concat(world.Kernel.applyEffect(aCompEffect.rest()));
    } else {
	var newResult = aCompEffect.run();
	if (newResult) {
	    return newResult;
	}
    }
    return [];
}

//////////////////////////////////////////////////////////////////////////
































///////////////////////////////////////////////////////////////
// Exports

world.Kernel.isImage = isImage;
world.Kernel.isScene = isScene;
world.Kernel.isColor = function(thing) {
    return (types.isColor(thing) ||
	    ((types.isString(thing) || types.isSymbol(thing)) &&
	     typeof(colorDb.get(thing)) != 'undefined'));
};
world.Kernel.colorDb = colorDb;

world.Kernel.sceneImage = function(width, height, children, withBorder) {
    return new SceneImage(width, height, children, withBorder);
};
world.Kernel.circleImage = function(radius, style, color) {
    return new CircleImage(radius, style, color);
};
world.Kernel.starImage = function(points, outer, inner, style, color) {
    return new StarImage(points, outer, inner, style, color);
};
world.Kernel.rectangleImage = function(width, height, style, color) {
    return new RectangleImage(width, height, style, color);
};
world.Kernel.triangleImage = function(side, style, color) {
    return new TriangleImage(side, style, color);
};
world.Kernel.ellipseImage = function(width, height, style, color) {
    return new EllipseImage(width, height, style, color);
};
world.Kernel.lineImage = function(x, y, color) {
    return new LineImage(x, y, color);
};
world.Kernel.overlayImage = function(img1, img2, shiftX, shiftY) {
    return new OverlayImage(img1, img2, shiftX, shiftY);
};
world.Kernel.rotateImage = function(angle, img) {
    return new RotateImage(angle, img);
};
world.Kernel.scaleImage = function(xFactor, yFactor, img) {
	return new ScaleImage(xFactor, yFactor, img);
};
world.Kernel.textImage = function(msg, size, color) {
    return new TextImage(msg, size, color);
};
world.Kernel.fileImage = function(path, rawImage) {
    return FileImage.makeInstance(path, rawImage);
};


world.Kernel.isSceneImage = function(x) { return x instanceof SceneImage; };
world.Kernel.isCircleImage = function(x) { return x instanceof CircleImage; };
world.Kernel.isStarImage = function(x) { return x instanceof StarImage; };
world.Kernel.isRectangleImage = function(x) { return x instanceof RectangleImage; };
world.Kernel.isTriangleImage = function(x) { return x instanceof TriangleImage; };
world.Kernel.isEllipseImage = function(x) { return x instanceof EllipseImage; };
world.Kernel.isLineImage = function(x) { return x instanceof LineImage; };
world.Kernel.isOverlayImage = function(x) { return x instanceof OverlayImage; };
world.Kernel.isRotateImage = function(x) { return x instanceof RotateImage; };
world.Kernel.isTextImage = function(x) { return x instanceof TextImage; };
world.Kernel.isFileImage = function(x) { return x instanceof FileImage; };







//////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////


// Feeds stimuli inputs into the world.  The functions here
// are responsible for converting to Scheme values.
//
// NOTE and WARNING: make sure to really do the coersions, even for
// strings.  Bad things happen otherwise, as in the sms stuff, where
// we're getting string-like values that aren't actually strings.



world.stimuli = {};
world.Kernel.stimuli = world.stimuli;


(function() {
    var handlers = [];

    var doNothing = function() {};


    var StimuliHandler = function(config, caller, restarter) {
	this.config = config;
	this.caller = caller;
	this.restarter = restarter;
	handlers.push(this);
    };

    //    StimuliHandler.prototype.failHandler = function(e) {
    //	this.onShutdown();
    //    	this.restarter(e);
    //    };	

    // doStimuli: CPS( (world -> effect) (world -> world) -> void )
    //
    // Processes a stimuli by compute the effect and applying it, and
    // computing a new world to replace the old.
    StimuliHandler.prototype.doStimuli = function(computeEffectF, computeWorldF, restArgs, k) {
	var effectUpdaters = [];
	var that = this;
	try {
	    that.change(function(w, k2) {
		var args = [w].concat(restArgs);
		var doStimuliHelper = function() {
		    if (computeWorldF) {
			that.caller(computeWorldF, args, k2);
		    } else {
			k2(w);
		    }
		};
		doStimuliHelper();
	    }, k);
 	    // if (computeEffectF) {
	    // 		    that.caller(computeEffectF, [args],
	    // 			    function(effect) {
	    // 			    	effectUpdaters = applyEffect(effect);
	    // 				doStimuliHelper();
	    // 			    },
	    //	    		    this.failHandler);
	    // 		}
	    // 		else { doStimuliHelper(); }
	    // 	    },
	    // 	    function() {
	    // 	    	helpers.forEachK(effectUpdaters,
	    // 				 function(effect, k2) { that.change(effect, k2); },
	    // 				 function(e) { throw e; },
	    // 				 k);
	    // 	    });
	} catch (e) { 
	    //		if (console && console.log && e.stack) {
	    //			console.log(e.stack);
	    //		}
	    this.onShutdown();
	}
    }


    // Orientation change
    // args: [azimuth, pitch, roll]
    StimuliHandler.prototype.onTilt = function(args, k) {
	var onTilt = this.lookup("onTilt");
	var onTiltEffect = this.lookup("onTiltEffect");
	this.doStimuli(onTiltEffect, onTilt, helpers.map(flt, args), k);
    };


    // Accelerations
    // args: [x, y, z]
    StimuliHandler.prototype.onAcceleration = function(args, k) {
	var onAcceleration = this.lookup('onAcceleration');
	var onAccelerationEffect = this.lookup('onAccelerationEffect');
	this.doStimuli(onAccelerationEffect, onAcceleration, helpers.map(flt, args), k);
    };


    // Shakes
    // args: []
    StimuliHandler.prototype.onShake = function(args, k) {
	var onShake = this.lookup('onShake');
	var onShakeEffect = this.lookup('onShakeEffect');
	this.doStimuli(onShakeEffect, onShake, [], k);
    };


    // Sms receiving
    // args: [sender, message]
    StimuliHandler.prototype.onSmsReceive = function(args, k) {
	var onSmsReceive = this.lookup('onSmsReceive');
	var onSmsReceiveEffect = this.lookup('onSmsReceiveEffect');
	// IMPORTANT: must coerse to string by using x+"".  Do not use
	// toString(): it's not safe.
	this.doStimuli(onSmsReceiveEffect, onSmsReceive, [args[0]+"", args[1]+""], k);
    };


    // Locations
    // args: [lat, lng]
    StimuliHandler.prototype.onLocation = function(args, k) {
	var onLocationChange = this.lookup('onLocationChange');
	var onLocationChangeEffect = this.lookup('onLocationChangeEffect');
	this.doStimuli(onLocationChangeEffect, onLocationChange, helpers.map(flt, args), k);
    };



    // Keystrokes
    // args: [e]
    StimuliHandler.prototype.onKey = function(args, k) {
	// getKeyCodeName: keyEvent -> String
	// Given an event, try to get the name of the key.
	var getKeyCodeName = function(e) {
	    var code = e.charCode || e.keyCode;
	    var keyname;
	    switch(code) {
	    case 16: keyname = "shift"; break;
	    case 17: keyname = "control"; break;
	    case 19: keyname = "pause"; break;
	    case 27: keyname = "escape"; break;
	    case 33: keyname = "prior"; break;
	    case 34: keyname = "next"; break;
	    case 35: keyname = "end"; break;
	    case 36: keyname = "home"; break;
	    case 37: keyname = "left"; break;
	    case 38: keyname = "up"; break;
	    case 39: keyname = "right"; break;
	    case 40: keyname = "down"; break;
	    case 42: keyname = "print"; break;
	    case 45: keyname = "insert"; break;
	    case 46: keyname = String.fromCharCode(127); break;
	    case 106: keyname = "*"; break;
	    case 107: keyname = "+"; break;
	    case 109: keyname = "-"; break;
	    case 110: keyname = "."; break;
	    case 111: keyname = "/"; break;
	    case 144: keyname = "numlock"; break;
	    case 145: keyname = "scroll"; break;
	    case 186: keyname = ";"; break;
	    case 187: keyname = "="; break;
	    case 188: keyname = ","; break;
	    case 189: keyname = "-"; break;
	    case 190: keyname = "."; break;
	    case 191: keyname = "/"; break;
	    case 192: keyname = "`"; break;
	    case 219: keyname = "["; break;
	    case 220: keyname = "\\"; break;
	    case 221: keyname = "]"; break;
	    case 222: keyname = "'"; break;
	    default: if (code >= 96 && code <= 105) {
		keyname = (code - 96).toString();
	    }
		else if (code >= 112 && code <= 123) {
		    keyname = "f" + (code - 111);
		}
		else {
		    keyname = String.fromCharCode(code).toLowerCase();
		}
		break;
	    }
	    return keyname;
	}
	var keyname = getKeyCodeName(args[0]);
	var onKey = this.lookup('onKey');
	var onKeyEffect = this.lookup('onKeyEffect');
	this.doStimuli(onKeyEffect, onKey, [keyname], k);
    };



    //    // Time ticks
    //    // args: []
    //    StimuliHandler.prototype.onTick = function(args, k) {
    //	var onTick = this.lookup('onTick');
    //	var onTickEffect = this.lookup('onTickEffect');
    //	this.doStimuli(onTickEffect, onTick, [], k);
    //    };



    // Announcements
    // args: [eventName, vals]
    StimuliHandler.prototype.onAnnounce = function(args, k) {
	var vals = args[1];
	var valsList = types.EMPTY;
	for (var i = 0; i < vals.length; i++) {
	    valsList = types.cons(vals[vals.length - i - 1], valsList);
	}

	var onAnnounce = this.lookup('onAnnounce');
	var onAnnounceEffect = this.lookup('onAnnounceEffect');	
	this.doStimuli(onAnnounce, onAnnounceEffect, [args[0], valsList], k);
    };



    // The shutdown stimuli: special case that forces a world computation to quit.
    // Also removes this instance from the list of handlers
    StimuliHandler.prototype.onShutdown = function() {	
	var index = handlers.indexOf(this);
	if (index != -1) {
	    handlers.splice(index, 1);
	}

	var shutdownWorld = this.lookup('shutdownWorld');
	if (shutdownWorld) {
	    shutdownWorld();
	}
    };


    //////////////////////////////////////////////////////////////////////
    // Helpers
    var flt = types.float;

    StimuliHandler.prototype.lookup = function(s) {
	return this.config.lookup(s);
    };

    StimuliHandler.prototype.change = function(f, k) {
	if (this.lookup('changeWorld')) {
	    this.lookup('changeWorld')(f, k);
	}
	else { k(); }
    };

    // applyEffect: compound-effect: (arrayof (world -> world))
    var applyEffect = function(e) {
	return world.Kernel.applyEffect(e);
    };

    var makeStimulusHandler = function(funName) {
	return function() {
	    var args = arguments;
	    for (var i = 0; i < handlers.length; i++) {
		(handlers[i])[funName](args, doNothing);
	    }
	    //		helpers.forEachK(handlers,
	    //				 function(h, k) { h[funName](args, k); },
	    //				 function(e) { throw e; },
	    //				 doNothing);
	}
    };

    //////////////////////////////////////////////////////////////////////
    // Exports

    world.stimuli.StimuliHandler = StimuliHandler;

    world.stimuli.onTilt = makeStimulusHandler('onTilt');
    world.stimuli.onAcceleration = makeStimulusHandler('onAcceleration');
    world.stimuli.onShake = makeStimulusHandler('onShake');
    world.stimuli.onSmsReceive = makeStimulusHandler('onSmsReceive');
    world.stimuli.onLocation = makeStimulusHandler('onLocation');
    world.stimuli.onKey = makeStimulusHandler('onKey');
    //    world.stimuli.onTick = makeStimulusHandler('onTick');
    world.stimuli.onAnnounce = makeStimulusHandler('onAnnounce');

    world.stimuli.massShutdown = function() {
	for (var i = 0; i < handlers.length; i++) {
	    var shutdownWorld = handlers[i].lookup('shutdownWorld');
	    if (shutdownWorld) {
		shutdownWorld();
	    }
	}
	handlers = [];
    };


})();

//////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////







(function() {

//     var make_dash_effect_colon_none =
// 	(plt.Kernel.invokeModule("moby/runtime/effect-struct")
// 	 .EXPORTS['make-effect:none']);

    world.config = {};
    world.Kernel.config = world.config;


    // augment: hash hash -> hash
    // Functionally extend a hashtable with another one.
    var augment = function(o, a) {
	var oo = {};
	for (var e in o) {
	    if (o.hasOwnProperty(e)) {
		oo[e] = o[e];
	    }
	}
	for (var e in a) {
	    if (a.hasOwnProperty(e)) {
		oo[e] = a[e];
	    }
	}
	return oo;
    }



    var WorldConfig = function() {
	// The following handler values are initially false until they're updated
	// by configuration.
      
	// A handler is a function:
	//     handler: world X Y ... -> Z


	this.vals = {
	    // changeWorld: (world -> world) -> void
	    // When called, this will update the world based on the
	    // updater passed to it.
	    changeWorld: false,

	    // shutdownWorld: -> void
	    // When called, this will shut down the world computation.
	    shutdownWorld: false,

	    // initialEffect: effect
	    // The initial effect to invoke when the world computation
	    // begins.
	    initialEffect: false,


	    // onRedraw: world -> scene
	    onRedraw: false,

	    // onDraw: world -> (sexpof dom)
	    onDraw: false,

	    // onDrawCss: world -> (sexpof css-style)
	    onDrawCss: false,


	    // tickDelay: number
	    tickDelay: false,
	    // onTick: world -> world
	    onTick: false,
	    // onTickEffect: world -> effect
	    onTickEffect: false,

	    // onKey: world key -> world
	    onKey: false,
	    // onKeyEffect: world key -> effect
	    onKeyEffect : false,

	    // onTilt: world number number number -> world
	    onTilt: false,
	    // onTiltEffect: world number number number -> effect
	    onTiltEffect: false,

	    // onAcceleration: world number number number -> world
	    onAcceleration: false,
	    // onAccelerationEffect: world number number number -> effect
	    onAccelerationEffect: false,

	    // onShake: world -> world
	    onShake: false,
	    // onShakeEffect: world -> effect
	    onShakeEffect: false,

	    // onSmsReceive: world -> world
	    onSmsReceive: false,
	    // onSmsReceiveEffect: world -> effect
	    onSmsReceiveEffect: false,

	    // onLocationChange: world number number -> world
	    onLocationChange : false,
	    // onLocationChangeEffect: world number number -> effect
	    onLocationChangeEffect: false,


	    // onAnnounce: world string X ... -> world
	    onAnnounce: false,
	    // onAnnounce: world string X ... -> effect
	    onAnnounceEffect: false,

	    // stopWhen: world -> boolean
	    stopWhen: false,
	    // stopWhenEffect: world -> effect
	    stopWhenEffect: false,



	    //////////////////////////////////////////////////////////////////////
	    // For universe game playing

	    // connectToGame: string
	    // Registers with some universe, given an identifier
	    // which is a URL to a Universe server.
	    connectToGame: false,
	    onGameStart: false,
	    onOpponentTurn: false,
	    onMyTurn: false,
	    afterMyTurn: false,
	    onGameFinish: false
	};
    }

  
    // WorldConfig.lookup: string -> handler
    // Looks up a value in the configuration.
    WorldConfig.prototype.lookup = function(key) {
//	plt.Kernel.check(key, plt.Kernel.isString, "WorldConfig.lookup", "string", 1);
	if (key in this.vals) {
	    return this.vals[key];
	} else {
	    throw Error("Can't find " + key + " in the configuration");
	}
    }
  


    // WorldConfig.updateAll: (hashof string handler) -> WorldConfig
    WorldConfig.prototype.updateAll = function(aHash) {
	var result = new WorldConfig();
	result.vals = augment(this.vals, aHash);
	return result;
    }

  
    world.config.WorldConfig = WorldConfig;

    // The following global variable CONFIG is mutated by either
    // big-bang from the regular world or the one in jsworld.
    world.config.CONFIG = new WorldConfig();


    // A handler is a function that consumes a config and produces a
    // config.


    //////////////////////////////////////////////////////////////////////

    var getNoneEffect = function() {
	throw new Error("getNoneEffect: We should not be calling effects!");
	//	return make_dash_effect_colon_none();
    }



    //////////////////////////////////////////////////////////////////////

    world.config.Kernel = world.config.Kernel || {};
    world.config.Kernel.getNoneEffect = getNoneEffect;


/*
    // makeSimplePropertyUpdater: (string (X -> boolean) string string) -> (X -> handler)
    var makeSimplePropertyUpdater = function(propertyName,
					     propertyPredicate,
					     propertyTypeName,
					     updaterName) {
	return function(val) {
	    plt.Kernel.check(val, propertyPredicate, updaterName, propertyTypeName, 1);
	    return addStringMethods(
		function(config) {
		    return config.updateAll({propertyName: val });
		}, updaterName);
	}
    };

    // connects to the game
    world.config.Kernel.connect_dash_to_dash_game = 
	makeSimplePropertyUpdater('connectToGame',
				  plt.Kernel.isString,
				  "string",
				  "connect-to-game");


    // Registers a handler for game-start events.
    world.config.Kernel.on_dash_game_dash_start = 
	makeSimplePropertyUpdater('onGameStart',
				  plt.Kernel.isFunction,
				  "function",
				  "on-game-start");


    // Registers a handler for opponent-turn events.
    world.config.Kernel.on_dash_opponent_dash_turn = 
	makeSimplePropertyUpdater('onOpponentTurn',
				  plt.Kernel.isFunction,
				  "function",
				  "on-opponent-turn");


    // Registers a handler for my turn.
    world.config.Kernel.on_dash_my_dash_turn = 
	makeSimplePropertyUpdater('onMyTurn',
				  plt.Kernel.isFunction,
				  "function",
				  "on-my-turn");

    // Register a handler after I make a move.
    world.config.Kernel.after_dash_my_dash_turn = 
	makeSimplePropertyUpdater('afterMyTurn',
				  plt.Kernel.isFunction,
				  "function",
				  "after-my-turn");

    world.config.Kernel.on_dash_game_dash_finish = 
	makeSimplePropertyUpdater('onGameFinish',
				  plt.Kernel.isFunction,
				  "function",
				  "on-game-finish");
*/



})();
