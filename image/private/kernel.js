

//////////////////////////////////////////////////////////////////////
var colorNamespace = MACHINE.modules['whalesong/image/private/color.rkt'].namespace;
var colorStruct = colorNamespace['struct:color'];
var makeColor = colorStruct.constructor;
var isColor = colorStruct.predicate;
var colorRed = function(c) { return colorStruct.accessor(c, 0); };
var colorGreen = function(c) { return colorStruct.accessor(c, 1); };
var colorBlue = function(c) { return colorStruct.accessor(c, 2); };
//////////////////////////////////////////////////////////////////////


var isColorOrColorString = function(thing) {
    return (isColor(thing) ||
	    ((plt.baselib.strings.isString(thing) ||
              plt.baselib.symbols.isSymbol(thing)) &&
	     typeof(colorDb.get(thing)) != 'undefined'));
}






// Base class for all images.
var BaseImage = function(pinholeX, pinholeY) {
    this.pinholeX = pinholeX;
    this.pinholeY = pinholeY;
}



var isImage = function(thing) {
    return (thing !== null &&
	    thing !== undefined &&
	    thing instanceof BaseImage);
}



BaseImage.prototype.updatePinhole = function(x, y) {
    var aCopy = plt.baselib.clone(this);
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
var makeCanvas = function(width, height) {
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
    var canvas = makeCanvas(width, height);

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
SceneImage.prototype = plt.baselib.heir(BaseImage.prototype);


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
	    !plt.baselib.equality.equals(rec1[0], 
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
FileImage.prototype = plt.baselib.heir(BaseImage.prototype);


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

OverlayImage.prototype = plt.baselib.heir(BaseImage.prototype);


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
	     plt.baselib.equality.equals(this.img1, other.img1, aUnionFind) &&
	     plt.baselib.equality.equals(this.img2, other.img2, aUnionFind) );
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

RotateImage.prototype = plt.baselib.heir(BaseImage.prototype);


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
			plt.baselib.equality.equals(this.img, other.img, aUnionFind) );
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

ScaleImage.prototype = plt.baselib.heir(BaseImage.prototype);


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
	     plt.baselib.equality.equals(this.img, other.img, aUnionFind) );
};

//////////////////////////////////////////////////////////////////////



var colorString = function(aColor) {
    return ("rgb(" + 
	    colorRed(aColor) + "," +
	    colorGreen(aColor) + ", " + 
	    colorBlue(aColor) + ")");
};



var RectangleImage = function(width, height, style, color) {
    BaseImage.call(this, width/2, height/2);
    this.width = width;
    this.height = height;
    this.style = style;
    this.color = color;
};
RectangleImage.prototype = plt.baselib.heir(BaseImage.prototype);


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
	    plt.baselib.equality.equals(this.color, other.color, aUnionFind));
};


//////////////////////////////////////////////////////////////////////

var TextImage = function(msg, size, color) {
    BaseImage.call(this, 0, 0);
    this.msg = msg;
    this.size = size;
    this.color = color;
    this.font = this.size + "px Optimer";

    
    var canvas = makeCanvas(0, 0);
    var ctx = canvas.getContext("2d");
    ctx.font = this.font;
    var metrics = ctx.measureText(msg);

    this.width = metrics.width;
    // KLUDGE: I don't know how to get at the height.
    this.height = ctx.measureText("m").width + 20;

}

TextImage.prototype = plt.baselib.heir(BaseImage.prototype);

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
	    plt.baselib.equality.equals(this.color, other.color, aUnionFind) &&
	    this.font == other.font);
};


//////////////////////////////////////////////////////////////////////

var CircleImage = function(radius, style, color) {
    BaseImage.call(this, radius, radius);
    this.radius = radius;
    this.style = style;
    this.color = color;
}
CircleImage.prototype = plt.baselib.heir(BaseImage.prototype);

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
	    plt.baselib.equality.equals(this.color, other.color, aUnionFind));
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

StarImage.prototype = plt.baselib.heir(BaseImage.prototype);

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
	    plt.baselib.equality.equals(this.color, other.color, aUnionFind));
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
TriangleImage.prototype = plt.baselib.heir(BaseImage.prototype);


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
	    plt.baselib.equality.equals(this.color, other.color, aUnionFind));
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

EllipseImage.prototype = plt.baselib.heir(BaseImage.prototype);


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
	    plt.baselib.equality.equals(this.color, other.color, aUnionFind));
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

LineImage.prototype = plt.baselib.heir(BaseImage.prototype);


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
	    plt.baselib.equality.equals(this.color, other.color, aUnionFind));
};












var makeSceneImage = function(width, height, children, withBorder) {
    return new SceneImage(width, height, children, withBorder);
};
var makeCircleImage = function(radius, style, color) {
    return new CircleImage(radius, style, color);
};
var makeStarImage = function(points, outer, inner, style, color) {
    return new StarImage(points, outer, inner, style, color);
};
var makeRectangleImage = function(width, height, style, color) {
    return new RectangleImage(width, height, style, color);
};
var makeTriangleImage = function(side, style, color) {
    return new TriangleImage(side, style, color);
};
var makeEllipseImage = function(width, height, style, color) {
    return new EllipseImage(width, height, style, color);
};
var makeLineImage = function(x, y, color) {
    return new LineImage(x, y, color);
};
var makeOverlayImage = function(img1, img2, shiftX, shiftY) {
    return new OverlayImage(img1, img2, shiftX, shiftY);
};
var makeRotateImage = function(angle, img) {
    return new RotateImage(angle, img);
};
var makeScaleImage = function(xFactor, yFactor, img) {
    return new ScaleImage(xFactor, yFactor, img);
};
var makeTextImage = function(msg, size, color) {
    return new TextImage(msg, size, color);
};
var makeFileImage = function(path, rawImage) {
    return FileImage.makeInstance(path, rawImage);
};






///////////////////////////////////////////////////////////////
// Exports


EXPORTS.makeCanvas = makeCanvas;



EXPORTS.BaseImage = BaseImage;
EXPORTS.SceneImage = SceneImage;
EXPORTS.CircleImage = CircleImage;
EXPORTS.StarImage = StarImage;
EXPORTS.RectangleImage = RectangleImage;
EXPORTS.TriangleImage = TriangleImage;
EXPORTS.EllipseImage = EllipseImage;
EXPORTS.LineImage = LineImage;
EXPORTS.OverlayImage = OverlayImage;
EXPORTS.RotateImage = RotateImage;
EXPORTS.ScaleImage = ScaleImage;
EXPORTS.TextImage = TextImage;
EXPORTS.FileImage = FileImage;

EXPORTS.colorDb = colorDb;

EXPORTS.makeSceneImage = makeSceneImage;
EXPORTS.makeCircleImage = makeCircleImage;
EXPORTS.makeStarImage = makeStarImage;
EXPORTS.makeRectangleImage = makeRectangleImage;
EXPORTS.makeTriangleImage = makeTriangleImage;
EXPORTS.makeEllipseImage = makeEllipseImage;
EXPORTS.makeLineImage = makeLineImage;
EXPORTS.makeOverlayImage = makeOverlayImage;
EXPORTS.makeRotateImage = makeRotateImage;
EXPORTS.makeScaleImage = makeScaleImage;
EXPORTS.makeTextImage = makeTextImage;
EXPORTS.makeFileImage = makeFileImage;



EXPORTS.isImage = isImage;
EXPORTS.isScene = isScene;
EXPORTS.isColorOrColorString = isColorOrColorString;
EXPORTS.isSceneImage = function(x) { return x instanceof SceneImage; };
EXPORTS.isCircleImage = function(x) { return x instanceof CircleImage; };
EXPORTS.isStarImage = function(x) { return x instanceof StarImage; };
EXPORTS.isRectangleImage = function(x) { return x instanceof RectangleImage; };
EXPORTS.isTriangleImage = function(x) { return x instanceof TriangleImage; };
EXPORTS.isEllipseImage = function(x) { return x instanceof EllipseImage; };
EXPORTS.isLineImage = function(x) { return x instanceof LineImage; };
EXPORTS.isOverlayImage = function(x) { return x instanceof OverlayImage; };
EXPORTS.isRotateImage = function(x) { return x instanceof RotateImage; };
EXPORTS.isScaleImage = function(x) { return x instanceof ScaleImage; };
EXPORTS.isTextImage = function(x) { return x instanceof TextImage; };
EXPORTS.isFileImage = function(x) { return x instanceof FileImage; };

