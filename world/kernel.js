

var PAUSE = plt.runtime.PAUSE;
var EMPTY = plt.baselib.lists.EMPTY;
var isString = plt.baselib.strings.isString;
var isBoolean = function(x) { return x === true || x === false; }
var isSymbol = plt.baselib.symbols.isSymbol;
var makePair = plt.baselib.lists.makePair;
var makeList = plt.baselib.lists.makeList;
var makeRational = plt.baselib.numbers.makeRational;



var finalizeClosureCall = plt.baselib.functions.finalizeClosureCall;





//////////////////////////////////////////////////////////////////////

var bigBang = function(MACHINE, initW, handlers) {
    PAUSE(function(restart) {
	
	var finalWorldValue = "not done yet";
	// Once we finally get a value back, we can call
	// restart at this point and finish the call to
	// big-bang.
        restart(function(MACHINE) {
            finalizeClosureCall(
                MACHINE, 
                finalWorldValue);
        });
	


	// var onBreak = function() {
	//     bigBangController.breaker();
	// }
	// state.addBreakRequestedListener(onBreak);
	// var bigBangController = rawJsworld.bigBang(
	//     initW, 
	//     state.getToplevelNodeHook()(),
	//     unwrappedConfigs,
	//     caller,
	//     function(v) {
	// 	state.removeBreakRequestedListener(onBreak);
	// 	restarter(v);
	//     },
 	//     onFail);

    });
};


//////////////////////////////////////////////////////////////////////

// Every world configuration function (on-tick, stop-when, ...)
// produces a WorldConfigOption instance.
var WorldConfigOption = function(name) {
    this.name = name;	    
};

WorldConfigOption.prototype.configure = function(config) {
    throw new Error('unimplemented WorldConfigOption');
};


WorldConfigOption.prototype.toDomNode = function(cache) {  
    var span = document.createElement('span');
    span.appendChild(document.createTextNode("(" + this.name + " ...)"));
    return span;
};

WorldConfigOption.prototype.toWrittenString = function(cache) {
    return "(" + this.name + " ...)";
};

WorldConfigOption.prototype.toDisplayedString = function(cache) {
    return "(" + this.name + " ...)";
};

var isWorldConfigOption = function(x) { return x instanceof WorldConfigOption; };

//////////////////////////////////////////////////////////////////////


var convertAttribList = function(attribList) {
    var nextElt;
    var key, val;
    var hash = {};
    while (attribList !== EMPTY) {
	nextElt = attribList.first;

	key = nextElt.first;
	val = nextElt.rest.first;

	key = String(key);

	if (isString(val)) {
	    val = String(val);
	} else if (isBoolean(val)) {
	    // do nothing: the representation is the same.
	} else if (isSymbol(val)) {
	    if (String(val) === 'true') {
		val = true;
	    } else if (String(val) === 'false') {
		val = false;
	    } else {
		val = String(val);
	    }
	} else {
	    // raise error: neither string nor boolean
	    throw new Error(
		plt.baselib.format.format(
		    "attribute value ~s neither a string nor a boolean",
		    [val]));
	}
	hash[key] = val;
	attribList = attribList.rest;
    }
    return hash;
}




//////////////////////////////////////////////////////////////////////


var OnTick = function(handler, aDelay) {
    WorldConfigOption.call(this, 'on-tick');
    this.handler = handler;
    this.aDelay = aDelay;
};

OnTick.prototype = plt.baselib.heir(WorldConfigOption.prototype);

OnTick.prototype.configure = function(config) {
    return config.updateAll(
	{ onTick: this.handler,
	  tickDelay: jsnums.toFixnum(jsnums.multiply(1000, this.aDelay))
	}
    );
};

