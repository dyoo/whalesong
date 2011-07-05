// Basic library functions.  This will include a few simple functions,
// but be augmented with several namespaces for the other libraries in
// the base library.
if (! this['plt']) { this['plt'] = {}; }
(function (plt) {
    var baselib = {};
    plt['baselib'] = baselib;



    // Simple object inheritance.
    var heir = function(parentPrototype) {
	var f = function() {}
	f.prototype = parentPrototype;
	return new f();
    };



    // clone: object -> object
    // Copies an object.  The new object should respond like the old
    // object, including to things like instanceof.
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




    baselib.heir = heir;
    baselib.clone = clone;


})(this['plt']);
