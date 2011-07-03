// Skeleton for basic library functions
if (! this['plt']) { this['plt'] = {}; }
(function (plt) {
    var baselib = {};
    plt['baselib'] = baselib;





    // Inheritance.
    var heir = function(parentPrototype) {
	var f = function() {}
	f.prototype = parentPrototype;
	return new f();
    };




    baselib.heir = heir;


})(this['plt']);
