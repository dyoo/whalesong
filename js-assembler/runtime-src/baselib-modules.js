(function(baselib) {
    var exports = {};
    baselib.modules = exports;


    var ModuleRecord = function(name, label) {
	this.name = name;
	this.label = label;
	this.isInvoked = false;
        this.prefix = false;
	this.namespace = {};
    };

    // Returns access to the names defined in the module.
    ModuleRecord.prototype.getNamespace = function() {
	return this.namespace;
    };

    ModuleRecord.prototype.finalizeModuleInvokation = function() {
	var i, len = this.prefix.names.length;
	for (i=0; i < len; i++) {
	    this.namespace[this.prefix.names[i]] = this.prefix[i];
	}
    };
    

    // External invokation of a module.
    ModuleRecord.prototype.invoke = function(MACHINE, succ, fail) {
        this._invoke(false, MACHINE, succ, fail);
    };

    // Internal invokation of a module.
    ModuleRecord.prototype.internalInvoke = function(MACHINE, succ, fail) {
        this._invoke(true, MACHINE, succ, fail);
    };

    // Private: general invokation of a module
    ModuleRecord.prototype._invoke = function(isInternal, MACHINE, succ, fail) {
        var that = this;
        MACHINE = MACHINE || plt.runtime.currentMachine;
        succ = succ || function(){};
        fail = fail || function(){};

        var oldErrorHandler = MACHINE.params['currentErrorHandler'];
        var afterGoodInvoke = function(MACHINE) { 
            MACHINE.params['currentErrorHandler'] = oldErrorHandler;
            succ();
        };

        if (this.isInvoked) {
            succ();
        } else {
            MACHINE.params['currentErrorHandler'] = function(MACHINE, anError) {
                MACHINE.params['currentErrorHandler'] = oldErrorHandler;
		fail(MACHINE, anError)
            };
            MACHINE.control.push(new plt.baselib.frames.CallFrame(afterGoodInvoke, null));
            if (isInternal) {
                throw that.label;
            } else {
                plt.runtime.trampoline(MACHINE, that.label);
            }
        }
    };








    exports.ModuleRecord = ModuleRecord;


})(this['plt'].baselib);