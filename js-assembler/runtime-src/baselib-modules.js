/*jslint sub: true, vars: true, nomen: true, plusplus: true, maxerr: 50, indent: 4 */
/*global plt*/

// Modules
(function (baselib, plt) {
    'use strict';
    var exports = {};
    baselib.modules = exports;


    var ModuleRecord = function (name, label) {
        this.name = name;
        this.label = label;
        this.isInvoked = false;
        this.prefix = false;
        this.namespace = {};

        // JavaScript-implemented code will assign privateExports
        // with all of the exported identifiers.
        this.privateExports = {};
    };

    // Returns access to the names defined in the module.
    ModuleRecord.prototype.getNamespace = function () {
        return this.namespace;
    };

    ModuleRecord.prototype.finalizeModuleInvokation = function () {
        var i, len = this.prefix.names.length;
        for (i = 0; i < len; i++) {
            this.namespace[this.prefix.names[i]] = this.prefix[i];
        }
    };
    

    // External invokation of a module.
    ModuleRecord.prototype.invoke = function (MACHINE, succ, fail) {
        this._invoke(false, MACHINE, succ, fail);
    };

    // Internal invokation of a module.
    ModuleRecord.prototype.internalInvoke = function (MACHINE, succ, fail) {
        this._invoke(true, MACHINE, succ, fail);
    };

    // Private: general invokation of a module
    ModuleRecord.prototype._invoke = function (isInternal, MACHINE, succ, fail) {
        var that = this;
        MACHINE = MACHINE || plt.runtime.currentMachine;
        succ = succ || function () {};
        fail = fail || function () {};

        var oldErrorHandler = MACHINE.params['currentErrorHandler'];
        var afterGoodInvoke = function (MACHINE) { 
            MACHINE.params['currentErrorHandler'] = oldErrorHandler;
            if (isInternal) { succ(); }
            else {
                throw new plt.runtime.HaltError(succ)
            }
        };

        if (this.isInvoked) {
            succ();
        } else {
            MACHINE.params['currentErrorHandler'] = function (MACHINE, anError) {
                MACHINE.params['currentErrorHandler'] = oldErrorHandler;
                fail(MACHINE, anError);
            };
            MACHINE.c.push(new plt.baselib.frames.CallFrame(afterGoodInvoke, null));
            if (isInternal) {
                throw that.label;
            } else {
                MACHINE.trampoline(that.label);
            }
        }
    };



    exports.ModuleRecord = ModuleRecord;


}(this.plt.baselib, this.plt));