/*jslint sub: true, vars: true, nomen: true, plusplus: true, maxerr: 50, indent: 4 */
/*global plt*/

// Modules
(function (baselib, plt) {
    'use strict';
    var exports = {};
    baselib.modules = exports;

    var hasOwnProperty = {}.hasOwnProperty;

    var Namespace = function(modrec) {
        this.modrec = modrec;

        // Returns the key/value pairs of the prefix.
        // mapping: string -> value
        this.mapping = {}; 
    };

    Namespace.prototype.get = function(name) {
        return this.mapping[name];
    };

    Namespace.prototype.hasKey = function(name) {
        return hasOwnProperty.call(this.mapping, name);
    };

    Namespace.prototype.set = function(name, value) {
        this.mapping[name] = value;
    };

    var ModuleRecord = function (name, label) {
        this.name = name;
        this.label = label;
        this.isInvoked = false;
        this.prefix = false;
        this.exports = new Namespace(this);
        this.externalNamespace = new Namespace(this);

        // JavaScript-implemented code will assign privateExports
        // with all of the exported identifiers.
        this.privateExports = {};
    };

    // Returns the offset into the prefix in which the value will be stored.
    ModuleRecord.prototype.getPrefixOffset = function(externalName) {
        var i;
        for (i = 0; i < this.prefix.names.length; i++) {
            if (this.prefix.names[i] === externalName) {
                return i;
            }
        }
        return void(0);
    };

    // Returns access to the names provided in the module.
    // Note that the names are the names internal to the module.
    ModuleRecord.prototype.getExports = function () {
        return this.exports;
    };    

    // Returns access to the names defined with their external names.
    ModuleRecord.prototype.getExternalNamespace = function() {
        return this.externalNamespace;
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
            if (isInternal) {
                MACHINE.params['currentErrorHandler'] = function (MACHINE, anError) {
                    MACHINE.params['currentErrorHandler'] = oldErrorHandler;
                    fail(MACHINE, anError);
                };
                MACHINE.c.push(new plt.baselib.frames.CallFrame(afterGoodInvoke, null));
                throw that.label;
            } else {
                MACHINE.exclusiveLock.acquire(
                    void(0),
                    function(release) {
                        MACHINE.params['currentErrorHandler'] = function (MACHINE, anError) {
                            MACHINE.params['currentErrorHandler'] = oldErrorHandler;
                            fail(MACHINE, anError);
                        };
                        MACHINE.c.push(new plt.baselib.frames.CallFrame(afterGoodInvoke, null));
                        MACHINE._trampoline(that.label, false, release);
                    });
            }
        }
    };
    


    exports.ModuleRecord = ModuleRecord;


}(this.plt.baselib, this.plt));
