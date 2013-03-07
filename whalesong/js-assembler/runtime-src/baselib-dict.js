/*jslint unparam: true, vars: true, white: true, newcap: true, nomen: true, plusplus: true, maxerr: 50, indent: 4 */

/*global window*/

// Dictionaries.  We need this due to JS weirdness, since object
// literals have issues with regards to magic properties like
// __proto__.  This is taken from Effective JavaScript, Item 45.
(function (baselib) {
    'use strict';

    var hasOwnProperty = {}.hasOwnProperty;
    
    baselib.Dict = function(elements) {
        this.elements = elements || {};
        this.hasSpecialProto = false;
        this.specialProto = undefined;
    };

    baselib.Dict.prototype.has = function(key) {
        if (key === '__proto__') {
            return this.hasSpecialProto;
        }
        return hasOwnProperty.call(this.elements, key);
    };

    baselib.Dict.prototype.get = function(key) {
        if (key === '__proto__') { 
            return this.specialProto;
        } else if (hasOwnProperty.call(this.elements, key)) {
            return this.elements[key];
        } else {
            return undefined;
        }
    };

    baselib.Dict.prototype.set = function(key, val) {
        if (key === '__proto__') {
            this.hasSpecialProto = true;
            this.specialProto = val;
        } else {
            this.elements[key] = val;
        }
    };

    baselib.Dict.prototype.remove = function(key) {
        if (key === '__proto__') {
            this.hasSpecialProto = false;
            this.specialProto = undefined;
        } else {
            delete this.elements[key];
        }
    };
}(window.plt.baselib));