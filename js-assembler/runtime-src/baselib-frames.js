/*jslint unparam: true, sub: true, vars: true, white: true, plusplus: true, maxerr: 50, indent: 4 */

// Frame structures.
(function(baselib) {
    'use strict';
    var exports = {};
    baselib.frames = exports;



    // A generic frame just holds marks.
    var Frame = function() {
	// The set of continuation marks.
	this.marks = [];

	// When we're in the middle of computing with-cont-mark, we
	// stash the key in here temporarily.
	this.pendingContinuationMarkKey = undefined;
	this.pendingApplyValuesProc = undefined;
	this.pendingBegin0Count = undefined;
	this.pendingBegin0Values = undefined;
    };


    // Frames must support marks and the temporary variables necessary to
    // support with-continuation-mark and with-values.

    // Specialized frames support more features:

    // A CallFrame represents a call stack frame, and includes the return address
    // as well as the function being called.
    var CallFrame = function(label, proc) {
	this.label = label;
	this.p = proc;

	// The set of continuation marks.
	this.marks = [];

	// When we're in the middle of computing with-cont-mark, we
	// stash the key in here temporarily.
	this.pendingContinuationMarkKey = undefined;
    };
    CallFrame.prototype = baselib.heir(Frame.prototype);



    // A prompt frame includes a return address, as well as a prompt tag
    // for supporting delimited continuations.
    var PromptFrame = function(label, tag) {
	this.label = label;
	this.tag = tag; // ContinuationPromptTag

	// The set of continuation marks.
	this.marks = [];

	// When we're in the middle of computing with-cont-mark, we
	// stash the key in here temporarily.
	this.pendingContinuationMarkKey = undefined;	
    };
    PromptFrame.prototype = baselib.heir(Frame.prototype);




    //////////////////////////////////////////////////////////////////////
    exports.Frame = Frame;
    exports.CallFrame = CallFrame;
    exports.PromptFrame = PromptFrame;



}(this.plt.baselib));