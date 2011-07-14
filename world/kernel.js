

var PAUSE = plt.runtime.PAUSE;



var bigBang = function(MACHINE, initW, handlers) {
    PAUSE(function(restart) {
	var bigBangController;
	var onBreak = function() {
	    bigBangController.breaker();
	}
	state.addBreakRequestedListener(onBreak);
	bigBangController = rawJsworld.bigBang(
	    initW, 
	    state.getToplevelNodeHook()(),
	    unwrappedConfigs,
	    caller,
	    function(v) {
		state.removeBreakRequestedListener(onBreak);
		restarter(v);
	    },
 	    onFail);
    });
};

