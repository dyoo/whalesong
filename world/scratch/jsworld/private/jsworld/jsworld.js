var jsworld = {};

// Stuff here is copy-and-pasted from Chris's JSWorld.  We
// namespace-protect it, and add the Javascript <-> Moby wrapper
// functions here.

(function() {

    /* Type signature notation
     * CPS(a b ... -> c) is used to denote
     *    a b ... (c -> void) -> void
     */

    jsworld.Jsworld = {};
    var Jsworld = jsworld.Jsworld;


    var currentFocusedNode = false;

    var doNothing = function() {};



    //
    // WORLD STUFFS
    //

    function InitialWorld() {}

    var world = new InitialWorld();
    var worldListeners = [];
    var eventDetachers = [];
    var runningBigBangs = [];

    var changingWorld = false;



    // Close all world computations.
    Jsworld.shutdown = function() {
	while(runningBigBangs.length > 0) {
	    var currentRecord = runningBigBangs.pop();
	    if (currentRecord) { currentRecord.pause(); }
	}
	clear_running_state();
    }



    function add_world_listener(listener) {
	worldListeners.push(listener);
    }


    function remove_world_listener(listener) {
	var index = worldListeners.indexOf(listener);
	if (index != -1) {
	    worldListeners.splice(index, 1);
	}
    }

    function clear_running_state() {
	world = new InitialWorld();
	worldListeners = [];

	for (var i = 0; i < eventDetachers.length; i++) {
		eventDetachers[i]();
	}
	eventDetachers = [];
	changingWorld = false;
    }


    // If we're in the middle of a change_world, delay.
    var DELAY_BEFORE_RETRY = 10;


    // change_world: CPS( CPS(world -> world) -> void )
    // Adjust the world, and notify all listeners.
    var change_world = function(updater, k) {

	// Check to see if we're in the middle of changing
	// the world already.  If so, put on the queue
	// and exit quickly.
	if (changingWorld) {
	    setTimeout(
		function() {
		    change_world(updater, k)}, 
		DELAY_BEFORE_RETRY);
	    return;
	}


	changingWorld = true;
	var originalWorld = world;

	var changeWorldHelp = function() {
		if (world instanceof WrappedWorldWithEffects) {
			var effects = world.getEffects();
			helpers.forEachK(effects,
				 function(anEffect, k2) { 
				     anEffect.invokeEffect(change_world, k2); 
				 },
				 function (e) { 
				     changingWorld = false;
				     throw e; 
				 },
				 function() {
				 	world = world.getWorld();
					changeWorldHelp2();
				 });
		} else {
			changeWorldHelp2();
		}
	};
	
	var changeWorldHelp2 = function() {
		helpers.forEachK(worldListeners,
				 function(listener, k2) { 
				     listener(world, originalWorld, k2);
				 },
				 function(e) { 
				     changingWorld = false;
				     world = originalWorld;
				     throw e; },
				 function() {
				     changingWorld = false;
				     k();
				 });
	};

	try {
		updater(world, function(newWorld) {
				world = newWorld;
				changeWorldHelp();
			});
	} catch(e) {
	    changingWorld = false;
	    world = originalWorld;

	    if (typeof(console) !== 'undefined' && console.log && e.stack) {
		console.log(e.stack);
	    }
	    throw e;
	}
    }
    Jsworld.change_world = change_world;




    //
    // STUFF THAT SHOULD REALLY BE IN ECMASCRIPT
    //
    Number.prototype.NaN0=function(){return isNaN(this)?0:this;}
    function getPosition(e){
	var left = 0;
	var top  = 0;
	while (e.offsetParent){
	    left += e.offsetLeft + (e.currentStyle?(parseInt(e.currentStyle.borderLeftWidth)).NaN0():0);
	    top  += e.offsetTop  + (e.currentStyle?(parseInt(e.currentStyle.borderTopWidth)).NaN0():0);
	    e     = e.offsetParent;
	}
	left += e.offsetLeft + (e.currentStyle?(parseInt(e.currentStyle.borderLeftWidth)).NaN0():0);
	top  += e.offsetTop  + (e.currentStyle?(parseInt(e.currentStyle.borderTopWidth)).NaN0():0);
	return {x:left, y:top};	
    }
    Jsworld.getPosition = getPosition;


    var gensym_counter = 0;
    function gensym(){ return gensym_counter++;}
    Jsworld.gensym = gensym;


    function map(a1, f) {
	var b = new Array(a1.length);
	for (var i = 0; i < a1.length; i++) {
		b[i] = f(a1[i]);
	}
	return b;
    }
    Jsworld.map = map;



    function concat_map(a, f) {
	var b = [];
	for (var i = 0; i < a.length; i++) {
		b = b.concat(f(a[i]));
	}
	return b;
    }


    function mapi(a, f) {
	var b = new Array(a.length);
	for (var i = 0; i < a.length; i++) {
		b[i] = f(a[i], i);
	}
	return b;
    }
    Jsworld.mapi = mapi;


    function fold(a, x, f) {
	for (var i = 0; i < a.length; i++) {
		x = f(a[i], x);
	}
	return x;
    }
    Jsworld.fold = fold;


    function augment(o, a) {
	var oo = {};
	for (var e in o)
	    oo[e] = o[e];
	for (var e in a)
	    oo[e] = a[e];
	return oo;
    }
    Jsworld.augment = augment;


    function assoc_cons(o, k, v) {
	var oo = {};
	for (var e in o)
	    oo[e] = o[e];
	oo[k] = v;
	return oo;
    }
    Jsworld.assoc_cons = assoc_cons;


    function cons(value, array) {
	return [value].concat(array);
    }
    Jsworld.cons = cons;


    function append(array1, array2){
	return array1.concat(array2);
    }
    Jsworld.append = append;

    function array_join(array1, array2){
	var joined = [];
	for (var i = 0; i < array1.length; i++)
	    joined.push([array1[i], array2[i]]);
	return joined;
    }
    Jsworld.array_join = array_join;


    function removeq(a, value) {
	for (var i = 0; i < a.length; i++)
	    if (a[i] === value){
		return a.slice(0, i).concat(a.slice(i+1));
	    }			
	return a;
    }
    Jsworld.removeq = removeq;

    function removef(a, value) {
	for (var i = 0; i < a.length; i++)
	    if ( f(a[i]) ){
		return a.slice(0, i).concat(a.slice(i+1));
	    }			
	return a;
    }
    Jsworld.removef = removef;


    function filter(a, f) {
	var b = [];
	for (var i = 0; i < a.length; i++) {
		if ( f(a[i]) ) {
			b.push(a[i]);
		}
	}
	return b;
    }
    Jsworld.filter = filter;


    function without(obj, attrib) {
	var o = {};
	for (var a in obj)
	    if (a != attrib)
		o[a] = obj[a];
	return o;
    }
    Jsworld.without = without;


    function memberq(a, x) {
	for (var i = 0; i < a.length; i++)
	    if (a[i] === x) return true;
	return false;
    }
    Jsworld.memberq = memberq;


    function member(a, x) {
	for (var i = 0; i < a.length; i++)
	    if (a[i] == x) return true;
	return false;
    }
    Jsworld.member = member;



    function head(a){
	return a[0];
    }
    Jsworld.head = head;


    function tail(a){
	return a.slice(1, a.length);
    }
    Jsworld.tail = tail;

    //
    // DOM UPDATING STUFFS
    //

    // tree(N): { node: N, children: [tree(N)] }
    // relation(N): { relation: 'parent', parent: N, child: N } | { relation: 'neighbor', left: N, right: N }
    // relations(N): [relation(N)]
    // nodes(N): [N]
    // css(N): [css_node(N)]
    // css_node(N): { node: N, attribs: attribs } | { className: string, attribs: attribs }
    // attrib: { attrib: string, values: [string] }
    // attribs: [attrib]

    // treeable(nodes(N), relations(N)) = bool
    /*function treeable(nodes, relations) {
    // for all neighbor relations between x and y
    for (var i = 0; i < relations.length; i++)
    if (relations[i].relation == 'neighbor') {
    var x = relations[i].left, y = relations[i].right;
 
    // there does not exist a neighbor relation between x and z!=y or z!=x and y
    for (var j = 0; j < relations.length; j++)
    if (relations[j].relation === 'neighbor')
    if (relations[j].left === x && relations[j].right !== y ||
    relations[j].left !== x && relations[j].right === y)
    return false;
    }
 
    // for all parent relations between x and y
    for (var i = 0; i < relations.length; i++)
    if (relations[i].relation == 'parent') {
    var x = relations[i].parent, y = relations[i].child;
 
    // there does not exist a parent relation between z!=x and y
    for (var j = 0; j < relations.length; j++)
    if (relations[j].relation == 'parent')
    if (relations[j].parent !== x && relations[j].child === y)
    return false;
    }
 
    // for all neighbor relations between x and y
    for (var i = 0; i < relations.length; i++)
    if (relations[i].relation == 'neighbor') {
    var x = relations[i].left, y = relations[i].right;
 
    // all parent relations between z and x or y share the same z
    for (var j = 0; j < relations.length; j++)
    if (relations[j].relation == 'parent')
    for (var k = 0; k < relations.length; k++)
    if (relations[k].relation == 'parent')
    if (relations[j].child === x && relations[k].child === y &&
    relations[j].parent !== relations[k].parent)
    return false;
    }
 
    return true;
    }*/


    // node_to_tree: dom -> dom-tree
    // Given a native dom node, produces the appropriate tree.
    function node_to_tree(domNode) {
	var result = [domNode];
	for (var c = domNode.firstChild; c != null; c = c.nextSibling) {
	    result.push(node_to_tree(c));
	}
	return result;
    }
    Jsworld.node_to_tree = node_to_tree;



    // nodes(tree(N)) = nodes(N)
    function nodes(tree) {
	var ret;
	
	if (tree.node.jsworldOpaque == true) {
	    return [tree.node];
	}

	ret = [tree.node];
	for (var i = 0; i < tree.children.length; i++)
	    ret = ret.concat(nodes(tree.children[i]));
	
	return ret;
    }


    // relations(tree(N)) = relations(N)
    function relations(tree) {
	var ret = [];	

	for (var i = 0; i < tree.children.length; i++)
	    ret.push({ relation: 'parent', 
		       parent: tree.node, 
		       child: tree.children[i].node });
	
	for (var i = 0; i < tree.children.length - 1; i++)
	    ret.push({ relation: 'neighbor', 
		       left: tree.children[i].node,
		       right: tree.children[i + 1].node });
	
	if (! tree.node.jsworldOpaque) {
	    for (var i = 0; i < tree.children.length; i++) {
		ret = ret.concat(relations(tree.children[i]));
	    }
	}
	
	return ret;
    }



    var removeAllChildren = function(n) {
	while (n.firstChild) {
	    n.removeChild(n.firstChild);
	}
    }


    // Preorder traversal.
    var preorder = function(node, f) {
	f(node, function() {
	    var child = node.firstChild;
	    var nextSibling;
	    while (child) {
		var nextSibling = child.nextSibling;
		preorder(child, f);
		child = nextSibling;
	    }
	});
    };


    // update_dom(nodes(Node), relations(Node)) = void
    function update_dom(toplevelNode, nodes, relations) {

	// TODO: rewrite this to move stuff all in one go... possible? necessary?
	
	// move all children to their proper parents
	for (var i = 0; i < relations.length; i++) {
	    if (relations[i].relation == 'parent') {
		var parent = relations[i].parent, child = relations[i].child;
		if (child.parentNode !== parent) {
		    parent.appendChild(child);
		}
	    }
	}
	
	// arrange siblings in proper order
	// truly terrible... BUBBLE SORT
	var unsorted = true;
	while (unsorted) {
	    unsorted = false;
	    for (var i = 0; i < relations.length; i++) {
		if (relations[i].relation == 'neighbor') {
		    var left = relations[i].left, right = relations[i].right;
				
		    if (! nodeEq(left.nextSibling, right)) {
			left.parentNode.insertBefore(left, right)
			unsorted = true;
		    }
		}
	    }
	}

	// Finally, remove nodes that shouldn't be attached anymore.
	var nodesPlus = nodes.concat([toplevelNode]);
	preorder(toplevelNode, function(aNode, continueTraversalDown) {
	    if (aNode.jsworldOpaque) {
		if (! isMemq(aNode, nodesPlus)) {
		    aNode.parentNode.removeChild(aNode);
		}
	    } else {
		if (! isMemq(aNode, nodesPlus)) {
		    aNode.parentNode.removeChild(aNode);
		} else {
		    continueTraversalDown();
		}
	    }
	});

	refresh_node_values(nodes);
    }
    

    // isMemq: X (arrayof X) -> boolean
    // Produces true if any of the elements of L are nodeEq to x.
    var isMemq = function(x, L) {
	var i;
	for (i = 0 ; i < L.length; i++) {
	    if (nodeEq(x, L[i])) {
		return true;
	    }
	}
	return false;
    };


    // nodeEq: node node -> boolean
    // Returns true if the two nodes should be the same.
    var nodeEq = function(node1, node2) {
	return (node1 && node2 && node1 === node2);
    }



    // camelCase: string -> string
    function camelCase(name) {
	return name.replace(/\-(.)/g, function(m, l){return l.toUpperCase()});
    }


    function set_css_attribs(node, attribs) {
	for (var j = 0; j < attribs.length; j++){
	    node.style[camelCase(attribs[j].attrib)] = attribs[j].values.join(" ");
	}
    }


    // isMatchingCssSelector: node css -> boolean
    // Returns true if the CSS selector matches.
    function isMatchingCssSelector(node, css) {
	if (css.id.match(/^\./)) {
	    // Check to see if we match the class
	    return ('className' in node && member(node['className'].split(/\s+/),
					      css.id.substring(1)));
	} else {
	    return ('id' in node && node.id == css.id);
	}
    }


    function update_css(nodes, css) {
	// clear CSS
	for (var i = 0; i < nodes.length; i++) {
	    if ( !nodes[i].jsworldOpaque ) {
		    clearCss(nodes[i]);
	    }
	}
	
	// set CSS
	for (var i = 0; i < css.length; i++)
	    if ('id' in css[i]) {
		for (var j = 0; j < nodes.length; j++)
		    if (isMatchingCssSelector(nodes[j], css[i])) {
			set_css_attribs(nodes[j], css[i].attribs);
		    }
	    }
	    else set_css_attribs(css[i].node, css[i].attribs);
    }


    var clearCss = function(node) {
	// FIXME: we should not be clearing the css
// 	if ('style' in node)
// 	    node.style.cssText = "";
    }



    // If any node cares about the world, send it in.
    function refresh_node_values(nodes) {
	for (var i = 0; i < nodes.length; i++) {
	    if (nodes[i].onWorldChange) {
		nodes[i].onWorldChange(world);
	    }
	}
    }



    function do_redraw(world, oldWorld, toplevelNode, redraw_func, redraw_css_func, k) {
	if (oldWorld instanceof InitialWorld) {
	    // Simple path
	    redraw_func(world,
		function(drawn) {
			var t = sexp2tree(drawn);
			var ns = nodes(t);
	    		// HACK: css before dom, due to excanvas hack.
	    		redraw_css_func(world,
				function(css) {
					update_css(ns, sexp2css(css));
					update_dom(toplevelNode, ns, relations(t));
					k();
				});
		});
	} else {
	    maintainingSelection(
		function(k2) {
		    // For legibility, here is the non-CPS version of the same function:
		    /*
			var oldRedraw = redraw_func(oldWorld);
 			var newRedraw = redraw_func(world);	    
 			var oldRedrawCss = redraw_css_func(oldWorld);
			var newRedrawCss = redraw_css_func(world);
			var t = sexp2tree(newRedraw);
 			var ns = nodes(t);

			// Try to save the current selection and preserve it across
			// dom updates.

 			if(oldRedraw !== newRedraw) {
				// Kludge: update the CSS styles first.
				// This is a workaround an issue with excanvas: any style change
				// clears the content of the canvas, so we do this first before
				// attaching the dom element.
				update_css(ns, sexp2css(newRedrawCss));
				update_dom(toplevelNode, ns, relations(t));
 			} else {
				if(oldRedrawCss !== newRedrawCss) {
					update_css(ns, sexp2css(newRedrawCss));
				}
 			}
		    */

		    // We try to avoid updating the dom if the value
		    // hasn't changed.
		    redraw_func(oldWorld,
			function(oldRedraw) {
			    redraw_func(world,
				function(newRedraw) {
				    redraw_css_func(oldWorld,
					function(oldRedrawCss) {
					    redraw_css_func(world,
						function(newRedrawCss) {
						    var t = sexp2tree(newRedraw);
						    var ns = nodes(t);

						    // Try to save the current selection and preserve it across
						    // dom updates.

 						    if(oldRedraw !== newRedraw) {
							// Kludge: update the CSS styles first.
							// This is a workaround an issue with excanvas: any style change
							// clears the content of the canvas, so we do this first before
							// attaching the dom element.
							update_css(ns, sexp2css(newRedrawCss));
							update_dom(toplevelNode, ns, relations(t));
						    } else {
							if (oldRedrawCss !== newRedrawCss) {
							    update_css(ns, sexp2css(newRedrawCss));
							}
						    }
						    k2();
						})
					})
				})
			});
		}, k);
	}
    }


    // maintainingSelection: (-> void) -> void
    // Calls the thunk f while trying to maintain the current focused selection.
    function maintainingSelection(f, k) {
	var currentFocusedSelection;
	if (hasCurrentFocusedSelection()) {
	    currentFocusedSelection = getCurrentFocusedSelection();
	    f(function() {
		currentFocusedSelection.restore();
		k();
	    });
	} else {
	    f(function() { k(); });
	}
    }



    function FocusedSelection() {
	this.focused = currentFocusedNode;
	this.selectionStart = currentFocusedNode.selectionStart;
	this.selectionEnd = currentFocusedNode.selectionEnd;
    }

    // Try to restore the focus.
    FocusedSelection.prototype.restore = function() {
	// FIXME: if we're scrolling through, what's visible
	// isn't restored yet.
	if (this.focused.parentNode) {
	    this.focused.selectionStart = this.selectionStart;
	    this.focused.selectionEnd = this.selectionEnd;
	    this.focused.focus();
	} else if (this.focused.id) {
	    var matching = document.getElementById(this.focused.id);
	    if (matching) {
		matching.selectionStart = this.selectionStart;
		matching.selectionEnd = this.selectionEnd;
		matching.focus();
	    }
	}
    };

    function hasCurrentFocusedSelection() {
	return currentFocusedNode != undefined;
    }

    function getCurrentFocusedSelection() {
	return new FocusedSelection();
    }



    //////////////////////////////////////////////////////////////////////

    function BigBangRecord(top, world, handlerCreators, handlers, attribs) {    
	this.top = top;
	this.world = world;
	this.handlers = handlers;
	this.handlerCreators = handlerCreators;
	this.attribs = attribs;
    }

    BigBangRecord.prototype.restart = function() {
	big_bang(this.top, this.world, this.handlerCreators, this.attribs);
    }
    
    BigBangRecord.prototype.pause = function() {
	for(var i = 0 ; i < this.handlers.length; i++) {
	    if (this.handlers[i] instanceof StopWhenHandler) {
		// Do nothing for now.
	    } else {
		this.handlers[i].onUnregister(top);
	    }
	}
    };
    //////////////////////////////////////////////////////////////////////

    // Notes: big_bang maintains a stack of activation records; it should be possible
    // to call big_bang re-entrantly.
    function big_bang(top, init_world, handlerCreators, attribs, k) {
	// clear_running_state();

	// Construct a fresh set of the handlers.
	var handlers = map(handlerCreators, function(x) { return x();} );
	if (runningBigBangs.length > 0) { 
	    runningBigBangs[runningBigBangs.length - 1].pause();
	}

	// Create an activation record for this big-bang.
	var activationRecord = 
	    new BigBangRecord(top, init_world, handlerCreators, handlers, attribs);
	runningBigBangs.push(activationRecord);
	function keepRecordUpToDate(w, oldW, k2) {
	    activationRecord.world = w;
	    k2();
	}
	add_world_listener(keepRecordUpToDate);



	// Monitor for termination and register the other handlers.
	var stopWhen = new StopWhenHandler(function(w, k2) { k2(false); },
					   function(w, k2) { k2(w); });
	for(var i = 0 ; i < handlers.length; i++) {
	    if (handlers[i] instanceof StopWhenHandler) {
		stopWhen = handlers[i];
	    } else {
		handlers[i].onRegister(top);
	    }
	}
	function watchForTermination(w, oldW, k2) {
	    stopWhen.test(w,
		function(stop) {
		    if (stop) {
			Jsworld.shutdown();
		        k(w);
	/*
			stopWhen.receiver(world,
			    function() {		    
				var currentRecord = runningBigBangs.pop();
				if (currentRecord) { currentRecord.pause(); }
				if (runningBigBangs.length > 0) {
				    var restartingBigBang = runningBigBangs.pop();
				    restartingBigBang.restart();
				}
				k();
			    });
	*/
		    }
		    else { k2(); }
		});
	};
	add_world_listener(watchForTermination);


	// Finally, begin the big-bang.
	copy_attribs(top, attribs);
	change_world(function(w, k2) { k2(init_world); }, doNothing);


    }
    Jsworld.big_bang = big_bang;





    // on_tick: number CPS(world -> world) -> handler
    function on_tick(delay, tick) {
	return function() {
	    var scheduleTick, ticker;


	    (new Date()).valueOf()

	    scheduleTick = function(t) {
		ticker.watchId = setTimeout(
		    function() { 
			ticker.watchId = undefined;
			var startTime = (new Date()).valueOf();
			change_world(tick, 
				     function() { 
					 var endTime = (new Date()).valueOf();
					 scheduleTick(Math.max(delay - (endTime - startTime),
							       0)); 
				     }); 
		    },
		    t);
	    };
	    
	    ticker = {
		watchId: -1,
		onRegister: function (top) { 
		    scheduleTick(delay);
		},

		onUnregister: function (top) {
		    if (ticker.watchId)
			clearTimeout(ticker.watchId);
		}
	    };
	    return ticker;
	};
    }
    Jsworld.on_tick = on_tick;


    function on_key(press) {
	return function() {
	    var wrappedPress = function(e) {
		    preventDefault(e);
		    stopPropagation(e);
		    change_world(function(w, k) { press(w, e, k); }, doNothing);
	    };
	    return {
		onRegister: function(top) { attachEvent(top, 'keydown', wrappedPress); },
		onUnregister: function(top) { detachEvent(top, 'keydown', wrappedPress); }
	    };
	}
    }
    Jsworld.on_key = on_key;


    
    //  on_draw: CPS(world -> (sexpof node)) CPS(world -> (sexpof css-style)) -> handler
    function on_draw(redraw, redraw_css) {
	var wrappedRedraw = function(w, k) {
	    redraw(w, function(newDomTree) {
	    	checkDomSexp(newDomTree, newDomTree);
	    	k(newDomTree);
	    });
	}

	return function() {
	    var drawer = {
		_top: null,
		_listener: function(w, oldW, k2) { 
		    do_redraw(w, oldW, drawer._top, wrappedRedraw, redraw_css, k2); 
		},
		onRegister: function (top) { 
		    drawer._top = top;
		    add_world_listener(drawer._listener);
		},

		onUnregister: function (top) {
		    remove_world_listener(drawer._listener);
		}
	    };
	    return drawer;
	};
    }
    Jsworld.on_draw = on_draw;



    function StopWhenHandler(test, receiver) {
	this.test = test;
	this.receiver = receiver;
    }
    // stop_when: CPS(world -> boolean) CPS(world -> boolean) -> handler
    function stop_when(test, receiver) {
	return function() {
	    if (receiver == undefined) {
		receiver = function(w, k) { k(w); };
	    }
	    return new StopWhenHandler(test, receiver);
	};
    }
    Jsworld.stop_when = stop_when;



    function on_world_change(f) {
	var listener = function(world, oldW, k) { f(world, k); };
	return function() {
	    return { 
		onRegister: function (top) { 
		    add_world_listener(listener); },
		onUnregister: function (top) {
		    remove_world_listener(listener)}
	    };
	};
    }
    Jsworld.on_world_change = on_world_change;





    // Compatibility for attaching events to nodes.
    function attachEvent(node, eventName, fn) {
	if (node.addEventListener) {
	    // Mozilla
	    node.addEventListener(eventName, fn, false);
	} else {
	    // IE
	    node.attachEvent('on' + eventName, fn, false);
	}
    }

    var detachEvent = function(node, eventName, fn) {
	if (node.addEventListener) {
	    // Mozilla
	    node.removeEventListener(eventName, fn, false);
	} else {
	    // IE
	    node.detachEvent('on' + eventName, fn, false);
	}
    }

    //
    // DOM CREATION STUFFS
    //

    // add_ev: node string CPS(world event -> world) -> void
    // Attaches a world-updating handler when the world is changed.
    function add_ev(node, event, f) {
	var eventHandler = function(e) { change_world(function(w, k) { f(w, e, k); },
						       doNothing); };
	attachEvent(node, event, eventHandler);
	eventDetachers.push(function() { detachEvent(node, event, eventHandler); });
    }

    // add_ev_after: node string CPS(world event -> world) -> void
    // Attaches a world-updating handler when the world is changed, but only
    // after the fired event has finished.
    function add_ev_after(node, event, f) {
	var eventHandler = function(e) {
		setTimeout(function() { change_world(function(w, k) { f(w, e, k); },
						     doNothing); },
			   0);
	};

	attachEvent(node, event, eventHandler);
	eventDetachers.push(function() { detachEvent(node, event, eventHandler); });
    }


    function addFocusTracking(node) {
	attachEvent(node, "focus", function(e) {
	    currentFocusedNode = node; });
	attachEvent(node, "blur", function(e) {
	    currentFocusedNode = undefined;
	});
	return node;
    }





    //
    // WORLD STUFFS
    //


    function sexp2tree(sexp) {
	if (isPage(sexp)) {
	    return sexp2tree(node_to_tree(sexp.toDomNode()));
	} else {
	    if(sexp.length == undefined) return { node: sexp, children: [] };
	    else return { node: sexp[0], children: map(sexp.slice(1), sexp2tree) };
	}
    }

    function sexp2attrib(sexp) {
	return { attrib: sexp[0], values: sexp.slice(1) };
    }

    function sexp2css_node(sexp) {
	var attribs = map(sexp.slice(1), sexp2attrib);
	if (typeof sexp[0] == 'string'){
	    return [{ id: sexp[0], attribs: attribs }];
	} else if ('length' in sexp[0]){
	    return map(sexp[0], function (id) { return { id: id, attribs: attribs } });
	} else {
	    return [{ node: sexp[0], attribs: attribs }];
	}
    }

    function sexp2css(sexp) {
	return concat_map(sexp, sexp2css_node);
    }



    function isTextNode(n) {
	return (n.nodeType == Node.TEXT_NODE);
    }


    function isElementNode(n) {
	return (n.nodeType == Node.ELEMENT_NODE);
    }


    var throwDomError = function(thing, topThing) {
	throw new JsworldDomError(
	    helpers.format(
		"Expected a non-empty array, received ~s within ~s",
		[thing, topThing]),
	    thing);
    };

    // checkDomSexp: X X -> boolean
    // Checks to see if thing is a DOM-sexp.  If not,
    // throws an object that explains why not.
    function checkDomSexp(thing, topThing) {
	if (isPage(thing)) {
	    return;
	}

	if (! thing instanceof Array) {
	    throwDomError(thing, topThing);
	}
	if (thing.length == 0) {
	    throwDomError(thing, topThing);
	}

	// Check that the first element is a Text or an element.
	if (isTextNode(thing[0])) {
	    if (thing.length > 1) {
		throw new JsworldDomError(helpers.format("Text node ~s can not have children",
							 [thing]),
					  thing);
	    }
	} else if (isElementNode(thing[0])) {
	    for (var i = 1; i < thing.length; i++) {
		checkDomSexp(thing[i], thing);
	    }
	} else {
	    throw new JsworldDomError(
		helpers.format(
		    "expected a Text or an Element, received ~s within ~s",
		    [thing, topThing]),
		thing[0]);
	}
    }

    function JsworldDomError(msg, elt) {
	this.msg = msg;
	this.elt = elt;
    }
    JsworldDomError.prototype.toString = function() {
	return "on-draw: " + this.msg;
    }





    //
    // DOM CREATION STUFFS
    //


    function copy_attribs(node, attribs) {
	if (attribs)
	    for (a in attribs) {
		if (attribs.hasOwnProperty(a)) {
		    if (typeof attribs[a] == 'function')
			add_ev(node, a, attribs[a]);
		    else{
			node[a] = attribs[a];//eval("node."+a+"='"+attribs[a]+"'");
		    }
		}
	    }
	return node;
    }


    //
    // NODE TYPES
    //

    function p(attribs) {
	return addFocusTracking(copy_attribs(document.createElement('p'), attribs));
    }
    Jsworld.p = p;

    function div(attribs) {
	return addFocusTracking(copy_attribs(document.createElement('div'), attribs));
    }
    Jsworld.div = div;

    // Used To Be: (world event -> world) (hashof X Y) -> domElement
    // Now: CPS(world event -> world) (hashof X Y) -> domElement
    function button(f, attribs) {
	var n = document.createElement('button');
	n.onclick = function(e) {return false;};
	add_ev(n, 'click', f);
	return addFocusTracking(copy_attribs(n, attribs));
    }
    Jsworld.button = button;




    var preventDefault = function(event) {
	if (event.preventDefault) {
	    event.preventDefault();
	} else {
	    event.returnValue = false;
	}
    }

    var stopPropagation = function(event) {
	if (event.stopPropagation) {
	    event.stopPropagation();
	} else {
	    event.cancelBubble = true;
	}
    }


    var stopClickPropagation = function(node) {
	attachEvent(node, "click",
		    function(e) {
			stopPropagation(e);
		    });
	return node;
    }
    

    // input: string CPS(world -> world) 
    function input(aType, updateF, attribs) {
	aType = aType.toLowerCase();
	var dispatchTable = { text : text_input,
			      password: text_input,
			      checkbox: checkbox_input
			      //button: button_input,
			      //radio: radio_input 
	};

	if (dispatchTable[aType]) {
	    return (dispatchTable[aType])(aType, updateF, attribs);
	}
	else {
	    throw new Error("js-input: does not currently support type " + aType);
	}
    }
    Jsworld.input = input;




    var text_input = function(type, updateF, attribs) {
	var n = document.createElement('input');
	n.type = type;

	var lastVal = n.value;
	var onEvent = function() {
	    if (! n.parentNode) { return; }
	    setTimeout(
		function() {
		    if (lastVal != n.value) {
			lastVal = n.value;
			change_world(function (w, k) {
			    updateF(w, n.value, k);
			}, doNothing);
		    }
		},
		0);
	}


// 	attachEvent(n, "keypress", onEvent);
// 	eventDetachers.push(function() {
// 	    detachEvent(n, "keypress", onEvent); });

	attachEvent(n, "keydown", onEvent);
	eventDetachers.push(function() {
	    detachEvent(n, "keydown", onEvent); });

	attachEvent(n, "change", onEvent);
	eventDetachers.push(function() {
	    detachEvent(n, "change", onEvent); });

// 	function onKey(w, e, k) {
// 	    updateF(w, n.value, k);
// 	}
// 	// This established the widget->world direction
// 	add_ev_after(n, 'keypress', onKey);

	// Every second, do a manual polling of the object, just in case.
// 	var delay = 1000;
// 	var intervalId = setInterval(function() {
// 	    if (! n.parentNode) {
// 		clearInterval(intervalId);
// 		return;
// 	    }
// 	    if (lastVal != n.value) {
// 		lastVal = n.value;
// 		change_world(function (w, k) {
// 		    updateF(w, n.value, k);
// 		}, doNothing);
// 	    }
// 	},
// 		    delay);
	return stopClickPropagation(
	    addFocusTracking(copy_attribs(n, attribs)));
    };


    var checkbox_input = function(type, updateF, attribs) {
	var n = document.createElement('input');
	n.type = type;
	var onCheck = function(w, e, k) {
	    updateF(w, n.checked, k);
	};
	// This established the widget->world direction
	add_ev_after(n, 'change', onCheck);
	
 	attachEvent(n, 'click', function(e) {
 	    stopPropagation(e);
 	});

	return copy_attribs(n, attribs);
    };


    var button_input = function(type, updateF, attribs) {
	var n = document.createElement('button');
	add_ev(n, 'click', function(w, e, k) { updateF(w, n.value, k); });
	return addFocusTracking(copy_attribs(n, attribs));
    };



    

    function text(s, attribs) {
	var result = document.createElement("div");
	result.appendChild(document.createTextNode(String(s)));
	result.jsworldOpaque = true;
	return result;
    }
    Jsworld.text = text;

    function select(attribs, opts, f){
	var n = document.createElement('select');
	for(var i = 0; i < opts.length; i++) {
	    n.add(option({value: opts[i]}), null);
	}
	n.jsworldOpaque = true;
	add_ev(n, 'change', f);
	var result = addFocusTracking(copy_attribs(n, attribs));
	return result;
    }
    Jsworld.select = select;

    function option(attribs){
	var node = document.createElement("option");
        node.text = attribs.value;
	node.value = attribs.value;
 	return node;
    }



    function textarea(attribs){
	return addFocusTracking(copy_attribs(document.createElement('textarea'), attribs));
    }
    Jsworld.textarea = textarea;

    function h1(attribs){
	return addFocusTracking(copy_attribs(document.createElement('h1'), attribs));
    }
    Jsworld.h1 = h1;

    function canvas(attribs){
	return addFocusTracking(copy_attribs(document.createElement('canvas'), attribs));	
    }
    Jsworld.canvas = canvas;


    function img(src, attribs) {
	var n = document.createElement('img');
	n.src = src;
	return addFocusTracking(copy_attribs(n, attribs));
    }
    Jsworld.img = img;



    function raw_node(node, attribs) {
	return addFocusTracking(copy_attribs(node, attribs));
    }
    Jsworld.raw_node = raw_node;





    //////////////////////////////////////////////////////////////////////
    //////////////////////////////////////////////////////////////////////
    // Effects

    // An effect is an object with an invokeEffect() method.
    
    var WrappedWorldWithEffects = function(w, effects) {
	if (w instanceof WrappedWorldWithEffects) {
	    this.w = w.w;
	    this.e = w.e.concat(effects);
	} else {
	    this.w = w;
	    this.e = effects;
	}
    };

    WrappedWorldWithEffects.prototype.getWorld = function() {
	return this.w;
    };

    WrappedWorldWithEffects.prototype.getEffects = function() {
	return this.e;
    };


    //////////////////////////////////////////////////////////////////////

    Jsworld.with_effect = function(w, e) {
	return new WrappedWorldWithEffects(w, [e]);
    };

    Jsworld.with_multiple_effects = function(w, effects) {
	return new WrappedWorldWithEffects(w, effects);
    };

    Jsworld.has_effects = function(w) {
	return w instanceof WrappedWorldWithEffects;
    };




    //////////////////////////////////////////////////////////////////////
    // Example effect: raise an alert.
    Jsworld.alert_effect = function(msg) {
	return new AlertEffect(msg);
    };

    var AlertEffect = function(msg) {
	this.msg = msg;
    };

    AlertEffect.prototype.invokeEffect = function(k) {
	alert(this.msg);
	k();
    };


    //////////////////////////////////////////////////////////////////////


    // Example effect: play a song, given its url
    Jsworld.music_effect = function(musicUrl) {
	return new MusicEffect(musicUrl);
    };

    var MusicEffect = function(musicUrl) {
	this.musicUrl = musicUrl;
    };

    MusicEffect.prototype.invokeEffect = function(k) {
	new Audio(url).play();
	k();
    };





    //////////////////////////////////////////////////////////////////////
    // Pages


    var Page = function(elts, attribs) {
	if (typeof(elts) === 'undefined') { 
	    elts = [];
	}
	this.elts = elts;
	this.attribs = attribs;
    };

    Page.prototype.add = function(elt, positionLeft, positionTop) {
	return new Page(this.elts.concat([{elt: elt, 
					   positionTop: positionTop,
					   positionLeft: positionLeft}]),
			this.attribs);
    };

    Page.prototype.toDomNode = function() {
	var aDiv = div();
	for (var i = 0 ; i < this.elts.length; i++) {
	    var elt = this.elts[i].elt;
	    if (! elt.style) {
		elt.style = '';
	    }

	    elt.style.position = 'absolute';
	    elt.style.left = this.elts[i].positionLeft + "px";
	    elt.style.top = this.elts[i].positionTop + "px";	    
	    aDiv.appendChild(elt);
	};
	copy_attribs(aDiv, this.attribs)
	return aDiv;
    };


    isPage = function(x) {
	return x instanceof Page;
    };

    Jsworld.isPage = isPage;

    Jsworld.emptyPage = function(attribs) {
	var result = new Page([], attribs);
	return result;
    };

    Jsworld.placeOnPage = function(elt, positionLeft, positionTop, page) {
	if (typeof(elt) === 'string') {
	    elt = text(elt);
	}
	return page.add(elt, positionLeft, positionTop);
    };



})();
