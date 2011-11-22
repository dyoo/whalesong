/*jslint browser: true, unparam: true, vars: true, white: true, plusplus: true, maxerr: 50, indent: 4, forin: true */
/*global plt,MACHINE,$,EXPORTS,TreeCursor*/
(function() {

    "use strict";

    var makePrimitiveProcedure = plt.baselib.functions.makePrimitiveProcedure;
    var makeClosure = plt.baselib.functions.makeClosure;
    var finalizeClosureCall = plt.baselib.functions.finalizeClosureCall;
    var PAUSE = plt.runtime.PAUSE;
    var isString = plt.baselib.strings.isString;
    var isSymbol = plt.baselib.symbols.isSymbol;
    var isList = plt.baselib.lists.isList;
    var isEmpty = plt.baselib.lists.isEmpty;
    var listLength = plt.baselib.lists.length;
    var makeList = plt.baselib.lists.makeList;
    var makePair = plt.baselib.lists.makePair;
    var makeSymbol = plt.baselib.symbols.makeSymbol;



    // EventHandler and the other classes here will be defined below.
    // We're just trying to keep jslint happy.
    var EventHandler, DomEventSource;



    // FIXME: as soon as we get real parameters, use parameters
    // instead.  Global: defines the currently running big bang.
    // Parameterized around the call to bigBang.
    var currentBigBangRecord;



    var resourceStructType = 
        MACHINE.modules['whalesong/resource/structs.rkt'].namespace['struct:resource'];

    var eventStructType = 
        MACHINE.modules['whalesong/web-world/event.rkt'].namespace['struct:event'];





    var shallowCloneNode = function(node) {
        var result = node.cloneNode(false);
        var i;
        // copy over the attributes as well
        if (node.attributes) {
            for (i = 0; i < node.attributes.length; i++) {
                console.log('copying: ', node.attributes[i]);
                $(result).attr(node.attributes[i].name,
                               node.attributes[i].value);
            }
        }
        $(result).data($(node).data());
        return result;
    };



    //////////////////////////////////////////////////////////////////////
    

    // domNodeToArrayTree: dom -> dom-tree
    // Given a native dom node, produces the appropriate array tree representation
    var domNodeToArrayTree = function(domNode) {
        var result = [domNode];
        var c;
        for (c = domNode.firstChild; c !== null; c = c.nextSibling) {
	    result.push(domNodeToArrayTree(c));
        }
        return result;
    };


    var arrayTreeToDomNode = function(tree) {
        var result = shallowCloneNode(tree[0]);
        var i;
        for (i = 1; i < tree.length; i++) {
            result.appendChild(arrayTreeToDomNode(tree[i]));
        }
        return result;
    };


    var domToArrayTreeCursor = function(dom) {
        var domOpenF = 
            // To go down, just take the children.
            function(tree) { 
                return tree.slice(1);
            };
        var domCloseF = 
            // To go back up, take the tree and reconstruct it.
            function(tree, children) { 
                return [tree[0]].concat(children);
            };
        var domAtomicF =
            function(tree) {
                return tree[0].nodeType !== 1;
            };
        return TreeCursor.adaptTreeCursor(domNodeToArrayTree($(dom).clone(true).get(0)),
                                          domOpenF,
                                          domCloseF,
                                          domAtomicF);
    };

    var treeText = function(tree) {
        var text = [];
        var visit = function(tree) {
            var i;
            if (tree[0].nodeType === 3) {
                text.push(tree[0].nodeValue);
            }
            for (i = 1; i < tree.length; i++) {
                visit(tree[i]);
            }
        };
        visit(tree);
        return text.join('');
    };

    //////////////////////////////////////////////////////////////////////

    

    // For the moment, we only support selection by id.
    var selectorMatches = function(selector, tree) {
        if (tree[0].nodeType === 1) {
            return tree[0].getAttribute('id') === selector;
        } else {
            return false;
        }
    };

    var EMPTY_PENDING_ACTIONS = plt.baselib.lists.EMPTY;


    //////////////////////////////////////////////////////////////////////
    // A MockView provides a functional interface to the DOM.  It
    // includes a cursor to the currently focused dom, the pending
    // actions to perform on the actual view, and a nonce to detect
    // freshness of the MockView.
    var MockView = function(cursor, pendingActions, eventHandlers, nonce) {
        this.cursor = cursor;

        // (listof (view -> void))
        this.pendingActions = pendingActions;

        this.eventHandlers = eventHandlers;
        this.nonce = nonce;
    };

    var isMockView = plt.baselib.makeClassPredicate(MockView);

    MockView.prototype.toString = function() {
        return "<#view>";
    };

    MockView.prototype.getPendingActions = function() {
        return plt.baselib.lists.listToArray(this.pendingActions).reverse();
    };


    MockView.prototype.act = function(actionForCursor, actionForEventHandlers, actionForReal) {
        if (arguments.length !== 3) { throw new Error("act: insufficient arguments"); }
        return new MockView(actionForCursor(this.cursor),
                            plt.baselib.lists.makePair(actionForReal, this.pendingActions),
                            actionForEventHandlers(this.eventHandlers),
                            this.nonce);
    };


    MockView.prototype.updateFocus = function(selector) {
        selector = selector.toString();
        return this.act(
            function(cursor) {
                var c = cursor.top();
                while (true) {
                    if (selectorMatches(selector, c.node)) {
                        return c;
                    }
                    if (c.canSucc()) {
                        c = c.succ();
                    } else {
                        throw new Error("unable to find " + selector);
                    }
                }
            },
            function(eventHandlers) { return eventHandlers; },
            function(view) {
                view.focus = document.getElementById(selector);
            }
        );
    };

    MockView.prototype.getText = function() {        
        var tree = this.cursor.node;
        return treeText(tree);
    };

    MockView.prototype.updateText = function(text) {
        return this.act(
            function(cursor) {
                return cursor.replaceNode([cursor.node[0]]
                                          .concat([[document.createTextNode(text)]]));
            },
            function(eventHandlers) { return eventHandlers; },
            function(view) {
                $(view.focus).text(text);
            }
        );
    };

    MockView.prototype.getAttr = function(name) {        
        return $(this.cursor.node[0]).attr(name);
    };


    MockView.prototype.updateAttr = function(name, value) {
        return this.act(
            function(cursor) {
                return cursor.replaceNode([$(shallowCloneNode(cursor.node[0]))
                                           .attr(name, value).get(0)]
                                          .concat(cursor.node.slice(1)));
            },
            function(eventHandlers) {
                return eventHandlers;
            },
            function(view) {
                $(view.focus).attr(name, value);
            });
    };

    MockView.prototype.removeAttr = function(name) {
        return this.act(
            function(cursor) {
                return cursor.replaceNode([$(shallowCloneNode(cursor.node[0]))
                                           .removeAttr(name).get(0)]
                                          .concat(cursor.node.slice(1)));
            },
            function(eventHandlers) {
                return eventHandlers;
            },
            function(view) {
                $(view.focus).removeAttr(name);
            });
    };






    MockView.prototype.getCss = function(name) {        
        return $(this.cursor.node[0]).css(name);
    };


    MockView.prototype.updateCss = function(name, value) {
        return this.act(
            function(cursor) {
                return cursor.replaceNode([$(shallowCloneNode(cursor.node[0]))
                                           .css(name, value).get(0)]
                                          .concat(cursor.node.slice(1)));
            },
            function(eventHandlers) {
                return eventHandlers;
            },
            function(view) {
                $(view.focus).css(name, value);
            });
    };



    MockView.prototype.getFormValue = function() {        
        return $(this.cursor.node[0]).val();
    };

    MockView.prototype.updateFormValue = function(value) {        
        return this.act(
            function(cursor) {
                return cursor.replaceNode([$(shallowCloneNode(cursor.node[0]))
                                           .val(value).get(0)]
                                          .concat(cursor.node.slice(1)));
            },
            function(eventHandlers) {
                return eventHandlers;
            },
            function(view) {
                $(view.focus).val(value);
            });
    };



    MockView.prototype.left = function() {
        return this.act(
            function(cursor) {
                return cursor.left();
            },
            function(eventHandlers) {
                return eventHandlers;
            },
            function(view) {
                view.focus = view.focus.previousSibling;
            });
    };

    MockView.prototype.right = function() {
        return this.act(
            function(cursor) {
                return cursor.right();
            },
            function(eventHandlers) {
                return eventHandlers;
            },
            function(view) {
                view.focus = view.focus.nextSibling;
            });
    };

    MockView.prototype.up = function() {
        return this.act(
            function(cursor) {
                return cursor.up();
            },
            function(eventHandlers) {
                return eventHandlers;
            },
            function(view) {
                view.focus = view.focus.parentNode;
            });
   };

    MockView.prototype.down = function() {
        return this.act(
            function(cursor) {
                return cursor.down();
            },
            function(eventHandlers) {
                return eventHandlers;
            },
            function(view) {
                view.focus = view.focus.firstChild;
            });
    };


    MockView.prototype.forward = function() {
        return this.act(
            function(cursor) {
                return cursor.succ();
            },
            function(eventHandlers) {
                return eventHandlers;
            },
            function(view) {
                if (view.focus.firstChild) {
                    view.focus = view.focus.firstChild;
                } else if (view.focus.nextSibling) {
                    view.focus = view.focus.nextSibling;
                } else {
                    while (view.focus !== view.top) {
                        view.focus = view.focus.parentNode;
                        if (view.focus.nextSibling) {
                            view.focus = view.focus.nextSibling;
                            return;
                        }
                    }
                }
            });
};

    MockView.prototype.backward = function() {
        return this.act(
            function(cursor) {
                return cursor.pred();
            },
            function(eventHandlers) {
                return eventHandlers;
            },
            function(view) {
                if (view.focus.previousSibling) {
                    view.focus = view.focus.previousSibling;
                    while (view.focus.children().length > 0) {
                        view.focus = view.focus.firstChild;
                        while(view.focus.nextSibling) { view.focus = view.focus.nextSibling; }
                    }
                } else {
                    view.focus = view.focus.parentNode;
                }

            });
    };







    
    var mockViewIdGensym = 0;

    MockView.prototype.bind = function(name, worldF) {
        var that = this;

        // HACK: every node that is bound needs to have an id.  We
        // enforce this by mutating the node.
        if (! this.cursor.node[0].id) {
            this.cursor.node[0].id = ("__webWorldId_" + mockViewIdGensym++);
        }   
        return this.act(
            function(cursor) {
                return cursor;
            },
            function(eventHandlers) {
                var handler = new EventHandler(name,
                                               new DomEventSource(
                                                   name,
                                                   that.cursor.node[0].id),
                                               worldF);
                var newHandlers = eventHandlers.concat([handler]);
                return newHandlers;
            },
            function(view) {
                // HACK: every node that is bound needs to have an id.  We
                // enforce this by mutating the node.
                if (! view.focus.id) {
                    view.focus.id = ("__webWorldId_" + mockViewIdGensym++);
                }
                var handler = new EventHandler(name, 
                                               new DomEventSource(
                                                   name, 
                                                   view.focus.id),
                                               worldF);
                view.addEventHandler(handler);
                currentBigBangRecord.startEventHandler(handler);
            });
    };

    MockView.prototype.show = function() {
        return this.act(
            function(cursor) {
                return cursor.replaceNode([$(shallowCloneNode(cursor.node[0]))
                                           .show().get(0)]
                                          .concat(cursor.node.slice(1)));
            },
            function(eventHandlers) { return eventHandlers; },
            function(view) {
                $(view.focus).show();
            }
        );
    };

    MockView.prototype.hide = function() {
        return this.act(
            function(cursor) {
                return cursor.replaceNode([$(shallowCloneNode(cursor.node[0]))
                                           .hide().get(0)]
                                          .concat(cursor.node.slice(1)));
            },
            function(eventHandlers) { return eventHandlers; },
            function(view) {
                $(view.focus).hide();
            }
        );
    };


    MockView.prototype.remove = function() {
        return this.act(
            function(cursor) {
                return cursor.deleteNode();
            },
            function(eventHandlers) {
                return eventHandlers;
            },
            function(view) {
                var elt = view.focus;
                if (view.focus.nextSibling) {
                    view.focus = view.focus.nextSibling;
                } else if (view.focus.previousSibling) {
                    view.focus = view.focus.previousSibling;
                } else {
                    view.focus = view.focus.parentNode;
                }
                $(elt).remove();
            });
    };


    MockView.prototype.appendChild = function(domNode) {
        return this.act(
            function(cursor) {
                if (cursor.canDown()) {
                    cursor = cursor.down();
                    while (cursor.canRight()) {
                        cursor = cursor.right();
                    }
                    return cursor.insertRight(domNodeToArrayTree(domNode));
                } else {
                    return cursor.insertDown(domNodeToArrayTree(domNode));
                }
            },
            function(eventHandlers) { return eventHandlers; },
            function(view) {
                var clone = $(domNode).clone(true);
                clone.appendTo($(view.focus));
                view.focus = clone.get(0);
            }
        );
    };

    MockView.prototype.insertRight = function(domNode) {
        return this.act(
            function(cursor) {
                return cursor.insertRight(domNodeToArrayTree(domNode));
            },
            function(eventHandlers) { return eventHandlers; },
            function(view) {
                var clone = $(domNode).clone(true);
                clone.insertAfter($(view.focus));
                view.focus = clone.get(0);
            }
        );
    };

    MockView.prototype.insertLeft = function(domNode) {
        return this.act(
            function(cursor) {
                return cursor.insertLeft(domNodeToArrayTree(domNode));
            },
            function(eventHandlers) { return eventHandlers; },
            function(view) {
                var clone = $(domNode).clone(true);
                clone.insertBefore($(view.focus));
                view.focus = clone.get(0);
            }
        );
    };



    MockView.prototype.id = function() {
        return this.cursor.node[0].id;
    };

    MockView.prototype.isUpMovementOk = function() {
        return this.cursor.canUp();
    };

    MockView.prototype.isDownMovementOk = function() {
        return this.cursor.canDown();
    };

    MockView.prototype.isLeftMovementOk = function() {
        return this.cursor.canLeft();
    };

    MockView.prototype.isRightMovementOk = function() {
        return this.cursor.canRight();
    };

    MockView.prototype.isForwardMovementOk = function() {
        return this.cursor.canSucc();
    };

    MockView.prototype.isBackwardMovementOk = function() {
        return this.cursor.canPred();
    };


    //////////////////////////////////////////////////////////////////////


    
    

    // A View represents a representation of the DOM tree.
    var View = function(top, eventHandlers) {
        // top: dom node
        this.top = top;
        // focus: dom node
        this.focus = top;
        this.eventHandlers = eventHandlers;
    };

    View.prototype.toString = function() { return "#<View>"; };


    var defaultToRender = function(){};
 
   View.prototype.initialRender = function(top) {
       $(top).empty();
       // Special case: if this.top is an html, we merge into the
       // existing page.
       if ($(this.top).children("title").length !== 0) {
           $(document.head).find('title').remove();
       }
       $(document.head).append($(this.top).children("title").clone(true));
       $(document.head).append($(this.top).children("link").clone(true));
       
       $(top).append($(this.top));

       // The snip here is meant to accomodate weirdness with canvas dom
       // elements.  cloning a canvas doesn't preserve how it draws.
       // However, we attach a toRender using jQuery's data(), which does
       // do the preservation we need.  On an initial render, we walk
       // through all the elements and toRender them.

       // It may be that this will deprecate the afterAttach stuff
       // that I'm using earlier.
       ($(this.top).data('toRender') || defaultToRender)();
       $('*', this.top).each(
           function(index, elt) {
               ($(elt).data('toRender') || defaultToRender).call(elt);
           });
   };


    View.prototype.addEventHandler = function(handler) {
        this.eventHandlers.push(handler);
    };

    // Return a list of the event sources from the view.
    // fixme: may need to apply the pending actions to get the real set.
    View.prototype.getEventHandlers = function() {
        return this.eventHandlers;
    };

    View.prototype.getMockAndResetFocus = function(nonce) {
        this.focus = this.top;
        return new MockView(domToArrayTreeCursor($(this.top).get(0)),
                            EMPTY_PENDING_ACTIONS,
                            this.eventHandlers.slice(0),
                            nonce);
    };





    var isView = plt.baselib.makeClassPredicate(View);
    var isResource = resourceStructType.predicate;


    // var resourcePath = function(r) { return resourceStructType.accessor(r, 0); };
    // var resourceKey = function(r) { return resourceStructType.accessor(r, 1); };
    var resourceContent = function(r) { return resourceStructType.accessor(r, 2); };



    var rscript = /<script\b[^<]*(?:(?!<\/script>)<[^<]*)*<\/script>/gi;

    // We have to do some kludgery to support the android browser,
    // which does not properly parse <link ...>.
    var rlink = /<link\b[^\/>]* \/>(.*?)/gi;

    var parseStringAsHtml = function(str) {
        var div = document.createElement("div");
        // inject the contents of the document in, removing the scripts
	// to avoid any 'Permission Denied' errors in IE
        div.innerHTML = str.replace(rscript, "").replace(rlink, "");
        var linkMatches = str.match(rlink);
        if (linkMatches) {
            for (var i = 0; i < linkMatches.length; i++) {
                $(div).append($(linkMatches[i]));
            }
        }
        return $(div);
    };

 
    // coerseToView: (U resource View) -> View
    // Coerse a value into a view.
    var coerseToView = function(x, onSuccess, onFail) {
        var dom;
        if (isView(x)) { 
            return onSuccess(x); 
        } else  if (isResource(x)) {
            try {
                dom = parseStringAsHtml(resourceContent(x).toString())
                    .css("margin", "0px")
                    .css("padding", "0px")
                    .css("border", "0px");
                dom.children("body").css("margin", "0px");
            } catch (exn1) {
                return onFail(exn1);
            }
            return onSuccess(new View(dom.get(0), []));
        } else if (isMockView(x)) {
            return onSuccess(new View(arrayTreeToDomNode(x.cursor.top().node),
                                      x.eventHandlers.slice(0)));
        } else {
            try {
                dom = plt.baselib.format.toDomNode(x);
            } catch (exn2) {
                return onFail(exn2);
            }
            return onSuccess(new View(dom, []));
        }
    };

    var coerseToMockView = function(x, onSuccess, onFail) {
        var dom;
        if (isMockView(x)) { 
            return onSuccess(x); 
        } else  if (isResource(x)) {
            try {
                dom = parseStringAsHtml(resourceContent(x).toString())
                    .css("margin", "0px")
                    .css("padding", "0px")
                    .css("border", "0px");
                dom.children("body").css("margin", "0px");
            } catch (exn1) {
                return onFail(exn1);
            }
            return onSuccess(new MockView(domToArrayTreeCursor(dom.get(0)),
                                          EMPTY_PENDING_ACTIONS,
                                          [], 
                                          undefined));
        } else {
            try {
                dom = plt.baselib.format.toDomNode(x);
            } catch (exn2) {
                return onFail(exn2);
            }
            return onSuccess(new MockView(domToArrayTreeCursor(dom), 
                                          EMPTY_PENDING_ACTIONS, 
                                          [],
                                          undefined));
        }
    };


    var isDomNode = function(x) {
        return (x.nodeType === 1);
    };


    var coerseToDomNode = function(x, onSuccess, onFail) {
        var dom;
        if (isDomNode(x)) { 
            return onSuccess(x); 
        } else  if (isResource(x)) {
            try {
                dom = parseStringAsHtml(resourceContent(x).toString())
                    .css("margin", "0px")
                    .css("padding", "0px")
                    .css("border", "0px");
            } catch (exn1) {
                return onFail(exn1);
            }
            return onSuccess(dom.get(0));
        } else if (isMockView(x)) {
            return onSuccess(arrayTreeToDomNode(x.cursor.top().node));
        } else {
            try {
                dom = plt.baselib.format.toDomNode(x);
            } catch (exn2) {
                return onFail(exn2);
            }
            return onSuccess(dom);
        }
    };








    //////////////////////////////////////////////////////////////////////
    //
    // The inputs into a big bang are WorldHandlers, which configure the big
    // bang in terms of the initial view, the inputs, event sources, etc.
    //

    var WorldHandler = function() {};
    var isWorldHandler = plt.baselib.makeClassPredicate(WorldHandler);


    var InitialViewHandler = function(view) {
        WorldHandler.call(this);
        // view: View
        this.view = view;
    };

    InitialViewHandler.prototype = plt.baselib.heir(WorldHandler.prototype);
    InitialViewHandler.prototype.toString = function() { return "#<initial-view>"; };
    var isInitialViewHandler = plt.baselib.makeClassPredicate(InitialViewHandler);


    var StopWhenHandler = function(stopWhen) {
        WorldHandler.call(this);
        // stopWhen: Racket procedure (World -> boolean)
        this.stopWhen = stopWhen;
    };

    StopWhenHandler.prototype = plt.baselib.heir(WorldHandler.prototype);
    StopWhenHandler.prototype.toString = function() { return "#<stop-when>"; };
    var isStopWhenHandler = plt.baselib.makeClassPredicate(StopWhenHandler);



    var ToDrawHandler = function(toDraw) {
        WorldHandler.call(this);
        // toDraw: Racket procedure (World View -> View)
        this.toDraw = toDraw;
    };

    ToDrawHandler.prototype = plt.baselib.heir(WorldHandler.prototype);
    ToDrawHandler.prototype.toString = function() { return "#<to-draw>"; };
    var isToDrawHandler = plt.baselib.makeClassPredicate(ToDrawHandler);





    // An EventHandler combines a EventSource with a racketWorldCallback.
    EventHandler = function(name, eventSource, racketWorldCallback) {
        WorldHandler.call(this);
        this.name = name;
        this.eventSource = eventSource;
        this.racketWorldCallback = racketWorldCallback;
    };
    EventHandler.prototype = plt.baselib.heir(WorldHandler.prototype);
    EventHandler.prototype.toString = function() { return "#<" + this.name + ">"; };
    var isEventHandler = plt.baselib.makeClassPredicate(EventHandler);



    var WithOutputToHandler = function(outputPort) {
        this.outputPort = outputPort;
    };
    WithOutputToHandler.prototype = plt.baselib.heir(WorldHandler.prototype);
    var isWithOutputToHandler = plt.baselib.makeClassPredicate(WithOutputToHandler);


    //////////////////////////////////////////////////////////////////////


    var find = function(handlers, pred) {
        var i;
        for (i = 0; i < handlers.length; i++) {
            if (pred(handlers[i])) {
                return handlers[i];
            }
        }
        return undefined;
    };

    var filter = function(handlers, pred) {
        var i, lst = [];
        for (i = 0; i < handlers.length; i++) {
            if (pred(handlers[i])) {
                lst.push(handlers[i]);
            }
        }
        return lst;
    };




    // convert an object to an event.
    // At the moment, we only copy over those values which are numbers or strings.
    var objectToEvent = function(obj) {
        var key, val;
        var result = makeList();
        // Note: for some reason, jslint is not satisfied that I check
        // that the object has a hasOwnProperty before I use it.  I've intentionally
        // turned off jslint's forin check because it's breaking here:
        for (key in obj) {
            if (obj.hasOwnProperty && obj.hasOwnProperty(key)) {
                val = obj[key];
                if (typeof(val) === 'number') {
                    result = makePair(makeList(makeSymbol(key),
                                               plt.baselib.numbers.makeFloat(val)),
                                      result);
                } else if (typeof(val) === 'string') {
                    result = makePair(makeList(makeSymbol(key), val),
                                      result);
                }                         
            }
        }
        return eventStructType.constructor(result);
    };





    /* Event sources.
       
       An event source is a way to send input to a web-world program.

       An event source may be started or stopped.


       Pause and Unpause are semantically meant to be cheaper than start, stop, so
       that's why they're a part of this API.
    */

    var EventSource = function() {};
    EventSource.prototype.onStart = function(fireEvent) {
    };

    EventSource.prototype.onStop = function() {
    };


    

    // TickEventSource sends tick events.
    var TickEventSource = function(delay) {
        this.delay = delay; // delay in milliseconds.

        this.id = undefined;
        // either undefined, or an integer representing the
        // id to cancel a timeout.
    };

    TickEventSource.prototype = plt.baselib.heir(EventSource.prototype);

    TickEventSource.prototype.onStart = function(fireEvent) {
        if (this.id === undefined) {
            this.id = setInterval(
                function(evt) {
                    fireEvent(undefined,
                              objectToEvent(evt));
                },
                this.delay);
        }
    };

    TickEventSource.prototype.onStop = function() {
        if (this.id !== undefined) {
            clearInterval(this.id);
            this.id = undefined;
        }
    };






    var MockLocationEventSource = function() {
        this.elt = undefined;
    };
    MockLocationEventSource.prototype = plt.baselib.heir(EventSource.prototype);
    MockLocationEventSource.prototype.onStart = function(fireEvent) {
        if (this.elt === undefined) {
            var mockLocationSetter = document.createElement("div");
	    
            var latInput = document.createElement("input");
            latInput.type = "text";
	    
            var latOutput = document.createElement("input");
            latOutput.type = "text";
            
            var submitButton = document.createElement("input");
            submitButton.type = "button";
            submitButton.value = "send lat/lng";
            submitButton.onclick = function() {
                fireEvent(undefined,
                          objectToEvent({ latitude: Number(latInput.value),
                                          longitude: Number(latOutput.value)}));
                return false;
            };
	    
            mockLocationSetter.style.border = "1pt solid black";
            mockLocationSetter.appendChild(
                document.createTextNode("mock location setter"));
            mockLocationSetter.appendChild(latInput);
            mockLocationSetter.appendChild(latOutput);
            mockLocationSetter.appendChild(submitButton);
            document.body.appendChild(mockLocationSetter);

            this.elt = mockLocationSetter;
        }
    };

    MockLocationEventSource.prototype.onStop = function() {
        if (this.elt !== undefined) { 
            document.body.removeChild(this.elt);
            this.elt = undefined;
        }
    };

    



    // This version really does use the geolocation object.
    var LocationEventSource = function() {
        this.id = undefined;
    };

    LocationEventSource.prototype = plt.baselib.heir(EventSource.prototype);

    LocationEventSource.prototype.onStart = function(fireEvent) {
        var that = this;
        if (this.id === undefined) {
            var success = function(position) {
                if (position.hasOwnProperty &&
                    position.hasOwnProperty('coords') &&
                    position.coords.hasOwnProperty &&
                    position.coords.hasOwnProperty('latitude') &&
                    position.coords.hasOwnProperty('longitude')) {
                    fireEvent(undefined,
                              objectToEvent({ 'latitude' : Number(position.coords.latitude),
                                              'longitude' : Number(position.coords.longitude) }));
                }
            };
            var fail = function(err) {
                // Quiet failure
            };
            // If we fail while trying to watch the position
            // using high accuracy, switch over to the coarse one.
            var onFailSwitchoverToCoerse = function() {
                navigator.geolocation.clearWatch(that.id);
                that.id = navigator.geolocation.watchPosition(
                    success,
                    fail);
            };
            if (!!(navigator.geolocation)) {
                navigator.geolocation.getCurrentPosition(success, fail);
                this.id = navigator.geolocation.watchPosition(
                    success,
                    onFailSwitchoverToCoerse,
                    { enableHighAccuracy : true,
                      // Try every ten seconds
                      maximumAge : 10000}); 
            }
        }
    };
    
    LocationEventSource.prototype.onStop = function() {
        if (this.id !== undefined) { 
            navigator.geolocation.clearWatch(this.id);
            this.id = undefined;
        }
    };






    // DomElementSource: string (U DOM string) -> EventSource
    // A DomEventSource allows DOM elements to send events over to
    // web-world.
    DomEventSource = function(type, elementOrId) {
        this.type = type;
        this.elementOrId = elementOrId;
        this.handler = undefined;
    };

    DomEventSource.prototype = plt.baselib.heir(EventSource.prototype);

    DomEventSource.prototype.onStart = function(fireEvent) {
        var element = this.elementOrId;
        if (typeof(this.elementOrId) === 'string') {
            element = document.getElementById(this.elementOrId);
        }

        if (! element) { return; }
        if (this.handler !== undefined) {
            $(element).unbind(this.type, this.handler);
            this.handler = undefined;
        }

        this.handler = function(evt) {
            if (element !== undefined) {
                fireEvent(element, objectToEvent(evt));
            }
        };
        $(element).bind(this.type, this.handler);
    };


    DomEventSource.prototype.onStop = function() {
        var element = this.elementOrId;
        if (typeof(this.elementOrId) === 'string') {
            element = document.getElementById(this.elementOrId);
        }

        if (this.handler !== undefined) {
            if (element !== undefined) {
                $(element).unbind(this.type, this.handler);
            }
            this.handler = undefined;
        }
    };







    var EventQueue = function() {
        this.elts = [];
    };
    EventQueue.prototype.queue = function(elt) {
        this.elts.push(elt);
    };

    EventQueue.prototype.dequeue = function() {
        return this.elts.shift();
    };

    EventQueue.prototype.isEmpty = function() {
        return this.elts.length === 0;
    };


    var EventQueueElement = function(who, handler, data) {
        this.who = who;
        this.handler = handler;
        this.data = data;
    };



    var defaultToDraw = function(MACHINE, world, view, success, fail) {
        coerseToMockView(world,
                         success,
                         fail);
    };


    var defaultStopWhen = function(MACHINE, world, view, success, fail) {
        return success(false);
    };


    // bigBang.
    var bigBang = function(MACHINE, world, handlers) {
        var oldCurrentBigBangRecord = currentBigBangRecord;

        var running = true;
        var dispatchingEvents = false;

        var top = $("<div/>").get(0);
        var view = (find(handlers, isInitialViewHandler) ||
                    { view : new View($('<div/>').get(0), []) }).view;
        var stopWhen = (find(handlers, isStopWhenHandler) ||
                        { stopWhen: defaultStopWhen }).stopWhen;
        var toDraw = (find(handlers, isToDrawHandler) || {toDraw : defaultToDraw} ).toDraw;

        var oldOutputPort = MACHINE.params.currentOutputPort;

        var eventQueue = new EventQueue();
        var eventHandlers = filter(handlers, isEventHandler).concat(view.getEventHandlers());
        view.eventHandlers = eventHandlers;

        MACHINE.params.currentDisplayer(MACHINE, top);
        
        // From this point forward, redirect standard output if requested.
        if (find(handlers, isWithOutputToHandler)) {
            MACHINE.params.currentOutputPort = find(handlers, isWithOutputToHandler).outputPort;
        }

        PAUSE(function(restart) {
            var onCleanRestart, onMessyRestart, 
            startEventHandlers, stopEventHandlers, 
            startEventHandler, stopEventHandler,
            dispatchEventsInQueue, refreshView;

            onCleanRestart = function() {
                running = false;
                stopEventHandlers();
                restart(function(MACHINE) {
                    MACHINE.params.currentOutputPort = oldOutputPort;
                    currentBigBangRecord = oldCurrentBigBangRecord;
                    finalizeClosureCall(MACHINE, world);
                });
            };

            onMessyRestart = function(exn) {
                running = false;
                stopEventHandlers();
                restart(function(MACHINE) {
                    currentBigBangRecord = oldCurrentBigBangRecord;
                    MACHINE.params.currentOutputPort = oldOutputPort;
                    plt.baselib.exceptions.raise(MACHINE, exn);
                });
            };

            startEventHandlers = function() {
                var i;
                for (i = 0; i < eventHandlers.length; i++) {
                    startEventHandler(eventHandlers[i]);
                }
            };

            stopEventHandlers = function() {
                var i;
                for (i = 0; i < eventHandlers.length; i++) {
                    stopEventHandler(eventHandlers[i]);
                }
            };

            startEventHandler = function(handler) {
                var fireEvent = function(who) {
                    if (! running) { return; }
                    var args = [].slice.call(arguments, 1);
                    eventQueue.queue(new EventQueueElement(who, handler, args));
                    if (! dispatchingEvents) {
                        dispatchingEvents = true;
                        setTimeout(
                            function() { 
                                dispatchEventsInQueue(
                                    function() {
                                        refreshView(function() {}, onMessyRestart);
                                    }, 
                                    onMessyRestart);
                            },
                            0);
                    }
                };
                handler.eventSource.onStart(fireEvent);
            };

            stopEventHandler = function(handler) {
                handler.eventSource.onStop();
            };


            dispatchEventsInQueue = function(success, fail) {
                // Apply all the events on the queue, call toDraw, and then stop.
                // If the world ever satisfies stopWhen, stop immediately and quit.
                var nextEvent;
                var data;
                var racketWorldCallback;
                var mockView;
                dispatchingEvents = true;
                if(! eventQueue.isEmpty() ) {
                    // Set up the proxy object so we can do what appear to be functional
                    // queries.
                    mockView = view.getMockAndResetFocus();
                    nextEvent = eventQueue.dequeue();
                    if (nextEvent.who !== undefined) {
                        mockView = mockView.updateFocus(nextEvent.who.id);
                    }

                    // FIXME: deal with event data here
                    racketWorldCallback = nextEvent.handler.racketWorldCallback;
                    data = nextEvent.data[0];
                    var onGoodWorldUpdate = 
                        function(newWorld) {
                            world = newWorld;
                            stopWhen(MACHINE,
                                     world,
                                     mockView,
                                     function(shouldStop) {
                                         if (shouldStop) {
                                             refreshView(onCleanRestart,
                                                         fail);
                                         } else {
                                             dispatchEventsInQueue(success, fail);
                                         }
                                     },
                                     fail);
                        };
                    if (plt.baselib.arity.isArityMatching(racketWorldCallback.racketArity, 3)) {
                        racketWorldCallback(MACHINE, 
                                            world,
                                            mockView,
                                            data,
                                            onGoodWorldUpdate,
                                            fail);
                    } else {
                        racketWorldCallback(MACHINE, 
                                            world,
                                            mockView,
                                            onGoodWorldUpdate,
                                            fail);
                    }
                } else {
                    dispatchingEvents = false;
                    success();
                }
            };

            refreshView = function(success, failure) {
                // Note: we create a random nonce, and watch to see if the MockView we get back
                // from the user came from here.  If not, we have no hope to do a nice, efficient
                // update, and have to do it from scratch.
                var nonce = Math.random();
                var originalMockView = view.getMockAndResetFocus(nonce);
                toDraw(MACHINE, 
                       world,
                       originalMockView,
                       function(newMockView) {
                           if (newMockView.nonce === nonce) {
                               var i;
                               var actions = newMockView.getPendingActions();
                               for (i = 0; i < actions.length; i++) {
                                   actions[i](view);
                               }
                           } else {
                               view.top = arrayTreeToDomNode(newMockView.cursor.top().node);
                               view.initialRender(top);
                               eventHandlers = newMockView.eventHandlers.slice(0);
                               view.eventHandlers = eventHandlers;
                               startEventHandlers();
                           }
                           success();
                       },
                       function(err) {
                           failure(err);
                       });
            };

            currentBigBangRecord = { stop : onCleanRestart,
                                     stopWithExn : onMessyRestart,
                                     startEventHandler : startEventHandler,
                                     stopEventHandler : stopEventHandler };
            view.initialRender(top);
            startEventHandlers();
            refreshView(function() {}, onMessyRestart);
        });
    };

    var wrapFunction = function(proc) {
        var f = function(MACHINE) {
            var success = arguments[arguments.length - 2];
            var fail = arguments[arguments.length - 1];
            var args = [].slice.call(arguments, 1, arguments.length - 2);
            return plt.baselib.functions.internalCallDuringPause.apply(null,
                                                                       [MACHINE,
                                                                        proc,
                                                                        success,
                                                                        fail].concat(args));
        };
        f.racketArity = proc.racketArity;
        return f;
    };




    var DomElementOutputPort = function(id) {
        this.id = id;
    };

    DomElementOutputPort.prototype = plt.baselib.heir(plt.baselib.ports.OutputPort.prototype);

    DomElementOutputPort.prototype.writeDomNode = function (MACHINE, v) {
        $(document.getElementById(this.id)).append(v);
        $(v).trigger({type : 'afterAttach'});
        $('*', v).trigger({type : 'afterAttach'});
    };




    var isAttributeList = function(x) {
        var children;
        if (isList(x) && (! isEmpty(x))){
            if (isSymbol(x.first) && x.first.val === '@') {
                children = x.rest;
                while(! isEmpty(children)) {
                    if (isList(children.first) &&
                        listLength(children.first) === 2 &&
                        isSymbol(children.first.first) &&
                        isString(children.first.rest.first)) {

                        children = children.rest;

                    } else {
                        return false;
                    }
                }
                return true;
            } else {
                return false;
            }
        } else {
            return false;
        }
    };



    // We keep a cache of valid element names.  The only keys here are
    // those elements whose names are valid.  We don't record invalid
    // ones, since there's an unbound number of those.
    var validElementNames = {};  
    var isValidElementName = function(name) {
        if (! (validElementNames.hasOwnProperty(name))) {
            // Permissive parsing: see that the name is a valid
            // element type.
            // Is there a nicer way to do this besides exception
            // handling?
            try {
                document.createElement(name);
                validElementNames[name] = true;
            } catch(e) {
                return false;
            }
        }
        return true;
    };


    // An xexp is one of the following:
    // xexp :== (name (@ (key value) ...) xexp ...)
    //      :== (name xexp ...)
    //      :== string
    var isXexp = function(x) {
        var children;
        if (isString(x)) { 
            return true; 
        }
        if (isSymbol(x)) {
            return true;
        }
        if (isList(x) && !(isEmpty(x))) {
            if (isSymbol(x.first)) {
                if (! isValidElementName(x.first.val)) {
                    return false;
                }

                children = x.rest;
                // Check the rest of the children.  The first is special.
                if (isEmpty(children)) {
                    return true;
                }
                if (isAttributeList(children.first)) {
                    children = children.rest;
                }
                while (! (isEmpty(children))) {
                    if (! isXexp(children.first)) {
                        return false;
                    }
                    children = children.rest;
                }
                return true;
            } else {
                return false;
            }
        }
        return false;
    };




    var assignAttributes = function(node, x) {
        var children, key, value;
        if (isList(x) && (! isEmpty(x))){
            if (isSymbol(x.first) && x.first.val === '@') {
                children = x.rest;
                while(! isEmpty(children)) {
                    if (isList(children.first) &&
                        listLength(children.first) === 2 &&
                        isSymbol(children.first.first) &&
                        isString(children.first.rest.first)) {
                        
                        key = children.first.first;
                        value = children.first.rest.first;
                        $(node).attr(key.val, value.toString());

                        children = children.rest;

                    } else {
                        return;
                    }
                }
                return;
            } else {
                return;
            }
        } else {
            return;
        }
    };
    var xexpToDom = function(x) {
        var children;
        var name;
        var node;
        if (isString(x)) { 
            return document.createTextNode(x); 
        }
        if (isSymbol(x)) {
            return $("<div>&" + x.val + ";</div>").get(0).firstChild;
        }
        if (isList(x) && !(isEmpty(x))) {
            if (isSymbol(x.first)) {
                name = x.first.val;
                node = document.createElement(name);
                children = x.rest;
                // Check the rest of the children.  The first is special.
                if (isEmpty(children)) {
                    return node;
                }
                if (isAttributeList(children.first)) {
                    assignAttributes(node, children.first);
                    children = children.rest;
                }
                while (! (isEmpty(children))) {
                    node.appendChild(xexpToDom(children.first));
                    children = children.rest;
                }
                return node;
            } else {
                return false;
            }
        }
        return false;
    };

    var firstLessThan = function(x, y) {
        return x[0] < y[0];
    };

    var domToXexp = function(dom) {
        var child, attrs, name, convertedChildren, i, attributes;
        if (dom.nodeType === 1) {
            attributes = [];
            attrs = plt.baselib.lists.EMPTY;
            name = plt.baselib.symbols.makeSymbol(dom.nodeName.toLowerCase());
            child = dom.firstChild;
            convertedChildren = plt.baselib.lists.EMPTY;
            for (i = 0; i < dom.attributes.length; i++) {
                attributes.push([dom.attributes[i].nodeName, dom.attributes[i].nodeValue]);
            } 
            attributes.sort(firstLessThan);
            for (i = 0; i < attributes.length; i++) {
                attrs = plt.baselib.lists.makePair(
                    plt.baselib.lists.makeList(plt.baselib.symbols.makeSymbol(attributes[i][0]),
                                               attributes[i][1]),
                    attrs);
            }
            while(child !== null) {
                if (child.nodeType === 1) {
                    convertedChildren = 
                        plt.baselib.lists.makePair(
                            domToXexp(child),
                            convertedChildren);
                } else if (child.nodeType === 3) {
                    convertedChildren = plt.baselib.lists.makePair(
                        domToXexp(child),
                        convertedChildren);
                }
                // Ignore other types.
                child = child.nextSibling;
            }

            if (attrs === plt.baselib.lists.EMPTY) {
                return plt.baselib.lists.makePair(
                    name,
                    plt.baselib.lists.reverse(convertedChildren));
            } else {
                return plt.baselib.lists.makePair(
                    name,
                    plt.baselib.lists.makePair(
                        plt.baselib.lists.makePair(plt.baselib.symbols.makeSymbol("@"),
                                                   attrs),
                        plt.baselib.lists.reverse(convertedChildren)));
            }
        } else if (dom.nodeType === 3) {
            return dom.nodeValue;
        } else {
            // If we can't convert it, return false.
            return false;
        }
    };     










    //////////////////////////////////////////////////////////////////////

    var checkReal = plt.baselib.check.checkReal;
    var checkString = plt.baselib.check.checkString;
    var checkSymbolOrString = plt.baselib.check.checkSymbolOrString;
    var checkProcedure = plt.baselib.check.checkProcedure;


    var checkWorldHandler = plt.baselib.check.makeCheckArgumentType(
        isWorldHandler,
        'world handler');

    var checkMockView = plt.baselib.check.makeCheckArgumentType(
        isMockView, 'view');

    var checkMockViewOnElement = plt.baselib.check.makeCheckArgumentType(
        function(x) {
            return isMockView(x) && (!(x.cursor.isOnAtomicElement()));
        },
        'element-focused view');


    var checkSelector = plt.baselib.check.makeCheckArgumentType(
        isString, 'selector');

    var checkXexp = plt.baselib.check.makeCheckArgumentType(
        isXexp, 'xexp');


    EXPORTS['big-bang'] = makeClosure(
        'big-bang',
        plt.baselib.arity.makeArityAtLeast(1),
        function(MACHINE) {
            var world = MACHINE.e[MACHINE.e.length - 1];
            var handlers = [];
            var i;
            for (i = 1; i < MACHINE.a; i++) {
                handlers.push(checkWorldHandler(MACHINE, 'big-bang', i));
            }
            return bigBang(MACHINE, world, handlers);
        });


    EXPORTS['initial-view'] = makeClosure(
        'initial-view',
        1,
        function(MACHINE) {
            var viewable = MACHINE.e[MACHINE.e.length - 1];
            PAUSE(function(restart) {
                coerseToView(viewable,
                             function(v) {
                                 restart(function(MACHINE) {
                                     finalizeClosureCall(MACHINE,
                                                         new InitialViewHandler(v));
                                 });
                             },
                             function(exn) {
                                 restart(function(MACHINE) {
                                     plt.baselib.exceptions.raise(
                                         MACHINE, 
                                         new Error(plt.baselib.format.format(
                                             "unable to translate ~s to view: ~a",
                                             [viewable, exn.message])));
                                 });
                             });
            });
        });

    EXPORTS['view?'] = makePrimitiveProcedure(
        'view?',
        1,
        function(M) {
            return isMockView(M.e[M.e.length - 1]);
        });


    EXPORTS['->view'] = makeClosure(
        '->view',
        1,
        function(MACHINE) {
            var viewable = MACHINE.e[MACHINE.e.length - 1];
            PAUSE(function(restart) {
                coerseToMockView(viewable,
                                 function(v) {
                                     restart(function(MACHINE) {
                                         finalizeClosureCall(MACHINE, v);
                                     });
                                 },
                                 function(exn) {
                                     restart(function(MACHINE) {
                                         plt.baselib.exceptions.raise(
                                             MACHINE, 
                                             new Error(plt.baselib.format.format(
                                                 "unable to translate ~s to view: ~a",
                                                 [viewable, exn.message])));
                                     });
                                 });
            });
        });
    
    EXPORTS['stop-when'] = makePrimitiveProcedure(
        'stop-when',
        1,
        function(MACHINE) {
            var stopWhen = wrapFunction(checkProcedure(MACHINE, 'stop-when', 0));
            return new StopWhenHandler(stopWhen);
        });

    EXPORTS['to-draw'] = makePrimitiveProcedure(
        'to-draw',
        1,
        function(MACHINE) {
            var toDraw = wrapFunction(checkProcedure(MACHINE, 'to-draw', 0));

            var coersingToMockView = function(MACHINE, world, view, success, fail) {
                return toDraw(MACHINE, world, view, 
                              function(v) { 
                                  coerseToMockView(v, success, fail);
                              },
                              fail);
            };
            return new ToDrawHandler(coersingToMockView);
        });

    EXPORTS['on-tick'] = makePrimitiveProcedure(
        'on-tick',
        plt.baselib.lists.makeList(1, 2),
        function(MACHINE) {
            var onTick = wrapFunction(checkProcedure(MACHINE, 'on-tick', 0));
            var delay = Math.floor(1000/28);
            if (MACHINE.a === 2) {
                delay = Math.floor(plt.baselib.numbers.toFixnum(checkReal(MACHINE, 'on-tick', 1)) * 1000);
            }
            return new EventHandler('on-tick', 
                                    new TickEventSource(delay), 
                                    onTick);
        });


    EXPORTS['view-focus?'] = makePrimitiveProcedure(
        'view-focus?',
        2,
        function(MACHINE) {
            var view = checkMockView(MACHINE, 'view-focus', 0);
            var selector = checkSelector(MACHINE, 'view-focus', 1);
            try {
                view.updateFocus(selector);
                return true;
            } catch (e) {
                return false;
            }
        });


    EXPORTS['view-focus'] = makePrimitiveProcedure(
        'view-focus',
        2,
        function(MACHINE) {
            var view = checkMockView(MACHINE, 'view-focus', 0);
            var selector = checkSelector(MACHINE, 'view-focus', 1);
            try {
                return view.updateFocus(selector);
            } catch (e) {
                plt.baselib.exceptions.raise(
                    MACHINE, 
                    new Error(plt.baselib.format.format(
                        "unable to focus to ~s: ~s",
                        [selector, e.message])));
            }
        });


    EXPORTS['view-left'] = makePrimitiveProcedure(
        'view-left',
        1,
        function(MACHINE) {
            var view = checkMockView(MACHINE, 'view-left', 0);
            try {
                return view.left();
            } catch (e) {
                plt.baselib.exceptions.raise(
                    MACHINE, 
                    new Error("unable to focus left"));
            }
        });

    EXPORTS['view-right'] = makePrimitiveProcedure(
        'view-right',
        1,
        function(MACHINE) {
            var view = checkMockView(MACHINE, 'view-right', 0);
            try {
                return view.right();
            } catch (e) {
                plt.baselib.exceptions.raise(
                    MACHINE, 
                    new Error("unable to focus right"));                
            }
        });

    EXPORTS['view-up'] = makePrimitiveProcedure(
        'view-up',
        1,
        function(MACHINE) {
            var view = checkMockView(MACHINE, 'view-up', 0);
            try {
                return view.up();
            } catch (e) {
                plt.baselib.exceptions.raise(
                    MACHINE, 
                    new Error("unable to focus up"));
            }
        });

    EXPORTS['view-down'] = makePrimitiveProcedure(
        'view-down',
        1,
        function(MACHINE) {
            var view = checkMockView(MACHINE, 'view-down', 0);
            try {
                return view.down();
            } catch(e) {
                plt.baselib.exceptions.raise(
                    MACHINE, 
                    new Error("unable to focus down"));
            }
        });

    EXPORTS['view-forward'] = makePrimitiveProcedure(
        'view-forward',
        1,
        function(MACHINE) {
            var view = checkMockView(MACHINE, 'view-forward', 0);
            try {
                return view.forward();
            } catch(e) {
                plt.baselib.exceptions.raise(
                    MACHINE, 
                    new Error("unable to focus forward"));                
            } 
        });

    EXPORTS['view-backward'] = makePrimitiveProcedure(
        'view-backward',
        1,
        function(MACHINE) {
            var view = checkMockView(MACHINE, 'view-backward', 0);
            try {
                return view.backward();
            } catch(e) {
                plt.baselib.exceptions.raise(
                    MACHINE, 
                    new Error("unable to focus backward"));
            }
        });


    EXPORTS['view-left?'] = makePrimitiveProcedure(
        'view-left?',
        1,
        function(MACHINE) {
            var view = checkMockView(MACHINE, 'view-left?', 0);
            return view.isLeftMovementOk();
        });

    EXPORTS['view-right?'] = makePrimitiveProcedure(
        'view-right?',
        1,
        function(MACHINE) {
            var view = checkMockView(MACHINE, 'view-right?', 0);
            return view.isRightMovementOk();
        });

    EXPORTS['view-up?'] = makePrimitiveProcedure(
        'view-up?',
        1,
        function(MACHINE) {
            var view = checkMockView(MACHINE, 'view-up?', 0);
            return view.isUpMovementOk();
        });

    EXPORTS['view-down?'] = makePrimitiveProcedure(
        'view-down?',
        1,
        function(MACHINE) {
            var view = checkMockView(MACHINE, 'view-down?', 0);
            return view.isDownMovementOk();
        });


    EXPORTS['view-forward?'] = makePrimitiveProcedure(
        'view-down?',
        1,
        function(MACHINE) {
            var view = checkMockView(MACHINE, 'view-forward?', 0);
            return view.isForwardMovementOk();
        });

    EXPORTS['view-backward?'] = makePrimitiveProcedure(
        'view-backward?',
        1,
        function(MACHINE) {
            var view = checkMockView(MACHINE, 'view-backward?', 0);
            return view.isBackwardMovementOk();
        });



    EXPORTS['view-text'] = makePrimitiveProcedure(
        'view-text',
        1,
        function(MACHINE) {
            var view = checkMockView(MACHINE, 'view-text', 0);
            return view.getText();
        });


    EXPORTS['update-view-text'] = makePrimitiveProcedure(
        'update-view-text',
        2,
        function(MACHINE) {
            var view = checkMockView(MACHINE, 'update-view-text', 0);
            var text = plt.baselib.format.toDisplayedString(MACHINE.e[MACHINE.e.length - 2]);
            return view.updateText(text);
        });




    EXPORTS['view-attr'] = makePrimitiveProcedure(
        'view-attr',
        2,
        function(MACHINE) {
            var view = checkMockViewOnElement(MACHINE, 'view-attr', 0);
            var name = checkSymbolOrString(MACHINE, 'view-attr', 1).toString();
            return view.getAttr(name);
        });


    EXPORTS['update-view-attr'] = makePrimitiveProcedure(
        'update-view-attr',
        3,
        function(MACHINE) {
            var view = checkMockViewOnElement(MACHINE, 'update-view-attr', 0);
            var name = checkSymbolOrString(MACHINE, 'update-view-attr', 1).toString();
            var value = checkSymbolOrString(MACHINE, 'update-view-attr', 2).toString();
            return view.updateAttr(name, value);
        });

    EXPORTS['remove-view-attr'] = makePrimitiveProcedure(
        'remove-view-attr',
        2,
        function(MACHINE) {
            var view = checkMockViewOnElement(MACHINE, 'remove-view-attr', 0);
            var name = checkSymbolOrString(MACHINE, 'remove-view-attr', 1).toString();
            return view.removeAttr(name);
        });

    EXPORTS['view-css'] = makePrimitiveProcedure(
        'view-css',
        2,
        function(MACHINE) {
            var view = checkMockViewOnElement(MACHINE, 'view-css', 0);
            var name = checkSymbolOrString(MACHINE, 'view-css', 1).toString();
            return view.getCss(name);
        });


    EXPORTS['update-view-css'] = makePrimitiveProcedure(
        'update-view-css',
        3,
        function(MACHINE) {
            var view = checkMockView(MACHINE, 'update-view-css', 0);
            var name = checkSymbolOrString(MACHINE, 'update-view-css', 1).toString();
            var value = checkSymbolOrString(MACHINE, 'update-view-css', 2).toString();
            return view.updateCss(name, value);
        });


    EXPORTS['view-bind'] = makePrimitiveProcedure(
        'view-bind',
        3,
        function(MACHINE) {
            var view = checkMockViewOnElement(MACHINE, 'view-bind', 0);
            var name = checkSymbolOrString(MACHINE, 'view-bind', 1);
            var worldF = wrapFunction(checkProcedure(MACHINE, 'view-bind', 2));
            return view.bind(name, worldF);
        });


    EXPORTS['view-form-value'] = makePrimitiveProcedure(
        'view-form-value',
        1,
        function(MACHINE) {
            var view = checkMockViewOnElement(MACHINE, 'view-form-value', 0);
            return view.getFormValue();
        });


    EXPORTS['update-view-form-value'] = makePrimitiveProcedure(
        'update-view-form-value',
        2,
        function(MACHINE) {
            var view = checkMockViewOnElement(MACHINE, 'update-view-form-value', 0);
            var value = checkSymbolOrString(MACHINE, 'update-view-form-value', 1).toString();
            return view.updateFormValue(value);
        });

    EXPORTS['view-show'] = makePrimitiveProcedure(
        'view-show',
        1,
        function(MACHINE) {
            var view = checkMockView(MACHINE, 'view-show', 0);
            return view.show();
        });


    EXPORTS['view-hide'] = makePrimitiveProcedure(
        'view-hide',
        1,
        function(MACHINE) {
            var view = checkMockView(MACHINE, 'view-hide', 0);
            return view.hide();
        });


    EXPORTS['view-remove'] = makePrimitiveProcedure(
        'view-remove',
        1,
        function(MACHINE) {
            var view = checkMockView(MACHINE, 'view-remove', 0);
            return view.remove();
        });


    
    EXPORTS['view-append-child'] = makeClosure(
        'view-append-child',
        2,
        function(MACHINE) {
            var view = checkMockViewOnElement(MACHINE, 'view-append-child', 0);
            var x = MACHINE.e[MACHINE.e.length - 2];
            PAUSE(function(restart) {
                coerseToDomNode(x,
                                function(dom) {
                                     restart(function(MACHINE) {
                                         var updatedView = view.appendChild(dom);
                                         finalizeClosureCall(MACHINE, updatedView);
                                     });
                                },
                                function(err) {
                                    restart(function(MACHINE) {
                                         plt.baselib.exceptions.raise(
                                             MACHINE, 
                                             new Error(plt.baselib.format.format(
                                                 "unable to translate ~s to dom node: ~a",
                                                 [x, err.message])));
                                        
                                    });
                                });
            });
        });


    EXPORTS['view-insert-right'] = makeClosure(
        'view-insert-right',
        2,
        function(MACHINE) {
            var view = checkMockView(MACHINE, 'view-insert-right', 0);
            var x = MACHINE.e[MACHINE.e.length - 2];
            PAUSE(function(restart) {
                coerseToDomNode(x,
                                function(dom) {
                                     restart(function(MACHINE) {
                                         var updatedView = view.insertRight(dom);
                                         finalizeClosureCall(MACHINE, updatedView);
                                     });
                                },
                                function(err) {
                                    restart(function(MACHINE) {
                                         plt.baselib.exceptions.raise(
                                             MACHINE, 
                                             new Error(plt.baselib.format.format(
                                                 "unable to translate ~s to dom node: ~a",
                                                 [x, err.message])));
                                        
                                    });
                                });
            });
        });




    EXPORTS['view-insert-left'] = makeClosure(
        'view-insert-left',
        2,
        function(MACHINE) {
            var view = checkMockView(MACHINE, 'view-insert-left', 0);
            var x = MACHINE.e[MACHINE.e.length - 2];
            PAUSE(function(restart) {
                coerseToDomNode(x,
                                function(dom) {
                                     restart(function(MACHINE) {
                                         var updatedView = view.insertLeft(dom);
                                         finalizeClosureCall(MACHINE, updatedView);
                                     });
                                },
                                function(err) {
                                    restart(function(MACHINE) {
                                         plt.baselib.exceptions.raise(
                                             MACHINE, 
                                             new Error(plt.baselib.format.format(
                                                 "unable to translate ~s to dom node: ~a",
                                                 [x, err.message])));
                                        
                                    });
                                });
            });
        });



    EXPORTS['view-id'] = makePrimitiveProcedure(
        'view-id',
        1,
        function(MACHINE) {
            var view = checkMockViewOnElement(MACHINE, 'view-hide', 0);
            return view.id();
        });




    EXPORTS['on-location-change'] = makePrimitiveProcedure(
        'on-location-change',
        1,
        function(MACHINE) {
            var onChange = wrapFunction(checkProcedure(MACHINE, 'on-location-change', 0));
            return new EventHandler('on-location-change', 
                                    new LocationEventSource(), 
                                    onChange);
        });


    EXPORTS['on-mock-location-change'] = makePrimitiveProcedure(
        'on-mock-location-change',
        1,
        function(MACHINE) {
            var onChange = wrapFunction(checkProcedure(MACHINE, 'on-mock-location-change', 0));
            return new EventHandler('on-mock-location-change', 
                                    new MockLocationEventSource(), 
                                    onChange);
        });


    EXPORTS['open-output-element'] = makePrimitiveProcedure(
        'open-output-element',
        1,
        function(MACHINE) {
            var id = checkString(MACHINE, 'open-output-element', 0);
            return new DomElementOutputPort(id.toString());
        });


    EXPORTS['xexp?'] = makePrimitiveProcedure(
        'xexp?',
        1,
        function(MACHINE) {
            return isXexp(MACHINE.e[MACHINE.e.length - 1]);
        });


    EXPORTS['xexp->dom'] = makePrimitiveProcedure(
        'xexp->dom',
        1,
        function(MACHINE) {
            var xexp = checkXexp(MACHINE, 'xexp->dom', 0);
            return xexpToDom(xexp);
        });


    EXPORTS['view->xexp'] = makePrimitiveProcedure(
        'view->xexp',
        1,
        function(MACHINE) {
            var mockView = checkMockView(MACHINE, 'view-hide', 0);
            var domNode = arrayTreeToDomNode(mockView.cursor.top().node);
            return domToXexp(domNode);
        });


    //////////////////////////////////////////////////////////////////////
}());