/*jslint browser: true, unparam: true, vars: true, white: true, plusplus: true, maxerr: 50, indent: 4 */
/*global plt,MACHINE,$,EXPORTS*/
(function() {

    "use strict";

    var makePrimitiveProcedure = plt.baselib.functions.makePrimitiveProcedure;
    var makeClosure = plt.baselib.functions.makeClosure;
    var finalizeClosureCall = plt.baselib.functions.finalizeClosureCall;
    var PAUSE = plt.runtime.PAUSE;
    var isString = plt.baselib.strings.isString;



    // FIXME: as soon as we get real parameters, use parameters
    // instead.  Global: defines the currently running big bang.
    // Parameterized around the call to bigBang.
    var currentBigBangRecord = undefined;



    var resourceStructType = 
        MACHINE.modules['whalesong/resource/structs.rkt'].namespace['struct:resource'];


    var domToCursor = function(dom) {
        var domOpenF = 
            // To go down, just take the children.
            function(n) { 
                return [].slice.call(n.childNodes, 0);
            };
        var domCloseF = 
            // To go back up, take the node, do a shallow cloning, and replace the children.
            function(node, children) { 
                var i;
                var newNode = node.cloneNode(false);
                for (i = 0; i < children.length; i++) {
                    newNode.appendChild(children[i].cloneNode(true));
                }
                return newNode; 
            };
        var domAtomicF =
            function(node) {
                return node.nodeType !== 1;
            };
        return TreeCursor.adaptTreeCursor(dom.cloneNode(true),
                                          domOpenF,
                                          domCloseF,
                                          domAtomicF);
    };





    // See Functional Pearl: The Zipper, by G\'erard Huet
    // J. Functional Programming 7 (5): 549--554 Sepember 1997
    var TreePath = function(parent, node, prevs, nexts) {
        this.parent = parent; // Parent can be the top (undefined), or a TreePath
        this.node = node;
        this.prevs = prevs;
        this.nexts = nexts;
    };

    TreePath.prototype.down = function() {
        var children = node.children();
        return new TreePath(this, node[0], [], children.slice(1));
    };

    TreePath.prototype.up = function() {
        var parent = this.parent;
        return new Tree
    };

    TreePath.prototype.left = function() {
    };

    TreePath.prototype.right = function() {
    };
    
    TreePath.prototype.succ = function() {
    };

    TreePath.prototype.pred = function() {
    };




    

    // For the moment, we only support selection by id.
    var idRegexp = new RegExp("^#");
    var selectorMatches = function(selector, node) {
        if (selector.match(idRegexp)) {
            if (node.nodeType === 1) {
                return node.getAttribute('id') === selector.substring(1);
            } else {
                return false;
            }
        }
        return false;
    };


    //////////////////////////////////////////////////////////////////////
    // A MockView provides a functional interface to the DOM.  It
    // includes a cursor to the currently focused dom, the pending
    // actions to perform on the actual view, and a nonce to detect
    // freshness of the MockView.
    var MockView = function(cursor, pendingActions, eventHandlers, nonce) {
        this.cursor = cursor;
        this.pendingActions = pendingActions;
        this.eventHandlers = eventHandlers;
        this.nonce = nonce;
    };

    var isMockView = plt.baselib.makeClassPredicate(MockView);

    MockView.prototype.act = function(actionForCursor, actionForEventHandlers, actionForReal) {
        if (arguments.length !== 3) { throw new Error("act: insufficient arguments"); }
        return new MockView(actionForCursor(this.cursor),
                            this.pendingActions.concat([actionForReal]),
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
                view.focus = view.top.find(selector);
            }
        );
    };

    MockView.prototype.getText = function() {        
        return $(this.cursor.node).text();
    };

    MockView.prototype.updateText = function(text) {
        return this.act(
            function(cursor) {
                return cursor.replaceNode($(cursor.node).clone(true).text(text).get(0));
            },
            function(eventHandlers) { return eventHandlers; },
            function(view) {
                view.focus.text(text);
            }
        )
    };

    MockView.prototype.getAttr = function(name) {        
        return $(this.cursor.node).attr(name);
    };


    MockView.prototype.updateAttr = function(name, value) {
        return this.act(
            function(cursor) {
                return cursor.replaceNode($(cursor.node).clone(true).attr(name, value).get(0));
            },
            function(eventHandlers) {
                return eventHandlers;
            },
            function(view) {
                view.focus.attr(name, value);
            })
    };


    MockView.prototype.getFormValue = function() {        
        return $(this.cursor.node).val();
    };

    MockView.prototype.updateFormValue = function(value) {        
        return this.act(
            function(cursor) {
                return cursor.replaceNode($(cursor.node).clone(true).val(value).get(0));
            },
            function(eventHandlers) {
                return eventHandlers;
            },
            function(view) {
                view.focus.val(value);
            })
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
                view.focus = view.focus.prev();
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
                view.focus = view.focus.next();
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
                view.focus = view.focus.parent();
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
                view.focus = view.focus.children(':first');
            });
    };
    
    var mockViewIdGensym = 0;

    MockView.prototype.bind = function(name, worldF) {
        var that = this;

        // HACK: every node that is bound needs to have an id.  We
        // enforce this by mutating the node.
        if (! this.cursor.node.id) {
            this.cursor.node.id = ("__webWorldId_" + mockViewIdGensym++);
        }   
        return this.act(
            function(cursor) {
                var newCursor = cursor.replaceNode($(cursor.node).clone(true).get(0));
                var handler = new EventHandler(name, 
                                               new DomEventSource(name, newCursor.node), 
                                               worldF);
                if (currentBigBangRecord !== undefined) {
                    currentBigBangRecord.startEventHandler(handler);
                }
                return newCursor;
            },
            function(eventHandlers) {
                var handler = new EventHandler(name,
                                               new DomEventSource(
                                                   name,
                                                   that.cursor.node.id),
                                               worldF);
                return eventHandlers.concat([handler]);
            },
            function(view) {
                // HACK: every node that is bound needs to have an id.  We
                // enforce this by mutating the node.
                if (! view.focus.get(0).id) {
                    view.focus.get(0).id = ("__webWorldId_" + mockViewIdGensym++);
                }
                var handler = new EventHandler(name, 
                                               new DomEventSource(
                                                   name, 
                                                   view.focus.get(0).id),
                                               worldF);
                view.addEventHandler(handler);
                currentBigBangRecord.startEventHandler(handler);
            });
    };

    MockView.prototype.show = function() {
        return this.act(
            function(cursor) {
                return cursor.replaceNode($(cursor.node).clone(true).show().get(0));
            },
            function(eventHandlers) { return eventHandlers; },
            function(view) {
                view.focus.show();
            }
        )
    };

    MockView.prototype.hide = function() {
        return this.act(
            function(cursor) {
                return cursor.replaceNode($(cursor.node).clone(true).hide().get(0));
            },
            function(eventHandlers) { return eventHandlers; },
            function(view) {
                view.focus.hide();
            }
        )
    };


    MockView.prototype.appendChild = function(domNode) {
        return this.act(
            function(cursor) {
                if (cursor.canDown()) {
                    cursor = cursor.down();
                    while (cursor.canRight()) {
                        cursor = cursor.right();
                    }
                    return cursor.insertRight(domNode.cloneNode(true));
                } else {
                    return cursor.insertDown(domNode.cloneNode(true));
                }
            },
            function(eventHandlers) { return eventHandlers; },
            function(view) {
                var clone = $(domNode).clone(true);
                clone.appendTo(view.focus);
                view.focus = clone;
            }
        )
    };

    MockView.prototype.id = function() {
        return this.cursor.node.id;
    };





    //////////////////////////////////////////////////////////////////////


    
    

    // A View represents a representation of the DOM tree.
    var View = function(top, eventHandlers) {
        // top: dom node
        this.top = top;
        this.focus = top;
        this.eventHandlers = eventHandlers;
    };

    View.prototype.toString = function() { return "#<View>"; };

    View.prototype.initialRender = function(top) {
        top.empty();
        $(document.head).append(this.top.find("head").children());

        // FIXME: we should pull in the styles applied to body and its
        // children and apply them here?
        if (this.top.find("body").length > 0) {
            top.append(this.top.find("body").children());
            this.top = top;
        } else {
            top.append(this.top);
        }
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
        return new MockView(domToCursor($(this.top).get(0)),
                            [],
                            [],
                            nonce);
    };





    var isView = plt.baselib.makeClassPredicate(View);
    var isResource = resourceStructType.predicate;


    // var resourcePath = function(r) { return resourceStructType.accessor(r, 0); };
    // var resourceKey = function(r) { return resourceStructType.accessor(r, 1); };
    var resourceContent = function(r) { return resourceStructType.accessor(r, 2); };





 
    // coerseToView: (U resource View) -> View
    // Coerse a value into a view.
    var coerseToView = function(x, onSuccess, onFail) {
        var dom;
        if (isView(x)) { 
            return onSuccess(x); 
        } else  if (isResource(x)) {
            try {
                dom = $(resourceContent(x).toString())
                    .css("margin", "0px")
                    .css("padding", "0px")
                    .css("border", "0px");
                dom.children("body").css("margin", "0px");
            } catch (exn) {
                return onFail(exn);
            }
            return onSuccess(new View(dom, []));
        } else if (isMockView(x)) {
            return onSuccess(new View($(x.cursor.top().node),
                                      x.eventHandlers));
        } else {
            try {
                dom = $(plt.baselib.format.toDomNode(x))
            } catch (exn) {
                return onFail(exn);
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
                dom = $(resourceContent(x).toString())
                    .css("margin", "0px")
                    .css("padding", "0px")
                    .css("border", "0px");
                dom.children("body").css("margin", "0px");
            } catch (exn) {
                return onFail(exn);
            }
            return onSuccess(new MockView(domToCursor(dom.get(0)), [], [], undefined));
        } else {
            try {
                dom = $(plt.baselib.format.toDomNode(x))
            } catch (exn) {
                return onFail(exn);
            }
            return onSuccess(new MockView(domToCursor(dom.get(0)), [], [], undefined));
        }
    };


    var coerseToDomNode = function(x, onSuccess, onFail) {
        var dom;
        if (isDomNode(x)) { 
            return onSuccess(x); 
        } else  if (isResource(x)) {
            try {
                dom = $(resourceContent(x).toString())
                    .css("margin", "0px")
                    .css("padding", "0px")
                    .css("border", "0px");
            } catch (exn) {
                return onFail(exn);
            }
            return onSuccess(dom.get(0));
        } else if (isMockView(x)) {
            return onSuccess(x.cursor.top().node);
        } else {
            try {
                dom = plt.baselib.format.toDomNode(x);
            } catch (exn) {
                return onFail(exn);
            }
            return onSuccess(dom);
        }
    };


    var isDomNode = function(x) {
        return (x.hasOwnProperty('nodeType') &&
                x.nodeType === 1);
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

    var EventHandler = function(name, eventSource, racketWorldCallback) {
        WorldHandler.call(this);
        this.name = name;
        this.eventSource = eventSource;
        this.racketWorldCallback = racketWorldCallback
    };
    EventHandler.prototype = plt.baselib.heir(WorldHandler.prototype);
    EventHandler.prototype.toString = function() { return "#<" + this.name + ">"; };
    var isEventHandler = plt.baselib.makeClassPredicate(EventHandler);

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

    // The default behavior of pause is to cause the event source to stop.
    EventSource.prototype.onPause = function() {
        this.onStop();
    };

    // The default behavior of unpause is to start an event source up again.
    EventSource.prototype.onUnpause = function(fireEvent) {
        this.onStart(fireEvent);
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
        this.id = setInterval(
            function() {
                fireEvent(undefined);
            },
            this.delay);
    };

    TickEventSource.prototype.onStop = function() {
        if (this.id !== undefined) {
            clearInterval(this.id);
            this.id = undefined;
        }
    };


    // DomElementSource: string (U DOM string) -> EventSource
    // A DomEventSource allows DOM elements to send events over to
    // web-world.
    var DomEventSource = function(type, elementOrId) {
        this.type = type;
        this.elementOrId = elementOrId;
        this.handler = undefined;
    };

    DomEventSource.prototype = plt.baselib.heir(EventSource.prototype);

    DomEventSource.prototype.onStart = function(fireEvent) {
        var element = this.elementOrId;
        if (typeof(this.elementOrId) === 'string') {
            element = $('#' + this.elementOrId).get(0);
        }

        this.handler = function(evt) {
            if (element !== undefined) {
                fireEvent(element, evt);
            }
        };
        if (element !== undefined) {
            $(element).bind(this.type, this.handler);
        }
    };

    DomEventSource.prototype.onStop = function() {
        var element = this.elementOrId;
        if (typeof(this.elementOrId) === 'string') {
            element = $('#' + this.elementOrId).get(0);
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
        return success(view);
    };


    var defaultStopWhen = function(MACHINE, world, view, success, fail) {
        return success(false);
    };


    // bigBang.
    var bigBang = function(MACHINE, world, handlers) {
        var oldArgcount = MACHINE.argcount;
        var oldCurrentBigBangRecord = currentBigBangRecord;

        var running = true;
        var dispatchingEvents = false;

        var view = (find(handlers, isInitialViewHandler) || { view : new View(top, []) }).view;
        var stopWhen = (find(handlers, isStopWhenHandler) || { stopWhen: defaultStopWhen }).stopWhen;
        var toDraw = (find(handlers, isToDrawHandler) || {toDraw : defaultToDraw} ).toDraw;

        var eventQueue = new EventQueue();

        var top = $("<div/>");
        var eventHandlers = filter(handlers, isEventHandler).concat(view.getEventHandlers());

        MACHINE.params.currentDisplayer(MACHINE, top);

        PAUSE(function(restart) {
            var i;

            var onCleanRestart = function() {
                running = false;
                stopEventHandlers();
                restart(function(MACHINE) {
                    MACHINE.argcount = oldArgcount;
                    currentBigBangRecord = oldCurrentBigBangRecord;
                    finalizeClosureCall(MACHINE, world);
                });
            };

            var onMessyRestart = function(exn) {
                running = false;
                stopEventHandlers();
                restart(function(MACHINE) {
                    currentBigBangRecord = oldCurrentBigBangRecord;
                    plt.baselib.exceptions.raise(MACHINE, exn);
                });
            };

            var startEventHandlers = function() {
                var i;
                for (i = 0; i < eventHandlers.length; i++) {
                    startEventHandler(eventHandlers[i]);
                }
            };

            var stopEventHandlers = function() {
                var i;
                for (i = 0; i < eventHandlers.length; i++) {
                    stopEventHandler(eventHandlers[i]);
                }
            };

            var startEventHandler = function(handler) {
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
                                        refreshView(function() {}, 
                                                    onMessyRestart);
                                    }, 
                                    onMessyRestart);
                            },
                            0);
                    }
                };
                handler.eventSource.onStart(fireEvent);
            };

            var stopEventHandler = function(handler) {
                handler.eventSource.onStop();
            };


            var dispatchEventsInQueue = function(success, fail) {
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
                        mockView = mockView.updateFocus('#' + nextEvent.who.id);
                    }

                    // FIXME: deal with event data here
                    racketWorldCallback = nextEvent.handler.racketWorldCallback;
                    racketWorldCallback(MACHINE, 
                                        world,
                                        mockView,
                                        // data, 
                                        function(newWorld) {
                                            world = newWorld;
                                            stopWhen(MACHINE,
                                                     world,
                                                     mockView,
                                                     function(shouldStop) {
                                                         if (shouldStop) {
                                                             refreshView(
                                                                 function() {
                                                                     onCleanRestart();
                                                                 },
                                                                 fail);
                                                         } else {
                                                             dispatchEventsInQueue(success, fail);
                                                         }
                                                     },
                                                     fail);
                                        },
                                        fail);
                } else {
                    dispatchingEvents = false;
                    success();
                }
            };

            var refreshView = function(success, failure) {
                // Note: we create a random nonce, and watch to see if the MockView we get back
                // from the user came from here.  If not, we have no hope to do a nice, efficient
                // update, and have to do it from scratch.
                var nonce = Math.random();
                
                toDraw(MACHINE, 
                       world,
                       view.getMockAndResetFocus(nonce),
                       function(newMockView) {
                           if (newMockView.nonce === nonce) {
                               var i;
                               var actions = newMockView.pendingActions;
                               for (i = 0; i < actions.length; i++) {
                                   actions[i](view);
                               }
                           } else {
                               view.top = $(newMockView.cursor.top().node);
                               view.initialRender(top);
                               eventHandlers = newMockView.eventHandlers;
                               startEventHandlers();
                           }
                           success();
                       },
                       function(err) {
                           failure(err);
                       })
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
        return function(MACHINE) {
            var success = arguments[arguments.length - 2];
            var fail = arguments[arguments.length - 1];
            var args = [].slice.call(arguments, 1, arguments.length - 2);
            return plt.baselib.functions.internalCallDuringPause.apply(null,
                                                                       [MACHINE,
                                                                        proc,
                                                                        success,
                                                                        fail].concat(args));
        };
    };



    // findDomNodeLocation: dom-node dom-node -> arrayof number
    // Given a node, returns the child indices we need to follow to reach
    // it from the top.
    // Assumption: top must be an ancestor of the node.  Otherwise, the
    // result is partial.
    var findDomNodeLocation = function(node, top) {
        var locator = [];
        var parent, i;
        while(node !== top && node.parentNode !== null) {
            parent = node.parentNode;
            for (i = 0; i < parent.childNodes.length; i++) {
                if (parent.childNodes[i] === node) {
                    locator.push(i);
                    break;
                }
            }
            node = parent;
        }
        return locator.reverse();
    };

    var findNodeFromLocation = function(top, location) {
        var i = 0;
        var node = top;
        for (i = 0; i < location.length; i++) {
            node = node.childNodes[location[i]];
        }
        return node;
    };



    //////////////////////////////////////////////////////////////////////

    var checkReal = plt.baselib.check.checkReal;
    var checkString = plt.baselib.check.checkString;
    var checkSymbolOrString = plt.baselib.check.checkSymbolOrString;
    
    var checkProcedure = plt.baselib.check.checkProcedure;

    var checkResourceOrView = plt.baselib.check.makeCheckArgumentType(
        function(x) { return isView(x) || isResource(x); },
        'resource or view');

    var checkWorldHandler = plt.baselib.check.makeCheckArgumentType(
        isWorldHandler,
        'world handler');

    var checkMockView = plt.baselib.check.makeCheckArgumentType(
        isMockView, 'view');



    var checkSelector = plt.baselib.check.makeCheckArgumentType(
        isString, 'selector');


    EXPORTS['big-bang'] = makeClosure(
        'big-bang',
        plt.baselib.arity.makeArityAtLeast(1),
        function(MACHINE) {
            var world = MACHINE.env[MACHINE.env.length - 1];
            var handlers = [];
            var i;
            for (i = 1; i < MACHINE.argcount; i++) {
                handlers.push(checkWorldHandler(MACHINE, 'big-bang', i));
            }
            return bigBang(MACHINE, world, handlers);
        });


    EXPORTS['initial-view'] = makeClosure(
        'initial-view',
        1,
        function(MACHINE) {
            var viewable = MACHINE.env[MACHINE.env.length - 1];
            var oldArgcount = MACHINE.argcount;
            PAUSE(function(restart) {
                coerseToView(viewable,
                             function(v) {
                                 restart(function(MACHINE) {
                                     MACHINE.argcount = oldArgcount;
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


    EXPORTS['->view'] = makeClosure(
        '->view',
        1,
        function(MACHINE) {
            var viewable = MACHINE.env[MACHINE.env.length - 1];
            var oldArgcount = MACHINE.argcount;
            PAUSE(function(restart) {
                coerseToMockView(viewable,
                                 function(v) {
                                     restart(function(MACHINE) {
                                         MACHINE.argcount = oldArgcount;
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
            if (MACHINE.argcount === 2) {
                delay = Math.floor(plt.baselib.numbers.toFixnum(checkReal(MACHINE, 'on-tick', 1)) * 1000);
            }
            return new EventHandler('on-tick', 
                                    new TickEventSource(delay), 
                                    onTick);
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
            return view.left();
        });

    EXPORTS['view-right'] = makePrimitiveProcedure(
        'view-right',
        1,
        function(MACHINE) {
            var view = checkMockView(MACHINE, 'view-right', 0);
            return view.right();
        });

    EXPORTS['view-up'] = makePrimitiveProcedure(
        'view-up',
        1,
        function(MACHINE) {
            var view = checkMockView(MACHINE, 'view-up', 0);
            return view.up();
        });

    EXPORTS['view-down'] = makePrimitiveProcedure(
        'view-down',
        1,
        function(MACHINE) {
            var view = checkMockView(MACHINE, 'view-down', 0);
            return view.down();
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
            var text = plt.baselib.format.toDisplayedString(MACHINE.env[MACHINE.env.length - 2]);
            return view.updateText(text);
        });




    EXPORTS['view-attr'] = makePrimitiveProcedure(
        'view-attr',
        2,
        function(MACHINE) {
            var view = checkMockView(MACHINE, 'view-attr', 0);
            var name = checkSymbolOrString(MACHINE, 'view-attr', 1).toString();
            return view.getAttr(name);
        });


    EXPORTS['update-view-attr'] = makePrimitiveProcedure(
        'update-view-attr',
        3,
        function(MACHINE) {
            var view = checkMockView(MACHINE, 'update-view-attr', 0);
            var name = checkSymbolOrString(MACHINE, 'update-view-attr', 1).toString();
            var value = checkSymbolOrString(MACHINE, 'update-view-attr', 2).toString();
            return view.updateAttr(name, value);
        });



    EXPORTS['view-bind'] = makePrimitiveProcedure(
        'view-bind',
        3,
        function(MACHINE) {
            var view = checkMockView(MACHINE, 'view-bind', 0);
            var name = checkSymbolOrString(MACHINE, 'view-bind', 1);
            var worldF = wrapFunction(checkProcedure(MACHINE, 'view-bind', 2));
            return view.bind(name, worldF);
        });


    EXPORTS['view-form-value'] = makePrimitiveProcedure(
        'view-form-value',
        1,
        function(MACHINE) {
            var view = checkMockView(MACHINE, 'view-form-value', 0);
            return view.getFormValue();
        });


    EXPORTS['update-view-form-value'] = makePrimitiveProcedure(
        'update-view-form-value',
        2,
        function(MACHINE) {
            var view = checkMockView(MACHINE, 'update-view-form-value', 0);
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


    
    EXPORTS['view-append-child'] = makeClosure(
        'view-append-child',
        2,
        function(MACHINE) {
            var view = checkMockView(MACHINE, 'view-append-child', 0);
            var oldArgcount = MACHINE.argcount;
            var x = MACHINE.env[MACHINE.env.length - 2];
            PAUSE(function(restart) {
                coerseToDomNode(x,
                                function(dom) {
                                     restart(function(MACHINE) {
                                         MACHINE.argcount = oldArgcount;
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
                                                 [x, exn.message])));
                                        
                                    });
                                });
            });
        });


    EXPORTS['view-id'] = makePrimitiveProcedure(
        'view-id',
        1,
        function(MACHINE) {
            var view = checkMockView(MACHINE, 'view-hide', 0);
            return view.id();
        });




    //////////////////////////////////////////////////////////////////////
}());