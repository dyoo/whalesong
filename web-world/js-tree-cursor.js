/*jslint vars: true, white: true, plusplus: true, maxerr: 50, indent: 4 */



// Offers functional views, traversals of the DOM and other tree-like structures.
// See Functional Pearl: The Zipper, by G\'erard Huet
// J. Functional Programming 7 (5): 549--554 Sepember 1997


var TreeCursor = (function() {
    "use strict";

    var TreeCursor = function(parent, node, prevs, nexts, openF, closeF, atomicF) {
        this.parent = parent; // Parent can be the top (undefined), or a TreeCursor
        this.node = node;
        this.prevs = prevs;
        this.nexts = nexts;

        // openF: node -> (arrayof node)
        this.openF = openF;

        // closeF: node (arrayof node) -> node
        // Given a node and its array of children, return a new node.
        this.closeF = closeF;

        // atomicF: node -> boolean
        // Produces true if the node should be treated atomically.
        this.atomicF = atomicF;
    };

    TreeCursor.prototype.canDown = function() {
        return (!(this.atomicF(this.node)) &&
                this.openF(this.node).length !== 0);
    };

    TreeCursor.prototype.down = function() {
        if (this.atomicF(this.node)) {
            throw new Error("down of atomic element");
        }
        var opened = this.openF(this.node);
        if (opened.length === 0) {
            throw new Error("down of empty");
        }
        return new TreeCursor(this, 
                              opened[0],
                              [],
                              opened.slice(1),
                              this.openF, 
                              this.closeF,
                              this.atomicF);
    };


    TreeCursor.prototype.canUp = function() {
        return this.parent !== undefined;
    };

    TreeCursor.prototype.up = function() {
        var parent = this.parent;
        return new TreeCursor(parent.parent,
                              this.closeF(parent.node,
                                          this.prevs.concat([this.node]).concat(this.nexts)),
                              parent.prevs,
                              parent.nexts,
                              this.openF,
                              this.closeF,
                              this.atomicF);
    };

    TreeCursor.prototype.canLeft = function() { return this.prevs.length !== 0; };

    TreeCursor.prototype.left = function() {
        if (this.prevs.length === 0) { throw new Error("left of first"); }
        return new TreeCursor(this.parent,
                              this.prevs[this.prevs.length - 1],
                              this.prevs.slice(0, this.prevs.length - 1),
                              [this.node].concat(this.nexts),
                              this.openF,
                              this.closeF,
                              this.atomicF);
    };

    TreeCursor.prototype.canRight = function() { return this.nexts.length !== 0; };

    TreeCursor.prototype.right = function() {
        if (this.nexts.length === 0) { throw new Error("right of last"); }
        return new TreeCursor(this.parent,
                              this.nexts[0],
                              this.prevs.concat([this.node]),
                              this.nexts.slice(1),
                              this.openF, 
                              this.closeF,
                              this.atomicF);
    };
    
    TreeCursor.prototype.succ = function() {
        var n;
        if (this.canDown()) {
            return this.down();
        } else if (this.canRight()) {
            return this.right();
        } else {
            n = this;
            while (true) {
                n = n.up();
                if (n.canRight()) {
                    return n.right();
                }
            }
        }
    };
 
    TreeCursor.prototype.pred = function() {
        var n;
        if (this.canLeft()) {
            n = this.left();
            while (n.canDown()) {
                n = n.down();
                while (n.canRight()) {
                    n = n.right();
                }
            }
            return n;
        } else {
            return this.up();
        }
    };

    TreeCursor.prototype.canPred = function() {
        return this.canLeft() || this.canUp();
    };

    TreeCursor.prototype.canSucc = function() {
        var n;
        if (this.canDown()) {
            return true;
        } else if (this.canRight()) {
            return true;
        } else {
            n = this;
            while (true) {
                if (! n.canUp()) { return false; }
                n = n.up();
                if (n.canRight()) {
                    return true;
                }
            }
        }
    };

    TreeCursor.prototype.top = function() {
        var n = this;
        while (n.canUp()) { n = n.up(); }
        return n;
    };


    //////////////////////////////////////////////////////////////////////


    TreeCursor.prototype.replaceNode = function(n) {
        return new TreeCursor(this.parent,
                              n,
                              this.prevs,
                              this.nexts,
                              this.openF,
                              this.closeF,
                              this.atomicF);
    };

    TreeCursor.prototype.insertRight = function(n) {
        return new TreeCursor(this.parent,
                              n,
                              this.prevs.concat(this.node),
                              this.nexts,
                              this.openF,
                              this.closeF,
                              this.atomicF);
    };

    TreeCursor.prototype.insertLeft = function(n) {
        return new TreeCursor(this.parent,
                              n,
                              this.prevs,
                              [this.node].concat(this.nexts),
                              this.openF,
                              this.closeF,
                              this.atomicF);
    };

    TreeCursor.prototype.insertDown = function(n) {
        if (this.atomicF(this.node)) {
            throw new Error("down of atomic element");
        }
        return new TreeCursor(this,
                              n,
                              [],
                              this.openF(this.node),
                              this.openF,
                              this.closeF,
                              this.atomicF);
    };

    TreeCursor.prototype.deleteNode = function() {
        var parent;
        if (this.nexts.length !== 0) {
            return new TreeCursor(this.parent,
                                  this.nexts[0],
                                  this.prevs,
                                  this.nexts.slice(1),
                                  this.openF,
                                  this.closeF,
                                  this.atomicF);
        } else if (this.prevs.length !== 0) {
            return new TreeCursor(this.parent,
                                  this.prevs[this.prevs.length - 1],
                                  this.prevs.slice(0, this.prevs.length - 1),
                                  this.nexts,
                                  this.openF,
                                  this.closeF,
                                  this.atomicF);
        } else {
            parent = this.parent;
            return new TreeCursor(parent.parent,
                                  this.closeF(parent.node, []),
                                  parent.prevs,
                                  parent.nexts,
                                  this.openF,
                                  this.closeF,
                                  this.atomicF);
        }
    };






    //////////////////////////////////////////////////////////////////////



    TreeCursor.adaptTreeCursor = function(node, openF, closeF, atomicF) {
        return new TreeCursor(undefined,
                              node,
                              [],
                              [],
                              openF,
                              closeF,
                              atomicF);
    };


    TreeCursor.arrayToCursor = function(anArray) {
        var arrayOpenF = function(n) { 
            if (n instanceof Array) { 
                return n;
            } else {
                return [];
            }
        };
        var arrayCloseF = function(n, children) { 
            if (n instanceof Array) {
                return children;
            } else {
                return n;
            }
        };

        var arrayAtomicF = function(n) {
            return !(n instanceof Array);
        };
        return TreeCursor.adaptTreeCursor(anArray,
                                          arrayOpenF,
                                          arrayCloseF,
                                          arrayAtomicF);
    };


    TreeCursor.domToCursor = function(dom) {
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


    return TreeCursor;
}());