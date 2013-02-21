/*jslint plusplus: true, vars: true, white: true, nomen: true, maxerr: 50, indent: 4 */

var LLRBTree = {};

// The code basically follows the structure of
// https://github.com/kazu-yamamoto/llrbtree
//
// Mostly comes from the code in:
//
// https://github.com/kazu-yamamoto/llrbtree/blob/master/Data/RBTree/LL.hs
//
// as well as:
//
// https://github.com/kazu-yamamoto/llrbtree/blob/master/Data/RBTree/Internal.hs

(function() {
    'use strict';

    // function declarations
    var turnR, turnB;
    var insert_, balanceL, balanceR, replaceX, remove_;
    var removeLT, removeGT, removeEQ;
    var isRed, isBlack, isBlackLeftBlack, isBlackLeftRed;
    var hardMin;
    var minimum, removeMin_;


    // red and black colors.
    var R = "R", B = "B";

    // An rbtree is either a Leaf or a Node.

    var Node = function(c, //h,
                        l, x, r) {
        this.c = c; // color: (U R B)
        //this.h = h; // height: int
        this.l = l; // left: rbtree
        this.x = x; // x : element
        this.r = r; // right: rbtree
    };



    var Leaf = function() {};
    // WARNING: DO NOT CONSTRUCT ANY OTHER INSTANCES OF LEAF, OR BAD
    // THINGS WILL HAPPEN.
    var EMPTY = new Leaf();






    var items_ = function(tree, elts) {
        if (tree === EMPTY) { return; }
        items_(tree.l, elts);
        elts.push(tree.x);
        items_(tree.r, elts);
    };

    var items = function(tree) {
        var elts = [];
        items_(tree, elts);
        return elts;
    };



    // Either returns the element, or undefined if we hit a leaf.
    var find = function(tree, x, cmp) {
        while (true) {
            if (tree === EMPTY) { return undefined; }
            else {
                var cmpval = cmp(x, tree.x);
                if (cmpval < 0) {
                    tree = tree.l;
                } else if (cmpval > 0) {
                    tree = tree.r;
                } else {
                    return tree.x;
                }
            }
        }
    };

    var contains = function(tree, x, cmp) {
        while (true) {
            if (tree === EMPTY) { return false; }
            else {
                var cmpval = cmp(x, tree.x);
                if (cmpval < 0) {
                    tree = tree.l;
                } else if (cmpval > 0) {
                    tree = tree.r;
                } else {
                    return true;
                }
            }
        }
    };


    var insert = function(tree, x, cmp) {
        return turnB(insert_(tree, x, cmp));
    };

    insert_ = function(tree, x, cmp) {
        var cmpval;
        if (tree === EMPTY) {
            return new Node(R, //1,
                            EMPTY, x, EMPTY);
        } else {
            cmpval = cmp(x, tree.x);
            if (cmpval < 0) {
                return balanceL(tree.c,// tree.h, 
                                insert_(tree.l, x, cmp), tree.x, tree.r);
            } else if (cmpval > 0) {
                return balanceR(tree.c,// tree.h,
                                tree.l, tree.x, insert_(tree.r, x, cmp));
            } else {
                return replaceX(tree, x);
            }
        }
    };

    balanceL = function(c,// h,
                        l, x, r) {
        if (c === B &&
            l !== EMPTY && l.c === R 
            && l.l !== EMPTY && l.l.c === R) {
            return new Node(R,// h+1,
                            turnB(l.l), l.x, new Node(B, //h,
                                                      l.r, x, r));
        } else {
            return new Node(c,// h,
                            l, x, r);
        }
    };

    balanceR = function(c,// h,
                        l, x, r) {
        if (c === B &&
           l !== EMPTY && l.c === R &&
           r !== EMPTY && r.c === R) {
            return new Node(R,// h+1,
                            turnB(l), x, turnB(r));
        } else if (r !== EMPTY &&
                  r.c === R) {
            return new Node(c,// h,
                            new Node(R,// r.h,
                                     l, x, r.l), r.x, r.r);
        } else {
            return new Node(c,// h,
                            l, x, r);
        }
    };


    var remove = function(tree, x, cmp) {
        var removed;
        if (tree === EMPTY) { 
            return tree; 
        } else {
            removed = remove_(turnR(tree), x, cmp);
            if (removed === EMPTY) {
                return removed;
            } else {
                return turnB(removed);
            }
        }
    };

    remove_ = function(tree, x, cmp) {
        var cmpval;
        if (tree === EMPTY) { 
            return tree; 
        } else {
            cmpval = cmp(x, tree.x);
            if (cmpval < 0) {
                return removeLT(x, tree.c,// tree.h,
                                tree.l, tree.x, tree.r, cmp);
            } else if (cmpval > 0) { 
                return removeGT(x, tree.c,// tree.h,
                                tree.l, tree.x, tree.r, cmp);
            } else {
                return removeEQ(x, tree.c,// tree.h,
                                tree.l, tree.x, tree.r, cmp);
            }
        }
    };

    removeLT = function(kx, c,// h,
                        l, x, r, cmp) {
        var isBB;
        var isBR;
        if (c === R) {
            isBB = isBlackLeftBlack(l);
            isBR = isBlackLeftRed(r);
            if (isBB && isBR) {
                return new Node(R,
                                //h,
                                new Node(B,// r.h,
                                         remove_(turnR(l), kx, cmp), x, r.l.l),
                                r.l.x,
                                new Node(B,// r.h,
                                         r.l.r, r.x, r.r));
            } else if (isBB) {
                return balanceR(B,// h-1,
                                remove_(turnR(l), kx, cmp), x, turnR(r));
            }
        }
        return new Node(c,// h,
                        remove_(l, kx, cmp), x,  r);
    };


    removeGT = function(kx, c,// h,
                        l, x, r, cmp) {
        var isBB, isBR;
        if (l !== EMPTY && l.c === R) {
            return balanceR(c,// h,
                            l.l, l.x, remove_(new Node(R,// h,
                                                             l.r, x, r), kx, cmp));
        }
        if (c === R) {
            isBB = isBlackLeftBlack(r);
            isBR = isBlackLeftRed(l);
            if (isBB && isBR) {
                return new Node(R, 
                                //h,
                                turnB(l.l), 
                                l.x, 
                                balanceR(B,// l.h,
                                         l.r, x, remove_(turnR(r), kx, cmp)));
            } 
            if (isBB) {
                return balanceR(B,// h-1,
                                turnR(l), x, remove_(turnR(r), kx, cmp));
            }
        }
        if (c === R) {
            return new Node(R,// h,
                            l, x, remove_(r, kx, cmp));
        }
        throw new Error("removeGT");
    };

    removeEQ = function(kx, c,// h,
                        l, x, r, cmp) {
        var isBB, isBR, m;
        if (c === R && l === EMPTY && r === EMPTY) {
            return EMPTY;
        }
        if (l !== EMPTY && l.c === R) {
            return balanceR(c,// h,
                            l.l, l.x, remove_(new Node(R,// h,
                                                             l.r, x, r), kx, cmp));
        }
        if (c === R) {
            isBB = isBlackLeftBlack(r);
            isBR = isBlackLeftRed(l);
            if (isBB && isBR) {
                m = minimum(r);
                return balanceR(R,// h,
                                turnB(l.l), l.x, balanceR(B,// l.h,
                                                          l.r, m, removeMin_(turnR(r))));
            }
            if (isBB) {
                m = minimum(r);
                return balanceR(B,// h-1,
                                turnR(l), m, removeMin_(turnR(r)));
            }
        }
        if (c === R &&
            r !== EMPTY && r.c === B) {
            m = minimum(r);
            return new Node(R,// h,
                            l, m, new Node(B,// r.h,
                                           removeMin_(r.l), r.x, r.r));
        }
        throw new Error("removeEQ");
    };


    removeMin_ = function(t) {
        // var h;
        var l, x, r, isBB, isBR;
        if (t !== EMPTY && t.c === R && 
            t.l === EMPTY && t.r === EMPTY) {
            return EMPTY;
        }
        if (t !== EMPTY && t.c === R) {
            //h = t.h;
            l = t.l; x = t.x; r = t.r;
            isBB = isBlackLeftBlack(l);
            isBR = isBlackLeftRed(r);
            if (isRed(l)) {
                return new Node(R,// h,
                                removeMin_(l), x, r);
            } else if (isBB && isBR) {
                return hardMin(t);
            } else if (isBB) {
                return balanceR(B,// h-1,
                                removeMin_(turnR(l)), x, turnR(r));
            } else {
                return new Node(R,// h,
                                new Node(B,// l.h,
                                         removeMin_(l.l), l.x, l.r), x, r);
            }
        }
        throw new Error("removeMin");
    };


    hardMin = function(t) {
        if (t !== EMPTY && t.c === R &&
            t.r !== EMPTY && t.r.c === B &&
            t.r.l !== EMPTY && t.r.l.c === R) {
            return new Node(R,
                            //t.h, 
                            new Node(B,// t.r.h,
                                     removeMin_(turnR(t.l)), t.x, t.r.l.l), 
                            t.r.l.x,
                            new Node(B,// t.r.h,
                                     t.r.l.r, t.r.x, t.r.r));
        }
        throw new Error("hardMin");
    };



    //////////////////////////////////////////////////////////////////////

    // turnB: llrbtree -> llrbtree
    turnB = function(tree) {
        if (tree === EMPTY) { throw new Error("turnB"); }
        return new Node(B, //tree.h,
                        tree.l, tree.x, tree.r);
    };

    // turnR: llrbtree -> llrbtree
    turnR = function(tree) {
        if (tree === EMPTY) { throw new Error("turnR"); }
        return new Node(R, //tree.h,
                        tree.l, tree.x, tree.r);
    };

    // turnR: llrbtree x -> llrbtree
    replaceX = function(tree, x) {
        if (tree === EMPTY) { throw new Error("replaceElt"); }
        return new Node(tree.c, //tree.h,
                        tree.l, x, tree.r);
    };

    // isBlack: llrbtree -> boolean
    isBlack = function(tree) {
        if (tree === EMPTY) { return true; }
        return tree.c === B;
    };

    // isRed: llrbtree -> boolean
    isRed = function(tree) {
        if (tree === EMPTY) { return false; }
        return tree.c === R;
    };

    // isBlackLeftBlack: llrbtree -> boolean
    isBlackLeftBlack = function(tree) {
        if (tree !== EMPTY) {
            if (tree.c === B) {
                if (tree.l === EMPTY) {
                    return true;
                } else {
                    return tree.l.c === B;
                }
            } else {
                return false;
            }
        } else {
            return false;
        }
    };


    // isBlackLeftRed: llrbtree -> boolean
    isBlackLeftRed = function(tree) {
        if (tree !== EMPTY) {
            if (tree.c === B) {
                if (tree.l !== EMPTY) {
                    return tree.l.c === R;
                } else {
                    return false;
                }
            } else {
                return false;
            }
        } else {
            return false;
        }
    };


    // minimum: llrbtree -> X
    // Returns the minimum element in the tree.
    minimum = function(tree) {
        if (tree === EMPTY) { throw new Error("minimum"); }
        while(true) {
            if (tree.l === EMPTY) { 
                return tree.x;
            }
            tree = tree.l;
        }
    };



    //////////////////////////////////////////////////////////////////////
    // This Map makes it easier to use the llrbtree as an associative array.
    // The nodes on the tree are key/value pairs, the comparator of which
    // focuses only on the key portion of the pair.
    
    var Map = function(cmp, tree) {
        this.cmp = cmp;
        this.tree = tree;
    };

    var makeMap = function(keycmp) {
        keycmp = keycmp || function(x, y) { var sx = String(x), sy = String(y);
                                            if (sx < sy) { return -1; }
                                            if (sx > sy) { return 1; }
                                            return 0; };
        return new Map(
            function(n1, n2) {
                return keycmp(n1[0], n2[0]);
            },
            EMPTY);
    };

    Map.prototype.put = function(key, val) {
        return new Map(this.cmp,
                       insert(this.tree, [key, val], this.cmp));
    };

    Map.prototype.contains = function(key) {
        return contains(this.tree, [key, undefined], this.cmp);
    };

    var defaultOnFail = function() { 
        throw new Error("lookup failed"); 
    }; 

    Map.prototype.get = function(key, onFail) {
        var x;
        onFail = onFail || defaultOnFail;
        x = find(this.tree, [key, undefined], this.cmp);
        if (x === undefined) { return onFail(); }
        return x[1];
    };

    Map.prototype.remove = function(key) {
        return new Map(this.cmp,
                       remove(this.tree, [key, undefined], this.cmp));
    };

    Map.prototype.isEmpty = function() {
        return this.tree === EMPTY;
    };

    Map.prototype.keys = function() {
        var result = items(this.tree), i;
        for (i = 0; i < result.length; i++) {
            result[i] = result[i][0];
        }
        return result;
    };

    Map.prototype.values = function() {
        var result = items(this.tree), i;
        for (i = 0; i < result.length; i++) {
            result[i] = result[i][1];
        }
        return result;
    };

    Map.prototype.items = function() {
        var result = items(this.tree), i;
        for (i = 0; i < result.length; i++) {
            // Make sure to copy so that you can't damage the internal
            // key/value pairs.
            result[i] = result[i].slice(0);
        }
        return result;
    };

    // Return the color at the tree.
    Map.prototype.color = function() {
        if (this.tree === EMPTY) { return B; }
        return this.tree.c;
    };

    // Navigate left
    Map.prototype.left = function() {
        if (this.tree === EMPTY) { throw new Error("left"); }
        return new Map(this.cmp, this.tree.l);
    };

    // Navigate right
    Map.prototype.right = function() {
        if (this.tree === EMPTY) { throw new Error("right"); }
        return new Map(this.cmp, this.tree.r);
    };

    // Get the key at the tree
    Map.prototype.key = function() {
        if (this.tree === EMPTY) { throw new Error("key"); }
        return this.tree.x[0];
    };

    // Get the value at the tree.
    Map.prototype.val = function() {
        if (this.tree === EMPTY) { throw new Error("val"); }
        return this.tree.x[1];
    };





    //////////////////////////////////////////////////////////////////////
    LLRBTree.EMPTY = EMPTY;
    LLRBTree.insert = insert;
    LLRBTree.contains = contains;
    LLRBTree.find = find;
    LLRBTree.remove = remove;
    LLRBTree.items = items;


    LLRBTree.makeMap = makeMap;
}());