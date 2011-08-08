/*jslint devel: false, browser: true, vars: true, plusplus: true, maxerr: 500, indent: 4 */
(function (baselib) {
    "use strict";

    // Union/find for circular equality testing.

    var UnionFind = function () {
        // this.parenMap holds the arrows from an arbitrary pointer
        // to its parent.
        this.parentMap = baselib.hashes.makeLowLevelEqHash();
    };

    // find: ptr -> UnionFindNode
    // Returns the representative for this ptr.
    UnionFind.prototype.find = function (ptr) {
        var parent = (this.parentMap.containsKey(ptr) ? 
                      this.parentMap.get(ptr) : ptr);
        if (parent === ptr) {
            return parent;
        } else {
            var rep = this.find(parent);
            // Path compression:
            this.parentMap.put(ptr, rep);
            return rep;
        }
    };

    // merge: ptr ptr -> void
    // Merge the representative nodes for ptr1 and ptr2.
    UnionFind.prototype.merge = function (ptr1, ptr2) {
        this.parentMap.put(this.find(ptr1), this.find(ptr2));
    };



    baselib.UnionFind = UnionFind;

}(this.plt.baselib));