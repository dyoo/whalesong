/*jslint unparam: true, sub: true, vars: true, white: true, plusplus: true, maxerr: 50, indent: 4 */

// Frame structures.
(function(baselib) {
    var exports = {};
    baselib.loadscript = exports;

    //////////////////////////////////////////////////////////////////////
    /* Lesser General Public License for more details.
     *
     * You should have received a copy of the GNU Lesser General Public
     * License along with this library; if not, write to the Free Software
     * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
     *
     * Contact information:
     *   Dao Gottwald  <dao at design-noir.de>
     *
     * @version  1.6
     * @url      http://design-noir.de/webdev/JS/loadScript/
     */

    var loadScript = function(url, callback, onError) {
        var f = arguments.callee;
        if (!("queue" in f))
            f.queue = {};
        var queue = f.queue;
        if (url in queue) { // script is already in the document
            if (callback) {
                if (queue[url]) // still loading
                    queue[url].push(callback);
                else // loaded
                    callback();
            }
            return;
        }
        queue[url] = callback ? [callback] : [];
        var script = document.createElement("script");
        script.type = "text/javascript";
        script.onload = script.onreadystatechange = function() {
            if (script.readyState && script.readyState != "loaded" && script.readyState != "complete")
                return;
            script.onreadystatechange = script.onload = null;
            document.getElementsByTagName("head")[0].removeChild(script);
            var work = queue[url];
            delete(queue[url]);
            while (work.length)
                work.shift()();
        };
        script.onerror = function() {
            script.onreadystatechange = script.onload = null;
            document.getElementsByTagName("head")[0].removeChild(script);
            onError();
        };
        script.src = url;
        document.getElementsByTagName("head")[0].appendChild(script);
    };

    //////////////////////////////////////////////////////////////////////
    exports.loadScript = loadScript;
}(this.plt.baselib));
