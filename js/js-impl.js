/*jslint devel: true, browser: false, unparam: true, sub: true, windows: false, vars: true, white: true, maxerr: 50, indent: 4 */

/*global $,plt,EXPORTS,document,window*/
(function() {
    "use strict";

    var VOID = plt.baselib.constants.VOID_VALUE;
    var PAUSE = plt.runtime.PAUSE;
    var makePrimitiveProcedure = plt.baselib.functions.makePrimitiveProcedure;
    var makeClosure = plt.baselib.functions.makeClosure;
    var makeCheckArgumentType = plt.baselib.check.makeCheckArgumentType;
    var checkSymbolOrString = plt.baselib.check.checkSymbolOrString;
    var checkString = plt.baselib.check.checkString;
    var checkAny = makeCheckArgumentType(function(x) { return true; },
                                         "any");

    var isJsString = function(x) { return typeof(x) === 'string'; };
    var checkJsString = makeCheckArgumentType(isJsString, 'JavaScript string');



    var isJsNumber = function(x) { return typeof(x) === 'number'; };
    var checkNumber = plt.baselib.check.checkNumber;
    var checkJsNumber = makeCheckArgumentType(isJsNumber, 'JavaScript number');



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
    var _loadScriptQueue = {};
    var loadScript = function(url, callback, onError) {
        var queue = _loadScriptQueue;
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



    EXPORTS['alert'] =
        makePrimitiveProcedure(
            'alert',
            1,
            function(MACHINE) {
                var elt = MACHINE.e[MACHINE.e.length - 1];
                alert(String(elt));
                return VOID;
            });

    EXPORTS['js-eval'] =
        makePrimitiveProcedure(
            'myalert',
            1,
            function(MACHINE) {
                var elt = MACHINE.e[MACHINE.e.length - 1];
                var obj = eval(String(elt));
                return obj;
            });

    EXPORTS['load-script'] =
        makeClosure(
            'load-script',
            1,
            function(MACHINE) {
                var url = checkString(MACHINE, 'load-string', 0);
                PAUSE(
                    function(restart) {
                        var onload = function() {
                            restart(function(MACHINE) {
                                plt.runtime.finalizeClosureCall(
                                    MACHINE, 
                                    VOID);
                            });
                        };
                        var onerror = function(e) {
                            restart(function(MACHINE) {
                                plt.baselib.exceptions.raiseFailure(
                                    MACHINE, 
                                    plt.baselib.format.format(
                                        "unable to load ~a: ~a",
                                        [url,
                                         ((e && e.message) ? e.message : "unknown error")]));
                            });
                        };
                        loadScript(url.toString(),
                                   onload,
                                   onerror);
                    }
                );                
            },
            void(0));
            


    EXPORTS['body'] = $(document.body);

    EXPORTS['$'] =
        makePrimitiveProcedure(
            '$',
            1,
            function(MACHINE) {
                var obj = MACHINE.e[MACHINE.e.length - 1];
                return $(obj);
            });

    EXPORTS['call-method'] = 
        makePrimitiveProcedure(
            'call-method',
            plt.baselib.arity.makeArityAtLeast(2),
            function(MACHINE) {
                var obj = MACHINE.e[MACHINE.e.length - 1];
                var methodName = MACHINE.e[MACHINE.e.length - 2];
                var args = [], i;
                for (i = 0; i < MACHINE.a - 2; i = i+1) {
                    args.push(MACHINE.e[MACHINE.e.length -1 - 2 - i]);
                }
                var result = obj[methodName].apply(obj, args);
                return result;
            });


    EXPORTS['window'] = window;


    EXPORTS['get-attr'] =
        makePrimitiveProcedure(
            'get-attr',
            plt.baselib.arity.makeArityAtLeast(2),
            function(MACHINE) {
                var obj = checkAny(MACHINE, 'get-attr', 0), attr, i;
                for (i = 1; i < MACHINE.a; i = i + 1) {
                    attr = checkSymbolOrString(MACHINE, 'get-attr', i).toString();
                    obj = obj[attr];
                }
                return obj;
            });


    EXPORTS['set-attr!'] =
        makePrimitiveProcedure(
            'set-attr!',
            3,
            function(MACHINE) {
                var obj = checkAny(MACHINE, 'set-attr!', 0);
                var attr = checkSymbolOrString(MACHINE, 'set-attr!', 1).toString();
                var val = checkAny(MACHINE, 'set-attr!', 2);
                obj[attr] = val;
                return VOID;
            });

    EXPORTS['js-string?'] = 
        makePrimitiveProcedure(
            'js-string?',
            1,
            function(MACHINE) {
                return typeof(checkAny(MACHINE, 'js-string?', 0)) === 'string';
            });

    EXPORTS['string->js-string'] =
        makePrimitiveProcedure(
            'string->js-string',
            1,
            function(MACHINE) {
                return checkString(MACHINE, 'string->js-string', 0).toString();
            });

    EXPORTS['js-string->string'] =
        makePrimitiveProcedure(
            'js-string->string',
            1,
            function(MACHINE) {
                return checkJsString(MACHINE, 'string->js-string', 0);
            });




    EXPORTS['js-number?'] = 
        makePrimitiveProcedure(
            'js-number?',
            1,
            function(MACHINE) {
                return isJsNumber(checkAny(MACHINE, 'js-string?', 0));
            });    
    EXPORTS['js-number->number'] = 
        makePrimitiveProcedure(
            'js-number->number',
            1,
            function(MACHINE) {
                return plt.baselib.numbers.makeFloat(checkJsNumber(MACHINE, 'js-string?', 0));
            });    

    EXPORTS['number->js-number'] = 
        makePrimitiveProcedure(
            'number->js-number',
            1,
            function(MACHINE) {
                return plt.baselib.numbers.toFixnum(checkNumber(MACHINE, 'js-string?', 0));
            });    


    EXPORTS['js-null?'] = 
        makePrimitiveProcedure(
            'js-null?',
            1,
            function(MACHINE) {
                return checkAny(MACHINE, 'js-null?', 0) === null;
            });    

    EXPORTS['js-null'] = null;



    // Javascript-specific extensions.  A small experiment.
    EXPORTS['viewport-width'] = 
        makePrimitiveProcedure(
            'viewport-width',
            0,
            function(MACHINE) {
                return $(window).width();
            });

    EXPORTS['viewport-height'] = 
        makePrimitiveProcedure(
            'viewport-height',
            0,
            function(MACHINE) {
                return $(window).height();
            });


    EXPORTS['in-javascript-context?'] =
        makePrimitiveProcedure(
            'in-javascript-context?',
            0,
            function(MACHINE) {
                return true;
            });
}());