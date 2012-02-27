/*jslint devel: true, browser: false, unparam: true, sub: true, windows: false, vars: true, white: true, maxerr: 50, indent: 4 */

/*global $,plt,EXPORTS,document,window*/
(function() {
    "use strict";

    var VOID = plt.baselib.constants.VOID_VALUE;
    var makePrimitiveProcedure = plt.baselib.functions.makePrimitiveProcedure;
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