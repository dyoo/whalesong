// Arity structure
/*jslint unparam: true, sub: true, vars: true, maxerr: 50, indent: 4 */
/*globals $*/
(function (baselib, $) {
    'use strict';
    var exports = {};
    baselib.ports = exports;


    // Output Ports
    var OutputPort = function () {};
    var isOutputPort = baselib.makeClassPredicate(OutputPort);


    var StandardOutputPort = function () {
        OutputPort.call(this);
    };
    StandardOutputPort.prototype = baselib.heir(OutputPort.prototype);
    StandardOutputPort.prototype.writeDomNode = function (MACHINE, domNode) {
        MACHINE.params['currentDisplayer'](MACHINE, domNode);
        $(domNode).trigger({type : 'afterAttach'});
        $('*', domNode).trigger({type : 'afterAttach'});
    };

    var StandardErrorPort = function () {
        OutputPort.call(this);
    };
    StandardErrorPort.prototype = baselib.heir(OutputPort.prototype);
    StandardErrorPort.prototype.writeDomNode = function (MACHINE, domNode) {
        MACHINE.params['currentErrorDisplayer'](MACHINE, domNode);
        $(domNode).trigger({type : 'afterAttach'});
        $('*', domNode).trigger({type : 'afterAttach'});
    };





    var OutputStringPort = function () {
        this.buf = [];
    };
    OutputStringPort.prototype = baselib.heir(OutputPort.prototype);
    OutputStringPort.prototype.writeDomNode = function (MACHINE, v) {
        this.buf.push($(v).text());
    };
    OutputStringPort.prototype.getOutputString = function () {
        return this.buf.join('');
    };
    var isOutputStringPort = baselib.makeClassPredicate(OutputStringPort);




    // Input ports
    // Input Ports need to provide two things:
    //
    // readByte:
    // callWhenReady:

    var InputPort = function () {};
    InputPort.prototype.readByte = function(MACHINE) {
        return baselib.constants.EOF_VALUE;
    };
    InputPort.prototype.callWhenReady = function(MACHINE, k) {
        throw new Error("unimplemented");
    };
    var isInputPort = baselib.makeClassPredicate(InputPort);


    var StandardInputPort = function() {
        this.content = [];
        this.closed = false;
    };
    StandardInputPort.prototype = baselib.heir(InputPort.prototype);

    StandardInputPort.prototype.readByte = function(MACHINE) {
        if (this.content.length !== 0) {
            return this.content.shift();
        }
        return baselib.constants.EOF_VALUE;
    };

    StandardInputPort.prototype.callWhenReady = function(MACHINE, k) {
        if (this.content.length > 0) {
            return k();
        }
        if (this.closed) {
            return k();
        }
        var that = this;
        var textFieldDiv = $("<div>" +
                             "  <input class='readline' type='text' size='80%'/>" +
                             "  <input class='eofread' type='button' value='EOF'/>"+
                             "</div>");
        var readLine = textFieldDiv.find(".readline");
        var eofRead = textFieldDiv.find(".eofread");
        var cleanupAndContinue = function() {
            readLine.unbind('keypress');
            eofRead.unbind('click');
            textFieldDiv.remove();
            return k();
        };

        readLine.keypress(
            function(e) {
                var val, i;
                // On return, send the text content into that.content;
                if (e.which === 13) {
                    e.stopPropagation();
                    e.preventDefault();
                    val = readLine.val();
                    for (i = 0; i < val.length; i++) {
                        that.content.push(val.charCodeAt(i));
                    }
                    that.content.push('\n'.charCodeAt(0));
                    cleanupAndContinue();
                }
            });
        eofRead.click(
            function(e) {
                e.stopPropagation();
                e.preventDefault();
                that.closed = true;
                cleanupAndContinue();
            });
        MACHINE.params['currentDisplayer'](MACHINE, textFieldDiv.get(0));
    };


    //////////////////////////////////////////////////////////////////////
    exports.OutputPort = OutputPort;
    exports.isOutputPort = isOutputPort;
    exports.StandardOutputPort = StandardOutputPort;
    exports.StandardErrorPort = StandardErrorPort;
    exports.OutputStringPort = OutputStringPort;
    exports.isOutputStringPort = isOutputStringPort;

    exports.InputPort = InputPort;
    exports.isInputPort = isInputPort;
    exports.StandardInputPort = StandardInputPort;


}(this.plt.baselib, $));