#lang racket/base

(require racket/list
         web-server/servlet
         web-server/servlet-env)


;; A hacky way to test the evaluation.
;;
;; Sets up a web server and opens a browser window.
;; The page on screen periodically polls the server to see if a program has
;; come in to be evaluated.  Whenever code does come in, evaluates and returns the
;; value to the user, along with the time it took to evaluate.


(provide make-evaluate
         (struct-out error-happened)
         (struct-out evaluated))



(define-struct error-happened (str t) #:transparent)
(define-struct evaluated (stdout value t
                                 browser) #:transparent)



;; make-evaluate: (Any output-port) -> void
;; Produce a JavaScript evaluator that cooperates with a browser.
;; The JavaScript-compiler is expected to write out a thunk.  When invoked,
;; the thunk should return a function that consumes three values, corresponding
;; to success, failure, and other parameters to evaluation.  For example:
;;
;; (make-evaluate (lambda (program op)
;;                          (fprintf op "(function() {
;;                                            return function(success, fail, params) {
;;                                                       success('ok');
;;                                            }})")))
;;
;; is a do-nothing evaluator that will always give back 'ok'. 
;;
;; At the moment, the evaluator will pass in a parameter that binds 'currentDisplayer' to a function
;; that captures output.
(define (make-evaluate javascript-compiler)
  (define port (+ 8000 (random 8000)))
  
  
  ;; This channel's meant to serialize use of the web server.
  (define ch (make-channel))
  
  
  ;; start up the web server
  ;; The web server responds to two types of requests
  ;; ?comet    Starting up the comet request path.
  ;; ?v       Getting a value back from evaluation.
  ;; ?e       Got an error.
  (void
   (thread (lambda ()
             (define (start req)
               (cond
                 ;; Server-side sync for a program
                 [(exists-binding? 'comet (request-bindings req))
                  (handle-comet req)]
                 
                 ;; Normal result came back
                 [(exists-binding? 'v (request-bindings req))
                  (handle-normal-response req)]
                 
                 ;; Error occurred
                 [(exists-binding? 'e (request-bindings req))
                  (handle-error-response req)]
                 
                 [else
                  (make-on-first-load-response)]))
             
             
             (serve/servlet start 
                            #:banner? #f
                            #:launch-browser? #t
                            #:quit? #f
                            #:port port
                            #:servlet-path "/eval"))))
  
  
  (define *alarm-timeout* 30000)
  
  (define (handle-comet req)
    (let/ec return
      (let* ([alarm (alarm-evt (+ (current-inexact-milliseconds) *alarm-timeout*))]
             [program (sync ch alarm)]
             [op (open-output-bytes)])
        (cond
          [(eq? program alarm)
           (try-again-response)]
          [else
           (with-handlers ([exn:fail? (lambda (exn)
                                        (let ([sentinel
                                               (format
                                                #<<EOF
(function () {
    return function(success, fail, params) {
        fail(~s);
    }
 });
EOF
                                                (exn-message exn))])
                                          
                                          (return 
                                           (response/full 200 #"Okay"
                                                          (current-seconds)
                                                          #"text/plain; charset=utf-8"
                                                          empty
                                                          (list #"" (string->bytes/utf-8 sentinel))))))])
             (javascript-compiler program op))
           
           (response/full 200 #"Okay" 
                          (current-seconds) 
                          #"text/plain; charset=utf-8"
                          empty 
                          (list #"" (get-output-bytes op)))]))))

  
  (define (try-again-response)
    (response/full 200 #"Try again"
                   (current-seconds)
                   #"text/plain; charset=utf-8"
                   empty
                   (list #"" #"")))
  
  (define (ok-response)
    (response/full 200 #"Okay"
                   (current-seconds)
                   TEXT/HTML-MIME-TYPE
                   empty
                   (list #"" #"<html><head></head><body><p>ok</p></body></html>")))
  
  
  
  (define (handle-normal-response req)
    (channel-put ch (make-evaluated (extract-binding/single 'o (request-bindings req))
                                    (extract-binding/single 'v (request-bindings req))
                                    (string->number
                                     (extract-binding/single 't (request-bindings req)))
                                    (extract-binding/single 'b (request-bindings req))))
    (ok-response))
  
  
  (define (handle-error-response req)
    (channel-put ch (make-error-happened 
                     (extract-binding/single 'e (request-bindings req))
                     (string->number
                      (extract-binding/single 't (request-bindings req)))))
    (ok-response))
  
  
  (define (make-on-first-load-response)
    (let ([op (open-output-bytes)])
      (fprintf op #<<EOF
<html>
<head>
<script>
// http://www.quirksmode.org/js/xmlhttp.html
//
// XMLHttpRequest wrapper.  Transparently restarts the request
// if a timeout occurs.
function sendRequest(url,callback,postData) {
	var req = createXMLHTTPObject(), method;

	if (!req) return;
	method = (postData) ? "POST" : "GET";
	req.open(method,url,true);
	if (postData) {
  	    req.setRequestHeader('Content-type','application/x-www-form-urlencoded');
        }
	req.onreadystatechange = function () {
		if (req.readyState != 4) return;
		if (req.status !== 200 && req.status !== 304) {
			return;
		}
                if (req.status === 200 && req.statusText === 'Try again') {
                   req.abort();
                   setTimeout(function() { sendRequest(url, callback, postData); }, 0);
                   return;
                }
		callback(req);
	}
	if (req.readyState == 4) return;
	req.send(postData);
}

var XMLHttpFactories = [
	function () {return new XMLHttpRequest()},
	function () {return new ActiveXObject("Msxml2.XMLHTTP")},
	function () {return new ActiveXObject("Msxml3.XMLHTTP")},
	function () {return new ActiveXObject("Microsoft.XMLHTTP")}
];

function createXMLHTTPObject() {
	var xmlhttp = false;
	for (var i=0;i<XMLHttpFactories.length;i++) {
		try {
			xmlhttp = XMLHttpFactories[i]();
		}
		catch (e) {
			continue;
		}
		break;
	}
	return xmlhttp;
}

var comet = function() {
    sendRequest("/eval", 
                function(req) {
                    // debug:
                    if (window.console && typeof(console.log) === 'function') { console.log(req.responseText); }

                    var invoke = eval(req.responseText)();
                    var output = [];
                    var startTime, endTime;
                    var params = { currentDisplayer: function(v) {
                                                         var pNode = document.createElement("span");
                                                         pNode.style.whiteSpace = 'pre';
                                                         pNode.appendChild(document.createTextNode(String(v)));
                                                         document.body.appendChild(pNode);
                                                         //console.log(v);
                                                         output.push(String(v)); } };

                    var onSuccess = function(v) {
                        endTime = new Date();
                        sendRequest("/eval", function(req) { setTimeout(comet, 0); },
                            "v=" + encodeURIComponent(String(v)) +
                            "&o=" + encodeURIComponent(output.join('')) +
                            "&t=" + encodeURIComponent(String(endTime - startTime)) +
                            "&b=" + encodeURIComponent(String(BrowserDetect.browser + ' ' + BrowserDetect.version + '/' + BrowserDetect.OS)));
                    };

                    var onFail = function(e) {
                        endTime = new Date();
                        sendRequest("/eval", function(req) { setTimeout(comet, 0); },
                            "e=" + encodeURIComponent(String(e)) +
                            "&t=" + encodeURIComponent(String(endTime - startTime)));
                    };
                    startTime = new Date();
                    invoke(onSuccess, onFail, params);
                },
                "comet=t");
};

var BrowserDetect = {
	init: function () {
		this.browser = this.searchString(this.dataBrowser) || "An unknown browser";
		this.version = this.searchVersion(navigator.userAgent)
			|| this.searchVersion(navigator.appVersion)
			|| "an unknown version";
		this.OS = this.searchString(this.dataOS) || "an unknown OS";
	},
	searchString: function (data) {
		for (var i=0;i<data.length;i++)	{
			var dataString = data[i].string;
			var dataProp = data[i].prop;
			this.versionSearchString = data[i].versionSearch || data[i].identity;
			if (dataString) {
				if (dataString.indexOf(data[i].subString) != -1)
					return data[i].identity;
			}
			else if (dataProp)
				return data[i].identity;
		}
	},
	searchVersion: function (dataString) {
		var index = dataString.indexOf(this.versionSearchString);
		if (index == -1) return;
		return parseFloat(dataString.substring(index+this.versionSearchString.length+1));
	},
	dataBrowser: [
		{
			string: navigator.userAgent,
			subString: "Chrome",
			identity: "Chrome"
		},
		{ 	string: navigator.userAgent,
			subString: "OmniWeb",
			versionSearch: "OmniWeb/",
			identity: "OmniWeb"
		},
		{
			string: navigator.vendor,
			subString: "Apple",
			identity: "Safari",
			versionSearch: "Version"
		},
		{
			prop: window.opera,
			identity: "Opera"
		},
		{
			string: navigator.vendor,
			subString: "iCab",
			identity: "iCab"
		},
		{
			string: navigator.vendor,
			subString: "KDE",
			identity: "Konqueror"
		},
		{
			string: navigator.userAgent,
			subString: "Firefox",
			identity: "Firefox"
		},
		{
			string: navigator.vendor,
			subString: "Camino",
			identity: "Camino"
		},
		{		// for newer Netscapes (6+)
			string: navigator.userAgent,
			subString: "Netscape",
			identity: "Netscape"
		},
		{
			string: navigator.userAgent,
			subString: "MSIE",
			identity: "Explorer",
			versionSearch: "MSIE"
		},
		{
			string: navigator.userAgent,
			subString: "Gecko",
			identity: "Mozilla",
			versionSearch: "rv"
		},
		{ 		// for older Netscapes (4-)
			string: navigator.userAgent,
			subString: "Mozilla",
			identity: "Netscape",
			versionSearch: "Mozilla"
		}
	],
	dataOS : [
		{
			string: navigator.platform,
			subString: "Win",
			identity: "Windows"
		},
		{
			string: navigator.platform,
			subString: "Mac",
			identity: "Mac"
		},
		{
			   string: navigator.userAgent,
			   subString: "iPhone",
			   identity: "iPhone/iPod"
	    },
		{
			string: navigator.platform,
			subString: "Linux",
			identity: "Linux"
		}
	]

};
BrowserDetect.init();


var whenLoaded = function() {
    setTimeout(comet, 0);
};

</script>
</head>
<body onload="whenLoaded()">
<p>Harness loaded.  Do not close this window.</p>
</body>
</html>
EOF
               )
      (response/full 200 #"Okay" 
                     (current-seconds) 
                     TEXT/HTML-MIME-TYPE
                     empty 
                     (list #"" (get-output-bytes op)))))
  
  
  
  ;; evaluate: sexp -> (values string number)
  ;; A little driver to test the evalution of expressions, using a browser to help.
  ;; Returns the captured result of stdout, plus # of milliseconds it took to execute.
  (define (evaluate e)
    ;; Send the program to the web browser, and wait for the thread to send back
    (channel-put ch e)
    (let ([result (channel-get ch)])
      (cond [(error-happened? result)
             (raise result)]
            [else
             result])))
  

  evaluate)