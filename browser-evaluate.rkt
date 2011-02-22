#lang racket/base

(require racket/list
         web-server/servlet
         web-server/servlet-env
         "package.rkt")

;; A hacky way to test the evaluation.
;;
;; Sets up a web server and opens a browser window.
;; The page on screen periodically polls the server to see if a program has
;; come in to be evaluated.  Whenever code does come in, evaluates and returns the
;; value to the user, along with the time it took to evaluate.


(provide evaluate)

(define port (+ 8000 (random 8000)))


;; This channel's meant to serialize use of the web server.
(define ch (make-channel))


;; start up the web server
;; The web server responds to two types of requests
;; ?p    Inputting a program
;; ?r    Getting a response
(void
 (thread (lambda ()
          (define (start req)
            (cond
              ;; Server-side sync for a program
              [(exists-binding? 'poke (request-bindings req))
               (handle-poke req)]
              
              ;; Normal result came back
              [(exists-binding? 'r (request-bindings req))
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

(define (handle-poke req)
  ;; FIXME: how do we handle timeouts?
  (let/ec return
    (let ([program (sync ch)]
          [op (open-output-bytes)])
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
        (package-anonymous program op))
      
      (response/full 200 #"Okay" 
                     (current-seconds) 
                     #"text/plain; charset=utf-8"
                     empty 
                     (list #"" (get-output-bytes op))))))




(define (handle-normal-response req)
  (channel-put ch (list (extract-binding/single 'r (request-bindings req))
                        (string->number
                         (extract-binding/single 't (request-bindings req)))))
  `(html (body (p "ok"))))


(define (handle-error-response req)
  (channel-put ch (make-error-happened 
                   (extract-binding/single 'e (request-bindings req))
                   (string->number
                    (extract-binding/single 't (request-bindings req)))))
  `(html (body (p "ok"))))



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
	var req = createXMLHTTPObject(), method, TIMEOUT = 5000, stillInProgress = true, timeoutId;

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
                stillInProgress = false;
                if (timeoutId) { clearTimeout(timeoutId); timeoutId = undefined; }
		callback(req);
	}
	if (req.readyState == 4) return;
	req.send(postData);
/*
        timeoutId = setTimeout(function() { if(stillInProgress) { 
                                                req.abort();
                                                // Reschedule
                                                setTimeout(function() { sendRequest(url, callback, postData);}, 0);
                                            }
                                          },
                               TIMEOUT);
*/
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

var poke = function() {
    sendRequest("/eval", 
                function(req) {
                    var invoke = eval(req.responseText)();
                    var output = [];
                    var startTime, endTime;
                    var params = { currentDisplayer: function(v) { output.push(String(v)); } };

                    var onSuccess = function() {
                        endTime = new Date();
                        sendRequest("/eval", function(req) { setTimeout(poke, 0); },
                            "r=" + encodeURIComponent(output.join('')) +
                            "&t=" + encodeURIComponent(String(endTime - startTime)));
                    };

                    var onFail = function(e) {
                        endTime = new Date();
                        sendRequest("/eval", function(req) { setTimeout(poke, 0); },
                            "e=" + encodeURIComponent(String(e)) +
                            "&t=" + encodeURIComponent(String(endTime - startTime)));
                    };
                    startTime = new Date();
                    invoke(onSuccess, onFail, params);
                },
                "poke=t");
};

var whenLoaded = function() {
    setTimeout(poke, 0);
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


(define-struct error-happened (str t) #:transparent)


;; evaluate: sexp -> (values string number)
;; A little driver to test the evalution of expressions, using a browser to help.
;; Returns the captured result of stdout, plus # of milliseconds it took to execute.
(define (evaluate e)
  ;; Send the program to the web browser, and wait for the thread to send back
  (channel-put ch e)
  (let ([output+time (channel-get ch)])
    (cond [(error-happened? output+time)
           (raise output+time)]
          [else
           (values (first output+time)
                   (second output+time))])))