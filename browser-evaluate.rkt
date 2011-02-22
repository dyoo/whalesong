#lang racket/base

(require net/sendurl
         racket/list
         web-server/servlet
         web-server/servlet-env
         "package.rkt")

;; A hacky way to test the evaluation.


;; Channel's meant to serialize use of the web server.
(define ch (make-channel))


;; start up the web server
;; The web server responds to two types of requests
;; ?p    Inputting a program
;; ?r    Getting a response
(void
 (thread (lambda ()
          (define (start req)
            (cond
              [(exists-binding? 'p (request-bindings req))
               ;; Create a web page whose content contains
               ;; the script.
               (let ([program (channel-get ch)]
                     [op (open-output-bytes)])
                 (fprintf op #<<EOF
<html>
<head>
<script>
EOF
                          )
                 (package program op)
                 (fprintf op #<<EOF
</script>
<script>
// http://www.quirksmode.org/js/xmlhttp.html
//
function sendRequest(url,callback,postData) {
	var req = createXMLHTTPObject();
	if (!req) return;
	var method = (postData) ? "POST" : "GET";
	req.open(method,url,true);
	req.setRequestHeader('User-Agent','XMLHTTP/1.0');
	if (postData) {
  	    req.setRequestHeader('Content-type','application/x-www-form-urlencoded');
        }
	req.onreadystatechange = function () {
		if (req.readyState != 4) return;
		if (req.status != 200 && req.status != 304) {
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


var whenLoaded = function() {
    invoke(function(v) {
        alert('ok'); 
        sendRequest("/eval", function(req) {},
                    "r=" + encodeURIComponent(v));
    });
};
</script>
</head>
<body onload="whenLoaded()">
<p>Running program.</p>
</body>
</html>
EOF
                          )
                 (response/full 200 #"Okay" 
                                (current-seconds) 
                                TEXT/HTML-MIME-TYPE
                                empty 
                                (list #"" (get-output-bytes op))))]

         
              
              [(exists-binding? 'r (request-bindings req))
               (channel-put ch (extract-binding/single 'r (request-bindings req)))
               `(html (body (p "ok")))]
              [else
               `(html (body (p "Loaded")))]))
          
          (serve/servlet start 
                         #:banner? #f
                         #:launch-browser? #f
                         #:quit? #f
                         #:port 8080
                         #:servlet-path "/eval"))))


;; A little driver to test the evalution of expressions, using a browser to help.
(define (evaluate e)
  ;; Send the program to the web browser, and wait for the thread to send back
  (send-url "http://localhost:8080/eval?p=t" #f)
  (channel-put ch e)
  (channel-get ch))