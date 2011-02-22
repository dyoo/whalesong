#lang racket/base

(require net/sendurl
         racket/list
         web-server/servlet
         web-server/servlet-env
         "package.rkt")

;; A hacky way to test the evaluation.
(provide evaluate)

(define port (+ 8000 (random 8000)))


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
    var output = [];
    MACHINE.params.currentDisplayer = function(v) {
        output.push(String(v));
    };
    setTimeout(
        function() {
            var startTime = new Date();
            invoke(function() {
                var endTime = new Date();
                document.body.appendChild(document.createTextNode(
                   "Program evaluated; sending back to DrRacket."));
                sendRequest("/eval", function(req) {},
                            "r=" + encodeURIComponent(output.join('')) +
                            "&t=" + encodeURIComponent(String(endTime - startTime)));
            });
        }, 0); 
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
               (channel-put ch (list (extract-binding/single 'r (request-bindings req))
                                     (extract-binding/single 't (request-bindings req))))
               `(html (body (p "ok")))]
              [else
               `(html (body (p "Loaded")))]))
          
          (serve/servlet start 
                         #:banner? #f
                         #:launch-browser? #f
                         #:quit? #f
                         #:port port
                         #:servlet-path "/eval"))))



;; evaluate: sexp -> (values string number)
;; A little driver to test the evalution of expressions, using a browser to help.
;; Returns the captured result of stdout, plus # of milliseconds it took to execute.
(define (evaluate e)
  ;; Send the program to the web browser, and wait for the thread to send back
  (send-url (format "http://localhost:~a/eval?p=t" port) #f)
  (channel-put ch e)
  (let ([output+time (channel-get ch)])
    (values (first output+time)
            (string->number (second output+time)))))