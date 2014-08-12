
// The following contains the Whalesong runtime.


(function(window, undefined) {
    // dyoo: this library has been modified slightly so it checks to see
    // if window.jQuery has already been installed.  This is to prevent an ugly issue
    // with regards to a memory leak if one tries to repeatedly load jQuery.
    // NOTE: this portion of the file (jquery-protect-header.js) is intentionally
    // unbalanced.  It'll be closed up by jquery-protect-footer.js.
    if (!window.jQuery) {
/*! jQuery v1.6.3 http://jquery.com/ | http://jquery.org/license */
(function(a,b){function cu(a){return f.isWindow(a)?a:a.nodeType===9?a.defaultView||a.parentWindow:!1}function cr(a){if(!cg[a]){var b=c.body,d=f("<"+a+">").appendTo(b),e=d.css("display");d.remove();if(e==="none"||e===""){ch||(ch=c.createElement("iframe"),ch.frameBorder=ch.width=ch.height=0),b.appendChild(ch);if(!ci||!ch.createElement)ci=(ch.contentWindow||ch.contentDocument).document,ci.write((c.compatMode==="CSS1Compat"?"<!doctype html>":"")+"<html><body>"),ci.close();d=ci.createElement(a),ci.body.appendChild(d),e=f.css(d,"display"),b.removeChild(ch)}cg[a]=e}return cg[a]}function cq(a,b){var c={};f.each(cm.concat.apply([],cm.slice(0,b)),function(){c[this]=a});return c}function cp(){cn=b}function co(){setTimeout(cp,0);return cn=f.now()}function cf(){try{return new a.ActiveXObject("Microsoft.XMLHTTP")}catch(b){}}function ce(){try{return new a.XMLHttpRequest}catch(b){}}function b$(a,c){a.dataFilter&&(c=a.dataFilter(c,a.dataType));var d=a.dataTypes,e={},g,h,i=d.length,j,k=d[0],l,m,n,o,p;for(g=1;g<i;g++){if(g===1)for(h in a.converters)typeof h=="string"&&(e[h.toLowerCase()]=a.converters[h]);l=k,k=d[g];if(k==="*")k=l;else if(l!=="*"&&l!==k){m=l+" "+k,n=e[m]||e["* "+k];if(!n){p=b;for(o in e){j=o.split(" ");if(j[0]===l||j[0]==="*"){p=e[j[1]+" "+k];if(p){o=e[o],o===!0?n=p:p===!0&&(n=o);break}}}}!n&&!p&&f.error("No conversion from "+m.replace(" "," to ")),n!==!0&&(c=n?n(c):p(o(c)))}}return c}function bZ(a,c,d){var e=a.contents,f=a.dataTypes,g=a.responseFields,h,i,j,k;for(i in g)i in d&&(c[g[i]]=d[i]);while(f[0]==="*")f.shift(),h===b&&(h=a.mimeType||c.getResponseHeader("content-type"));if(h)for(i in e)if(e[i]&&e[i].test(h)){f.unshift(i);break}if(f[0]in d)j=f[0];else{for(i in d){if(!f[0]||a.converters[i+" "+f[0]]){j=i;break}k||(k=i)}j=j||k}if(j){j!==f[0]&&f.unshift(j);return d[j]}}function bY(a,b,c,d){if(f.isArray(b))f.each(b,function(b,e){c||bA.test(a)?d(a,e):bY(a+"["+(typeof e=="object"||f.isArray(e)?b:"")+"]",e,c,d)});else if(!c&&b!=null&&typeof b=="object")for(var e in b)bY(a+"["+e+"]",b[e],c,d);else d(a,b)}function bX(a,c){var d,e,g=f.ajaxSettings.flatOptions||{};for(d in c)c[d]!==b&&((g[d]?a:e||(e={}))[d]=c[d]);e&&f.extend(!0,a,e)}function bW(a,c,d,e,f,g){f=f||c.dataTypes[0],g=g||{},g[f]=!0;var h=a[f],i=0,j=h?h.length:0,k=a===bP,l;for(;i<j&&(k||!l);i++)l=h[i](c,d,e),typeof l=="string"&&(!k||g[l]?l=b:(c.dataTypes.unshift(l),l=bW(a,c,d,e,l,g)));(k||!l)&&!g["*"]&&(l=bW(a,c,d,e,"*",g));return l}function bV(a){return function(b,c){typeof b!="string"&&(c=b,b="*");if(f.isFunction(c)){var d=b.toLowerCase().split(bL),e=0,g=d.length,h,i,j;for(;e<g;e++)h=d[e],j=/^\+/.test(h),j&&(h=h.substr(1)||"*"),i=a[h]=a[h]||[],i[j?"unshift":"push"](c)}}}function by(a,b,c){var d=b==="width"?a.offsetWidth:a.offsetHeight,e=b==="width"?bt:bu;if(d>0){c!=="border"&&f.each(e,function(){c||(d-=parseFloat(f.css(a,"padding"+this))||0),c==="margin"?d+=parseFloat(f.css(a,c+this))||0:d-=parseFloat(f.css(a,"border"+this+"Width"))||0});return d+"px"}d=bv(a,b,b);if(d<0||d==null)d=a.style[b]||0;d=parseFloat(d)||0,c&&f.each(e,function(){d+=parseFloat(f.css(a,"padding"+this))||0,c!=="padding"&&(d+=parseFloat(f.css(a,"border"+this+"Width"))||0),c==="margin"&&(d+=parseFloat(f.css(a,c+this))||0)});return d+"px"}function bl(a,b){b.src?f.ajax({url:b.src,async:!1,dataType:"script"}):f.globalEval((b.text||b.textContent||b.innerHTML||"").replace(bd,"/*$0*/")),b.parentNode&&b.parentNode.removeChild(b)}function bk(a){f.nodeName(a,"input")?bj(a):"getElementsByTagName"in a&&f.grep(a.getElementsByTagName("input"),bj)}function bj(a){if(a.type==="checkbox"||a.type==="radio")a.defaultChecked=a.checked}function bi(a){return"getElementsByTagName"in a?a.getElementsByTagName("*"):"querySelectorAll"in a?a.querySelectorAll("*"):[]}function bh(a,b){var c;if(b.nodeType===1){b.clearAttributes&&b.clearAttributes(),b.mergeAttributes&&b.mergeAttributes(a),c=b.nodeName.toLowerCase();if(c==="object")b.outerHTML=a.outerHTML;else if(c!=="input"||a.type!=="checkbox"&&a.type!=="radio"){if(c==="option")b.selected=a.defaultSelected;else if(c==="input"||c==="textarea")b.defaultValue=a.defaultValue}else a.checked&&(b.defaultChecked=b.checked=a.checked),b.value!==a.value&&(b.value=a.value);b.removeAttribute(f.expando)}}function bg(a,b){if(b.nodeType===1&&!!f.hasData(a)){var c=f.expando,d=f.data(a),e=f.data(b,d);if(d=d[c]){var g=d.events;e=e[c]=f.extend({},d);if(g){delete e.handle,e.events={};for(var h in g)for(var i=0,j=g[h].length;i<j;i++)f.event.add(b,h+(g[h][i].namespace?".":"")+g[h][i].namespace,g[h][i],g[h][i].data)}}}}function bf(a,b){return f.nodeName(a,"table")?a.getElementsByTagName("tbody")[0]||a.appendChild(a.ownerDocument.createElement("tbody")):a}function V(a,b,c){b=b||0;if(f.isFunction(b))return f.grep(a,function(a,d){var e=!!b.call(a,d,a);return e===c});if(b.nodeType)return f.grep(a,function(a,d){return a===b===c});if(typeof b=="string"){var d=f.grep(a,function(a){return a.nodeType===1});if(Q.test(b))return f.filter(b,d,!c);b=f.filter(b,d)}return f.grep(a,function(a,d){return f.inArray(a,b)>=0===c})}function U(a){return!a||!a.parentNode||a.parentNode.nodeType===11}function M(a,b){return(a&&a!=="*"?a+".":"")+b.replace(y,"`").replace(z,"&")}function L(a){var b,c,d,e,g,h,i,j,k,l,m,n,o,p=[],q=[],r=f._data(this,"events");if(!(a.liveFired===this||!r||!r.live||a.target.disabled||a.button&&a.type==="click")){a.namespace&&(n=new RegExp("(^|\\.)"+a.namespace.split(".").join("\\.(?:.*\\.)?")+"(\\.|$)")),a.liveFired=this;var s=r.live.slice(0);for(i=0;i<s.length;i++)g=s[i],g.origType.replace(w,"")===a.type?q.push(g.selector):s.splice(i--,1);e=f(a.target).closest(q,a.currentTarget);for(j=0,k=e.length;j<k;j++){m=e[j];for(i=0;i<s.length;i++){g=s[i];if(m.selector===g.selector&&(!n||n.test(g.namespace))&&!m.elem.disabled){h=m.elem,d=null;if(g.preType==="mouseenter"||g.preType==="mouseleave")a.type=g.preType,d=f(a.relatedTarget).closest(g.selector)[0],d&&f.contains(h,d)&&(d=h);(!d||d!==h)&&p.push({elem:h,handleObj:g,level:m.level})}}}for(j=0,k=p.length;j<k;j++){e=p[j];if(c&&e.level>c)break;a.currentTarget=e.elem,a.data=e.handleObj.data,a.handleObj=e.handleObj,o=e.handleObj.origHandler.apply(e.elem,arguments);if(o===!1||a.isPropagationStopped()){c=e.level,o===!1&&(b=!1);if(a.isImmediatePropagationStopped())break}}return b}}function J(a,c,d){var e=f.extend({},d[0]);e.type=a,e.originalEvent={},e.liveFired=b,f.event.handle.call(c,e),e.isDefaultPrevented()&&d[0].preventDefault()}function D(){return!0}function C(){return!1}function m(a,c,d){var e=c+"defer",g=c+"queue",h=c+"mark",i=f.data(a,e,b,!0);i&&(d==="queue"||!f.data(a,g,b,!0))&&(d==="mark"||!f.data(a,h,b,!0))&&setTimeout(function(){!f.data(a,g,b,!0)&&!f.data(a,h,b,!0)&&(f.removeData(a,e,!0),i.resolve())},0)}function l(a){for(var b in a)if(b!=="toJSON")return!1;return!0}function k(a,c,d){if(d===b&&a.nodeType===1){var e="data-"+c.replace(j,"$1-$2").toLowerCase();d=a.getAttribute(e);if(typeof d=="string"){try{d=d==="true"?!0:d==="false"?!1:d==="null"?null:f.isNaN(d)?i.test(d)?f.parseJSON(d):d:parseFloat(d)}catch(g){}f.data(a,c,d)}else d=b}return d}var c=a.document,d=a.navigator,e=a.location,f=function(){function K(){if(!e.isReady){try{c.documentElement.doScroll("left")}catch(a){setTimeout(K,1);return}e.ready()}}var e=function(a,b){return new e.fn.init(a,b,h)},f=a.jQuery,g=a.$,h,i=/^(?:[^#<]*(<[\w\W]+>)[^>]*$|#([\w\-]*)$)/,j=/\S/,k=/^\s+/,l=/\s+$/,m=/\d/,n=/^<(\w+)\s*\/?>(?:<\/\1>)?$/,o=/^[\],:{}\s]*$/,p=/\\(?:["\\\/bfnrt]|u[0-9a-fA-F]{4})/g,q=/"[^"\\\n\r]*"|true|false|null|-?\d+(?:\.\d*)?(?:[eE][+\-]?\d+)?/g,r=/(?:^|:|,)(?:\s*\[)+/g,s=/(webkit)[ \/]([\w.]+)/,t=/(opera)(?:.*version)?[ \/]([\w.]+)/,u=/(msie) ([\w.]+)/,v=/(mozilla)(?:.*? rv:([\w.]+))?/,w=/-([a-z]|[0-9])/ig,x=/^-ms-/,y=function(a,b){return(b+"").toUpperCase()},z=d.userAgent,A,B,C,D=Object.prototype.toString,E=Object.prototype.hasOwnProperty,F=Array.prototype.push,G=Array.prototype.slice,H=String.prototype.trim,I=Array.prototype.indexOf,J={};e.fn=e.prototype={constructor:e,init:function(a,d,f){var g,h,j,k;if(!a)return this;if(a.nodeType){this.context=this[0]=a,this.length=1;return this}if(a==="body"&&!d&&c.body){this.context=c,this[0]=c.body,this.selector=a,this.length=1;return this}if(typeof a=="string"){a.charAt(0)!=="<"||a.charAt(a.length-1)!==">"||a.length<3?g=i.exec(a):g=[null,a,null];if(g&&(g[1]||!d)){if(g[1]){d=d instanceof e?d[0]:d,k=d?d.ownerDocument||d:c,j=n.exec(a),j?e.isPlainObject(d)?(a=[c.createElement(j[1])],e.fn.attr.call(a,d,!0)):a=[k.createElement(j[1])]:(j=e.buildFragment([g[1]],[k]),a=(j.cacheable?e.clone(j.fragment):j.fragment).childNodes);return e.merge(this,a)}h=c.getElementById(g[2]);if(h&&h.parentNode){if(h.id!==g[2])return f.find(a);this.length=1,this[0]=h}this.context=c,this.selector=a;return this}return!d||d.jquery?(d||f).find(a):this.constructor(d).find(a)}if(e.isFunction(a))return f.ready(a);a.selector!==b&&(this.selector=a.selector,this.context=a.context);return e.makeArray(a,this)},selector:"",jquery:"1.6.3",length:0,size:function(){return this.length},toArray:function(){return G.call(this,0)},get:function(a){return a==null?this.toArray():a<0?this[this.length+a]:this[a]},pushStack:function(a,b,c){var d=this.constructor();e.isArray(a)?F.apply(d,a):e.merge(d,a),d.prevObject=this,d.context=this.context,b==="find"?d.selector=this.selector+(this.selector?" ":"")+c:b&&(d.selector=this.selector+"."+b+"("+c+")");return d},each:function(a,b){return e.each(this,a,b)},ready:function(a){e.bindReady(),B.done(a);return this},eq:function(a){return a===-1?this.slice(a):this.slice(a,+a+1)},first:function(){return this.eq(0)},last:function(){return this.eq(-1)},slice:function(){return this.pushStack(G.apply(this,arguments),"slice",G.call(arguments).join(","))},map:function(a){return this.pushStack(e.map(this,function(b,c){return a.call(b,c,b)}))},end:function(){return this.prevObject||this.constructor(null)},push:F,sort:[].sort,splice:[].splice},e.fn.init.prototype=e.fn,e.extend=e.fn.extend=function(){var a,c,d,f,g,h,i=arguments[0]||{},j=1,k=arguments.length,l=!1;typeof i=="boolean"&&(l=i,i=arguments[1]||{},j=2),typeof i!="object"&&!e.isFunction(i)&&(i={}),k===j&&(i=this,--j);for(;j<k;j++)if((a=arguments[j])!=null)for(c in a){d=i[c],f=a[c];if(i===f)continue;l&&f&&(e.isPlainObject(f)||(g=e.isArray(f)))?(g?(g=!1,h=d&&e.isArray(d)?d:[]):h=d&&e.isPlainObject(d)?d:{},i[c]=e.extend(l,h,f)):f!==b&&(i[c]=f)}return i},e.extend({noConflict:function(b){a.$===e&&(a.$=g),b&&a.jQuery===e&&(a.jQuery=f);return e},isReady:!1,readyWait:1,holdReady:function(a){a?e.readyWait++:e.ready(!0)},ready:function(a){if(a===!0&&!--e.readyWait||a!==!0&&!e.isReady){if(!c.body)return setTimeout(e.ready,1);e.isReady=!0;if(a!==!0&&--e.readyWait>0)return;B.resolveWith(c,[e]),e.fn.trigger&&e(c).trigger("ready").unbind("ready")}},bindReady:function(){if(!B){B=e._Deferred();if(c.readyState==="complete")return setTimeout(e.ready,1);if(c.addEventListener)c.addEventListener("DOMContentLoaded",C,!1),a.addEventListener("load",e.ready,!1);else if(c.attachEvent){c.attachEvent("onreadystatechange",C),a.attachEvent("onload",e.ready);var b=!1;try{b=a.frameElement==null}catch(d){}c.documentElement.doScroll&&b&&K()}}},isFunction:function(a){return e.type(a)==="function"},isArray:Array.isArray||function(a){return e.type(a)==="array"},isWindow:function(a){return a&&typeof a=="object"&&"setInterval"in a},isNaN:function(a){return a==null||!m.test(a)||isNaN(a)},type:function(a){return a==null?String(a):J[D.call(a)]||"object"},isPlainObject:function(a){if(!a||e.type(a)!=="object"||a.nodeType||e.isWindow(a))return!1;try{if(a.constructor&&!E.call(a,"constructor")&&!E.call(a.constructor.prototype,"isPrototypeOf"))return!1}catch(c){return!1}var d;for(d in a);return d===b||E.call(a,d)},isEmptyObject:function(a){for(var b in a)return!1;return!0},error:function(a){throw a},parseJSON:function(b){if(typeof b!="string"||!b)return null;b=e.trim(b);if(a.JSON&&a.JSON.parse)return a.JSON.parse(b);if(o.test(b.replace(p,"@").replace(q,"]").replace(r,"")))return(new Function("return "+b))();e.error("Invalid JSON: "+b)},parseXML:function(c){var d,f;try{a.DOMParser?(f=new DOMParser,d=f.parseFromString(c,"text/xml")):(d=new ActiveXObject("Microsoft.XMLDOM"),d.async="false",d.loadXML(c))}catch(g){d=b}(!d||!d.documentElement||d.getElementsByTagName("parsererror").length)&&e.error("Invalid XML: "+c);return d},noop:function(){},globalEval:function(b){b&&j.test(b)&&(a.execScript||function(b){a.eval.call(a,b)})(b)},camelCase:function(a){return a.replace(x,"ms-").replace(w,y)},nodeName:function(a,b){return a.nodeName&&a.nodeName.toUpperCase()===b.toUpperCase()},each:function(a,c,d){var f,g=0,h=a.length,i=h===b||e.isFunction(a);if(d){if(i){for(f in a)if(c.apply(a[f],d)===!1)break}else for(;g<h;)if(c.apply(a[g++],d)===!1)break}else if(i){for(f in a)if(c.call(a[f],f,a[f])===!1)break}else for(;g<h;)if(c.call(a[g],g,a[g++])===!1)break;return a},trim:H?function(a){return a==null?"":H.call(a)}:function(a){return a==null?"":(a+"").replace(k,"").replace(l,"")},makeArray:function(a,b){var c=b||[];if(a!=null){var d=e.type(a);a.length==null||d==="string"||d==="function"||d==="regexp"||e.isWindow(a)?F.call(c,a):e.merge(c,a)}return c},inArray:function(a,b){if(!b)return-1;if(I)return I.call(b,a);for(var c=0,d=b.length;c<d;c++)if(b[c]===a)return c;return-1},merge:function(a,c){var d=a.length,e=0;if(typeof c.length=="number")for(var f=c.length;e<f;e++)a[d++]=c[e];else while(c[e]!==b)a[d++]=c[e++];a.length=d;return a},grep:function(a,b,c){var d=[],e;c=!!c;for(var f=0,g=a.length;f<g;f++)e=!!b(a[f],f),c!==e&&d.push(a[f]);return d},map:function(a,c,d){var f,g,h=[],i=0,j=a.length,k=a instanceof e||j!==b&&typeof j=="number"&&(j>0&&a[0]&&a[j-1]||j===0||e.isArray(a));if(k)for(;i<j;i++)f=c(a[i],i,d),f!=null&&(h[h.length]=f);else for(g in a)f=c(a[g],g,d),f!=null&&(h[h.length]=f);return h.concat.apply([],h)},guid:1,proxy:function(a,c){if(typeof c=="string"){var d=a[c];c=a,a=d}if(!e.isFunction(a))return b;var f=G.call(arguments,2),g=function(){return a.apply(c,f.concat(G.call(arguments)))};g.guid=a.guid=a.guid||g.guid||e.guid++;return g},access:function(a,c,d,f,g,h){var i=a.length;if(typeof c=="object"){for(var j in c)e.access(a,j,c[j],f,g,d);return a}if(d!==b){f=!h&&f&&e.isFunction(d);for(var k=0;k<i;k++)g(a[k],c,f?d.call(a[k],k,g(a[k],c)):d,h);return a}return i?g(a[0],c):b},now:function(){return(new Date).getTime()},uaMatch:function(a){a=a.toLowerCase();var b=s.exec(a)||t.exec(a)||u.exec(a)||a.indexOf("compatible")<0&&v.exec(a)||[];return{browser:b[1]||"",version:b[2]||"0"}},sub:function(){function a(b,c){return new a.fn.init(b,c)}e.extend(!0,a,this),a.superclass=this,a.fn=a.prototype=this(),a.fn.constructor=a,a.sub=this.sub,a.fn.init=function(d,f){f&&f instanceof e&&!(f instanceof a)&&(f=a(f));return e.fn.init.call(this,d,f,b)},a.fn.init.prototype=a.fn;var b=a(c);return a},browser:{}}),e.each("Boolean Number String Function Array Date RegExp Object".split(" "),function(a,b){J["[object "+b+"]"]=b.toLowerCase()}),A=e.uaMatch(z),A.browser&&(e.browser[A.browser]=!0,e.browser.version=A.version),e.browser.webkit&&(e.browser.safari=!0),j.test(" ")&&(k=/^[\s\xA0]+/,l=/[\s\xA0]+$/),h=e(c),c.addEventListener?C=function(){c.removeEventListener("DOMContentLoaded",C,!1),e.ready()}:c.attachEvent&&(C=function(){c.readyState==="complete"&&(c.detachEvent("onreadystatechange",C),e.ready())});return e}(),g="done fail isResolved isRejected promise then always pipe".split(" "),h=[].slice;f.extend({_Deferred:function(){var a=[],b,c,d,e={done:function(){if(!d){var c=arguments,g,h,i,j,k;b&&(k=b,b=0);for(g=0,h=c.length;g<h;g++)i=c[g],j=f.type(i),j==="array"?e.done.apply(e,i):j==="function"&&a.push(i);k&&e.resolveWith(k[0],k[1])}return this},resolveWith:function(e,f){if(!d&&!b&&!c){f=f||[],c=1;try{while(a[0])a.shift().apply(e,f)}finally{b=[e,f],c=0}}return this},resolve:function(){e.resolveWith(this,arguments);return this},isResolved:function(){return!!c||!!b},cancel:function(){d=1,a=[];return this}};return e},Deferred:function(a){var b=f._Deferred(),c=f._Deferred(),d;f.extend(b,{then:function(a,c){b.done(a).fail(c);return this},always:function(){return b.done.apply(b,arguments).fail.apply(this,arguments)},fail:c.done,rejectWith:c.resolveWith,reject:c.resolve,isRejected:c.isResolved,pipe:function(a,c){return f.Deferred(function(d){f.each({done:[a,"resolve"],fail:[c,"reject"]},function(a,c){var e=c[0],g=c[1],h;f.isFunction(e)?b[a](function(){h=e.apply(this,arguments),h&&f.isFunction(h.promise)?h.promise().then(d.resolve,d.reject):d[g+"With"](this===b?d:this,[h])}):b[a](d[g])})}).promise()},promise:function(a){if(a==null){if(d)return d;d=a={}}var c=g.length;while(c--)a[g[c]]=b[g[c]];return a}}),b.done(c.cancel).fail(b.cancel),delete b.cancel,a&&a.call(b,b);return b},when:function(a){function i(a){return function(c){b[a]=arguments.length>1?h.call(arguments,0):c,--e||g.resolveWith(g,h.call(b,0))}}var b=arguments,c=0,d=b.length,e=d,g=d<=1&&a&&f.isFunction(a.promise)?a:f.Deferred();if(d>1){for(;c<d;c++)b[c]&&f.isFunction(b[c].promise)?b[c].promise().then(i(c),g.reject):--e;e||g.resolveWith(g,b)}else g!==a&&g.resolveWith(g,d?[a]:[]);return g.promise()}}),f.support=function(){var a=c.createElement("div"),b=c.documentElement,d,e,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u;a.setAttribute("className","t"),a.innerHTML="   <link><table></table><a href='/a' style='top:1px;float:left;opacity:.55;'>a</a><input type=checkbox>",d=a.getElementsByTagName("*"),e=a.getElementsByTagName("a")[0];if(!d||!d.length||!e)return{};g=c.createElement("select"),h=g.appendChild(c.createElement("option")),i=a.getElementsByTagName("input")[0],k={leadingWhitespace:a.firstChild.nodeType===3,tbody:!a.getElementsByTagName("tbody").length,htmlSerialize:!!a.getElementsByTagName("link").length,style:/top/.test(e.getAttribute("style")),hrefNormalized:e.getAttribute("href")==="/a",opacity:/^0.55$/.test(e.style.opacity),cssFloat:!!e.style.cssFloat,checkOn:i.value==="on",optSelected:h.selected,getSetAttribute:a.className!=="t",submitBubbles:!0,changeBubbles:!0,focusinBubbles:!1,deleteExpando:!0,noCloneEvent:!0,inlineBlockNeedsLayout:!1,shrinkWrapBlocks:!1,reliableMarginRight:!0},i.checked=!0,k.noCloneChecked=i.cloneNode(!0).checked,g.disabled=!0,k.optDisabled=!h.disabled;try{delete a.test}catch(v){k.deleteExpando=!1}!a.addEventListener&&a.attachEvent&&a.fireEvent&&(a.attachEvent("onclick",function(){k.noCloneEvent=!1}),a.cloneNode(!0).fireEvent("onclick")),i=c.createElement("input"),i.value="t",i.setAttribute("type","radio"),k.radioValue=i.value==="t",i.setAttribute("checked","checked"),a.appendChild(i),l=c.createDocumentFragment(),l.appendChild(a.firstChild),k.checkClone=l.cloneNode(!0).cloneNode(!0).lastChild.checked,a.innerHTML="",a.style.width=a.style.paddingLeft="1px",m=c.getElementsByTagName("body")[0],o=c.createElement(m?"div":"body"),p={visibility:"hidden",width:0,height:0,border:0,margin:0,background:"none"},m&&f.extend(p,{position:"absolute",left:"-1000px",top:"-1000px"});for(t in p)o.style[t]=p[t];o.appendChild(a),n=m||b,n.insertBefore(o,n.firstChild),k.appendChecked=i.checked,k.boxModel=a.offsetWidth===2,"zoom"in a.style&&(a.style.display="inline",a.style.zoom=1,k.inlineBlockNeedsLayout=a.offsetWidth===2,a.style.display="",a.innerHTML="<div style='width:4px;'></div>",k.shrinkWrapBlocks=a.offsetWidth!==2),a.innerHTML="<table><tr><td style='padding:0;border:0;display:none'></td><td>t</td></tr></table>",q=a.getElementsByTagName("td"),u=q[0].offsetHeight===0,q[0].style.display="",q[1].style.display="none",k.reliableHiddenOffsets=u&&q[0].offsetHeight===0,a.innerHTML="",c.defaultView&&c.defaultView.getComputedStyle&&(j=c.createElement("div"),j.style.width="0",j.style.marginRight="0",a.appendChild(j),k.reliableMarginRight=(parseInt((c.defaultView.getComputedStyle(j,null)||{marginRight:0}).marginRight,10)||0)===0),o.innerHTML="",n.removeChild(o);if(a.attachEvent)for(t in{submit:1,change:1,focusin:1})s="on"+t,u=s in a,u||(a.setAttribute(s,"return;"),u=typeof a[s]=="function"),k[t+"Bubbles"]=u;o=l=g=h=m=j=a=i=null;return k}(),f.boxModel=f.support.boxModel;var i=/^(?:\{.*\}|\[.*\])$/,j=/([a-z])([A-Z])/g;f.extend({cache:{},uuid:0,expando:"jQuery"+(f.fn.jquery+Math.random()).replace(/\D/g,""),noData:{embed:!0,object:"clsid:D27CDB6E-AE6D-11cf-96B8-444553540000",applet:!0},hasData:function(a){a=a.nodeType?f.cache[a[f.expando]]:a[f.expando];return!!a&&!l(a)},data:function(a,c,d,e){if(!!f.acceptData(a)){var g,h,i=f.expando,j=typeof c=="string",k=a.nodeType,l=k?f.cache:a,m=k?a[f.expando]:a[f.expando]&&f.expando;if((!m||e&&m&&l[m]&&!l[m][i])&&j&&d===b)return;m||(k?a[f.expando]=m=++f.uuid:m=f.expando),l[m]||(l[m]={},k||(l[m].toJSON=f.noop));if(typeof c=="object"||typeof c=="function")e?l[m][i]=f.extend(l[m][i],c):l[m]=f.extend(l[m],c);g=l[m],e&&(g[i]||(g[i]={}),g=g[i]),d!==b&&(g[f.camelCase(c)]=d);if(c==="events"&&!g[c])return g[i]&&g[i].events;j?(h=g[c],h==null&&(h=g[f.camelCase(c)])):h=g;return h}},removeData:function(a,b,c){if(!!f.acceptData(a)){var d,e=f.expando,g=a.nodeType,h=g?f.cache:a,i=g?a[f.expando]:f.expando;if(!h[i])return;if(b){d=c?h[i][e]:h[i];if(d){d[b]||(b=f.camelCase(b)),delete d[b];if(!l(d))return}}if(c){delete h[i][e];if(!l(h[i]))return}var j=h[i][e];f.support.deleteExpando||!h.setInterval?delete h[i]:h[i]=null,j?(h[i]={},g||(h[i].toJSON=f.noop),h[i][e]=j):g&&(f.support.deleteExpando?delete a[f.expando]:a.removeAttribute?a.removeAttribute(f.expando):a[f.expando]=null)}},_data:function(a,b,c){return f.data(a,b,c,!0)},acceptData:function(a){if(a.nodeName){var b=f.noData[a.nodeName.toLowerCase()];if(b)return b!==!0&&a.getAttribute("classid")===b}return!0}}),f.fn.extend({data:function(a,c){var d=null;if(typeof a=="undefined"){if(this.length){d=f.data(this[0]);if(this[0].nodeType===1){var e=this[0].attributes,g;for(var h=0,i=e.length;h<i;h++)g=e[h].name,g.indexOf("data-")===0&&(g=f.camelCase(g.substring(5)),k(this[0],g,d[g]))}}return d}if(typeof a=="object")return this.each(function(){f.data(this,a)});var j=a.split(".");j[1]=j[1]?"."+j[1]:"";if(c===b){d=this.triggerHandler("getData"+j[1]+"!",[j[0]]),d===b&&this.length&&(d=f.data(this[0],a),d=k(this[0],a,d));return d===b&&j[1]?this.data(j[0]):d}return this.each(function(){var b=f(this),d=[j[0],c];b.triggerHandler("setData"+j[1]+"!",d),f.data(this,a,c),b.triggerHandler("changeData"+j[1]+"!",d)})},removeData:function(a){return this.each(function(){f.removeData(this,a)})}}),f.extend({_mark:function(a,c){a&&(c=(c||"fx")+"mark",f.data(a,c,(f.data(a,c,b,!0)||0)+1,!0))},_unmark:function(a,c,d){a!==!0&&(d=c,c=a,a=!1);if(c){d=d||"fx";var e=d+"mark",g=a?0:(f.data(c,e,b,!0)||1)-1;g?f.data(c,e,g,!0):(f.removeData(c,e,!0),m(c,d,"mark"))}},queue:function(a,c,d){if(a){c=(c||"fx")+"queue";var e=f.data(a,c,b,!0);d&&(!e||f.isArray(d)?e=f.data(a,c,f.makeArray(d),!0):e.push(d));return e||[]}},dequeue:function(a,b){b=b||"fx";var c=f.queue(a,b),d=c.shift(),e;d==="inprogress"&&(d=c.shift()),d&&(b==="fx"&&c.unshift("inprogress"),d.call(a,function(){f.dequeue(a,b)})),c.length||(f.removeData(a,b+"queue",!0),m(a,b,"queue"))}}),f.fn.extend({queue:function(a,c){typeof a!="string"&&(c=a,a="fx");if(c===b)return f.queue(this[0],a);return this.each(function(){var b=f.queue(this,a,c);a==="fx"&&b[0]!=="inprogress"&&f.dequeue(this,a)})},dequeue:function(a){return this.each(function(){f.dequeue(this,a)})},delay:function(a,b){a=f.fx?f.fx.speeds[a]||a:a,b=b||"fx";return this.queue(b,function(){var c=this;setTimeout(function(){f.dequeue(c,b)},a)})},clearQueue:function(a){return this.queue(a||"fx",[])},promise:function(a,c){function m(){--h||d.resolveWith(e,[e])}typeof a!="string"&&(c=a,a=b),a=a||"fx";var d=f.Deferred(),e=this,g=e.length,h=1,i=a+"defer",j=a+"queue",k=a+"mark",l;while(g--)if(l=f.data(e[g],i,b,!0)||(f.data(e[g],j,b,!0)||f.data(e[g],k,b,!0))&&f.data(e[g],i,f._Deferred(),!0))h++,l.done(m);m();return d.promise()}});var n=/[\n\t\r]/g,o=/\s+/,p=/\r/g,q=/^(?:button|input)$/i,r=/^(?:button|input|object|select|textarea)$/i,s=/^a(?:rea)?$/i,t=/^(?:autofocus|autoplay|async|checked|controls|defer|disabled|hidden|loop|multiple|open|readonly|required|scoped|selected)$/i,u,v;f.fn.extend({attr:function(a,b){return f.access(this,a,b,!0,f.attr)},removeAttr:function(a){return this.each(function(){f.removeAttr(this,a)})},prop:function(a,b){return f.access(this,a,b,!0,f.prop)},removeProp:function(a){a=f.propFix[a]||a;return this.each(function(){try{this[a]=b,delete this[a]}catch(c){}})},addClass:function(a){var b,c,d,e,g,h,i;if(f.isFunction(a))return this.each(function(b){f(this).addClass(a.call(this,b,this.className))});if(a&&typeof a=="string"){b=a.split(o);for(c=0,d=this.length;c<d;c++){e=this[c];if(e.nodeType===1)if(!e.className&&b.length===1)e.className=a;else{g=" "+e.className+" ";for(h=0,i=b.length;h<i;h++)~g.indexOf(" "+b[h]+" ")||(g+=b[h]+" ");e.className=f.trim(g)}}}return this},removeClass:function(a){var c,d,e,g,h,i,j;if(f.isFunction(a))return this.each(function(b){f(this).removeClass(a.call(this,b,this.className))});if(a&&typeof a=="string"||a===b){c=(a||"").split(o);for(d=0,e=this.length;d<e;d++){g=this[d];if(g.nodeType===1&&g.className)if(a){h=(" "+g.className+" ").replace(n," ");for(i=0,j=c.length;i<j;i++)h=h.replace(" "+c[i]+" "," ");g.className=f.trim(h)}else g.className=""}}return this},toggleClass:function(a,b){var c=typeof a,d=typeof b=="boolean";if(f.isFunction(a))return this.each(function(c){f(this).toggleClass(a.call(this,c,this.className,b),b)});return this.each(function(){if(c==="string"){var e,g=0,h=f(this),i=b,j=a.split(o);while(e=j[g++])i=d?i:!h.hasClass(e),h[i?"addClass":"removeClass"](e)}else if(c==="undefined"||c==="boolean")this.className&&f._data(this,"__className__",this.className),this.className=this.className||a===!1?"":f._data(this,"__className__")||""})},hasClass:function(a){var b=" "+a+" ";for(var c=0,d=this.length;c<d;c++)if(this[c].nodeType===1&&(" "+this[c].className+" ").replace(n," ").indexOf(b)>-1)return!0;return!1},val:function(a){var c,d,e=this[0];if(!arguments.length){if(e){c=f.valHooks[e.nodeName.toLowerCase()]||f.valHooks[e.type];if(c&&"get"in c&&(d=c.get(e,"value"))!==b)return d;d=e.value;return typeof d=="string"?d.replace(p,""):d==null?"":d}return b}var g=f.isFunction(a);return this.each(function(d){var e=f(this),h;if(this.nodeType===1){g?h=a.call(this,d,e.val()):h=a,h==null?h="":typeof h=="number"?h+="":f.isArray(h)&&(h=f.map(h,function(a){return a==null?"":a+""})),c=f.valHooks[this.nodeName.toLowerCase()]||f.valHooks[this.type];if(!c||!("set"in c)||c.set(this,h,"value")===b)this.value=h}})}}),f.extend({valHooks:{option:{get:function(a){var b=a.attributes.value;return!b||b.specified?a.value:a.text}},select:{get:function(a){var b,c=a.selectedIndex,d=[],e=a.options,g=a.type==="select-one";if(c<0)return null;for(var h=g?c:0,i=g?c+1:e.length;h<i;h++){var j=e[h];if(j.selected&&(f.support.optDisabled?!j.disabled:j.getAttribute("disabled")===null)&&(!j.parentNode.disabled||!f.nodeName(j.parentNode,"optgroup"))){b=f(j).val();if(g)return b;d.push(b)}}if(g&&!d.length&&e.length)return f(e[c]).val();return d},set:function(a,b){var c=f.makeArray(b);f(a).find("option").each(function(){this.selected=f.inArray(f(this).val(),c)>=0}),c.length||(a.selectedIndex=-1);return c}}},attrFn:{val:!0,css:!0,html:!0,text:!0,data:!0,width:!0,height:!0,offset:!0},attrFix:{tabindex:"tabIndex"},attr:function(a,c,d,e){var g=a.nodeType;if(!a||g===3||g===8||g===2)return b;if(e&&c in f.attrFn)return f(a)[c](d);if(!("getAttribute"in a))return f.prop(a,c,d);var h,i,j=g!==1||!f.isXMLDoc(a);j&&(c=f.attrFix[c]||c,i=f.attrHooks[c],i||(t.test(c)?i=v:u&&(i=u)));if(d!==b){if(d===null){f.removeAttr(a,c);return b}if(i&&"set"in i&&j&&(h=i.set(a,d,c))!==b)return h;a.setAttribute(c,""+d);return d}if(i&&"get"in i&&j&&(h=i.get(a,c))!==null)return h;h=a.getAttribute(c);return h===null?b:h},removeAttr:function(a,b){var c;a.nodeType===1&&(b=f.attrFix[b]||b,f.attr(a,b,""),a.removeAttribute(b),t.test(b)&&(c=f.propFix[b]||b)in a&&(a[c]=!1))},attrHooks:{type:{set:function(a,b){if(q.test(a.nodeName)&&a.parentNode)f.error("type property can't be changed");else if(!f.support.radioValue&&b==="radio"&&f.nodeName(a,"input")){var c=a.value;a.setAttribute("type",b),c&&(a.value=c);return b}}},value:{get:function(a,b){if(u&&f.nodeName(a,"button"))return u.get(a,b);return b in a?a.value:null},set:function(a,b,c){if(u&&f.nodeName(a,"button"))return u.set(a,b,c);a.value=b}}},propFix:{tabindex:"tabIndex",readonly:"readOnly","for":"htmlFor","class":"className",maxlength:"maxLength",cellspacing:"cellSpacing",cellpadding:"cellPadding",rowspan:"rowSpan",colspan:"colSpan",usemap:"useMap",frameborder:"frameBorder",contenteditable:"contentEditable"},prop:function(a,c,d){var e=a.nodeType;if(!a||e===3||e===8||e===2)return b;var g,h,i=e!==1||!f.isXMLDoc(a);i&&(c=f.propFix[c]||c,h=f.propHooks[c]);return d!==b?h&&"set"in h&&(g=h.set(a,d,c))!==b?g:a[c]=d:h&&"get"in h&&(g=h.get(a,c))!==null?g:a[c]},propHooks:{tabIndex:{get:function(a){var c=a.getAttributeNode("tabindex");return c&&c.specified?parseInt(c.value,10):r.test(a.nodeName)||s.test(a.nodeName)&&a.href?0:b}}}}),f.attrHooks.tabIndex=f.propHooks.tabIndex,v={get:function(a,c){var d;return f.prop(a,c)===!0||(d=a.getAttributeNode(c))&&d.nodeValue!==!1?c.toLowerCase():b},set:function(a,b,c){var d;b===!1?f.removeAttr(a,c):(d=f.propFix[c]||c,d in a&&(a[d]=!0),a.setAttribute(c,c.toLowerCase()));return c}},f.support.getSetAttribute||(u=f.valHooks.button={get:function(a,c){var d;d=a.getAttributeNode(c);return d&&d.nodeValue!==""?d.nodeValue:b},set:function(a,b,d){var e=a.getAttributeNode(d);e||(e=c.createAttribute(d),a.setAttributeNode(e));return e.nodeValue=b+""}},f.each(["width","height"],function(a,b){f.attrHooks[b]=f.extend(f.attrHooks[b],{set:function(a,c){if(c===""){a.setAttribute(b,"auto");return c}}})})),f.support.hrefNormalized||f.each(["href","src","width","height"],function(a,c){f.attrHooks[c]=f.extend(f.attrHooks[c],{get:function(a){var d=a.getAttribute(c,2);return d===null?b:d}})}),f.support.style||(f.attrHooks.style={get:function(a){return a.style.cssText.toLowerCase()||b},set:function(a,b){return a.style.cssText=""+b}}),f.support.optSelected||(f.propHooks.selected=f.extend(f.propHooks.selected,{get:function(a){var b=a.parentNode;b&&(b.selectedIndex,b.parentNode&&b.parentNode.selectedIndex);return null}})),f.support.checkOn||f.each(["radio","checkbox"],function(){f.valHooks[this]={get:function(a){return a.getAttribute("value")===null?"on":a.value}}}),f.each(["radio","checkbox"],function(){f.valHooks[this]=f.extend(f.valHooks[this],{set:function(a,b){if(f.isArray(b))return a.checked=f.inArray(f(a).val(),b)>=0}})});var w=/\.(.*)$/,x=/^(?:textarea|input|select)$/i,y=/\./g,z=/ /g,A=/[^\w\s.|`]/g,B=function(a){return a.replace(A,"\\$&")};f.event={add:function(a,c,d,e){if(a.nodeType!==3&&a.nodeType!==8){if(d===!1)d=C;else if(!d)return;var g,h;d.handler&&(g=d,d=g.handler),d.guid||(d.guid=f.guid++);var i=f._data(a);if(!i)return;var j=i.events,k=i.handle;j||(i.events=j={}),k||(i.handle=k=function(a){return typeof f!="undefined"&&(!a||f.event.triggered!==a.type)?f.event.handle.apply(k.elem,arguments):b}),k.elem=a,c=c.split(" ");var l,m=0,n;while(l=c[m++]){h=g?f.extend({},g):{handler:d,data:e},l.indexOf(".")>-1?(n=l.split("."),l=n.shift(),h.namespace=n.slice(0).sort().join(".")):(n=[],h.namespace=""),h.type=l,h.guid||(h.guid=d.guid);var o=j[l],p=f.event.special[l]||{};if(!o){o=j[l]=[];if(!p.setup||p.setup.call(a,e,n,k)===!1)a.addEventListener?a.addEventListener(l,k,!1):a.attachEvent&&a.attachEvent("on"+l,k)}p.add&&(p.add.call(a,h),h.handler.guid||(h.handler.guid=d.guid)),o.push(h),f.event.global[l]=!0}a=null}},global:{},remove:function(a,c,d,e){if(a.nodeType!==3&&a.nodeType!==8){d===!1&&(d=C);var g,h,i,j,k=0,l,m,n,o,p,q,r,s=f.hasData(a)&&f._data(a),t=s&&s.events;if(!s||!t)return;c&&c.type&&(d=c.handler,c=c.type);if(!c||typeof c=="string"&&c.charAt(0)==="."){c=c||"";for(h in t)f.event.remove(a,h+c);return}c=c.split(" ");while(h=c[k++]){r=h,q=null,l=h.indexOf(".")<0,m=[],l||(m=h.split("."),h=m.shift(),n=new RegExp("(^|\\.)"+f.map(m.slice(0).sort(),B).join("\\.(?:.*\\.)?")+"(\\.|$)")),p=t[h];if(!p)continue;if(!d){for(j=0;j<p.length;j++){q=p[j];if(l||n.test(q.namespace))f.event.remove(a,r,q.handler,j),p.splice(j--,1)}continue}o=f.event.special[h]||{};for(j=e||0;j<p.length;j++){q=p[j];if(d.guid===q.guid){if(l||n.test(q.namespace))e==null&&p.splice(j--,1),o.remove&&o.remove.call(a,q);if(e!=null)break}}if(p.length===0||e!=null&&p.length===1)(!o.teardown||o.teardown.call(a,m)===!1)&&f.removeEvent(a,h,s.handle),g=null
,delete t[h]}if(f.isEmptyObject(t)){var u=s.handle;u&&(u.elem=null),delete s.events,delete s.handle,f.isEmptyObject(s)&&f.removeData(a,b,!0)}}},customEvent:{getData:!0,setData:!0,changeData:!0},trigger:function(c,d,e,g){var h=c.type||c,i=[],j;h.indexOf("!")>=0&&(h=h.slice(0,-1),j=!0),h.indexOf(".")>=0&&(i=h.split("."),h=i.shift(),i.sort());if(!!e&&!f.event.customEvent[h]||!!f.event.global[h]){c=typeof c=="object"?c[f.expando]?c:new f.Event(h,c):new f.Event(h),c.type=h,c.exclusive=j,c.namespace=i.join("."),c.namespace_re=new RegExp("(^|\\.)"+i.join("\\.(?:.*\\.)?")+"(\\.|$)");if(g||!e)c.preventDefault(),c.stopPropagation();if(!e){f.each(f.cache,function(){var a=f.expando,b=this[a];b&&b.events&&b.events[h]&&f.event.trigger(c,d,b.handle.elem)});return}if(e.nodeType===3||e.nodeType===8)return;c.result=b,c.target=e,d=d!=null?f.makeArray(d):[],d.unshift(c);var k=e,l=h.indexOf(":")<0?"on"+h:"";do{var m=f._data(k,"handle");c.currentTarget=k,m&&m.apply(k,d),l&&f.acceptData(k)&&k[l]&&k[l].apply(k,d)===!1&&(c.result=!1,c.preventDefault()),k=k.parentNode||k.ownerDocument||k===c.target.ownerDocument&&a}while(k&&!c.isPropagationStopped());if(!c.isDefaultPrevented()){var n,o=f.event.special[h]||{};if((!o._default||o._default.call(e.ownerDocument,c)===!1)&&(h!=="click"||!f.nodeName(e,"a"))&&f.acceptData(e)){try{l&&e[h]&&(n=e[l],n&&(e[l]=null),f.event.triggered=h,e[h]())}catch(p){}n&&(e[l]=n),f.event.triggered=b}}return c.result}},handle:function(c){c=f.event.fix(c||a.event);var d=((f._data(this,"events")||{})[c.type]||[]).slice(0),e=!c.exclusive&&!c.namespace,g=Array.prototype.slice.call(arguments,0);g[0]=c,c.currentTarget=this;for(var h=0,i=d.length;h<i;h++){var j=d[h];if(e||c.namespace_re.test(j.namespace)){c.handler=j.handler,c.data=j.data,c.handleObj=j;var k=j.handler.apply(this,g);k!==b&&(c.result=k,k===!1&&(c.preventDefault(),c.stopPropagation()));if(c.isImmediatePropagationStopped())break}}return c.result},props:"altKey attrChange attrName bubbles button cancelable charCode clientX clientY ctrlKey currentTarget data detail eventPhase fromElement handler keyCode layerX layerY metaKey newValue offsetX offsetY pageX pageY prevValue relatedNode relatedTarget screenX screenY shiftKey srcElement target toElement view wheelDelta which".split(" "),fix:function(a){if(a[f.expando])return a;var d=a;a=f.Event(d);for(var e=this.props.length,g;e;)g=this.props[--e],a[g]=d[g];a.target||(a.target=a.srcElement||c),a.target.nodeType===3&&(a.target=a.target.parentNode),!a.relatedTarget&&a.fromElement&&(a.relatedTarget=a.fromElement===a.target?a.toElement:a.fromElement);if(a.pageX==null&&a.clientX!=null){var h=a.target.ownerDocument||c,i=h.documentElement,j=h.body;a.pageX=a.clientX+(i&&i.scrollLeft||j&&j.scrollLeft||0)-(i&&i.clientLeft||j&&j.clientLeft||0),a.pageY=a.clientY+(i&&i.scrollTop||j&&j.scrollTop||0)-(i&&i.clientTop||j&&j.clientTop||0)}a.which==null&&(a.charCode!=null||a.keyCode!=null)&&(a.which=a.charCode!=null?a.charCode:a.keyCode),!a.metaKey&&a.ctrlKey&&(a.metaKey=a.ctrlKey),!a.which&&a.button!==b&&(a.which=a.button&1?1:a.button&2?3:a.button&4?2:0);return a},guid:1e8,proxy:f.proxy,special:{ready:{setup:f.bindReady,teardown:f.noop},live:{add:function(a){f.event.add(this,M(a.origType,a.selector),f.extend({},a,{handler:L,guid:a.handler.guid}))},remove:function(a){f.event.remove(this,M(a.origType,a.selector),a)}},beforeunload:{setup:function(a,b,c){f.isWindow(this)&&(this.onbeforeunload=c)},teardown:function(a,b){this.onbeforeunload===b&&(this.onbeforeunload=null)}}}},f.removeEvent=c.removeEventListener?function(a,b,c){a.removeEventListener&&a.removeEventListener(b,c,!1)}:function(a,b,c){a.detachEvent&&a.detachEvent("on"+b,c)},f.Event=function(a,b){if(!this.preventDefault)return new f.Event(a,b);a&&a.type?(this.originalEvent=a,this.type=a.type,this.isDefaultPrevented=a.defaultPrevented||a.returnValue===!1||a.getPreventDefault&&a.getPreventDefault()?D:C):this.type=a,b&&f.extend(this,b),this.timeStamp=f.now(),this[f.expando]=!0},f.Event.prototype={preventDefault:function(){this.isDefaultPrevented=D;var a=this.originalEvent;!a||(a.preventDefault?a.preventDefault():a.returnValue=!1)},stopPropagation:function(){this.isPropagationStopped=D;var a=this.originalEvent;!a||(a.stopPropagation&&a.stopPropagation(),a.cancelBubble=!0)},stopImmediatePropagation:function(){this.isImmediatePropagationStopped=D,this.stopPropagation()},isDefaultPrevented:C,isPropagationStopped:C,isImmediatePropagationStopped:C};var E=function(a){var b=a.relatedTarget,c=!1,d=a.type;a.type=a.data,b!==this&&(b&&(c=f.contains(this,b)),c||(f.event.handle.apply(this,arguments),a.type=d))},F=function(a){a.type=a.data,f.event.handle.apply(this,arguments)};f.each({mouseenter:"mouseover",mouseleave:"mouseout"},function(a,b){f.event.special[a]={setup:function(c){f.event.add(this,b,c&&c.selector?F:E,a)},teardown:function(a){f.event.remove(this,b,a&&a.selector?F:E)}}}),f.support.submitBubbles||(f.event.special.submit={setup:function(a,b){if(!f.nodeName(this,"form"))f.event.add(this,"click.specialSubmit",function(a){var b=a.target,c=f.nodeName(b,"input")?b.type:"";(c==="submit"||c==="image")&&f(b).closest("form").length&&J("submit",this,arguments)}),f.event.add(this,"keypress.specialSubmit",function(a){var b=a.target,c=f.nodeName(b,"input")?b.type:"";(c==="text"||c==="password")&&f(b).closest("form").length&&a.keyCode===13&&J("submit",this,arguments)});else return!1},teardown:function(a){f.event.remove(this,".specialSubmit")}});if(!f.support.changeBubbles){var G,H=function(a){var b=f.nodeName(a,"input")?a.type:"",c=a.value;b==="radio"||b==="checkbox"?c=a.checked:b==="select-multiple"?c=a.selectedIndex>-1?f.map(a.options,function(a){return a.selected}).join("-"):"":f.nodeName(a,"select")&&(c=a.selectedIndex);return c},I=function(c){var d=c.target,e,g;if(!!x.test(d.nodeName)&&!d.readOnly){e=f._data(d,"_change_data"),g=H(d),(c.type!=="focusout"||d.type!=="radio")&&f._data(d,"_change_data",g);if(e===b||g===e)return;if(e!=null||g)c.type="change",c.liveFired=b,f.event.trigger(c,arguments[1],d)}};f.event.special.change={filters:{focusout:I,beforedeactivate:I,click:function(a){var b=a.target,c=f.nodeName(b,"input")?b.type:"";(c==="radio"||c==="checkbox"||f.nodeName(b,"select"))&&I.call(this,a)},keydown:function(a){var b=a.target,c=f.nodeName(b,"input")?b.type:"";(a.keyCode===13&&!f.nodeName(b,"textarea")||a.keyCode===32&&(c==="checkbox"||c==="radio")||c==="select-multiple")&&I.call(this,a)},beforeactivate:function(a){var b=a.target;f._data(b,"_change_data",H(b))}},setup:function(a,b){if(this.type==="file")return!1;for(var c in G)f.event.add(this,c+".specialChange",G[c]);return x.test(this.nodeName)},teardown:function(a){f.event.remove(this,".specialChange");return x.test(this.nodeName)}},G=f.event.special.change.filters,G.focus=G.beforeactivate}f.support.focusinBubbles||f.each({focus:"focusin",blur:"focusout"},function(a,b){function e(a){var c=f.event.fix(a);c.type=b,c.originalEvent={},f.event.trigger(c,null,c.target),c.isDefaultPrevented()&&a.preventDefault()}var d=0;f.event.special[b]={setup:function(){d++===0&&c.addEventListener(a,e,!0)},teardown:function(){--d===0&&c.removeEventListener(a,e,!0)}}}),f.each(["bind","one"],function(a,c){f.fn[c]=function(a,d,e){var g;if(typeof a=="object"){for(var h in a)this[c](h,d,a[h],e);return this}if(arguments.length===2||d===!1)e=d,d=b;c==="one"?(g=function(a){f(this).unbind(a,g);return e.apply(this,arguments)},g.guid=e.guid||f.guid++):g=e;if(a==="unload"&&c!=="one")this.one(a,d,e);else for(var i=0,j=this.length;i<j;i++)f.event.add(this[i],a,g,d);return this}}),f.fn.extend({unbind:function(a,b){if(typeof a=="object"&&!a.preventDefault)for(var c in a)this.unbind(c,a[c]);else for(var d=0,e=this.length;d<e;d++)f.event.remove(this[d],a,b);return this},delegate:function(a,b,c,d){return this.live(b,c,d,a)},undelegate:function(a,b,c){return arguments.length===0?this.unbind("live"):this.die(b,null,c,a)},trigger:function(a,b){return this.each(function(){f.event.trigger(a,b,this)})},triggerHandler:function(a,b){if(this[0])return f.event.trigger(a,b,this[0],!0)},toggle:function(a){var b=arguments,c=a.guid||f.guid++,d=0,e=function(c){var e=(f.data(this,"lastToggle"+a.guid)||0)%d;f.data(this,"lastToggle"+a.guid,e+1),c.preventDefault();return b[e].apply(this,arguments)||!1};e.guid=c;while(d<b.length)b[d++].guid=c;return this.click(e)},hover:function(a,b){return this.mouseenter(a).mouseleave(b||a)}});var K={focus:"focusin",blur:"focusout",mouseenter:"mouseover",mouseleave:"mouseout"};f.each(["live","die"],function(a,c){f.fn[c]=function(a,d,e,g){var h,i=0,j,k,l,m=g||this.selector,n=g?this:f(this.context);if(typeof a=="object"&&!a.preventDefault){for(var o in a)n[c](o,d,a[o],m);return this}if(c==="die"&&!a&&g&&g.charAt(0)==="."){n.unbind(g);return this}if(d===!1||f.isFunction(d))e=d||C,d=b;a=(a||"").split(" ");while((h=a[i++])!=null){j=w.exec(h),k="",j&&(k=j[0],h=h.replace(w,""));if(h==="hover"){a.push("mouseenter"+k,"mouseleave"+k);continue}l=h,K[h]?(a.push(K[h]+k),h=h+k):h=(K[h]||h)+k;if(c==="live")for(var p=0,q=n.length;p<q;p++)f.event.add(n[p],"live."+M(h,m),{data:d,selector:m,handler:e,origType:h,origHandler:e,preType:l});else n.unbind("live."+M(h,m),e)}return this}}),f.each("blur focus focusin focusout load resize scroll unload click dblclick mousedown mouseup mousemove mouseover mouseout mouseenter mouseleave change select submit keydown keypress keyup error".split(" "),function(a,b){f.fn[b]=function(a,c){c==null&&(c=a,a=null);return arguments.length>0?this.bind(b,a,c):this.trigger(b)},f.attrFn&&(f.attrFn[b]=!0)}),function(){function u(a,b,c,d,e,f){for(var g=0,h=d.length;g<h;g++){var i=d[g];if(i){var j=!1;i=i[a];while(i){if(i.sizcache===c){j=d[i.sizset];break}if(i.nodeType===1){f||(i.sizcache=c,i.sizset=g);if(typeof b!="string"){if(i===b){j=!0;break}}else if(k.filter(b,[i]).length>0){j=i;break}}i=i[a]}d[g]=j}}}function t(a,b,c,d,e,f){for(var g=0,h=d.length;g<h;g++){var i=d[g];if(i){var j=!1;i=i[a];while(i){if(i.sizcache===c){j=d[i.sizset];break}i.nodeType===1&&!f&&(i.sizcache=c,i.sizset=g);if(i.nodeName.toLowerCase()===b){j=i;break}i=i[a]}d[g]=j}}}var a=/((?:\((?:\([^()]+\)|[^()]+)+\)|\[(?:\[[^\[\]]*\]|['"][^'"]*['"]|[^\[\]'"]+)+\]|\\.|[^ >+~,(\[\\]+)+|[>+~])(\s*,\s*)?((?:.|\r|\n)*)/g,d=0,e=Object.prototype.toString,g=!1,h=!0,i=/\\/g,j=/\W/;[0,0].sort(function(){h=!1;return 0});var k=function(b,d,f,g){f=f||[],d=d||c;var h=d;if(d.nodeType!==1&&d.nodeType!==9)return[];if(!b||typeof b!="string")return f;var i,j,n,o,q,r,s,t,u=!0,w=k.isXML(d),x=[],y=b;do{a.exec(""),i=a.exec(y);if(i){y=i[3],x.push(i[1]);if(i[2]){o=i[3];break}}}while(i);if(x.length>1&&m.exec(b))if(x.length===2&&l.relative[x[0]])j=v(x[0]+x[1],d);else{j=l.relative[x[0]]?[d]:k(x.shift(),d);while(x.length)b=x.shift(),l.relative[b]&&(b+=x.shift()),j=v(b,j)}else{!g&&x.length>1&&d.nodeType===9&&!w&&l.match.ID.test(x[0])&&!l.match.ID.test(x[x.length-1])&&(q=k.find(x.shift(),d,w),d=q.expr?k.filter(q.expr,q.set)[0]:q.set[0]);if(d){q=g?{expr:x.pop(),set:p(g)}:k.find(x.pop(),x.length===1&&(x[0]==="~"||x[0]==="+")&&d.parentNode?d.parentNode:d,w),j=q.expr?k.filter(q.expr,q.set):q.set,x.length>0?n=p(j):u=!1;while(x.length)r=x.pop(),s=r,l.relative[r]?s=x.pop():r="",s==null&&(s=d),l.relative[r](n,s,w)}else n=x=[]}n||(n=j),n||k.error(r||b);if(e.call(n)==="[object Array]")if(!u)f.push.apply(f,n);else if(d&&d.nodeType===1)for(t=0;n[t]!=null;t++)n[t]&&(n[t]===!0||n[t].nodeType===1&&k.contains(d,n[t]))&&f.push(j[t]);else for(t=0;n[t]!=null;t++)n[t]&&n[t].nodeType===1&&f.push(j[t]);else p(n,f);o&&(k(o,h,f,g),k.uniqueSort(f));return f};k.uniqueSort=function(a){if(r){g=h,a.sort(r);if(g)for(var b=1;b<a.length;b++)a[b]===a[b-1]&&a.splice(b--,1)}return a},k.matches=function(a,b){return k(a,null,null,b)},k.matchesSelector=function(a,b){return k(b,null,null,[a]).length>0},k.find=function(a,b,c){var d;if(!a)return[];for(var e=0,f=l.order.length;e<f;e++){var g,h=l.order[e];if(g=l.leftMatch[h].exec(a)){var j=g[1];g.splice(1,1);if(j.substr(j.length-1)!=="\\"){g[1]=(g[1]||"").replace(i,""),d=l.find[h](g,b,c);if(d!=null){a=a.replace(l.match[h],"");break}}}}d||(d=typeof b.getElementsByTagName!="undefined"?b.getElementsByTagName("*"):[]);return{set:d,expr:a}},k.filter=function(a,c,d,e){var f,g,h=a,i=[],j=c,m=c&&c[0]&&k.isXML(c[0]);while(a&&c.length){for(var n in l.filter)if((f=l.leftMatch[n].exec(a))!=null&&f[2]){var o,p,q=l.filter[n],r=f[1];g=!1,f.splice(1,1);if(r.substr(r.length-1)==="\\")continue;j===i&&(i=[]);if(l.preFilter[n]){f=l.preFilter[n](f,j,d,i,e,m);if(!f)g=o=!0;else if(f===!0)continue}if(f)for(var s=0;(p=j[s])!=null;s++)if(p){o=q(p,f,s,j);var t=e^!!o;d&&o!=null?t?g=!0:j[s]=!1:t&&(i.push(p),g=!0)}if(o!==b){d||(j=i),a=a.replace(l.match[n],"");if(!g)return[];break}}if(a===h)if(g==null)k.error(a);else break;h=a}return j},k.error=function(a){throw"Syntax error, unrecognized expression: "+a};var l=k.selectors={order:["ID","NAME","TAG"],match:{ID:/#((?:[\w\u00c0-\uFFFF\-]|\\.)+)/,CLASS:/\.((?:[\w\u00c0-\uFFFF\-]|\\.)+)/,NAME:/\[name=['"]*((?:[\w\u00c0-\uFFFF\-]|\\.)+)['"]*\]/,ATTR:/\[\s*((?:[\w\u00c0-\uFFFF\-]|\\.)+)\s*(?:(\S?=)\s*(?:(['"])(.*?)\3|(#?(?:[\w\u00c0-\uFFFF\-]|\\.)*)|)|)\s*\]/,TAG:/^((?:[\w\u00c0-\uFFFF\*\-]|\\.)+)/,CHILD:/:(only|nth|last|first)-child(?:\(\s*(even|odd|(?:[+\-]?\d+|(?:[+\-]?\d*)?n\s*(?:[+\-]\s*\d+)?))\s*\))?/,POS:/:(nth|eq|gt|lt|first|last|even|odd)(?:\((\d*)\))?(?=[^\-]|$)/,PSEUDO:/:((?:[\w\u00c0-\uFFFF\-]|\\.)+)(?:\((['"]?)((?:\([^\)]+\)|[^\(\)]*)+)\2\))?/},leftMatch:{},attrMap:{"class":"className","for":"htmlFor"},attrHandle:{href:function(a){return a.getAttribute("href")},type:function(a){return a.getAttribute("type")}},relative:{"+":function(a,b){var c=typeof b=="string",d=c&&!j.test(b),e=c&&!d;d&&(b=b.toLowerCase());for(var f=0,g=a.length,h;f<g;f++)if(h=a[f]){while((h=h.previousSibling)&&h.nodeType!==1);a[f]=e||h&&h.nodeName.toLowerCase()===b?h||!1:h===b}e&&k.filter(b,a,!0)},">":function(a,b){var c,d=typeof b=="string",e=0,f=a.length;if(d&&!j.test(b)){b=b.toLowerCase();for(;e<f;e++){c=a[e];if(c){var g=c.parentNode;a[e]=g.nodeName.toLowerCase()===b?g:!1}}}else{for(;e<f;e++)c=a[e],c&&(a[e]=d?c.parentNode:c.parentNode===b);d&&k.filter(b,a,!0)}},"":function(a,b,c){var e,f=d++,g=u;typeof b=="string"&&!j.test(b)&&(b=b.toLowerCase(),e=b,g=t),g("parentNode",b,f,a,e,c)},"~":function(a,b,c){var e,f=d++,g=u;typeof b=="string"&&!j.test(b)&&(b=b.toLowerCase(),e=b,g=t),g("previousSibling",b,f,a,e,c)}},find:{ID:function(a,b,c){if(typeof b.getElementById!="undefined"&&!c){var d=b.getElementById(a[1]);return d&&d.parentNode?[d]:[]}},NAME:function(a,b){if(typeof b.getElementsByName!="undefined"){var c=[],d=b.getElementsByName(a[1]);for(var e=0,f=d.length;e<f;e++)d[e].getAttribute("name")===a[1]&&c.push(d[e]);return c.length===0?null:c}},TAG:function(a,b){if(typeof b.getElementsByTagName!="undefined")return b.getElementsByTagName(a[1])}},preFilter:{CLASS:function(a,b,c,d,e,f){a=" "+a[1].replace(i,"")+" ";if(f)return a;for(var g=0,h;(h=b[g])!=null;g++)h&&(e^(h.className&&(" "+h.className+" ").replace(/[\t\n\r]/g," ").indexOf(a)>=0)?c||d.push(h):c&&(b[g]=!1));return!1},ID:function(a){return a[1].replace(i,"")},TAG:function(a,b){return a[1].replace(i,"").toLowerCase()},CHILD:function(a){if(a[1]==="nth"){a[2]||k.error(a[0]),a[2]=a[2].replace(/^\+|\s*/g,"");var b=/(-?)(\d*)(?:n([+\-]?\d*))?/.exec(a[2]==="even"&&"2n"||a[2]==="odd"&&"2n+1"||!/\D/.test(a[2])&&"0n+"+a[2]||a[2]);a[2]=b[1]+(b[2]||1)-0,a[3]=b[3]-0}else a[2]&&k.error(a[0]);a[0]=d++;return a},ATTR:function(a,b,c,d,e,f){var g=a[1]=a[1].replace(i,"");!f&&l.attrMap[g]&&(a[1]=l.attrMap[g]),a[4]=(a[4]||a[5]||"").replace(i,""),a[2]==="~="&&(a[4]=" "+a[4]+" ");return a},PSEUDO:function(b,c,d,e,f){if(b[1]==="not")if((a.exec(b[3])||"").length>1||/^\w/.test(b[3]))b[3]=k(b[3],null,null,c);else{var g=k.filter(b[3],c,d,!0^f);d||e.push.apply(e,g);return!1}else if(l.match.POS.test(b[0])||l.match.CHILD.test(b[0]))return!0;return b},POS:function(a){a.unshift(!0);return a}},filters:{enabled:function(a){return a.disabled===!1&&a.type!=="hidden"},disabled:function(a){return a.disabled===!0},checked:function(a){return a.checked===!0},selected:function(a){a.parentNode&&a.parentNode.selectedIndex;return a.selected===!0},parent:function(a){return!!a.firstChild},empty:function(a){return!a.firstChild},has:function(a,b,c){return!!k(c[3],a).length},header:function(a){return/h\d/i.test(a.nodeName)},text:function(a){var b=a.getAttribute("type"),c=a.type;return a.nodeName.toLowerCase()==="input"&&"text"===c&&(b===c||b===null)},radio:function(a){return a.nodeName.toLowerCase()==="input"&&"radio"===a.type},checkbox:function(a){return a.nodeName.toLowerCase()==="input"&&"checkbox"===a.type},file:function(a){return a.nodeName.toLowerCase()==="input"&&"file"===a.type},password:function(a){return a.nodeName.toLowerCase()==="input"&&"password"===a.type},submit:function(a){var b=a.nodeName.toLowerCase();return(b==="input"||b==="button")&&"submit"===a.type},image:function(a){return a.nodeName.toLowerCase()==="input"&&"image"===a.type},reset:function(a){var b=a.nodeName.toLowerCase();return(b==="input"||b==="button")&&"reset"===a.type},button:function(a){var b=a.nodeName.toLowerCase();return b==="input"&&"button"===a.type||b==="button"},input:function(a){return/input|select|textarea|button/i.test(a.nodeName)},focus:function(a){return a===a.ownerDocument.activeElement}},setFilters:{first:function(a,b){return b===0},last:function(a,b,c,d){return b===d.length-1},even:function(a,b){return b%2===0},odd:function(a,b){return b%2===1},lt:function(a,b,c){return b<c[3]-0},gt:function(a,b,c){return b>c[3]-0},nth:function(a,b,c){return c[3]-0===b},eq:function(a,b,c){return c[3]-0===b}},filter:{PSEUDO:function(a,b,c,d){var e=b[1],f=l.filters[e];if(f)return f(a,c,b,d);if(e==="contains")return(a.textContent||a.innerText||k.getText([a])||"").indexOf(b[3])>=0;if(e==="not"){var g=b[3];for(var h=0,i=g.length;h<i;h++)if(g[h]===a)return!1;return!0}k.error(e)},CHILD:function(a,b){var c=b[1],d=a;switch(c){case"only":case"first":while(d=d.previousSibling)if(d.nodeType===1)return!1;if(c==="first")return!0;d=a;case"last":while(d=d.nextSibling)if(d.nodeType===1)return!1;return!0;case"nth":var e=b[2],f=b[3];if(e===1&&f===0)return!0;var g=b[0],h=a.parentNode;if(h&&(h.sizcache!==g||!a.nodeIndex)){var i=0;for(d=h.firstChild;d;d=d.nextSibling)d.nodeType===1&&(d.nodeIndex=++i);h.sizcache=g}var j=a.nodeIndex-f;return e===0?j===0:j%e===0&&j/e>=0}},ID:function(a,b){return a.nodeType===1&&a.getAttribute("id")===b},TAG:function(a,b){return b==="*"&&a.nodeType===1||a.nodeName.toLowerCase()===b},CLASS:function(a,b){return(" "+(a.className||a.getAttribute("class"))+" ").indexOf(b)>-1},ATTR:function(a,b){var c=b[1],d=l.attrHandle[c]?l.attrHandle[c](a):a[c]!=null?a[c]:a.getAttribute(c),e=d+"",f=b[2],g=b[4];return d==null?f==="!=":f==="="?e===g:f==="*="?e.indexOf(g)>=0:f==="~="?(" "+e+" ").indexOf(g)>=0:g?f==="!="?e!==g:f==="^="?e.indexOf(g)===0:f==="$="?e.substr(e.length-g.length)===g:f==="|="?e===g||e.substr(0,g.length+1)===g+"-":!1:e&&d!==!1},POS:function(a,b,c,d){var e=b[2],f=l.setFilters[e];if(f)return f(a,c,b,d)}}},m=l.match.POS,n=function(a,b){return"\\"+(b-0+1)};for(var o in l.match)l.match[o]=new RegExp(l.match[o].source+/(?![^\[]*\])(?![^\(]*\))/.source),l.leftMatch[o]=new RegExp(/(^(?:.|\r|\n)*?)/.source+l.match[o].source.replace(/\\(\d+)/g,n));var p=function(a,b){a=Array.prototype.slice.call(a,0);if(b){b.push.apply(b,a);return b}return a};try{Array.prototype.slice.call(c.documentElement.childNodes,0)[0].nodeType}catch(q){p=function(a,b){var c=0,d=b||[];if(e.call(a)==="[object Array]")Array.prototype.push.apply(d,a);else if(typeof a.length=="number")for(var f=a.length;c<f;c++)d.push(a[c]);else for(;a[c];c++)d.push(a[c]);return d}}var r,s;c.documentElement.compareDocumentPosition?r=function(a,b){if(a===b){g=!0;return 0}if(!a.compareDocumentPosition||!b.compareDocumentPosition)return a.compareDocumentPosition?-1:1;return a.compareDocumentPosition(b)&4?-1:1}:(r=function(a,b){if(a===b){g=!0;return 0}if(a.sourceIndex&&b.sourceIndex)return a.sourceIndex-b.sourceIndex;var c,d,e=[],f=[],h=a.parentNode,i=b.parentNode,j=h;if(h===i)return s(a,b);if(!h)return-1;if(!i)return 1;while(j)e.unshift(j),j=j.parentNode;j=i;while(j)f.unshift(j),j=j.parentNode;c=e.length,d=f.length;for(var k=0;k<c&&k<d;k++)if(e[k]!==f[k])return s(e[k],f[k]);return k===c?s(a,f[k],-1):s(e[k],b,1)},s=function(a,b,c){if(a===b)return c;var d=a.nextSibling;while(d){if(d===b)return-1;d=d.nextSibling}return 1}),k.getText=function(a){var b="",c;for(var d=0;a[d];d++)c=a[d],c.nodeType===3||c.nodeType===4?b+=c.nodeValue:c.nodeType!==8&&(b+=k.getText(c.childNodes));return b},function(){var a=c.createElement("div"),d="script"+(new Date).getTime(),e=c.documentElement;a.innerHTML="<a name='"+d+"'/>",e.insertBefore(a,e.firstChild),c.getElementById(d)&&(l.find.ID=function(a,c,d){if(typeof c.getElementById!="undefined"&&!d){var e=c.getElementById(a[1]);return e?e.id===a[1]||typeof e.getAttributeNode!="undefined"&&e.getAttributeNode("id").nodeValue===a[1]?[e]:b:[]}},l.filter.ID=function(a,b){var c=typeof a.getAttributeNode!="undefined"&&a.getAttributeNode("id");return a.nodeType===1&&c&&c.nodeValue===b}),e.removeChild(a),e=a=null}(),function(){var a=c.createElement("div");a.appendChild(c.createComment("")),a.getElementsByTagName("*").length>0&&(l.find.TAG=function(a,b){var c=b.getElementsByTagName(a[1]);if(a[1]==="*"){var d=[];for(var e=0;c[e];e++)c[e].nodeType===1&&d.push(c[e]);c=d}return c}),a.innerHTML="<a href='#'></a>",a.firstChild&&typeof a.firstChild.getAttribute!="undefined"&&a.firstChild.getAttribute("href")!=="#"&&(l.attrHandle.href=function(a){return a.getAttribute("href",2)}),a=null}(),c.querySelectorAll&&function(){var a=k,b=c.createElement("div"),d="__sizzle__";b.innerHTML="<p class='TEST'></p>";if(!b.querySelectorAll||b.querySelectorAll(".TEST").length!==0){k=function(b,e,f,g){e=e||c;if(!g&&!k.isXML(e)){var h=/^(\w+$)|^\.([\w\-]+$)|^#([\w\-]+$)/.exec(b);if(h&&(e.nodeType===1||e.nodeType===9)){if(h[1])return p(e.getElementsByTagName(b),f);if(h[2]&&l.find.CLASS&&e.getElementsByClassName)return p(e.getElementsByClassName(h[2]),f)}if(e.nodeType===9){if(b==="body"&&e.body)return p([e.body],f);if(h&&h[3]){var i=e.getElementById(h[3]);if(!i||!i.parentNode)return p([],f);if(i.id===h[3])return p([i],f)}try{return p(e.querySelectorAll(b),f)}catch(j){}}else if(e.nodeType===1&&e.nodeName.toLowerCase()!=="object"){var m=e,n=e.getAttribute("id"),o=n||d,q=e.parentNode,r=/^\s*[+~]/.test(b);n?o=o.replace(/'/g,"\\$&"):e.setAttribute("id",o),r&&q&&(e=e.parentNode);try{if(!r||q)return p(e.querySelectorAll("[id='"+o+"'] "+b),f)}catch(s){}finally{n||m.removeAttribute("id")}}}return a(b,e,f,g)};for(var e in a)k[e]=a[e];b=null}}(),function(){var a=c.documentElement,b=a.matchesSelector||a.mozMatchesSelector||a.webkitMatchesSelector||a.msMatchesSelector;if(b){var d=!b.call(c.createElement("div"),"div"),e=!1;try{b.call(c.documentElement,"[test!='']:sizzle")}catch(f){e=!0}k.matchesSelector=function(a,c){c=c.replace(/\=\s*([^'"\]]*)\s*\]/g,"='$1']");if(!k.isXML(a))try{if(e||!l.match.PSEUDO.test(c)&&!/!=/.test(c)){var f=b.call(a,c);if(f||!d||a.document&&a.document.nodeType!==11)return f}}catch(g){}return k(c,null,null,[a]).length>0}}}(),function(){var a=c.createElement("div");a.innerHTML="<div class='test e'></div><div class='test'></div>";if(!!a.getElementsByClassName&&a.getElementsByClassName("e").length!==0){a.lastChild.className="e";if(a.getElementsByClassName("e").length===1)return;l.order.splice(1,0,"CLASS"),l.find.CLASS=function(a,b,c){if(typeof b.getElementsByClassName!="undefined"&&!c)return b.getElementsByClassName(a[1])},a=null}}(),c.documentElement.contains?k.contains=function(a,b){return a!==b&&(a.contains?a.contains(b):!0)}:c.documentElement.compareDocumentPosition?k.contains=function(a,b){return!!(a.compareDocumentPosition(b)&16)}:k.contains=function(){return!1},k.isXML=function(a){var b=(a?a.ownerDocument||a:0).documentElement;return b?b.nodeName!=="HTML":!1};var v=function(a,b){var c,d=[],e="",f=b.nodeType?[b]:b;while(c=l.match.PSEUDO.exec(a))e+=c[0],a=a.replace(l.match.PSEUDO,"");a=l.relative[a]?a+"*":a;for(var g=0,h=f.length;g<h;g++)k(a,f[g],d);return k.filter(e,d)};f.find=k,f.expr=k.selectors,f.expr[":"]=f.expr.filters,f.unique=k.uniqueSort,f.text=k.getText,f.isXMLDoc=k.isXML,f.contains=k.contains}();var N=/Until$/,O=/^(?:parents|prevUntil|prevAll)/,P=/,/,Q=/^.[^:#\[\.,]*$/,R=Array.prototype.slice,S=f.expr.match.POS,T={children:!0,contents:!0,next:!0,prev:!0};f.fn.extend({find:function(a){var b=this,c,d;if(typeof a!="string")return f(a).filter(function(){for(c=0,d=b.length;c<d;c++)if(f.contains(b[c],this))return!0});var e=this.pushStack("","find",a),g,h,i;for(c=0,d=this.length;c<d;c++){g=e.length,f.find(a,this[c],e);if(c>0)for(h=g;h<e.length;h++)for(i=0;i<g;i++)if(e[i]===e[h]){e.splice(h--,1);break}}return e},has:function(a){var b=f(a);return this.filter(function(){for(var a=0,c=b.length;a<c;a++)if(f.contains(this,b[a]))return!0})},not:function(a){return this.pushStack(V(this,a,!1),"not",a)},filter:function(a){return this.pushStack(V(this,a,!0),"filter",a)},is:function(a){return!!a&&(typeof a=="string"?f.filter(a,this).length>0:this.filter(a).length>0)},closest:function(a,b){var c=[],d,e,g=this[0];if(f.isArray(a)){var h,i,j={},k=1;if(g&&a.length){for(d=0,e=a.length;d<e;d++)i=a[d],j[i]||(j[i]=S.test(i)?f(i,b||this.context):i);while(g&&g.ownerDocument&&g!==b){for(i in j)h=j[i],(h.jquery?h.index(g)>-1:f(g).is(h))&&c.push({selector:i,elem:g,level:k});g=g.parentNode,k++}}return c}var l=S.test(a)||typeof a!="string"?f(a,b||this.context):0;for(d=0,e=this.length;d<e;d++){g=this[d];while(g){if(l?l.index(g)>-1:f.find.matchesSelector(g,a)){c.push(g);break}g=g.parentNode;if(!g||!g.ownerDocument||g===b||g.nodeType===11)break}}c=c.length>1?f.unique(c):c;return this.pushStack(c,"closest",a)},index:function(a){if(!a)return this[0]&&this[0].parentNode?this.prevAll().length:-1;if(typeof a=="string")return f.inArray(this[0],f(a));return f.inArray(a.jquery?a[0]:a,this)},add:function(a,b){var c=typeof a=="string"?f(a,b):f.makeArray(a&&a.nodeType?[a]:a),d=f.merge(this.get(),c);return this.pushStack(U(c[0])||U(d[0])?d:f.unique(d))},andSelf:function(){return this.add(this.prevObject)}}),f.each({parent:function(a){var b=a.parentNode;return b&&b.nodeType!==11?b:null},parents:function(a){return f.dir(a,"parentNode")},parentsUntil:function(a,b,c){return f.dir(a,"parentNode",c)},next:function(a){return f.nth(a,2,"nextSibling")},prev:function(a){return f.nth(a,2,"previousSibling")},nextAll:function(a){return f.dir(a,"nextSibling")},prevAll:function(a){return f.dir(a,"previousSibling")},nextUntil:function(a,b,c){return f.dir(a,"nextSibling",c)},prevUntil:function(a,b,c){return f.dir(a,"previousSibling",c)},siblings:function(a){return f.sibling(a.parentNode.firstChild,a)},children:function(a){return f.sibling(a.firstChild)},contents:function(a){return f.nodeName(a,"iframe")?a.contentDocument||a.contentWindow.document:f.makeArray(a.childNodes)}},function(a,b){f.fn[a]=function(c,d){var e=f.map(this,b,c),g=R.call(arguments);N.test(a)||(d=c),d&&typeof d=="string"&&(e=f.filter(d,e)),e=this.length>1&&!T[a]?f.unique(e):e,(this.length>1||P.test(d))&&O.test(a)&&(e=e.reverse());return this.pushStack(e,a,g.join(","))}}),f.extend({filter:function(a,b,c){c&&(a=":not("+a+")");return b.length===1?f.find.matchesSelector(b[0],a)?[b[0]]:[]:f.find.matches(a,b)},dir:function(a,c,d){var e=[],g=a[c];while(g&&g.nodeType!==9&&(d===b||g.nodeType!==1||!f(g).is(d)))g.nodeType===1&&e.push(g),g=g[c];return e},nth:function(a,b,c,d){b=b||1;var e=0;for(;a;a=a[c])if(a.nodeType===1&&++e===b)break;return a},sibling:function(a,b){var c=[];for(;a;a=a.nextSibling)a.nodeType===1&&a!==b&&c.push(a);return c}});var W=/ jQuery\d+="(?:\d+|null)"/g,X=/^\s+/,Y=/<(?!area|br|col|embed|hr|img|input|link|meta|param)(([\w:]+)[^>]*)\/>/ig,Z=/<([\w:]+)/,$=/<tbody/i,_=/<|&#?\w+;/,ba=/<(?:script|object|embed|option|style)/i,bb=/checked\s*(?:[^=]|=\s*.checked.)/i,bc=/\/(java|ecma)script/i,bd=/^\s*<!(?:\[CDATA\[|\-\-)/,be={option:[1,"<select multiple='multiple'>","</select>"],legend:[1,"<fieldset>","</fieldset>"],thead:[1,"<table>","</table>"],tr:[2,"<table><tbody>","</tbody></table>"],td:[3,"<table><tbody><tr>","</tr></tbody></table>"],col:[2,"<table><tbody></tbody><colgroup>","</colgroup></table>"],area:[1,"<map>","</map>"],_default:[0,"",""]};be.optgroup=be.option,be.tbody=be.tfoot=be.colgroup=be.caption=be.thead,be.th=be.td,f.support.htmlSerialize||(be._default=[1,"div<div>","</div>"]),f.fn.extend({text:function(a){if(f.isFunction(a))return this.each(function(b){var c=f(this);c.text(a.call(this,b,c.text()))});if(typeof a!="object"&&a!==b)return this.empty().append((this[0]&&this[0].ownerDocument||c).createTextNode(a));return f.text(this)},wrapAll:function(a){if(f.isFunction(a))return this.each(function(b){f(this).wrapAll(a.call(this,b))});if(this[0]){var b=f(a,this[0].ownerDocument).eq(0).clone(!0);this[0].parentNode&&b.insertBefore(this[0]),b.map(function(){var a=this;while(a.firstChild&&a.firstChild.nodeType===1)a=a.firstChild;return a}).append(this)}return this},wrapInner:function(a){if(f.isFunction(a))return this.each(function(b){f(this).wrapInner(a.call(this,b))});return this.each(function(){var b=f(this),c=b.contents();c.length?c.wrapAll(a):b.append(a)})},wrap:function(a){return this.each(function(){f(this).wrapAll(a)})},unwrap:function(){return this.parent().each(function(){f.nodeName(this,"body")||f(this).replaceWith(this.childNodes)}).end()},append:function(){return this.domManip(arguments,!0,function(a){this.nodeType===1&&this.appendChild(a)})},prepend:function(){return this.domManip(arguments,!0,function(a){this.nodeType===1&&this.insertBefore(a,this.firstChild)})},before:function(){if(this[0]&&this[0].parentNode)return this.domManip(arguments,!1,function(a){this.parentNode.insertBefore(a,this)});if(arguments.length){var a=f(arguments[0]);a.push.apply(a,this.toArray());return this.pushStack(a,"before",arguments)}},after:function(){if(this[0]&&this[0].parentNode)return this.domManip(arguments,!1,function(a){this.parentNode.insertBefore(a,this.nextSibling)});if(arguments.length){var a=this.pushStack(this,"after",arguments);a.push.apply(a,f(arguments[0]).toArray());return a}},remove:function(a,b){for(var c=0,d;(d=this[c])!=null;c++)if(!a||f.filter(a,[d]).length)!b&&d.nodeType===1&&(f.cleanData(d.getElementsByTagName("*")),f.cleanData([d])),d.parentNode&&d.parentNode.removeChild(d);return this},empty:function(){for(var a=0,b;(b=this[a])!=null;a++){b.nodeType===1&&f.cleanData(b.getElementsByTagName("*"));while(b.firstChild)b.removeChild(b.firstChild)}return this},clone:function(a,b){a=a==null?!1:a,b=b==null?a:b;return this.map(function(){return f.clone(this,a,b)})},html:function(a){if(a===b)return this[0]&&this[0].nodeType===1?this[0].innerHTML.replace(W,""):null;if(typeof a=="string"&&!ba.test(a)&&(f.support.leadingWhitespace||!X.test(a))&&!be[(Z.exec(a)||["",""])[1].toLowerCase()]){a=a.replace(Y,"<$1></$2>");try{for(var c=0,d=this.length;c<d;c++)this[c].nodeType===1&&(f.cleanData(this[c].getElementsByTagName("*")),this[c].innerHTML=a)}catch(e){this.empty().append(a)}}else f.isFunction(a)?this.each(function(b){var c=f(this);c.html(a.call(this,b,c.html()))}):this.empty().append(a);return this},replaceWith:function(a){if(this[0]&&this[0].parentNode){if(f.isFunction(a))return this.each(function(b){var c=f(this),d=c.html();c.replaceWith(a.call(this,b,d))});typeof a!="string"&&(a=f(a).detach());return this.each(function(){var b=this.nextSibling,c=this.parentNode;f(this).remove(),b?f(b).before(a):f(c).append(a)})}return this.length?this.pushStack(f(f.isFunction(a)?a():a),"replaceWith",a):this},detach:function(a){return this.remove(a,!0)},domManip:function(a,c,d){var e,g,h,i,j=a[0],k=[];if(!f.support.checkClone&&arguments.length===3&&typeof j=="string"&&bb.test(j))return this.each(function(){f(this).domManip(a,c,d,!0)});if(f.isFunction(j))return this.each(function(e){var g=f(this);a[0]=j.call(this,e,c?g.html():b),g.domManip(a,c,d)});if(this[0]){i=j&&j.parentNode,f.support.parentNode&&i&&i.nodeType===11&&i.childNodes.length===this.length?e={fragment:i}:e=f.buildFragment(a,this,k),h=e.fragment,h.childNodes.length===1?g=h=h.firstChild:g=h.firstChild;if(g){c=c&&f.nodeName(g,"tr");for(var l=0,m=this.length,n=m-1;l<m;l++)d.call(c?bf(this[l],g):this[l],e.cacheable||m>1&&l<n?f.clone(h,!0,!0):h)}k.length&&f.each(k,bl)}return this}}),f.buildFragment=function(a,b,d){var e,g,h,i;b&&b[0]&&(i=b[0].ownerDocument||b[0]),i.createDocumentFragment||(i=c),a.length===1&&typeof a[0]=="string"&&a[0].length<512&&i===c&&a[0].charAt(0)==="<"&&!ba.test(a[0])&&(f.support.checkClone||!bb.test(a[0]))&&(g=!0,h=f.fragments[a[0]],h&&h!==1&&(e=h)),e||(e=i.createDocumentFragment(),f.clean(a,i,e,d)),g&&(f.fragments[a[0]]=h?e:1);
return{fragment:e,cacheable:g}},f.fragments={},f.each({appendTo:"append",prependTo:"prepend",insertBefore:"before",insertAfter:"after",replaceAll:"replaceWith"},function(a,b){f.fn[a]=function(c){var d=[],e=f(c),g=this.length===1&&this[0].parentNode;if(g&&g.nodeType===11&&g.childNodes.length===1&&e.length===1){e[b](this[0]);return this}for(var h=0,i=e.length;h<i;h++){var j=(h>0?this.clone(!0):this).get();f(e[h])[b](j),d=d.concat(j)}return this.pushStack(d,a,e.selector)}}),f.extend({clone:function(a,b,c){var d=a.cloneNode(!0),e,g,h;if((!f.support.noCloneEvent||!f.support.noCloneChecked)&&(a.nodeType===1||a.nodeType===11)&&!f.isXMLDoc(a)){bh(a,d),e=bi(a),g=bi(d);for(h=0;e[h];++h)g[h]&&bh(e[h],g[h])}if(b){bg(a,d);if(c){e=bi(a),g=bi(d);for(h=0;e[h];++h)bg(e[h],g[h])}}e=g=null;return d},clean:function(a,b,d,e){var g;b=b||c,typeof b.createElement=="undefined"&&(b=b.ownerDocument||b[0]&&b[0].ownerDocument||c);var h=[],i;for(var j=0,k;(k=a[j])!=null;j++){typeof k=="number"&&(k+="");if(!k)continue;if(typeof k=="string")if(!_.test(k))k=b.createTextNode(k);else{k=k.replace(Y,"<$1></$2>");var l=(Z.exec(k)||["",""])[1].toLowerCase(),m=be[l]||be._default,n=m[0],o=b.createElement("div");o.innerHTML=m[1]+k+m[2];while(n--)o=o.lastChild;if(!f.support.tbody){var p=$.test(k),q=l==="table"&&!p?o.firstChild&&o.firstChild.childNodes:m[1]==="<table>"&&!p?o.childNodes:[];for(i=q.length-1;i>=0;--i)f.nodeName(q[i],"tbody")&&!q[i].childNodes.length&&q[i].parentNode.removeChild(q[i])}!f.support.leadingWhitespace&&X.test(k)&&o.insertBefore(b.createTextNode(X.exec(k)[0]),o.firstChild),k=o.childNodes}var r;if(!f.support.appendChecked)if(k[0]&&typeof (r=k.length)=="number")for(i=0;i<r;i++)bk(k[i]);else bk(k);k.nodeType?h.push(k):h=f.merge(h,k)}if(d){g=function(a){return!a.type||bc.test(a.type)};for(j=0;h[j];j++)if(e&&f.nodeName(h[j],"script")&&(!h[j].type||h[j].type.toLowerCase()==="text/javascript"))e.push(h[j].parentNode?h[j].parentNode.removeChild(h[j]):h[j]);else{if(h[j].nodeType===1){var s=f.grep(h[j].getElementsByTagName("script"),g);h.splice.apply(h,[j+1,0].concat(s))}d.appendChild(h[j])}}return h},cleanData:function(a){var b,c,d=f.cache,e=f.expando,g=f.event.special,h=f.support.deleteExpando;for(var i=0,j;(j=a[i])!=null;i++){if(j.nodeName&&f.noData[j.nodeName.toLowerCase()])continue;c=j[f.expando];if(c){b=d[c]&&d[c][e];if(b&&b.events){for(var k in b.events)g[k]?f.event.remove(j,k):f.removeEvent(j,k,b.handle);b.handle&&(b.handle.elem=null)}h?delete j[f.expando]:j.removeAttribute&&j.removeAttribute(f.expando),delete d[c]}}}});var bm=/alpha\([^)]*\)/i,bn=/opacity=([^)]*)/,bo=/([A-Z]|^ms)/g,bp=/^-?\d+(?:px)?$/i,bq=/^-?\d/,br=/^([\-+])=([\-+.\de]+)/,bs={position:"absolute",visibility:"hidden",display:"block"},bt=["Left","Right"],bu=["Top","Bottom"],bv,bw,bx;f.fn.css=function(a,c){if(arguments.length===2&&c===b)return this;return f.access(this,a,c,!0,function(a,c,d){return d!==b?f.style(a,c,d):f.css(a,c)})},f.extend({cssHooks:{opacity:{get:function(a,b){if(b){var c=bv(a,"opacity","opacity");return c===""?"1":c}return a.style.opacity}}},cssNumber:{fillOpacity:!0,fontWeight:!0,lineHeight:!0,opacity:!0,orphans:!0,widows:!0,zIndex:!0,zoom:!0},cssProps:{"float":f.support.cssFloat?"cssFloat":"styleFloat"},style:function(a,c,d,e){if(!!a&&a.nodeType!==3&&a.nodeType!==8&&!!a.style){var g,h,i=f.camelCase(c),j=a.style,k=f.cssHooks[i];c=f.cssProps[i]||i;if(d===b){if(k&&"get"in k&&(g=k.get(a,!1,e))!==b)return g;return j[c]}h=typeof d,h==="string"&&(g=br.exec(d))&&(d=+(g[1]+1)*+g[2]+parseFloat(f.css(a,c)),h="number");if(d==null||h==="number"&&isNaN(d))return;h==="number"&&!f.cssNumber[i]&&(d+="px");if(!k||!("set"in k)||(d=k.set(a,d))!==b)try{j[c]=d}catch(l){}}},css:function(a,c,d){var e,g;c=f.camelCase(c),g=f.cssHooks[c],c=f.cssProps[c]||c,c==="cssFloat"&&(c="float");if(g&&"get"in g&&(e=g.get(a,!0,d))!==b)return e;if(bv)return bv(a,c)},swap:function(a,b,c){var d={};for(var e in b)d[e]=a.style[e],a.style[e]=b[e];c.call(a);for(e in b)a.style[e]=d[e]}}),f.curCSS=f.css,f.each(["height","width"],function(a,b){f.cssHooks[b]={get:function(a,c,d){var e;if(c){if(a.offsetWidth!==0)return by(a,b,d);f.swap(a,bs,function(){e=by(a,b,d)});return e}},set:function(a,b){if(!bp.test(b))return b;b=parseFloat(b);if(b>=0)return b+"px"}}}),f.support.opacity||(f.cssHooks.opacity={get:function(a,b){return bn.test((b&&a.currentStyle?a.currentStyle.filter:a.style.filter)||"")?parseFloat(RegExp.$1)/100+"":b?"1":""},set:function(a,b){var c=a.style,d=a.currentStyle,e=f.isNaN(b)?"":"alpha(opacity="+b*100+")",g=d&&d.filter||c.filter||"";c.zoom=1;if(b>=1&&f.trim(g.replace(bm,""))===""){c.removeAttribute("filter");if(d&&!d.filter)return}c.filter=bm.test(g)?g.replace(bm,e):g+" "+e}}),f(function(){f.support.reliableMarginRight||(f.cssHooks.marginRight={get:function(a,b){var c;f.swap(a,{display:"inline-block"},function(){b?c=bv(a,"margin-right","marginRight"):c=a.style.marginRight});return c}})}),c.defaultView&&c.defaultView.getComputedStyle&&(bw=function(a,c){var d,e,g;c=c.replace(bo,"-$1").toLowerCase();if(!(e=a.ownerDocument.defaultView))return b;if(g=e.getComputedStyle(a,null))d=g.getPropertyValue(c),d===""&&!f.contains(a.ownerDocument.documentElement,a)&&(d=f.style(a,c));return d}),c.documentElement.currentStyle&&(bx=function(a,b){var c,d=a.currentStyle&&a.currentStyle[b],e=a.runtimeStyle&&a.runtimeStyle[b],f=a.style;!bp.test(d)&&bq.test(d)&&(c=f.left,e&&(a.runtimeStyle.left=a.currentStyle.left),f.left=b==="fontSize"?"1em":d||0,d=f.pixelLeft+"px",f.left=c,e&&(a.runtimeStyle.left=e));return d===""?"auto":d}),bv=bw||bx,f.expr&&f.expr.filters&&(f.expr.filters.hidden=function(a){var b=a.offsetWidth,c=a.offsetHeight;return b===0&&c===0||!f.support.reliableHiddenOffsets&&(a.style.display||f.css(a,"display"))==="none"},f.expr.filters.visible=function(a){return!f.expr.filters.hidden(a)});var bz=/%20/g,bA=/\[\]$/,bB=/\r?\n/g,bC=/#.*$/,bD=/^(.*?):[ \t]*([^\r\n]*)\r?$/mg,bE=/^(?:color|date|datetime|datetime-local|email|hidden|month|number|password|range|search|tel|text|time|url|week)$/i,bF=/^(?:about|app|app\-storage|.+\-extension|file|res|widget):$/,bG=/^(?:GET|HEAD)$/,bH=/^\/\//,bI=/\?/,bJ=/<script\b[^<]*(?:(?!<\/script>)<[^<]*)*<\/script>/gi,bK=/^(?:select|textarea)/i,bL=/\s+/,bM=/([?&])_=[^&]*/,bN=/^([\w\+\.\-]+:)(?:\/\/([^\/?#:]*)(?::(\d+))?)?/,bO=f.fn.load,bP={},bQ={},bR,bS,bT=["*/"]+["*"];try{bR=e.href}catch(bU){bR=c.createElement("a"),bR.href="",bR=bR.href}bS=bN.exec(bR.toLowerCase())||[],f.fn.extend({load:function(a,c,d){if(typeof a!="string"&&bO)return bO.apply(this,arguments);if(!this.length)return this;var e=a.indexOf(" ");if(e>=0){var g=a.slice(e,a.length);a=a.slice(0,e)}var h="GET";c&&(f.isFunction(c)?(d=c,c=b):typeof c=="object"&&(c=f.param(c,f.ajaxSettings.traditional),h="POST"));var i=this;f.ajax({url:a,type:h,dataType:"html",data:c,complete:function(a,b,c){c=a.responseText,a.isResolved()&&(a.done(function(a){c=a}),i.html(g?f("<div>").append(c.replace(bJ,"")).find(g):c)),d&&i.each(d,[c,b,a])}});return this},serialize:function(){return f.param(this.serializeArray())},serializeArray:function(){return this.map(function(){return this.elements?f.makeArray(this.elements):this}).filter(function(){return this.name&&!this.disabled&&(this.checked||bK.test(this.nodeName)||bE.test(this.type))}).map(function(a,b){var c=f(this).val();return c==null?null:f.isArray(c)?f.map(c,function(a,c){return{name:b.name,value:a.replace(bB,"\r\n")}}):{name:b.name,value:c.replace(bB,"\r\n")}}).get()}}),f.each("ajaxStart ajaxStop ajaxComplete ajaxError ajaxSuccess ajaxSend".split(" "),function(a,b){f.fn[b]=function(a){return this.bind(b,a)}}),f.each(["get","post"],function(a,c){f[c]=function(a,d,e,g){f.isFunction(d)&&(g=g||e,e=d,d=b);return f.ajax({type:c,url:a,data:d,success:e,dataType:g})}}),f.extend({getScript:function(a,c){return f.get(a,b,c,"script")},getJSON:function(a,b,c){return f.get(a,b,c,"json")},ajaxSetup:function(a,b){b?bX(a,f.ajaxSettings):(b=a,a=f.ajaxSettings),bX(a,b);return a},ajaxSettings:{url:bR,isLocal:bF.test(bS[1]),global:!0,type:"GET",contentType:"application/x-www-form-urlencoded",processData:!0,async:!0,accepts:{xml:"application/xml, text/xml",html:"text/html",text:"text/plain",json:"application/json, text/javascript","*":bT},contents:{xml:/xml/,html:/html/,json:/json/},responseFields:{xml:"responseXML",text:"responseText"},converters:{"* text":a.String,"text html":!0,"text json":f.parseJSON,"text xml":f.parseXML},flatOptions:{context:!0,url:!0}},ajaxPrefilter:bV(bP),ajaxTransport:bV(bQ),ajax:function(a,c){function w(a,c,l,m){if(s!==2){s=2,q&&clearTimeout(q),p=b,n=m||"",v.readyState=a>0?4:0;var o,r,u,w=c,x=l?bZ(d,v,l):b,y,z;if(a>=200&&a<300||a===304){if(d.ifModified){if(y=v.getResponseHeader("Last-Modified"))f.lastModified[k]=y;if(z=v.getResponseHeader("Etag"))f.etag[k]=z}if(a===304)w="notmodified",o=!0;else try{r=b$(d,x),w="success",o=!0}catch(A){w="parsererror",u=A}}else{u=w;if(!w||a)w="error",a<0&&(a=0)}v.status=a,v.statusText=""+(c||w),o?h.resolveWith(e,[r,w,v]):h.rejectWith(e,[v,w,u]),v.statusCode(j),j=b,t&&g.trigger("ajax"+(o?"Success":"Error"),[v,d,o?r:u]),i.resolveWith(e,[v,w]),t&&(g.trigger("ajaxComplete",[v,d]),--f.active||f.event.trigger("ajaxStop"))}}typeof a=="object"&&(c=a,a=b),c=c||{};var d=f.ajaxSetup({},c),e=d.context||d,g=e!==d&&(e.nodeType||e instanceof f)?f(e):f.event,h=f.Deferred(),i=f._Deferred(),j=d.statusCode||{},k,l={},m={},n,o,p,q,r,s=0,t,u,v={readyState:0,setRequestHeader:function(a,b){if(!s){var c=a.toLowerCase();a=m[c]=m[c]||a,l[a]=b}return this},getAllResponseHeaders:function(){return s===2?n:null},getResponseHeader:function(a){var c;if(s===2){if(!o){o={};while(c=bD.exec(n))o[c[1].toLowerCase()]=c[2]}c=o[a.toLowerCase()]}return c===b?null:c},overrideMimeType:function(a){s||(d.mimeType=a);return this},abort:function(a){a=a||"abort",p&&p.abort(a),w(0,a);return this}};h.promise(v),v.success=v.done,v.error=v.fail,v.complete=i.done,v.statusCode=function(a){if(a){var b;if(s<2)for(b in a)j[b]=[j[b],a[b]];else b=a[v.status],v.then(b,b)}return this},d.url=((a||d.url)+"").replace(bC,"").replace(bH,bS[1]+"//"),d.dataTypes=f.trim(d.dataType||"*").toLowerCase().split(bL),d.crossDomain==null&&(r=bN.exec(d.url.toLowerCase()),d.crossDomain=!(!r||r[1]==bS[1]&&r[2]==bS[2]&&(r[3]||(r[1]==="http:"?80:443))==(bS[3]||(bS[1]==="http:"?80:443)))),d.data&&d.processData&&typeof d.data!="string"&&(d.data=f.param(d.data,d.traditional)),bW(bP,d,c,v);if(s===2)return!1;t=d.global,d.type=d.type.toUpperCase(),d.hasContent=!bG.test(d.type),t&&f.active++===0&&f.event.trigger("ajaxStart");if(!d.hasContent){d.data&&(d.url+=(bI.test(d.url)?"&":"?")+d.data,delete d.data),k=d.url;if(d.cache===!1){var x=f.now(),y=d.url.replace(bM,"$1_="+x);d.url=y+(y===d.url?(bI.test(d.url)?"&":"?")+"_="+x:"")}}(d.data&&d.hasContent&&d.contentType!==!1||c.contentType)&&v.setRequestHeader("Content-Type",d.contentType),d.ifModified&&(k=k||d.url,f.lastModified[k]&&v.setRequestHeader("If-Modified-Since",f.lastModified[k]),f.etag[k]&&v.setRequestHeader("If-None-Match",f.etag[k])),v.setRequestHeader("Accept",d.dataTypes[0]&&d.accepts[d.dataTypes[0]]?d.accepts[d.dataTypes[0]]+(d.dataTypes[0]!=="*"?", "+bT+"; q=0.01":""):d.accepts["*"]);for(u in d.headers)v.setRequestHeader(u,d.headers[u]);if(d.beforeSend&&(d.beforeSend.call(e,v,d)===!1||s===2)){v.abort();return!1}for(u in{success:1,error:1,complete:1})v[u](d[u]);p=bW(bQ,d,c,v);if(!p)w(-1,"No Transport");else{v.readyState=1,t&&g.trigger("ajaxSend",[v,d]),d.async&&d.timeout>0&&(q=setTimeout(function(){v.abort("timeout")},d.timeout));try{s=1,p.send(l,w)}catch(z){s<2?w(-1,z):f.error(z)}}return v},param:function(a,c){var d=[],e=function(a,b){b=f.isFunction(b)?b():b,d[d.length]=encodeURIComponent(a)+"="+encodeURIComponent(b)};c===b&&(c=f.ajaxSettings.traditional);if(f.isArray(a)||a.jquery&&!f.isPlainObject(a))f.each(a,function(){e(this.name,this.value)});else for(var g in a)bY(g,a[g],c,e);return d.join("&").replace(bz,"+")}}),f.extend({active:0,lastModified:{},etag:{}});var b_=f.now(),ca=/(\=)\?(&|$)|\?\?/i;f.ajaxSetup({jsonp:"callback",jsonpCallback:function(){return f.expando+"_"+b_++}}),f.ajaxPrefilter("json jsonp",function(b,c,d){var e=b.contentType==="application/x-www-form-urlencoded"&&typeof b.data=="string";if(b.dataTypes[0]==="jsonp"||b.jsonp!==!1&&(ca.test(b.url)||e&&ca.test(b.data))){var g,h=b.jsonpCallback=f.isFunction(b.jsonpCallback)?b.jsonpCallback():b.jsonpCallback,i=a[h],j=b.url,k=b.data,l="$1"+h+"$2";b.jsonp!==!1&&(j=j.replace(ca,l),b.url===j&&(e&&(k=k.replace(ca,l)),b.data===k&&(j+=(/\?/.test(j)?"&":"?")+b.jsonp+"="+h))),b.url=j,b.data=k,a[h]=function(a){g=[a]},d.always(function(){a[h]=i,g&&f.isFunction(i)&&a[h](g[0])}),b.converters["script json"]=function(){g||f.error(h+" was not called");return g[0]},b.dataTypes[0]="json";return"script"}}),f.ajaxSetup({accepts:{script:"text/javascript, application/javascript, application/ecmascript, application/x-ecmascript"},contents:{script:/javascript|ecmascript/},converters:{"text script":function(a){f.globalEval(a);return a}}}),f.ajaxPrefilter("script",function(a){a.cache===b&&(a.cache=!1),a.crossDomain&&(a.type="GET",a.global=!1)}),f.ajaxTransport("script",function(a){if(a.crossDomain){var d,e=c.head||c.getElementsByTagName("head")[0]||c.documentElement;return{send:function(f,g){d=c.createElement("script"),d.async="async",a.scriptCharset&&(d.charset=a.scriptCharset),d.src=a.url,d.onload=d.onreadystatechange=function(a,c){if(c||!d.readyState||/loaded|complete/.test(d.readyState))d.onload=d.onreadystatechange=null,e&&d.parentNode&&e.removeChild(d),d=b,c||g(200,"success")},e.insertBefore(d,e.firstChild)},abort:function(){d&&d.onload(0,1)}}}});var cb=a.ActiveXObject?function(){for(var a in cd)cd[a](0,1)}:!1,cc=0,cd;f.ajaxSettings.xhr=a.ActiveXObject?function(){return!this.isLocal&&ce()||cf()}:ce,function(a){f.extend(f.support,{ajax:!!a,cors:!!a&&"withCredentials"in a})}(f.ajaxSettings.xhr()),f.support.ajax&&f.ajaxTransport(function(c){if(!c.crossDomain||f.support.cors){var d;return{send:function(e,g){var h=c.xhr(),i,j;c.username?h.open(c.type,c.url,c.async,c.username,c.password):h.open(c.type,c.url,c.async);if(c.xhrFields)for(j in c.xhrFields)h[j]=c.xhrFields[j];c.mimeType&&h.overrideMimeType&&h.overrideMimeType(c.mimeType),!c.crossDomain&&!e["X-Requested-With"]&&(e["X-Requested-With"]="XMLHttpRequest");try{for(j in e)h.setRequestHeader(j,e[j])}catch(k){}h.send(c.hasContent&&c.data||null),d=function(a,e){var j,k,l,m,n;try{if(d&&(e||h.readyState===4)){d=b,i&&(h.onreadystatechange=f.noop,cb&&delete cd[i]);if(e)h.readyState!==4&&h.abort();else{j=h.status,l=h.getAllResponseHeaders(),m={},n=h.responseXML,n&&n.documentElement&&(m.xml=n),m.text=h.responseText;try{k=h.statusText}catch(o){k=""}!j&&c.isLocal&&!c.crossDomain?j=m.text?200:404:j===1223&&(j=204)}}}catch(p){e||g(-1,p)}m&&g(j,k,m,l)},!c.async||h.readyState===4?d():(i=++cc,cb&&(cd||(cd={},f(a).unload(cb)),cd[i]=d),h.onreadystatechange=d)},abort:function(){d&&d(0,1)}}}});var cg={},ch,ci,cj=/^(?:toggle|show|hide)$/,ck=/^([+\-]=)?([\d+.\-]+)([a-z%]*)$/i,cl,cm=[["height","marginTop","marginBottom","paddingTop","paddingBottom"],["width","marginLeft","marginRight","paddingLeft","paddingRight"],["opacity"]],cn;f.fn.extend({show:function(a,b,c){var d,e;if(a||a===0)return this.animate(cq("show",3),a,b,c);for(var g=0,h=this.length;g<h;g++)d=this[g],d.style&&(e=d.style.display,!f._data(d,"olddisplay")&&e==="none"&&(e=d.style.display=""),e===""&&f.css(d,"display")==="none"&&f._data(d,"olddisplay",cr(d.nodeName)));for(g=0;g<h;g++){d=this[g];if(d.style){e=d.style.display;if(e===""||e==="none")d.style.display=f._data(d,"olddisplay")||""}}return this},hide:function(a,b,c){if(a||a===0)return this.animate(cq("hide",3),a,b,c);for(var d=0,e=this.length;d<e;d++)if(this[d].style){var g=f.css(this[d],"display");g!=="none"&&!f._data(this[d],"olddisplay")&&f._data(this[d],"olddisplay",g)}for(d=0;d<e;d++)this[d].style&&(this[d].style.display="none");return this},_toggle:f.fn.toggle,toggle:function(a,b,c){var d=typeof a=="boolean";f.isFunction(a)&&f.isFunction(b)?this._toggle.apply(this,arguments):a==null||d?this.each(function(){var b=d?a:f(this).is(":hidden");f(this)[b?"show":"hide"]()}):this.animate(cq("toggle",3),a,b,c);return this},fadeTo:function(a,b,c,d){return this.filter(":hidden").css("opacity",0).show().end().animate({opacity:b},a,c,d)},animate:function(a,b,c,d){var e=f.speed(b,c,d);if(f.isEmptyObject(a))return this.each(e.complete,[!1]);a=f.extend({},a);return this[e.queue===!1?"each":"queue"](function(){e.queue===!1&&f._mark(this);var b=f.extend({},e),c=this.nodeType===1,d=c&&f(this).is(":hidden"),g,h,i,j,k,l,m,n,o;b.animatedProperties={};for(i in a){g=f.camelCase(i),i!==g&&(a[g]=a[i],delete a[i]),h=a[g],f.isArray(h)?(b.animatedProperties[g]=h[1],h=a[g]=h[0]):b.animatedProperties[g]=b.specialEasing&&b.specialEasing[g]||b.easing||"swing";if(h==="hide"&&d||h==="show"&&!d)return b.complete.call(this);c&&(g==="height"||g==="width")&&(b.overflow=[this.style.overflow,this.style.overflowX,this.style.overflowY],f.css(this,"display")==="inline"&&f.css(this,"float")==="none"&&(f.support.inlineBlockNeedsLayout?(j=cr(this.nodeName),j==="inline"?this.style.display="inline-block":(this.style.display="inline",this.style.zoom=1)):this.style.display="inline-block"))}b.overflow!=null&&(this.style.overflow="hidden");for(i in a)k=new f.fx(this,b,i),h=a[i],cj.test(h)?k[h==="toggle"?d?"show":"hide":h]():(l=ck.exec(h),m=k.cur(),l?(n=parseFloat(l[2]),o=l[3]||(f.cssNumber[i]?"":"px"),o!=="px"&&(f.style(this,i,(n||1)+o),m=(n||1)/k.cur()*m,f.style(this,i,m+o)),l[1]&&(n=(l[1]==="-="?-1:1)*n+m),k.custom(m,n,o)):k.custom(m,h,""));return!0})},stop:function(a,b){a&&this.queue([]),this.each(function(){var a=f.timers,c=a.length;b||f._unmark(!0,this);while(c--)a[c].elem===this&&(b&&a[c](!0),a.splice(c,1))}),b||this.dequeue();return this}}),f.each({slideDown:cq("show",1),slideUp:cq("hide",1),slideToggle:cq("toggle",1),fadeIn:{opacity:"show"},fadeOut:{opacity:"hide"},fadeToggle:{opacity:"toggle"}},function(a,b){f.fn[a]=function(a,c,d){return this.animate(b,a,c,d)}}),f.extend({speed:function(a,b,c){var d=a&&typeof a=="object"?f.extend({},a):{complete:c||!c&&b||f.isFunction(a)&&a,duration:a,easing:c&&b||b&&!f.isFunction(b)&&b};d.duration=f.fx.off?0:typeof d.duration=="number"?d.duration:d.duration in f.fx.speeds?f.fx.speeds[d.duration]:f.fx.speeds._default,d.old=d.complete,d.complete=function(a){f.isFunction(d.old)&&d.old.call(this),d.queue!==!1?f.dequeue(this):a!==!1&&f._unmark(this)};return d},easing:{linear:function(a,b,c,d){return c+d*a},swing:function(a,b,c,d){return(-Math.cos(a*Math.PI)/2+.5)*d+c}},timers:[],fx:function(a,b,c){this.options=b,this.elem=a,this.prop=c,b.orig=b.orig||{}}}),f.fx.prototype={update:function(){this.options.step&&this.options.step.call(this.elem,this.now,this),(f.fx.step[this.prop]||f.fx.step._default)(this)},cur:function(){if(this.elem[this.prop]!=null&&(!this.elem.style||this.elem.style[this.prop]==null))return this.elem[this.prop];var a,b=f.css(this.elem,this.prop);return isNaN(a=parseFloat(b))?!b||b==="auto"?0:b:a},custom:function(a,b,c){function g(a){return d.step(a)}var d=this,e=f.fx;this.startTime=cn||co(),this.start=a,this.end=b,this.unit=c||this.unit||(f.cssNumber[this.prop]?"":"px"),this.now=this.start,this.pos=this.state=0,g.elem=this.elem,g()&&f.timers.push(g)&&!cl&&(cl=setInterval(e.tick,e.interval))},show:function(){this.options.orig[this.prop]=f.style(this.elem,this.prop),this.options.show=!0,this.custom(this.prop==="width"||this.prop==="height"?1:0,this.cur()),f(this.elem).show()},hide:function(){this.options.orig[this.prop]=f.style(this.elem,this.prop),this.options.hide=!0,this.custom(this.cur(),0)},step:function(a){var b=cn||co(),c=!0,d=this.elem,e=this.options,g,h;if(a||b>=e.duration+this.startTime){this.now=this.end,this.pos=this.state=1,this.update(),e.animatedProperties[this.prop]=!0;for(g in e.animatedProperties)e.animatedProperties[g]!==!0&&(c=!1);if(c){e.overflow!=null&&!f.support.shrinkWrapBlocks&&f.each(["","X","Y"],function(a,b){d.style["overflow"+b]=e.overflow[a]}),e.hide&&f(d).hide();if(e.hide||e.show)for(var i in e.animatedProperties)f.style(d,i,e.orig[i]);e.complete.call(d)}return!1}e.duration==Infinity?this.now=b:(h=b-this.startTime,this.state=h/e.duration,this.pos=f.easing[e.animatedProperties[this.prop]](this.state,h,0,1,e.duration),this.now=this.start+(this.end-this.start)*this.pos),this.update();return!0}},f.extend(f.fx,{tick:function(){for(var a=f.timers,b=0;b<a.length;++b)a[b]()||a.splice(b--,1);a.length||f.fx.stop()},interval:13,stop:function(){clearInterval(cl),cl=null},speeds:{slow:600,fast:200,_default:400},step:{opacity:function(a){f.style(a.elem,"opacity",a.now)},_default:function(a){a.elem.style&&a.elem.style[a.prop]!=null?a.elem.style[a.prop]=(a.prop==="width"||a.prop==="height"?Math.max(0,a.now):a.now)+a.unit:a.elem[a.prop]=a.now}}}),f.expr&&f.expr.filters&&(f.expr.filters.animated=function(a){return f.grep(f.timers,function(b){return a===b.elem}).length});var cs=/^t(?:able|d|h)$/i,ct=/^(?:body|html)$/i;"getBoundingClientRect"in c.documentElement?f.fn.offset=function(a){var b=this[0],c;if(a)return this.each(function(b){f.offset.setOffset(this,a,b)});if(!b||!b.ownerDocument)return null;if(b===b.ownerDocument.body)return f.offset.bodyOffset(b);try{c=b.getBoundingClientRect()}catch(d){}var e=b.ownerDocument,g=e.documentElement;if(!c||!f.contains(g,b))return c?{top:c.top,left:c.left}:{top:0,left:0};var h=e.body,i=cu(e),j=g.clientTop||h.clientTop||0,k=g.clientLeft||h.clientLeft||0,l=i.pageYOffset||f.support.boxModel&&g.scrollTop||h.scrollTop,m=i.pageXOffset||f.support.boxModel&&g.scrollLeft||h.scrollLeft,n=c.top+l-j,o=c.left+m-k;return{top:n,left:o}}:f.fn.offset=function(a){var b=this[0];if(a)return this.each(function(b){f.offset.setOffset(this,a,b)});if(!b||!b.ownerDocument)return null;if(b===b.ownerDocument.body)return f.offset.bodyOffset(b);f.offset.initialize();var c,d=b.offsetParent,e=b,g=b.ownerDocument,h=g.documentElement,i=g.body,j=g.defaultView,k=j?j.getComputedStyle(b,null):b.currentStyle,l=b.offsetTop,m=b.offsetLeft;while((b=b.parentNode)&&b!==i&&b!==h){if(f.offset.supportsFixedPosition&&k.position==="fixed")break;c=j?j.getComputedStyle(b,null):b.currentStyle,l-=b.scrollTop,m-=b.scrollLeft,b===d&&(l+=b.offsetTop,m+=b.offsetLeft,f.offset.doesNotAddBorder&&(!f.offset.doesAddBorderForTableAndCells||!cs.test(b.nodeName))&&(l+=parseFloat(c.borderTopWidth)||0,m+=parseFloat(c.borderLeftWidth)||0),e=d,d=b.offsetParent),f.offset.subtractsBorderForOverflowNotVisible&&c.overflow!=="visible"&&(l+=parseFloat(c.borderTopWidth)||0,m+=parseFloat(c.borderLeftWidth)||0),k=c}if(k.position==="relative"||k.position==="static")l+=i.offsetTop,m+=i.offsetLeft;f.offset.supportsFixedPosition&&k.position==="fixed"&&(l+=Math.max(h.scrollTop,i.scrollTop),m+=Math.max(h.scrollLeft,i.scrollLeft));return{top:l,left:m}},f.offset={initialize:function(){var a=c.body,b=c.createElement("div"),d,e,g,h,i=parseFloat(f.css(a,"marginTop"))||0,j="<div style='position:absolute;top:0;left:0;margin:0;border:5px solid #000;padding:0;width:1px;height:1px;'><div></div></div><table style='position:absolute;top:0;left:0;margin:0;border:5px solid #000;padding:0;width:1px;height:1px;' cellpadding='0' cellspacing='0'><tr><td></td></tr></table>";f.extend(b.style,{position:"absolute",top:0,left:0,margin:0,border:0,width:"1px",height:"1px",visibility:"hidden"}),b.innerHTML=j,a.insertBefore(b,a.firstChild),d=b.firstChild,e=d.firstChild,h=d.nextSibling.firstChild.firstChild,this.doesNotAddBorder=e.offsetTop!==5,this.doesAddBorderForTableAndCells=h.offsetTop===5,e.style.position="fixed",e.style.top="20px",this.supportsFixedPosition=e.offsetTop===20||e.offsetTop===15,e.style.position=e.style.top="",d.style.overflow="hidden",d.style.position="relative",this.subtractsBorderForOverflowNotVisible=e.offsetTop===-5,this.doesNotIncludeMarginInBodyOffset=a.offsetTop!==i,a.removeChild(b),f.offset.initialize=f.noop},bodyOffset:function(a){var b=a.offsetTop,c=a.offsetLeft;f.offset.initialize(),f.offset.doesNotIncludeMarginInBodyOffset&&(b+=parseFloat(f.css(a,"marginTop"))||0,c+=parseFloat(f.css(a,"marginLeft"))||0);return{top:b,left:c}},setOffset:function(a,b,c){var d=f.css(a,"position");d==="static"&&(a.style.position="relative");var e=f(a),g=e.offset(),h=f.css(a,"top"),i=f.css(a,"left"),j=(d==="absolute"||d==="fixed")&&f.inArray("auto",[h,i])>-1,k={},l={},m,n;j?(l=e.position(),m=l.top,n=l.left):(m=parseFloat(h)||0,n=parseFloat(i)||0),f.isFunction(b)&&(b=b.call(a,c,g)),b.top!=null&&(k.top=b.top-g.top+m),b.left!=null&&(k.left=b.left-g.left+n),"using"in b?b.using.call(a,k):e.css(k)}},f.fn.extend({position:function(){if(!this[0])return null;var a=this[0],b=this.offsetParent(),c=this.offset(),d=ct.test(b[0].nodeName)?{top:0,left:0}:b.offset();c.top-=parseFloat(f.css(a,"marginTop"))||0,c.left-=parseFloat(f.css(a,"marginLeft"))||0,d.top+=parseFloat(f.css(b[0],"borderTopWidth"))||0,d.left+=parseFloat(f.css(b[0],"borderLeftWidth"))||0;return{top:c.top-d.top,left:c.left-d.left}},offsetParent:function(){return this.map(function(){var a=this.offsetParent||c.body;while(a&&!ct.test(a.nodeName)&&f.css(a,"position")==="static")a=a.offsetParent;return a})}}),f.each(["Left","Top"],function(a,c){var d="scroll"+c;f.fn[d]=function(c){var e,g;if(c===b){e=this[0];if(!e)return null;g=cu(e);return g?"pageXOffset"in g?g[a?"pageYOffset":"pageXOffset"]:f.support.boxModel&&g.document.documentElement[d]||g.document.body[d]:e[d]}return this.each(function(){g=cu(this),g?g.scrollTo(a?f(g).scrollLeft():c,a?c:f(g).scrollTop()):this[d]=c})}}),f.each(["Height","Width"],function(a,c){var d=c.toLowerCase();f.fn["inner"+c]=function(){var a=this[0];return a&&a.style?parseFloat(f.css(a,d,"padding")):null},f.fn["outer"+c]=function(a){var b=this[0];return b&&b.style?parseFloat(f.css(b,d,a?"margin":"border")):null},f.fn[d]=function(a){var e=this[0];if(!e)return a==null?null:this;if(f.isFunction(a))return this.each(function(b){var c=f(this);c[d](a.call(this,b,c[d]()))});if(f.isWindow(e)){var g=e.document.documentElement["client"+c],h=e.document.body;return e.document.compatMode==="CSS1Compat"&&g||h&&h["client"+c]||g}if(e.nodeType===9)return Math.max(e.documentElement["client"+c],e.body["scroll"+c],e.documentElement["scroll"+c],e.body["offset"+c],e.documentElement["offset"+c]);if(a===b){var i=f.css(e,d),j=parseFloat(i);return f.isNaN(j)?i:j}return this.css(d,typeof a=="string"?a:a+"px")}}),a.jQuery=a.$=f})(window);    }
})(window, void(0));// Scheme numbers.


var __PLTNUMBERS_TOP__;
if (typeof(exports) !== 'undefined') {
    __PLTNUMBERS_TOP__ = exports;
} else {
    if (! this['jsnums']) {
 	this['jsnums'] = {};
    }
    __PLTNUMBERS_TOP__  = this['jsnums'];
}

//var jsnums = {};


// The numeric tower has the following levels:
//     integers
//     rationals
//     floats
//     complex numbers
//
// with the representations:
//     integers: fixnum or BigInteger [level=0]
//     rationals: Rational [level=1]
//     floats: FloatPoint [level=2]
//     complex numbers: Complex [level=3]

// We try to stick with the unboxed fixnum representation for
// integers, since that's what scheme programs commonly deal with, and
// we want that common type to be lightweight.


// A boxed-scheme-number is either BigInteger, Rational, FloatPoint, or Complex.
// An integer-scheme-number is either fixnum or BigInteger.


(function() {
    'use strict';
    // Abbreviation
    var Numbers = __PLTNUMBERS_TOP__;
    //var Numbers = jsnums;


    // makeNumericBinop: (fixnum fixnum -> any) (scheme-number scheme-number -> any) -> (scheme-number scheme-number) X
    // Creates a binary function that works either on fixnums or boxnums.
    // Applies the appropriate binary function, ensuring that both scheme numbers are
    // lifted to the same level.
    var makeNumericBinop = function(onFixnums, onBoxednums, options) {
	options = options || {};
	return function(x, y) {
	    if (options.isXSpecialCase && options.isXSpecialCase(x))
		return options.onXSpecialCase(x, y);
	    if (options.isYSpecialCase && options.isYSpecialCase(y))
		return options.onYSpecialCase(x, y);

	    if (typeof(x) === 'number' &&
		typeof(y) === 'number') {
		return onFixnums(x, y);
	    }
	    if (typeof(x) === 'number') {
		x = liftFixnumInteger(x, y);
	    }
	    if (typeof(y) === 'number') {
		y = liftFixnumInteger(y, x);
	    }

	    if (x.level < y.level) x = x.liftTo(y);
	    if (y.level < x.level) y = y.liftTo(x);
	    return onBoxednums(x, y);
	};
    }
    
    
    // fromFixnum: fixnum -> scheme-number
    var fromFixnum = function(x) {
	if (isNaN(x) || (! isFinite(x))) {
	    return FloatPoint.makeInstance(x);
	}
	var nf = Math.floor(x);
	if (nf === x) {
            if (isOverflow(nf)) {
		return makeBignum(expandExponent(x+''));
            } else {
		return nf;
	    }
	} else {
            return FloatPoint.makeInstance(x);
	}
    };

    var expandExponent = function(s) {
	var match = s.match(scientificPattern), mantissaChunks, exponent;
	if (match) {
	    mantissaChunks = match[1].match(/^([^.]*)(.*)$/);
	    exponent = Number(match[2]);

	    if (mantissaChunks[2].length === 0) {
		return mantissaChunks[1] + zfill(exponent);
	    }

	    if (exponent >= mantissaChunks[2].length - 1) {
		return (mantissaChunks[1] + 
			mantissaChunks[2].substring(1) + 
			zfill(exponent - (mantissaChunks[2].length - 1)));
	    } else {
		return (mantissaChunks[1] +
			mantissaChunks[2].substring(1, 1+exponent));
	    }
	} else {
	    return s;
	}
    };

    // zfill: integer -> string
    // builds a string of "0"'s of length n.
    var zfill = function(n) {
	var buffer = [];
	buffer.length = n;
	for (var i = 0; i < n; i++) {
	    buffer[i] = '0';
	}
	return buffer.join('');
    };
    

    
    // liftFixnumInteger: fixnum-integer boxed-scheme-number -> boxed-scheme-number
    // Lifts up fixnum integers to a boxed type.
    var liftFixnumInteger = function(x, other) {
	switch(other.level) {
	case 0: // BigInteger
	    return makeBignum(x);
	case 1: // Rational
	    return new Rational(x, 1);
	case 2: // FloatPoint
	    return new FloatPoint(x);
	case 3: // Complex
	    return new Complex(x, 0);
	default:
	    throwRuntimeError("IMPOSSIBLE: cannot lift fixnum integer to " + other.toString(), x, other);
	}
    };
    
    
    // throwRuntimeError: string (scheme-number | undefined) (scheme-number | undefined) -> void
    // Throws a runtime error with the given message string.
    var throwRuntimeError = function(msg, x, y) {
	Numbers['onThrowRuntimeError'](msg, x, y);
    };



    // onThrowRuntimeError: string (scheme-number | undefined) (scheme-number | undefined) -> void
    // By default, will throw a new Error with the given message.
    // Override Numbers['onThrowRuntimeError'] if you need to do something special.
    var onThrowRuntimeError = function(msg, x, y) {
	throw new Error(msg);
    };


    // isSchemeNumber: any -> boolean
    // Returns true if the thing is a scheme number.
    var isSchemeNumber = function(thing) {
	return (typeof(thing) === 'number'
		|| (thing instanceof Rational ||
		    thing instanceof FloatPoint ||
		    thing instanceof Complex ||
		    thing instanceof BigInteger));
    };


    // isRational: scheme-number -> boolean
    var isRational = function(n) {
	return (typeof(n) === 'number' ||
		(isSchemeNumber(n) && n.isRational()));
    };

    // isReal: scheme-number -> boolean
    var isReal = function(n) {
	return (typeof(n) === 'number' ||
		(isSchemeNumber(n) && n.isReal()));
    };

    // isExact: scheme-number -> boolean
    var isExact = function(n) {
	return (typeof(n) === 'number' || 
		(isSchemeNumber(n) && n.isExact()));
    };

    // isExact: scheme-number -> boolean
    var isInexact = function(n) {
	if (typeof(n) === 'number') {
	    return false;
	} else {
	    return (isSchemeNumber(n) && n.isInexact());
	}
    };

    // isInteger: scheme-number -> boolean
    var isInteger = function(n) {
	return (typeof(n) === 'number' ||
		(isSchemeNumber(n) && n.isInteger()));
    };

    // isExactInteger: scheme-number -> boolean
    var isExactInteger = function(n) {
	return (typeof(n) === 'number' ||
		(isSchemeNumber(n) && 
		 n.isInteger() && 
		 n.isExact()));
    }



    // toFixnum: scheme-number -> javascript-number
    var toFixnum = function(n) {
	if (typeof(n) === 'number')
	    return n;
	return n.toFixnum();
    };

    // toExact: scheme-number -> scheme-number
    var toExact = function(n) {
	if (typeof(n) === 'number')
	    return n;
	return n.toExact();
    };


    // toExact: scheme-number -> scheme-number
    var toInexact = function(n) {
	if (typeof(n) === 'number')
	    return FloatPoint.makeInstance(n);
	return n.toInexact();
    };



    //////////////////////////////////////////////////////////////////////


    // add: scheme-number scheme-number -> scheme-number
    var add = function(x, y) {
        var sum;
        if (typeof(x) === 'number' && typeof(y) === 'number') {
            sum = x + y;
            if (isOverflow(sum)) {
		return (makeBignum(x)).add(makeBignum(y));
            }
        }
        if (x instanceof FloatPoint && y instanceof FloatPoint) {
            return x.add(y);
        }
        return addSlow(x, y);        
    };

    var addSlow = makeNumericBinop(
	function(x, y) {
	    var sum = x + y;
	    if (isOverflow(sum)) {
		return (makeBignum(x)).add(makeBignum(y));
	    } else {
		return sum;
	    }
	},
	function(x, y) {
	    return x.add(y);
	},
	{isXSpecialCase: function(x) { 
	    return isExactInteger(x) && _integerIsZero(x) },
	 onXSpecialCase: function(x, y) { return y; },
	 isYSpecialCase: function(y) { 
	     return isExactInteger(y) && _integerIsZero(y) },
	 onYSpecialCase: function(x, y) { return x; }
	});


    // subtract: scheme-number scheme-number -> scheme-number
    var subtract = makeNumericBinop(
	function(x, y) {
	    var diff = x - y;
	    if (isOverflow(diff)) {
		return (makeBignum(x)).subtract(makeBignum(y));
	    } else {
		return diff;
	    }
	},
	function(x, y) {
	    return x.subtract(y);
	},
	{isXSpecialCase: function(x) { 
	    return isExactInteger(x) && _integerIsZero(x) },
	 onXSpecialCase: function(x, y) { return negate(y); },
	 isYSpecialCase: function(y) { 
	     return isExactInteger(y) && _integerIsZero(y) },
	 onYSpecialCase: function(x, y) { return x; }
	});


    // mulitply: scheme-number scheme-number -> scheme-number
    var multiply = function(x, y) {
        var prod;
        if (typeof(x) === 'number' && typeof(y) === 'number') {
	    prod = x * y;
	    if (isOverflow(prod)) {
		return (makeBignum(x)).multiply(makeBignum(y));
            } else {
                return prod;
            }
        }
        if (x instanceof FloatPoint && y instanceof FloatPoint) {
            return x.multiply(y);
        }
        return multiplySlow(x, y);
    };
    var multiplySlow = makeNumericBinop(
	function(x, y) {
	    var prod = x * y;
	    if (isOverflow(prod)) {
		return (makeBignum(x)).multiply(makeBignum(y));
	    } else {
		return prod;
	    }
	},
	function(x, y) {
	    return x.multiply(y);
	},
	{isXSpecialCase: function(x) { 
	    return (isExactInteger(x) && 
		    (_integerIsZero(x) || _integerIsOne(x) || _integerIsNegativeOne(x))) },
	 onXSpecialCase: function(x, y) { 
	     if (_integerIsZero(x))
		 return 0;
	     if (_integerIsOne(x))
		 return y;
	     if (_integerIsNegativeOne(x))
		 return negate(y);
	 },
	 isYSpecialCase: function(y) { 
	     return (isExactInteger(y) && 
		     (_integerIsZero(y) || _integerIsOne(y) || _integerIsNegativeOne(y)))},
	 onYSpecialCase: function(x, y) { 
	     if (_integerIsZero(y))
		 return 0;
	     if (_integerIsOne(y))
		 return x;
	     if (_integerIsNegativeOne(y)) 
		 return negate(x);
	 }
	});

    
    // divide: scheme-number scheme-number -> scheme-number
    var divide = makeNumericBinop(
	function(x, y) {
	    if (_integerIsZero(y))
		throwRuntimeError("/: division by zero", x, y);
	    var div = x / y;
	    if (isOverflow(div)) {
		return (makeBignum(x)).divide(makeBignum(y));
	    } else if (Math.floor(div) !== div) {
		return Rational.makeInstance(x, y);
	    } else {
		return div;
	    }
	},
	function(x, y) {
	    return x.divide(y);
	},
	{ isXSpecialCase: function(x) {
	    return (eqv(x, 0));
	},
	  onXSpecialCase: function(x, y) {
	      if (eqv(y, 0)) {
		  throwRuntimeError("/: division by zero", x, y);
	      }
	      return 0;
	  },
	  isYSpecialCase: function(y) { 
	    return (eqv(y, 0)); },
	  onYSpecialCase: function(x, y) {
	      throwRuntimeError("/: division by zero", x, y);
	  }
	});
    
    
    // equals: scheme-number scheme-number -> boolean
    var equals = makeNumericBinop(
	function(x, y) {
	    return x === y;
	},
	function(x, y) {
	    return x.equals(y);
	});


    // eqv: scheme-number scheme-number -> boolean
    var eqv = function(x, y) {
	if (x === y)
	    return true;
	if (typeof(x) === 'number' && typeof(y) === 'number')
	    return x === y;
	if (x === NEGATIVE_ZERO || y === NEGATIVE_ZERO)
	    return x === y;
	if (x instanceof Complex || y instanceof Complex) {
	    return (eqv(realPart(x), realPart(y)) &&
		    eqv(imaginaryPart(x), imaginaryPart(y)));
	}
	var ex = isExact(x), ey = isExact(y);
	return (((ex && ey) || (!ex && !ey)) && equals(x, y));
    };

    // approxEqual: scheme-number scheme-number scheme-number -> boolean
    var approxEquals = function(x, y, delta) {
	return lessThan(abs(subtract(x, y)),
                        delta);
    };

    // greaterThanOrEqual: scheme-number scheme-number -> boolean
    var greaterThanOrEqual = makeNumericBinop(
	function(x, y) {
	    return x >= y;
	},
	function(x, y) {
	    if (!(isReal(x) && isReal(y)))
		throwRuntimeError(
		    ">=: couldn't be applied to complex number", x, y);
	    return x.greaterThanOrEqual(y);
	});


    // lessThanOrEqual: scheme-number scheme-number -> boolean
    var lessThanOrEqual = makeNumericBinop(
	function(x, y){

	    return x <= y;
	},
	function(x, y) {
	    if (!(isReal(x) && isReal(y)))
		throwRuntimeError("<=: couldn't be applied to complex number", x, y);
	    return x.lessThanOrEqual(y);
	});


    // greaterThan: scheme-number scheme-number -> boolean
    var greaterThan = makeNumericBinop(
	function(x, y){
	    return x > y;
	},
	function(x, y) {
	    if (!(isReal(x) && isReal(y)))
		throwRuntimeError(">: couldn't be applied to complex number", x, y);
	    return x.greaterThan(y);
	});


    // lessThan: scheme-number scheme-number -> boolean
    var lessThan = makeNumericBinop(
	function(x, y){

	    return x < y;
	},
	function(x, y) {
	    if (!(isReal(x) && isReal(y)))
		throwRuntimeError("<: couldn't be applied to complex number", x, y);
	    return x.lessThan(y);
	});



    // expt: scheme-number scheme-number -> scheme-number
    var expt = (function() {
	var _expt = makeNumericBinop(
	    function(x, y){
		var pow = Math.pow(x, y);
		if (isOverflow(pow)) {
		    return (makeBignum(x)).expt(makeBignum(y));
		} else {
		    return pow;
		}
	    },
	    function(x, y) {
		if (equals(y, 0)) {
		    return add(y, 1);
		} else {
		    return x.expt(y);
		}
	    });
	return function(x, y) {
	    if (equals(y, 0)) 
		return add(y, 1);
	    if (isReal(y) && lessThan(y, 0)) {
		return _expt(divide(1, x), negate(y));
	    }
	    return _expt(x, y);
	};
    })();


    // exp: scheme-number -> scheme-number
    var exp = function(n) {
	if ( eqv(n, 0) ) {
		return 1;
	}
	if (typeof(n) === 'number') {
	    return FloatPoint.makeInstance(Math.exp(n));
	}
	return n.exp();
    };


    // modulo: scheme-number scheme-number -> scheme-number
    var modulo = function(m, n) {
	if (! isInteger(m)) {
	    throwRuntimeError('modulo: the first argument '
			      + m + " is not an integer.", m, n);
	}
	if (! isInteger(n)) {
	    throwRuntimeError('modulo: the second argument '
			      + n + " is not an integer.", m, n);
	}
	var result;
	if (typeof(m) === 'number') {
	    result = m % n;
	    if (n < 0) {
		if (result <= 0)
		    return result;
		else
		    return result + n;
	    } else {
		if (result < 0)
		    return result + n;
		else
		    return result;
	    }
	}
	result = _integerModulo(floor(m), floor(n));
	// The sign of the result should match the sign of n.
	if (lessThan(n, 0)) {
	    if (lessThanOrEqual(result, 0)) {
		return result;
	    }
	    return add(result, n);

	} else {
	    if (lessThan(result, 0)) {
		return add(result, n);
	    }
	    return result;
	}
    };



    // numerator: scheme-number -> scheme-number
    var numerator = function(n) {
	if (typeof(n) === 'number')
	    return n;
	return n.numerator();
    };


    // denominator: scheme-number -> scheme-number
    var denominator = function(n) {
	if (typeof(n) === 'number')
	    return 1;
	return n.denominator();
    };

    // sqrt: scheme-number -> scheme-number
    var sqrt = function(n) {
	if (typeof(n) === 'number') {
	    if (n >= 0) {
		var result = Math.sqrt(n);
		if (Math.floor(result) === result) {
		    return result;
		} else {
		    return FloatPoint.makeInstance(result);
		}
	    } else {
		return (Complex.makeInstance(0, sqrt(-n)));
	    }
	}
	return n.sqrt();
    };

    // abs: scheme-number -> scheme-number
    var abs = function(n) {
	if (typeof(n) === 'number') {
	    return Math.abs(n);
	}
	return n.abs();
    };

    // floor: scheme-number -> scheme-number
    var floor = function(n) {
	if (typeof(n) === 'number')
	    return n;
	return n.floor();
    };

    // ceiling: scheme-number -> scheme-number
    var ceiling = function(n) {
	if (typeof(n) === 'number')
	    return n;
	return n.ceiling();
    };

    // conjugate: scheme-number -> scheme-number
    var conjugate = function(n) {
	if (typeof(n) === 'number')
	    return n;
	return n.conjugate();
    };

    // magnitude: scheme-number -> scheme-number
    var magnitude = function(n) {
	if (typeof(n) === 'number')
	    return Math.abs(n);
	return n.magnitude();
    };


    // log: scheme-number -> scheme-number
    var log = function(n) {
	if ( eqv(n, 1) ) {
		return 0;
	}
	if (typeof(n) === 'number') {
	    return FloatPoint.makeInstance(Math.log(n));
	}
	return n.log();
    };

    // angle: scheme-number -> scheme-number
    var angle = function(n) {
	if (typeof(n) === 'number') {
	    if (n > 0)
		return 0;
	    else
		return FloatPoint.pi;
	}
	return n.angle();
    };

    // tan: scheme-number -> scheme-number
    var tan = function(n) {
	if (eqv(n, 0)) { return 0; }
	if (typeof(n) === 'number') {
	    return FloatPoint.makeInstance(Math.tan(n));
	}
	return n.tan();
    };

    // atan: scheme-number -> scheme-number
    var atan = function(n) {
	if (eqv(n, 0)) { return 0; }
	if (typeof(n) === 'number') {
	    return FloatPoint.makeInstance(Math.atan(n));
	}
	return n.atan();
    };

    // cos: scheme-number -> scheme-number
    var cos = function(n) {
	if (eqv(n, 0)) { return 1; }
	if (typeof(n) === 'number') {
	    return FloatPoint.makeInstance(Math.cos(n));
	}
	return n.cos();
    };

    // sin: scheme-number -> scheme-number
    var sin = function(n) {
	if (eqv(n, 0)) { return 0; }
	if (typeof(n) === 'number') {
	    return FloatPoint.makeInstance(Math.sin(n));
	}
	return n.sin();
    };

    // acos: scheme-number -> scheme-number
    var acos = function(n) {
	if (eqv(n, 1)) { return 0; }
	if (typeof(n) === 'number') {
	    return FloatPoint.makeInstance(Math.acos(n));
	}
	return n.acos();
    };

    // asin: scheme-number -> scheme-number
    var asin = function(n) {
        if (eqv(n, 0)) { return 0; }
	if (typeof(n) === 'number') {
	    return FloatPoint.makeInstance(Math.asin(n));
	}
	return n.asin();
    };

    // imaginaryPart: scheme-number -> scheme-number
    var imaginaryPart = function(n) {
	if (typeof(n) === 'number') {
	    return 0;
	}
	return n.imaginaryPart();
    };

    // realPart: scheme-number -> scheme-number
    var realPart = function(n) {
	if (typeof(n) === 'number') {
	    return n;
	}
	return n.realPart();
    };

    // round: scheme-number -> scheme-number
    var round = function(n) {
	if (typeof(n) === 'number') {
	    return n;
	}
	return n.round();
    };



    // sqr: scheme-number -> scheme-number
    var sqr = function(x) {
	return multiply(x, x);
    };


    // integerSqrt: scheme-number -> scheme-number
    var integerSqrt = function(x) {
	if (! isInteger(x)) {
	    throwRuntimeError('integer-sqrt: the argument ' + x.toString() +
			      " is not an integer.", x);
	}
	if (typeof (x) === 'number') {
	    if(x < 0) {
	        return Complex.makeInstance(0,
					    Math.floor(Math.sqrt(-x)))
	    } else {
		return Math.floor(Math.sqrt(x));
	    }
	}
	return x.integerSqrt();
    };


    // gcd: scheme-number [scheme-number ...] -> scheme-number
    var gcd = function(first, rest) {
	if (! isInteger(first)) {
	    throwRuntimeError('gcd: the argument ' + first.toString() +
			      " is not an integer.", first);
	}
	var a = abs(first), t, b;
	for(var i = 0; i < rest.length; i++) {
	    b = abs(rest[i]);	
	    if (! isInteger(b)) {
		throwRuntimeError('gcd: the argument ' + b.toString() +
				  " is not an integer.", b);
	    }
	    while (! _integerIsZero(b)) {
		t = a;
		a = b;
		b = _integerModulo(t, b);
	    }
	}
	return a;
    };

    // lcm: scheme-number [scheme-number ...] -> scheme-number
    var lcm = function(first, rest) {
	if (! isInteger(first)) {
	    throwRuntimeError('lcm: the argument ' + first.toString() +
			      " is not an integer.", first);
	}
	var result = abs(first);
	if (_integerIsZero(result)) { return 0; }
	for (var i = 0; i < rest.length; i++) {
	    if (! isInteger(rest[i])) {
		throwRuntimeError('lcm: the argument ' + rest[i].toString() +
				  " is not an integer.", rest[i]);
	    }
	    var divisor = _integerGcd(result, rest[i]);
	    if (_integerIsZero(divisor)) {
		return 0;
	    }
	    result = divide(multiply(result, rest[i]), divisor);
	}
	return result;
    };
    

    var quotient = function(x, y) {
 	if (! isInteger(x)) {
	    throwRuntimeError('quotient: the first argument ' + x.toString() +
			      " is not an integer.", x);
	}
	if (! isInteger(y)) {
	    throwRuntimeError('quotient: the second argument ' + y.toString() +
			      " is not an integer.", y);
	}
	return _integerQuotient(x, y);
    };

    
    var remainder = function(x, y) {
	if (! isInteger(x)) {
	    throwRuntimeError('remainder: the first argument ' + x.toString() +
			      " is not an integer.", x);
	}
	if (! isInteger(y)) {
	    throwRuntimeError('remainder: the second argument ' + y.toString() +
			      " is not an integer.", y);
	}
	return _integerRemainder(x, y);
    };


    // Implementation of the hyperbolic functions
    // http://en.wikipedia.org/wiki/Hyperbolic_cosine
    var cosh = function(x) {
	if (eqv(x, 0)) {
	    return FloatPoint.makeInstance(1.0);
	}
	return divide(add(exp(x), exp(negate(x))),
		      2);
    };
	
    var sinh = function(x) {
	return divide(subtract(exp(x), exp(negate(x))),
		      2);
    };


        
    var makeComplexPolar = function(r, theta) {
	// special case: if theta is zero, just return
	// the scalar.
	if (eqv(theta, 0)) {
	    return r;
	}
	return Complex.makeInstance(multiply(r, cos(theta)),
				    multiply(r, sin(theta)));
    };



    //////////////////////////////////////////////////////////////////////

    // Helpers


    // IsFinite: scheme-number -> boolean
    // Returns true if the scheme number is finite or not.
    var isSchemeNumberFinite = function(n) {
	if (typeof(n) === 'number') {
	    return isFinite(n);
	} else {
	    return n.isFinite();
	}
    };

    // isOverflow: javascript-number -> boolean
    // Returns true if we consider the number an overflow.
    var MIN_FIXNUM = -(9e15);
    var MAX_FIXNUM = (9e15);
    var isOverflow = function(n) {
	return (n < MIN_FIXNUM ||  MAX_FIXNUM < n);
    };


    // negate: scheme-number -> scheme-number
    // multiplies a number times -1.
    var negate = function(n) {
	if (typeof(n) === 'number') {
	    return -n;
	}
	return n.negate();
    };


    // halve: scheme-number -> scheme-number
    // Divide a number by 2.
    var halve = function(n) {
	return divide(n, 2);
    };


    // timesI: scheme-number scheme-number
    // multiplies a number times i.
    var timesI = function(x) {
	return multiply(x, plusI);
    };


    // fastExpt: computes n^k by squaring.
    // n^k = (n^2)^(k/2)
    // Assumes k is non-negative integer.
    var fastExpt = function(n, k) {
	var acc = 1;
	while (true) {
	    if (_integerIsZero(k)) {
		return acc;
	    }
	    if (equals(modulo(k, 2), 0)) {
		n = multiply(n, n);
		k = divide(k, 2);
	    } else {
		acc = multiply(acc, n);
		k = subtract(k, 1);
	    }
	}
    };



    //////////////////////////////////////////////////////////////////////
    //////////////////////////////////////////////////////////////////////
    //////////////////////////////////////////////////////////////////////
    //////////////////////////////////////////////////////////////////////
    //////////////////////////////////////////////////////////////////////


    // Integer operations
    // Integers are either represented as fixnums or as BigIntegers.

    // makeIntegerBinop: (fixnum fixnum -> X) (BigInteger BigInteger -> X) -> X
    // Helper to collect the common logic for coersing integer fixnums or bignums to a
    // common type before doing an operation.
    var makeIntegerBinop = function(onFixnums, onBignums, options) {
	options = options || {};
	return (function(m, n) {
	    if (m instanceof Rational) {
		m = numerator(m);
	    } else if (m instanceof Complex) {
		m = realPart(m);
	    }

	    if (n instanceof Rational) {
		n = numerator(n);
	    }else if (n instanceof Complex) {
		n = realPart(n);
	    }

	    if (typeof(m) === 'number' && typeof(n) === 'number') {
		var result = onFixnums(m, n);
		if (! isOverflow(result) ||
		    (options.ignoreOverflow)) {
		    return result;
		}
	    }
	    if (m instanceof FloatPoint || n instanceof FloatPoint) {
		if (options.doNotCoerseToFloating) {
		    return onFixnums(toFixnum(m), toFixnum(n));
		}
		else {
		    return FloatPoint.makeInstance(
			onFixnums(toFixnum(m), toFixnum(n)));
		}
	    }
	    if (typeof(m) === 'number') {
		m = makeBignum(m);
	    }
	    if (typeof(n) === 'number') {
		n = makeBignum(n);
	    }
	    return onBignums(m, n);
	});
    };


    var makeIntegerUnOp = function(onFixnums, onBignums, options) {
	options = options || {};
	return (function(m) {
	    if (m instanceof Rational) {
		m = numerator(m);
	    } else if (m instanceof Complex) {
		m = realPart(m);
	    }

	    if (typeof(m) === 'number') {
		var result = onFixnums(m);
		if (! isOverflow(result) ||
		    (options.ignoreOverflow)) {
		    return result;
		}
	    }
	    if (m instanceof FloatPoint) {
		return onFixnums(toFixnum(m));
	    }
	    if (typeof(m) === 'number') {
		m = makeBignum(m);
	    }
	    return onBignums(m);
	});
    };



    // _integerModulo: integer-scheme-number integer-scheme-number -> integer-scheme-number
    var _integerModulo = makeIntegerBinop(
	function(m, n) {
	    return m % n;
	},
	function(m, n) {
	    return bnMod.call(m, n);
	});


    // _integerGcd: integer-scheme-number integer-scheme-number -> integer-scheme-number
    var _integerGcd = makeIntegerBinop(
	function(a, b) {
	    var t;
	    while (b !== 0) {
		t = a;
		a = b;
		b = t % b;
	    }
	    return a;
	},
	function(m, n) {
	    return bnGCD.call(m, n);
	});


    // _integerIsZero: integer-scheme-number -> boolean
    // Returns true if the number is zero.
    var _integerIsZero = makeIntegerUnOp(
	function(n){
	    return n === 0;
	},
	function(n) {
	    return bnEquals.call(n, BigInteger.ZERO);
	}
    );


    // _integerIsOne: integer-scheme-number -> boolean
    var _integerIsOne = makeIntegerUnOp(
	function(n) {
	    return n === 1;
	},
	function(n) {
	    return bnEquals.call(n, BigInteger.ONE);
	});
    

 
    // _integerIsNegativeOne: integer-scheme-number -> boolean
    var _integerIsNegativeOne = makeIntegerUnOp(
	function(n) {
	    return n === -1;
	},
	function(n) {
	    return bnEquals.call(n, BigInteger.NEGATIVE_ONE);
	});
    


    // _integerAdd: integer-scheme-number integer-scheme-number -> integer-scheme-number
    var _integerAdd = makeIntegerBinop(
	function(m, n) {
	    return m + n;
	},
	function(m, n) {
	    return bnAdd.call(m, n);
	});

    // _integerSubtract: integer-scheme-number integer-scheme-number -> integer-scheme-number
    var _integerSubtract = makeIntegerBinop(
	function(m, n) {
	    return m - n;
	},
	function(m, n) {
	    return bnSubtract.call(m, n);
	});

    // _integerMultiply: integer-scheme-number integer-scheme-number -> integer-scheme-number
    var _integerMultiply = makeIntegerBinop(
	function(m, n) {
	    return m * n;
	},
	function(m, n) {
	    return bnMultiply.call(m, n);
	});

    //_integerQuotient: integer-scheme-number integer-scheme-number -> integer-scheme-number
    var _integerQuotient = makeIntegerBinop(
	function(m, n) {
	    return ((m - (m % n))/ n);
	},
	function(m, n) {
            return bnDivide.call(m, n);
	});

    var _integerRemainder = makeIntegerBinop(
	function(m, n) {
	    return m % n;
	},
	function(m, n) {
	    return bnRemainder.call(m, n);
	});


    // _integerDivideToFixnum: integer-scheme-number integer-scheme-number -> fixnum
    var _integerDivideToFixnum = makeIntegerBinop(
	function(m, n) {
	    return m / n;
	},
	function(m, n) {
	    return toFixnum(m) / toFixnum(n);
	},
	{ignoreOverflow: true,
	 doNotCoerseToFloating: true});


    // _integerEquals: integer-scheme-number integer-scheme-number -> boolean
    var _integerEquals = makeIntegerBinop(
	function(m, n) {
	    return m === n;
	},
	function(m, n) {
	    return bnEquals.call(m, n);
	},
	{doNotCoerseToFloating: true});

    // _integerGreaterThan: integer-scheme-number integer-scheme-number -> boolean
    var _integerGreaterThan = makeIntegerBinop(
	function(m, n) {
	    return m > n;
	},
	function(m, n) {
	    return bnCompareTo.call(m, n) > 0;
	},
	{doNotCoerseToFloating: true});

    // _integerLessThan: integer-scheme-number integer-scheme-number -> boolean
    var _integerLessThan = makeIntegerBinop(
	function(m, n) {
	    return m < n;
	},
	function(m, n) {
	    return bnCompareTo.call(m, n) < 0;
	},
	{doNotCoerseToFloating: true});

    // _integerGreaterThanOrEqual: integer-scheme-number integer-scheme-number -> boolean
    var _integerGreaterThanOrEqual = makeIntegerBinop(
	function(m, n) {
	    return m >= n;
	},
	function(m, n) {
	    return bnCompareTo.call(m, n) >= 0;
	},
	{doNotCoerseToFloating: true});

    // _integerLessThanOrEqual: integer-scheme-number integer-scheme-number -> boolean
    var _integerLessThanOrEqual = makeIntegerBinop(
	function(m, n) {
	    return m <= n;
	},
	function(m, n) {
	    return bnCompareTo.call(m, n) <= 0;
	},
	{doNotCoerseToFloating: true});



    //////////////////////////////////////////////////////////////////////
    // The boxed number types are expected to implement the following
    // interface.
    //
    // toString: -> string

    // level: number

    // liftTo: scheme-number -> scheme-number

    // isFinite: -> boolean

    // isInteger: -> boolean
    // Produce true if this number can be coersed into an integer.

    // isRational: -> boolean
    // Produce true if the number is rational.

    // isReal: -> boolean
    // Produce true if the number is real.

    // isExact: -> boolean
    // Produce true if the number is exact

    // toExact: -> scheme-number
    // Produce an exact number.

    // toFixnum: -> javascript-number
    // Produce a javascript number.

    // greaterThan: scheme-number -> boolean
    // Compare against instance of the same type.

    // greaterThanOrEqual: scheme-number -> boolean
    // Compare against instance of the same type.

    // lessThan: scheme-number -> boolean
    // Compare against instance of the same type.

    // lessThanOrEqual: scheme-number -> boolean
    // Compare against instance of the same type.

    // add: scheme-number -> scheme-number
    // Add with an instance of the same type.

    // subtract: scheme-number -> scheme-number
    // Subtract with an instance of the same type.

    // multiply: scheme-number -> scheme-number
    // Multiply with an instance of the same type.

    // divide: scheme-number -> scheme-number
    // Divide with an instance of the same type.

    // numerator: -> scheme-number
    // Return the numerator.

    // denominator: -> scheme-number
    // Return the denominator.

    // integerSqrt: -> scheme-number
    // Produce the integer square root.

    // sqrt: -> scheme-number
    // Produce the square root.

    // abs: -> scheme-number
    // Produce the absolute value.

    // floor: -> scheme-number
    // Produce the floor.

    // ceiling: -> scheme-number
    // Produce the ceiling.

    // conjugate: -> scheme-number
    // Produce the conjugate.

    // magnitude: -> scheme-number
    // Produce the magnitude.

    // log: -> scheme-number
    // Produce the log.

    // angle: -> scheme-number
    // Produce the angle.

    // atan: -> scheme-number
    // Produce the arc tangent.

    // cos: -> scheme-number
    // Produce the cosine.

    // sin: -> scheme-number
    // Produce the sine.

    // expt: scheme-number -> scheme-number
    // Produce the power to the input.

    // exp: -> scheme-number
    // Produce e raised to the given power.

    // acos: -> scheme-number
    // Produce the arc cosine.

    // asin: -> scheme-number
    // Produce the arc sine.

    // imaginaryPart: -> scheme-number
    // Produce the imaginary part

    // realPart: -> scheme-number
    // Produce the real part.

    // round: -> scheme-number
    // Round to the nearest integer.

    // equals: scheme-number -> boolean
    // Produce true if the given number of the same type is equal.



    //////////////////////////////////////////////////////////////////////

    // Rationals


    var Rational = function(n, d) {
	this.n = n;
	this.d = d;
    };


    Rational.prototype.toString = function() {
	if (_integerIsOne(this.d)) {
	    return this.n.toString() + "";
	} else {
	    return this.n.toString() + "/" + this.d.toString();
	}
    };


    Rational.prototype.level = 1;


    Rational.prototype.liftTo = function(target) {
	if (target.level === 2)
	    return new FloatPoint(
		_integerDivideToFixnum(this.n, this.d));
	if (target.level === 3)
	    return new Complex(this, 0);
	return throwRuntimeError("invalid level of Number", this, target);
    };

    Rational.prototype.isFinite = function() {
	return true;
    };

    Rational.prototype.equals = function(other) {
	return (other instanceof Rational &&
		_integerEquals(this.n, other.n) &&
		_integerEquals(this.d, other.d));
    };



    Rational.prototype.isInteger = function() {
	return _integerIsOne(this.d);
    };

    Rational.prototype.isRational = function() {
        return true;
    };

    Rational.prototype.isReal = function() {
	return true;
    };


    Rational.prototype.add = function(other) {
	return Rational.makeInstance(_integerAdd(_integerMultiply(this.n, other.d),
						 _integerMultiply(this.d, other.n)),
				     _integerMultiply(this.d, other.d));
    };

    Rational.prototype.subtract = function(other) {
	return Rational.makeInstance(_integerSubtract(_integerMultiply(this.n, other.d),
						      _integerMultiply(this.d, other.n)),
				     _integerMultiply(this.d, other.d));
    };

    Rational.prototype.negate = function() { 
	return Rational.makeInstance(-this.n, this.d) 
    };

    Rational.prototype.multiply = function(other) {
	return Rational.makeInstance(_integerMultiply(this.n, other.n),
				     _integerMultiply(this.d, other.d));
    };

    Rational.prototype.divide = function(other) {
	if (_integerIsZero(this.d) || _integerIsZero(other.n)) {
	    throwRuntimeError("/: division by zero", this, other);
	}
	return Rational.makeInstance(_integerMultiply(this.n, other.d),
				     _integerMultiply(this.d, other.n));
    };


    Rational.prototype.toExact = function() {
	return this;
    };

    Rational.prototype.toInexact = function() {
	return FloatPoint.makeInstance(this.toFixnum());
    };


    Rational.prototype.isExact = function() {
        return true;
    };

    Rational.prototype.isInexact = function() {
        return false;
    };


    Rational.prototype.toFixnum = function() {
	return _integerDivideToFixnum(this.n, this.d);
    };

    Rational.prototype.numerator = function() {
	return this.n;
    };

    Rational.prototype.denominator = function() {
	return this.d;
    };

    Rational.prototype.greaterThan = function(other) {
	return _integerGreaterThan(_integerMultiply(this.n, other.d),
				   _integerMultiply(this.d, other.n));
    };

    Rational.prototype.greaterThanOrEqual = function(other) {
	return _integerGreaterThanOrEqual(_integerMultiply(this.n, other.d),
					  _integerMultiply(this.d, other.n));
    };

    Rational.prototype.lessThan = function(other) {
	return _integerLessThan(_integerMultiply(this.n, other.d),
				_integerMultiply(this.d, other.n));
    };

    Rational.prototype.lessThanOrEqual = function(other) {
	return _integerLessThanOrEqual(_integerMultiply(this.n, other.d),
				       _integerMultiply(this.d, other.n));
    };

    Rational.prototype.integerSqrt = function() {
	var result = sqrt(this);
	if (isRational(result)) {
	    return toExact(floor(result));
	} else if (isReal(result)) {
	    return toExact(floor(result));
	} else {
	    return Complex.makeInstance(toExact(floor(realPart(result))),
					toExact(floor(imaginaryPart(result))));
	}
    };


    Rational.prototype.sqrt = function() {
	if (_integerGreaterThanOrEqual(this.n,  0)) {
	    var newN = sqrt(this.n);
	    var newD = sqrt(this.d);
	    if (equals(floor(newN), newN) &&
		equals(floor(newD), newD)) {
		return Rational.makeInstance(newN, newD);
	    } else {
		return FloatPoint.makeInstance(_integerDivideToFixnum(newN, newD));
	    }
	} else {
	    var newN = sqrt(negate(this.n));
	    var newD = sqrt(this.d);
	    if (equals(floor(newN), newN) &&
		equals(floor(newD), newD)) {
		return Complex.makeInstance(
		    0,
		    Rational.makeInstance(newN, newD));
	    } else {
		return Complex.makeInstance(
		    0,
		    FloatPoint.makeInstance(_integerDivideToFixnum(newN, newD)));
	    }
	}
    };

    Rational.prototype.abs = function() {
	return Rational.makeInstance(abs(this.n),
				     this.d);
    };


    Rational.prototype.floor = function() {
	var quotient = _integerQuotient(this.n, this.d);
	if (_integerLessThan(this.n, 0)) {
	    return subtract(quotient, 1);
	} else {
	    return quotient;
	}
    };


    Rational.prototype.ceiling = function() {
	var quotient = _integerQuotient(this.n, this.d);
	if (_integerLessThan(this.n, 0)) {
	    return quotient;
	} else {
	    return add(quotient, 1);
	}
    };

    Rational.prototype.conjugate = function() {
	return this;
    };

    Rational.prototype.magnitude = Rational.prototype.abs;

    Rational.prototype.log = function(){
	return FloatPoint.makeInstance(Math.log(this.n / this.d));
    };

    Rational.prototype.angle = function(){
	if (_integerIsZero(this.n))
	    return 0;
	if (_integerGreaterThan(this.n, 0))
	    return 0;
	else
	    return FloatPoint.pi;
    };

    Rational.prototype.tan = function(){
	return FloatPoint.makeInstance(Math.tan(_integerDivideToFixnum(this.n, this.d)));
    };

    Rational.prototype.atan = function(){
	return FloatPoint.makeInstance(Math.atan(_integerDivideToFixnum(this.n, this.d)));
    };

    Rational.prototype.cos = function(){
	return FloatPoint.makeInstance(Math.cos(_integerDivideToFixnum(this.n, this.d)));
    };

    Rational.prototype.sin = function(){
	return FloatPoint.makeInstance(Math.sin(_integerDivideToFixnum(this.n, this.d)));
    };

    Rational.prototype.expt = function(a){
	if (isExactInteger(a) && greaterThanOrEqual(a, 0)) {
	    return fastExpt(this, a);
	}
	return FloatPoint.makeInstance(Math.pow(_integerDivideToFixnum(this.n, this.d),
						_integerDivideToFixnum(a.n, a.d)));
    };

    Rational.prototype.exp = function(){
	return FloatPoint.makeInstance(Math.exp(_integerDivideToFixnum(this.n, this.d)));
    };

    Rational.prototype.acos = function(){
	return FloatPoint.makeInstance(Math.acos(_integerDivideToFixnum(this.n, this.d)));
    };

    Rational.prototype.asin = function(){
	return FloatPoint.makeInstance(Math.asin(_integerDivideToFixnum(this.n, this.d)));
    };

    Rational.prototype.imaginaryPart = function(){
	return 0;
    };

    Rational.prototype.realPart = function(){
	return this;
    };


    Rational.prototype.round = function() {
	// FIXME: not correct when values are bignums
	if (equals(this.d, 2)) {
	    // Round to even if it's a n/2
	    var v = _integerDivideToFixnum(this.n, this.d);
	    var fl = Math.floor(v);
	    var ce = Math.ceil(v);
	    if (_integerIsZero(fl % 2)) {
		return fl;
	    }
	    else {
		return ce;
	    }
	} else {
	    return Math.round(this.n / this.d);
	}
    };


    Rational.makeInstance = function(n, d) {
	if (n === undefined)
	    throwRuntimeError("n undefined", n, d);

	if (d === undefined) { d = 1; }

	if (_integerLessThan(d, 0)) {
	    n = negate(n);
	    d = negate(d);
	}

	var divisor = _integerGcd(abs(n), abs(d));
	n = _integerQuotient(n, divisor);
	d = _integerQuotient(d, divisor);

	// Optimization: if we can get around construction the rational
	// in favor of just returning n, do it:
	if (_integerIsOne(d) || _integerIsZero(n)) {
	    return n;
	}

	return new Rational(n, d);
    };



    // Floating Point numbers
    var FloatPoint = function(n) {
	this.n = n;
    };
    FloatPoint = FloatPoint;


    var NaN = new FloatPoint(Number.NaN);
    var inf = new FloatPoint(Number.POSITIVE_INFINITY);
    var neginf = new FloatPoint(Number.NEGATIVE_INFINITY);

    // We use these two constants to represent the floating-point coersion
    // of bignums that can't be represented with fidelity.
    var TOO_POSITIVE_TO_REPRESENT = new FloatPoint(Number.POSITIVE_INFINITY);
    var TOO_NEGATIVE_TO_REPRESENT = new FloatPoint(Number.NEGATIVE_INFINITY);

    // Negative zero is a distinguished value representing -0.0.
    // There should only be one instance for -0.0.
    var NEGATIVE_ZERO = new FloatPoint(-0.0);
    var INEXACT_ZERO = new FloatPoint(0.0);

    FloatPoint.pi = new FloatPoint(Math.PI);
    FloatPoint.e = new FloatPoint(Math.E);
    FloatPoint.nan = NaN;
    FloatPoint.inf = inf;
    FloatPoint.neginf = neginf;

    FloatPoint.makeInstance = function(n) {
	if (isNaN(n)) {
	    return FloatPoint.nan;
	} else if (n === Number.POSITIVE_INFINITY) {
	    return FloatPoint.inf;
	} else if (n === Number.NEGATIVE_INFINITY) {
	    return FloatPoint.neginf;
	} else if (n === 0) {
	    if ((1/n) === -Infinity) {
		return NEGATIVE_ZERO;
	    } else {
		return INEXACT_ZERO;
	    }
	}
	return new FloatPoint(n);
    };


    FloatPoint.prototype.isExact = function() {
	return false;
    };

    FloatPoint.prototype.isInexact = function() {
	return true;
    };


    FloatPoint.prototype.isFinite = function() {
	return (isFinite(this.n) ||
		this === TOO_POSITIVE_TO_REPRESENT ||
		this === TOO_NEGATIVE_TO_REPRESENT);
    };


    FloatPoint.prototype.toExact = function() {
	// The precision of ieee is about 16 decimal digits, which we use here.
	if (! isFinite(this.n) || isNaN(this.n)) {
	    throwRuntimeError("toExact: no exact representation for " + this, this);
	}

	var stringRep = this.n.toString();
	var match = stringRep.match(/^(.*)\.(.*)$/);
	if (match) {
	    var intPart = parseInt(match[1]);
	    var fracPart = parseInt(match[2]);
	    var tenToDecimalPlaces = Math.pow(10, match[2].length);
	    return Rational.makeInstance(Math.round(this.n * tenToDecimalPlaces),
					 tenToDecimalPlaces);
	}
	else {
	    return this.n;
	}
    };

    FloatPoint.prototype.toInexact = function() {
	return this;
    };

    FloatPoint.prototype.isInexact = function() {
	return true;
    };


    FloatPoint.prototype.level = 2;


    FloatPoint.prototype.liftTo = function(target) {
	if (target.level === 3)
	    return new Complex(this, 0);
	return throwRuntimeError("invalid level of Number", this, target);
    };

    FloatPoint.prototype.toString = function() {
	if (isNaN(this.n))
	    return "+nan.0";
	if (this.n === Number.POSITIVE_INFINITY)
	    return "+inf.0";
	if (this.n === Number.NEGATIVE_INFINITY)
	    return "-inf.0";
	if (this === NEGATIVE_ZERO)
	    return "-0.0";
	var partialResult = this.n.toString();
	if (! partialResult.match('\\.')) {
	    return partialResult + ".0";
	} else {
	    return partialResult;
	}
    };


    FloatPoint.prototype.equals = function(other, aUnionFind) {
	return ((other instanceof FloatPoint) &&
		((this.n === other.n)));
    };



    FloatPoint.prototype.isRational = function() {
        return this.isFinite();
    };

    FloatPoint.prototype.isInteger = function() {
	return this.isFinite() && this.n === Math.floor(this.n);
    };

    FloatPoint.prototype.isReal = function() {
	return true;
    };


    // sign: Number -> {-1, 0, 1}
    var sign = function(n) {
	if (lessThan(n, 0)) {
	    return -1;
	} else if (greaterThan(n, 0)) {
	    return 1;
	} else if (n === NEGATIVE_ZERO) {
	    return -1;
	} else {
	    return 0;
	}
    };


    FloatPoint.prototype.add = function(other) {
	if (this.isFinite() && other.isFinite()) {
	    return FloatPoint.makeInstance(this.n + other.n);
	} else {
	    if (isNaN(this.n) || isNaN(other.n)) {
		return NaN;
	    } else if (this.isFinite() && ! other.isFinite()) {
		return other;
	    } else if (!this.isFinite() && other.isFinite()) {
		return this;
	    } else {
		return ((sign(this) * sign(other) === 1) ?
			this : NaN);
	    };
	}
    };

    FloatPoint.prototype.subtract = function(other) {
	if (this.isFinite() && other.isFinite()) {
	    return FloatPoint.makeInstance(this.n - other.n);
	} else if (isNaN(this.n) || isNaN(other.n)) {
	    return NaN;
	} else if (! this.isFinite() && ! other.isFinite()) {
	    if (sign(this) === sign(other)) {
		return NaN;
	    } else {
		return this;
	    }
	} else if (this.isFinite()) {
	    return multiply(other, -1);
	} else {  // other.isFinite()
	    return this;
	}
    };


    FloatPoint.prototype.negate = function() {
	return FloatPoint.makeInstance(-this.n);
    };

    FloatPoint.prototype.multiply = function(other) {
	return FloatPoint.makeInstance(this.n * other.n);
    };

    FloatPoint.prototype.divide = function(other) {
        return FloatPoint.makeInstance(this.n / other.n);
    };


    FloatPoint.prototype.toFixnum = function() {
	return this.n;
    };

    FloatPoint.prototype.numerator = function() {
	var stringRep = this.n.toString();
	var match = stringRep.match(/^(.*)\.(.*)$/);
	if (match) {
	    var afterDecimal = parseInt(match[2]);
	    var factorToInt = Math.pow(10, match[2].length);
	    var extraFactor = _integerGcd(factorToInt, afterDecimal);
	    var multFactor = factorToInt / extraFactor;
	    return FloatPoint.makeInstance( Math.round(this.n * multFactor) );
	} else {
	    return this;
	}
    };

    FloatPoint.prototype.denominator = function() {
	var stringRep = this.n.toString();
	var match = stringRep.match(/^(.*)\.(.*)$/);
	if (match) {
	    var afterDecimal = parseInt(match[2]);
	    var factorToInt = Math.pow(10, match[2].length);
	    var extraFactor = _integerGcd(factorToInt, afterDecimal);
	    return FloatPoint.makeInstance( Math.round(factorToInt/extraFactor) );
	} else {
	    return FloatPoint.makeInstance(1);
	}
    };


    FloatPoint.prototype.floor = function() {
	return FloatPoint.makeInstance(Math.floor(this.n));
    };

    FloatPoint.prototype.ceiling = function() {
	return FloatPoint.makeInstance(Math.ceil(this.n));
    };


    FloatPoint.prototype.greaterThan = function(other) {
	return this.n > other.n;
    };

    FloatPoint.prototype.greaterThanOrEqual = function(other) {
	return this.n >= other.n;
    };

    FloatPoint.prototype.lessThan = function(other) {
	return this.n < other.n;
    };

    FloatPoint.prototype.lessThanOrEqual = function(other) {
	return this.n <= other.n;
    };


    FloatPoint.prototype.integerSqrt = function() {
	if (this === NEGATIVE_ZERO) { return this; }
	if (isInteger(this)) {
	    if(this.n >= 0) {
	        return FloatPoint.makeInstance(Math.floor(Math.sqrt(this.n)));
	    } else {
	        return Complex.makeInstance(
		    INEXACT_ZERO,
		    FloatPoint.makeInstance(Math.floor(Math.sqrt(-this.n))));
	    }
	} else {
	    throwRuntimeError("integerSqrt: can only be applied to an integer", this);
	}
    };

    FloatPoint.prototype.sqrt = function() {
	if (this.n < 0) {
	    var result = Complex.makeInstance(
		0,
		FloatPoint.makeInstance(Math.sqrt(-this.n)));
	    return result;
	} else {
	    return FloatPoint.makeInstance(Math.sqrt(this.n));
	}
    };

    FloatPoint.prototype.abs = function() {
	return FloatPoint.makeInstance(Math.abs(this.n));
    };



    FloatPoint.prototype.log = function(){
	if (this.n < 0)
	    return (new Complex(this, 0)).log();
	else
	    return FloatPoint.makeInstance(Math.log(this.n));
    };

    FloatPoint.prototype.angle = function(){
	if (0 === this.n)
	    return 0;
	if (this.n > 0)
	    return 0;
	else
	    return FloatPoint.pi;
    };

    FloatPoint.prototype.tan = function(){
	return FloatPoint.makeInstance(Math.tan(this.n));
    };

    FloatPoint.prototype.atan = function(){
	return FloatPoint.makeInstance(Math.atan(this.n));
    };

    FloatPoint.prototype.cos = function(){
	return FloatPoint.makeInstance(Math.cos(this.n));
    };

    FloatPoint.prototype.sin = function(){
	return FloatPoint.makeInstance(Math.sin(this.n));
    };

    FloatPoint.prototype.expt = function(a){
	if (this.n === 1) {
	    if (a.isFinite()) {
		return this;
	    } else if (isNaN(a.n)){
		return this;
	    } else {
		return this;
	    }
	} else {
	    return FloatPoint.makeInstance(Math.pow(this.n, a.n));
	}
    };

    FloatPoint.prototype.exp = function(){
	return FloatPoint.makeInstance(Math.exp(this.n));
    };

    FloatPoint.prototype.acos = function(){
	return FloatPoint.makeInstance(Math.acos(this.n));
    };

    FloatPoint.prototype.asin = function(){
	return FloatPoint.makeInstance(Math.asin(this.n));
    };

    FloatPoint.prototype.imaginaryPart = function(){
	return 0;
    };

    FloatPoint.prototype.realPart = function(){
	return this;
    };


    FloatPoint.prototype.round = function(){
	if (isFinite(this.n)) {
	    if (this === NEGATIVE_ZERO) {
		return this;
	    }
	    if (Math.abs(Math.floor(this.n) - this.n) === 0.5) {
		if (Math.floor(this.n) % 2 === 0)
		    return FloatPoint.makeInstance(Math.floor(this.n));
		return FloatPoint.makeInstance(Math.ceil(this.n));
	    } else {
		return FloatPoint.makeInstance(Math.round(this.n));
	    }
	} else {
	    return this;
	}
    };


    FloatPoint.prototype.conjugate = function() {
	return this;
    };

    FloatPoint.prototype.magnitude = FloatPoint.prototype.abs;



    //////////////////////////////////////////////////////////////////////
    // Complex numbers
    //////////////////////////////////////////////////////////////////////

    var Complex = function(r, i){
	this.r = r;
	this.i = i;
    };

    // Constructs a complex number from two basic number r and i.  r and i can
    // either be plt.type.Rational or plt.type.FloatPoint.
    Complex.makeInstance = function(r, i){
	if (i === undefined) { i = 0; }
	if (isExact(i) && isInteger(i) && _integerIsZero(i)) {
	    return r;
	}
	if (isInexact(r) || isInexact(i)) {
	    r = toInexact(r);
	    i = toInexact(i);
	}
	return new Complex(r, i);
    };

    Complex.prototype.toString = function() {
	var realPart = this.r.toString(), imagPart = this.i.toString();
	if (imagPart[0] === '-' || imagPart[0] === '+') {
	    return realPart + imagPart + 'i';
	} else {
	    return realPart + "+" + imagPart + 'i';
	}
    };


    Complex.prototype.isFinite = function() {
	return isSchemeNumberFinite(this.r) && isSchemeNumberFinite(this.i);
    };


    Complex.prototype.isRational = function() {
	return isRational(this.r) && eqv(this.i, 0);
    };

    Complex.prototype.isInteger = function() {
	return (isInteger(this.r) &&
		eqv(this.i, 0));
    };

    Complex.prototype.toExact = function() {
	return Complex.makeInstance( toExact(this.r), toExact(this.i) );
    };

    Complex.prototype.toInexact = function() {
	return Complex.makeInstance(toInexact(this.r),
				    toInexact(this.i));
    };


    Complex.prototype.isExact = function() {
        return isExact(this.r) && isExact(this.i);
    };


    Complex.prototype.isInexact = function() {
	return isInexact(this.r) || isInexact(this.i);
    };


    Complex.prototype.level = 3;


    Complex.prototype.liftTo = function(target){
	throwRuntimeError("Don't know how to lift Complex number", this, target);
    };

    Complex.prototype.equals = function(other) {
	var result = ((other instanceof Complex) &&
		      (equals(this.r, other.r)) &&
		      (equals(this.i, other.i)));
	return result;
    };



    Complex.prototype.greaterThan = function(other) {
	if (! this.isReal() || ! other.isReal()) {
	    throwRuntimeError(">: expects argument of type real number", this, other);
	}
	return greaterThan(this.r, other.r);
    };

    Complex.prototype.greaterThanOrEqual = function(other) {
	if (! this.isReal() || ! other.isReal()) {
	    throwRuntimeError(">=: expects argument of type real number", this, other);
	}
	return greaterThanOrEqual(this.r, other.r);
    };

    Complex.prototype.lessThan = function(other) {
	if (! this.isReal() || ! other.isReal()) {
	    throwRuntimeError("<: expects argument of type real number", this, other);
	}
	return lessThan(this.r, other.r);
    };

    Complex.prototype.lessThanOrEqual = function(other) {
	if (! this.isReal() || ! other.isReal()) {
	    throwRuntimeError("<=: expects argument of type real number", this, other);
	}
	return lessThanOrEqual(this.r, other.r);
    };


    Complex.prototype.abs = function(){
	if (!equals(this.i, 0).valueOf())
	    throwRuntimeError("abs: expects argument of type real number", this);
	return abs(this.r);
    };

    Complex.prototype.toFixnum = function(){
	if (!equals(this.i, 0).valueOf())
	    throwRuntimeError("toFixnum: expects argument of type real number", this);
	return toFixnum(this.r);
    };

    Complex.prototype.numerator = function() {
	if (!this.isReal())
	    throwRuntimeError("numerator: can only be applied to real number", this);
	return numerator(this.n);
    };


    Complex.prototype.denominator = function() {
	if (!this.isReal())
	    throwRuntimeError("floor: can only be applied to real number", this);
	return denominator(this.n);
    };

    Complex.prototype.add = function(other){
	return Complex.makeInstance(
	    add(this.r, other.r),
	    add(this.i, other.i));
    };

    Complex.prototype.subtract = function(other){
	return Complex.makeInstance(
	    subtract(this.r, other.r),
	    subtract(this.i, other.i));
    };

    Complex.prototype.negate = function() {
	return Complex.makeInstance(negate(this.r),
				    negate(this.i));
    };


    Complex.prototype.multiply = function(other){
	// If the other value is real, just do primitive division
	if (other.isReal()) {
	    return Complex.makeInstance(
		multiply(this.r, other.r),
		multiply(this.i, other.r));
	}
	var r = subtract(
	    multiply(this.r, other.r),
	    multiply(this.i, other.i));
	var i = add(
	    multiply(this.r, other.i),
	    multiply(this.i, other.r));
	return Complex.makeInstance(r, i);
    };





    Complex.prototype.divide = function(other){
	var a, b, c, d, r, x, y;
	// If the other value is real, just do primitive division
	if (other.isReal()) {
	    return Complex.makeInstance(
		divide(this.r, other.r),
		divide(this.i, other.r));
	}

	if (this.isInexact() || other.isInexact()) {
	    // http://portal.acm.org/citation.cfm?id=1039814
	    // We currently use Smith's method, though we should
	    // probably switch over to Priest's method.
	    a = this.r;
	    b = this.i;
	    c = other.r;
	    d = other.i;
	    if (lessThanOrEqual(abs(d), abs(c))) {
		r = divide(d, c);
		x = divide(add(a, multiply(b, r)),
			   add(c, multiply(d, r)));
		y = divide(subtract(b, multiply(a, r)),
			   add(c, multiply(d, r)));
	    } else {
		r = divide(c, d);
		x = divide(add(multiply(a, r), b),
			   add(multiply(c, r), d));
		y = divide(subtract(multiply(b, r), a),
			   add(multiply(c, r), d));
	    }
	    return Complex.makeInstance(x, y);
	} else {
	    var con = conjugate(other);
	    var up = multiply(this, con);

	    // Down is guaranteed to be real by this point.
	    var down = realPart(multiply(other, con));

	    var result = Complex.makeInstance(
		divide(realPart(up), down),
		divide(imaginaryPart(up), down));
	    return result;
	}
    };

    Complex.prototype.conjugate = function(){
	var result = Complex.makeInstance(
	    this.r,
	    subtract(0, this.i));

	return result;
    };

    Complex.prototype.magnitude = function(){
	var sum = add(
	    multiply(this.r, this.r),
	    multiply(this.i, this.i));
	return sqrt(sum);
    };

    Complex.prototype.isReal = function(){
	return eqv(this.i, 0);
    };

    Complex.prototype.integerSqrt = function() {
	if (isInteger(this)) {
	    return integerSqrt(this.r);
	} else {
	    throwRuntimeError("integerSqrt: can only be applied to an integer", this);
	}
    };

    Complex.prototype.sqrt = function(){
	if (this.isReal())
	    return sqrt(this.r);
	// http://en.wikipedia.org/wiki/Square_root#Square_roots_of_negative_and_complex_numbers
	var r_plus_x = add(this.magnitude(), this.r);

	var r = sqrt(halve(r_plus_x));

	var i = divide(this.i, sqrt(multiply(r_plus_x, 2)));


	return Complex.makeInstance(r, i);
    };

    Complex.prototype.log = function(){
	var m = this.magnitude();
	var theta = this.angle();
	var result = add(
	    log(m),
	    timesI(theta));
	return result;
    };

    Complex.prototype.angle = function(){
	if (this.isReal()) {
	    return angle(this.r);
	}
	if (equals(0, this.r)) {
	    var tmp = halve(FloatPoint.pi);
	    return greaterThan(this.i, 0) ?
		tmp : negate(tmp);
	} else {
	    var tmp = atan(divide(abs(this.i), abs(this.r)));
	    if (greaterThan(this.r, 0)) {
		return greaterThan(this.i, 0) ?
		    tmp : negate(tmp);
	    } else {
		return greaterThan(this.i, 0) ?
		    subtract(FloatPoint.pi, tmp) : subtract(tmp, FloatPoint.pi);
	    }
	}
    };

    var plusI = Complex.makeInstance(0, 1);
    var minusI = Complex.makeInstance(0, -1);


    Complex.prototype.tan = function() {
	return divide(this.sin(), this.cos());
    };

    Complex.prototype.atan = function(){
	if (equals(this, plusI) ||
	    equals(this, minusI)) {
	    return neginf;
	}
	return multiply(
	    plusI,
	    multiply(
		FloatPoint.makeInstance(0.5),
		log(divide(
		    add(plusI, this),
		    add(
			plusI,
			subtract(0, this))))));
    };

    Complex.prototype.cos = function(){
	if (this.isReal())
	    return cos(this.r);
	var iz = timesI(this);
	var iz_negate = negate(iz);

	return halve(add(exp(iz), exp(iz_negate)));
    };

    Complex.prototype.sin = function(){
	if (this.isReal())
	    return sin(this.r);
	var iz = timesI(this);
	var iz_negate = negate(iz);
	var z2 = Complex.makeInstance(0, 2);
	var exp_negate = subtract(exp(iz), exp(iz_negate));
	var result = divide(exp_negate, z2);
	return result;
    };


    Complex.prototype.expt = function(y){
	if (isExactInteger(y) && greaterThanOrEqual(y, 0)) {
	    return fastExpt(this, y);
	}
	var expo = multiply(y, this.log());
	return exp(expo);
    };

    Complex.prototype.exp = function(){
	var r = exp(this.r);
	var cos_a = cos(this.i);
	var sin_a = sin(this.i);

	return multiply(
	    r,
	    add(cos_a, timesI(sin_a)));
    };

    Complex.prototype.acos = function(){
	if (this.isReal())
	    return acos(this.r);
	var pi_half = halve(FloatPoint.pi);
	var iz = timesI(this);
	var root = sqrt(subtract(1, sqr(this)));
	var l = timesI(log(add(iz, root)));
	return add(pi_half, l);
    };

    Complex.prototype.asin = function(){
	if (this.isReal())
	    return asin(this.r);

	var oneNegateThisSq =
	    subtract(1, sqr(this));
	var sqrtOneNegateThisSq = sqrt(oneNegateThisSq);
	return multiply(2, atan(divide(this,
				       add(1, sqrtOneNegateThisSq))));
    };

    Complex.prototype.ceiling = function(){
	if (!this.isReal())
	    throwRuntimeError("ceiling: can only be applied to real number", this);
	return ceiling(this.r);
    };

    Complex.prototype.floor = function(){
	if (!this.isReal())
	    throwRuntimeError("floor: can only be applied to real number", this);
	return floor(this.r);
    };

    Complex.prototype.imaginaryPart = function(){
	return this.i;
    };

    Complex.prototype.realPart = function(){
	return this.r;
    };

    Complex.prototype.round = function(){
	if (!this.isReal())
	    throwRuntimeError("round: can only be applied to real number", this);
	return round(this.r);
    };



    var rationalRegexp = new RegExp("^([+-]?\\d+)/(\\d+)$");
    var complexRegexp = new RegExp("^([+-]?[\\d\\w/\\.]*)([+-])([\\d\\w/\\.]*)i$");
    var digitRegexp = new RegExp("^[+-]?\\d+$");
    var flonumRegexp = new RegExp("^([+-]?\\d*)\\.(\\d*)$");
    var scientificPattern = new RegExp("^([+-]?\\d*\\.?\\d*)[Ee](\\+?\\d+)$");

    // fromString: string -> (scheme-number | false)
    var fromString = function(x) {
	var aMatch = x.match(rationalRegexp);
	if (aMatch) {
	    return Rational.makeInstance(fromString(aMatch[1]),
					 fromString(aMatch[2]));
	}

	var cMatch = x.match(complexRegexp);
	if (cMatch) {
	    return Complex.makeInstance(fromString(cMatch[1] || "0"),
					fromString(cMatch[2] + (cMatch[3] || "1")));
	}

	// Floating point tests
	if (x === '+nan.0' || x === '-nan.0')
	    return FloatPoint.nan;
	if (x === '+inf.0')
	    return FloatPoint.inf;
	if (x === '-inf.0')
	    return FloatPoint.neginf;
	if (x === "-0.0") {
	    return NEGATIVE_ZERO;
	}
	if (x.match(flonumRegexp) ||  x.match(scientificPattern)) {
	    return FloatPoint.makeInstance(Number(x));
	}

	// Finally, integer tests.
	if (x.match(digitRegexp)) {
	    var n = Number(x);
	    if (isOverflow(n)) {
		return makeBignum(x);
	    } else {
		return n;
	    }
	} else {
	    return false;
	}
    };





    //////////////////////////////////////////////////////////////////////
    //////////////////////////////////////////////////////////////////////
    //////////////////////////////////////////////////////////////////////
    //////////////////////////////////////////////////////////////////////
    //////////////////////////////////////////////////////////////////////

    // The code below comes from Tom Wu's BigInteger implementation:

    // Copyright (c) 2005  Tom Wu
    // All Rights Reserved.
    // See "LICENSE" for details.

    // Basic JavaScript BN library - subset useful for RSA encryption.

    // Bits per digit
    var dbits;

    // JavaScript engine analysis
    var canary = 0xdeadbeefcafe;
    var j_lm = ((canary&0xffffff)==0xefcafe);

    // (public) Constructor
    function BigInteger(a,b,c) {
	if(a != null)
	    if("number" == typeof a) this.fromNumber(a,b,c);
	else if(b == null && "string" != typeof a) this.fromString(a,256);
	else this.fromString(a,b);
    }

    // return new, unset BigInteger
    function nbi() { return new BigInteger(null); }

    // am: Compute w_j += (x*this_i), propagate carries,
    // c is initial carry, returns final carry.
    // c < 3*dvalue, x < 2*dvalue, this_i < dvalue
    // We need to select the fastest one that works in this environment.

    // am1: use a single mult and divide to get the high bits,
    // max digit bits should be 26 because
    // max internal value = 2*dvalue^2-2*dvalue (< 2^53)
    function am1(i,x,w,j,c,n) {
	while(--n >= 0) {
	    var v = x*this[i++]+w[j]+c;
	    c = Math.floor(v/0x4000000);
	    w[j++] = v&0x3ffffff;
	}
	return c;
    }
    // am2 avoids a big mult-and-extract completely.
    // Max digit bits should be <= 30 because we do bitwise ops
    // on values up to 2*hdvalue^2-hdvalue-1 (< 2^31)
    function am2(i,x,w,j,c,n) {
	var xl = x&0x7fff, xh = x>>15;
	while(--n >= 0) {
	    var l = this[i]&0x7fff;
	    var h = this[i++]>>15;
	    var m = xh*l+h*xl;
	    l = xl*l+((m&0x7fff)<<15)+w[j]+(c&0x3fffffff);
	    c = (l>>>30)+(m>>>15)+xh*h+(c>>>30);
	    w[j++] = l&0x3fffffff;
	}
	return c;
    }
    // Alternately, set max digit bits to 28 since some
    // browsers slow down when dealing with 32-bit numbers.
    function am3(i,x,w,j,c,n) {
	var xl = x&0x3fff, xh = x>>14;
	while(--n >= 0) {
	    var l = this[i]&0x3fff;
	    var h = this[i++]>>14;
	    var m = xh*l+h*xl;
	    l = xl*l+((m&0x3fff)<<14)+w[j]+c;
	    c = (l>>28)+(m>>14)+xh*h;
	    w[j++] = l&0xfffffff;
	}
	return c;
    }
    if(j_lm && (typeof(navigator) !== 'undefined' && navigator.appName == "Microsoft Internet Explorer")) {
	BigInteger.prototype.am = am2;
	dbits = 30;
    }
    else if(j_lm && (typeof(navigator) !== 'undefined' && navigator.appName != "Netscape")) {
	BigInteger.prototype.am = am1;
	dbits = 26;
    }
    else { // Mozilla/Netscape seems to prefer am3
	BigInteger.prototype.am = am3;
	dbits = 28;
    }

    BigInteger.prototype.DB = dbits;
    BigInteger.prototype.DM = ((1<<dbits)-1);
    BigInteger.prototype.DV = (1<<dbits);

    var BI_FP = 52;
    BigInteger.prototype.FV = Math.pow(2,BI_FP);
    BigInteger.prototype.F1 = BI_FP-dbits;
    BigInteger.prototype.F2 = 2*dbits-BI_FP;

    // Digit conversions
    var BI_RM = "0123456789abcdefghijklmnopqrstuvwxyz";
    var BI_RC = [];
    var rr,vv;
    rr = "0".charCodeAt(0);
    for(vv = 0; vv <= 9; ++vv) BI_RC[rr++] = vv;
    rr = "a".charCodeAt(0);
    for(vv = 10; vv < 36; ++vv) BI_RC[rr++] = vv;
    rr = "A".charCodeAt(0);
    for(vv = 10; vv < 36; ++vv) BI_RC[rr++] = vv;

    function int2char(n) { return BI_RM.charAt(n); }
    function intAt(s,i) {
	var c = BI_RC[s.charCodeAt(i)];
	return (c==null)?-1:c;
    }

    // (protected) copy this to r
    function bnpCopyTo(r) {
	for(var i = this.t-1; i >= 0; --i) r[i] = this[i];
	r.t = this.t;
	r.s = this.s;
    }

    // (protected) set from integer value x, -DV <= x < DV
    function bnpFromInt(x) {
	this.t = 1;
	this.s = (x<0)?-1:0;
	if(x > 0) this[0] = x;
	else if(x < -1) this[0] = x+DV;
	else this.t = 0;
    }

    // return bigint initialized to value
    function nbv(i) { var r = nbi(); r.fromInt(i); return r; }

    // (protected) set from string and radix
    function bnpFromString(s,b) {
	var k;
	if(b == 16) k = 4;
	else if(b == 8) k = 3;
	else if(b == 256) k = 8; // byte array
	else if(b == 2) k = 1;
	else if(b == 32) k = 5;
	else if(b == 4) k = 2;
	else { this.fromRadix(s,b); return; }
	this.t = 0;
	this.s = 0;
	var i = s.length, mi = false, sh = 0;
	while(--i >= 0) {
	    var x = (k==8)?s[i]&0xff:intAt(s,i);
	    if(x < 0) {
		if(s.charAt(i) == "-") mi = true;
		continue;
	    }
	    mi = false;
	    if(sh == 0)
		this[this.t++] = x;
	    else if(sh+k > this.DB) {
		this[this.t-1] |= (x&((1<<(this.DB-sh))-1))<<sh;
		this[this.t++] = (x>>(this.DB-sh));
	    }
	    else
		this[this.t-1] |= x<<sh;
	    sh += k;
	    if(sh >= this.DB) sh -= this.DB;
	}
	if(k == 8 && (s[0]&0x80) != 0) {
	    this.s = -1;
	    if(sh > 0) this[this.t-1] |= ((1<<(this.DB-sh))-1)<<sh;
	}
	this.clamp();
	if(mi) BigInteger.ZERO.subTo(this,this);
    }

    // (protected) clamp off excess high words
    function bnpClamp() {
	var c = this.s&this.DM;
	while(this.t > 0 && this[this.t-1] == c) --this.t;
    }

    // (public) return string representation in given radix
    function bnToString(b) {
	if(this.s < 0) return "-"+this.negate().toString(b);
	var k;
	if(b == 16) k = 4;
	else if(b == 8) k = 3;
	else if(b == 2) k = 1;
	else if(b == 32) k = 5;
	else if(b == 4) k = 2;
	else return this.toRadix(b);
	var km = (1<<k)-1, d, m = false, r = [], i = this.t;
	var p = this.DB-(i*this.DB)%k;
	if(i-- > 0) {
	    if(p < this.DB && (d = this[i]>>p) > 0) { m = true; r.push(int2char(d)); }
	    while(i >= 0) {
		if(p < k) {
		    d = (this[i]&((1<<p)-1))<<(k-p);
		    d |= this[--i]>>(p+=this.DB-k);
		}
		else {
		    d = (this[i]>>(p-=k))&km;
		    if(p <= 0) { p += this.DB; --i; }
		}
		if(d > 0) m = true;
		if(m) r.push(int2char(d));
	    }
	}
	return m?r.join(""):"0";
    }

    // (public) -this
    function bnNegate() { var r = nbi(); BigInteger.ZERO.subTo(this,r); return r; }

    // (public) |this|
    function bnAbs() { return (this.s<0)?this.negate():this; }

    // (public) return + if this > a, - if this < a, 0 if equal
    function bnCompareTo(a) {
	var r = this.s-a.s;
	if(r != 0) return r;
	var i = this.t;
	if ( this.s < 0 ) {
		r = a.t - i;
	}
	else {
		r = i - a.t;
	}
	if(r != 0) return r;
	while(--i >= 0) if((r=this[i]-a[i]) != 0) return r;
	return 0;
    }

    // returns bit length of the integer x
    function nbits(x) {
	var r = 1, t;
	if((t=x>>>16) != 0) { x = t; r += 16; }
	if((t=x>>8) != 0) { x = t; r += 8; }
	if((t=x>>4) != 0) { x = t; r += 4; }
	if((t=x>>2) != 0) { x = t; r += 2; }
	if((t=x>>1) != 0) { x = t; r += 1; }
	return r;
    }

    // (public) return the number of bits in "this"
    function bnBitLength() {
	if(this.t <= 0) return 0;
	return this.DB*(this.t-1)+nbits(this[this.t-1]^(this.s&this.DM));
    }

    // (protected) r = this << n*DB
    function bnpDLShiftTo(n,r) {
	var i;
	for(i = this.t-1; i >= 0; --i) r[i+n] = this[i];
	for(i = n-1; i >= 0; --i) r[i] = 0;
	r.t = this.t+n;
	r.s = this.s;
    }

    // (protected) r = this >> n*DB
    function bnpDRShiftTo(n,r) {
	for(var i = n; i < this.t; ++i) r[i-n] = this[i];
	r.t = Math.max(this.t-n,0);
	r.s = this.s;
    }

    // (protected) r = this << n
    function bnpLShiftTo(n,r) {
	var bs = n%this.DB;
	var cbs = this.DB-bs;
	var bm = (1<<cbs)-1;
	var ds = Math.floor(n/this.DB), c = (this.s<<bs)&this.DM, i;
	for(i = this.t-1; i >= 0; --i) {
	    r[i+ds+1] = (this[i]>>cbs)|c;
	    c = (this[i]&bm)<<bs;
	}
	for(i = ds-1; i >= 0; --i) r[i] = 0;
	r[ds] = c;
	r.t = this.t+ds+1;
	r.s = this.s;
	r.clamp();
    }

    // (protected) r = this >> n
    function bnpRShiftTo(n,r) {
	r.s = this.s;
	var ds = Math.floor(n/this.DB);
	if(ds >= this.t) { r.t = 0; return; }
	var bs = n%this.DB;
	var cbs = this.DB-bs;
	var bm = (1<<bs)-1;
	r[0] = this[ds]>>bs;
	for(var i = ds+1; i < this.t; ++i) {
	    r[i-ds-1] |= (this[i]&bm)<<cbs;
	    r[i-ds] = this[i]>>bs;
	}
	if(bs > 0) r[this.t-ds-1] |= (this.s&bm)<<cbs;
	r.t = this.t-ds;
	r.clamp();
    }

    // (protected) r = this - a
    function bnpSubTo(a,r) {
	var i = 0, c = 0, m = Math.min(a.t,this.t);
	while(i < m) {
	    c += this[i]-a[i];
	    r[i++] = c&this.DM;
	    c >>= this.DB;
	}
	if(a.t < this.t) {
	    c -= a.s;
	    while(i < this.t) {
		c += this[i];
		r[i++] = c&this.DM;
		c >>= this.DB;
	    }
	    c += this.s;
	}
	else {
	    c += this.s;
	    while(i < a.t) {
		c -= a[i];
		r[i++] = c&this.DM;
		c >>= this.DB;
	    }
	    c -= a.s;
	}
	r.s = (c<0)?-1:0;
	if(c < -1) r[i++] = this.DV+c;
	else if(c > 0) r[i++] = c;
	r.t = i;
	r.clamp();
    }

    // (protected) r = this * a, r != this,a (HAC 14.12)
    // "this" should be the larger one if appropriate.
    function bnpMultiplyTo(a,r) {
	var x = this.abs(), y = a.abs();
	var i = x.t;
	r.t = i+y.t;
	while(--i >= 0) r[i] = 0;
	for(i = 0; i < y.t; ++i) r[i+x.t] = x.am(0,y[i],r,i,0,x.t);
	r.s = 0;
	r.clamp();
	if(this.s != a.s) BigInteger.ZERO.subTo(r,r);
    }

    // (protected) r = this^2, r != this (HAC 14.16)
    function bnpSquareTo(r) {
	var x = this.abs();
	var i = r.t = 2*x.t;
	while(--i >= 0) r[i] = 0;
	for(i = 0; i < x.t-1; ++i) {
	    var c = x.am(i,x[i],r,2*i,0,1);
	    if((r[i+x.t]+=x.am(i+1,2*x[i],r,2*i+1,c,x.t-i-1)) >= x.DV) {
		r[i+x.t] -= x.DV;
		r[i+x.t+1] = 1;
	    }
	}
	if(r.t > 0) r[r.t-1] += x.am(i,x[i],r,2*i,0,1);
	r.s = 0;
	r.clamp();
    }


    // (protected) divide this by m, quotient and remainder to q, r (HAC 14.20)
    // r != q, this != m.  q or r may be null.
    function bnpDivRemTo(m,q,r) {
	var pm = m.abs();
	if(pm.t <= 0) return;
	var pt = this.abs();
	if(pt.t < pm.t) {
	    if(q != null) q.fromInt(0);
	    if(r != null) this.copyTo(r);
	    return;
	}
	if(r == null) r = nbi();
	var y = nbi(), ts = this.s, ms = m.s;
	var nsh = this.DB-nbits(pm[pm.t-1]);	// normalize modulus
	if(nsh > 0) { pm.lShiftTo(nsh,y); pt.lShiftTo(nsh,r); }
	else { pm.copyTo(y); pt.copyTo(r); }
	var ys = y.t;
	var y0 = y[ys-1];
	if(y0 == 0) return;
	var yt = y0*(1<<this.F1)+((ys>1)?y[ys-2]>>this.F2:0);
	var d1 = this.FV/yt, d2 = (1<<this.F1)/yt, e = 1<<this.F2;
	var i = r.t, j = i-ys, t = (q==null)?nbi():q;
	y.dlShiftTo(j,t);
	if(r.compareTo(t) >= 0) {
	    r[r.t++] = 1;
	    r.subTo(t,r);
	}
	BigInteger.ONE.dlShiftTo(ys,t);
	t.subTo(y,y);	// "negative" y so we can replace sub with am later
	while(y.t < ys) y[y.t++] = 0;
	while(--j >= 0) {
	    // Estimate quotient digit
	    var qd = (r[--i]==y0)?this.DM:Math.floor(r[i]*d1+(r[i-1]+e)*d2);
	    if((r[i]+=y.am(0,qd,r,j,0,ys)) < qd) {	// Try it out
		y.dlShiftTo(j,t);
		r.subTo(t,r);
		while(r[i] < --qd) r.subTo(t,r);
	    }
	}
	if(q != null) {
	    r.drShiftTo(ys,q);
	    if(ts != ms) BigInteger.ZERO.subTo(q,q);
	}
	r.t = ys;
	r.clamp();
	if(nsh > 0) r.rShiftTo(nsh,r);	// Denormalize remainder
	if(ts < 0) BigInteger.ZERO.subTo(r,r);
    }

    // (public) this mod a
    function bnMod(a) {
	var r = nbi();
	this.abs().divRemTo(a,null,r);
	if(this.s < 0 && r.compareTo(BigInteger.ZERO) > 0) a.subTo(r,r);
	return r;
    }

    // Modular reduction using "classic" algorithm
    function Classic(m) { this.m = m; }
    function cConvert(x) {
	if(x.s < 0 || x.compareTo(this.m) >= 0) return x.mod(this.m);
	else return x;
    }
    function cRevert(x) { return x; }
    function cReduce(x) { x.divRemTo(this.m,null,x); }
    function cMulTo(x,y,r) { x.multiplyTo(y,r); this.reduce(r); }
    function cSqrTo(x,r) { x.squareTo(r); this.reduce(r); }

    Classic.prototype.convert = cConvert;
    Classic.prototype.revert = cRevert;
    Classic.prototype.reduce = cReduce;
    Classic.prototype.mulTo = cMulTo;
    Classic.prototype.sqrTo = cSqrTo;

    // (protected) return "-1/this % 2^DB"; useful for Mont. reduction
    // justification:
    //         xy == 1 (mod m)
    //         xy =  1+km
    //   xy(2-xy) = (1+km)(1-km)
    // x[y(2-xy)] = 1-k^2m^2
    // x[y(2-xy)] == 1 (mod m^2)
    // if y is 1/x mod m, then y(2-xy) is 1/x mod m^2
    // should reduce x and y(2-xy) by m^2 at each step to keep size bounded.
    // JS multiply "overflows" differently from C/C++, so care is needed here.
    function bnpInvDigit() {
	if(this.t < 1) return 0;
	var x = this[0];
	if((x&1) == 0) return 0;
	var y = x&3;		// y == 1/x mod 2^2
	y = (y*(2-(x&0xf)*y))&0xf;	// y == 1/x mod 2^4
	y = (y*(2-(x&0xff)*y))&0xff;	// y == 1/x mod 2^8
	y = (y*(2-(((x&0xffff)*y)&0xffff)))&0xffff;	// y == 1/x mod 2^16
	// last step - calculate inverse mod DV directly;
	// assumes 16 < DB <= 32 and assumes ability to handle 48-bit ints
	y = (y*(2-x*y%this.DV))%this.DV;		// y == 1/x mod 2^dbits
	// we really want the negative inverse, and -DV < y < DV
	return (y>0)?this.DV-y:-y;
    }

    // Montgomery reduction
    function Montgomery(m) {
	this.m = m;
	this.mp = m.invDigit();
	this.mpl = this.mp&0x7fff;
	this.mph = this.mp>>15;
	this.um = (1<<(m.DB-15))-1;
	this.mt2 = 2*m.t;
    }

    // xR mod m
    function montConvert(x) {
	var r = nbi();
	x.abs().dlShiftTo(this.m.t,r);
	r.divRemTo(this.m,null,r);
	if(x.s < 0 && r.compareTo(BigInteger.ZERO) > 0) this.m.subTo(r,r);
	return r;
    }

    // x/R mod m
    function montRevert(x) {
	var r = nbi();
	x.copyTo(r);
	this.reduce(r);
	return r;
    }

    // x = x/R mod m (HAC 14.32)
    function montReduce(x) {
	while(x.t <= this.mt2)	// pad x so am has enough room later
	    x[x.t++] = 0;
	for(var i = 0; i < this.m.t; ++i) {
	    // faster way of calculating u0 = x[i]*mp mod DV
	    var j = x[i]&0x7fff;
	    var u0 = (j*this.mpl+(((j*this.mph+(x[i]>>15)*this.mpl)&this.um)<<15))&x.DM;
	    // use am to combine the multiply-shift-add into one call
	    j = i+this.m.t;
	    x[j] += this.m.am(0,u0,x,i,0,this.m.t);
	    // propagate carry
	    while(x[j] >= x.DV) { x[j] -= x.DV; x[++j]++; }
	}
	x.clamp();
	x.drShiftTo(this.m.t,x);
	if(x.compareTo(this.m) >= 0) x.subTo(this.m,x);
    }

    // r = "x^2/R mod m"; x != r
    function montSqrTo(x,r) { x.squareTo(r); this.reduce(r); }

    // r = "xy/R mod m"; x,y != r
    function montMulTo(x,y,r) { x.multiplyTo(y,r); this.reduce(r); }

    Montgomery.prototype.convert = montConvert;
    Montgomery.prototype.revert = montRevert;
    Montgomery.prototype.reduce = montReduce;
    Montgomery.prototype.mulTo = montMulTo;
    Montgomery.prototype.sqrTo = montSqrTo;

    // (protected) true iff this is even
    function bnpIsEven() { return ((this.t>0)?(this[0]&1):this.s) == 0; }

    // (protected) this^e, e < 2^32, doing sqr and mul with "r" (HAC 14.79)
    function bnpExp(e,z) {
	    if(e > 0xffffffff || e < 1) return BigInteger.ONE;
	    var r = nbi(), r2 = nbi(), g = z.convert(this), i = nbits(e)-1;
	    g.copyTo(r);
	    while(--i >= 0) {
	        z.sqrTo(r,r2);
	        if((e&(1<<i)) > 0) z.mulTo(r2,g,r);
	        else { var t = r; r = r2; r2 = t; }
	    }
	    return z.revert(r);
    }

    // (public) this^e % m, 0 <= e < 2^32
    function bnModPowInt(e,m) {
	var z;
	if(e < 256 || m.isEven()) z = new Classic(m); else z = new Montgomery(m);
	return this.exp(e,z);
    }

    // protected
    BigInteger.prototype.copyTo = bnpCopyTo;
    BigInteger.prototype.fromInt = bnpFromInt;
    BigInteger.prototype.fromString = bnpFromString;
    BigInteger.prototype.clamp = bnpClamp;
    BigInteger.prototype.dlShiftTo = bnpDLShiftTo;
    BigInteger.prototype.drShiftTo = bnpDRShiftTo;
    BigInteger.prototype.lShiftTo = bnpLShiftTo;
    BigInteger.prototype.rShiftTo = bnpRShiftTo;
    BigInteger.prototype.subTo = bnpSubTo;
    BigInteger.prototype.multiplyTo = bnpMultiplyTo;
    BigInteger.prototype.squareTo = bnpSquareTo;
    BigInteger.prototype.divRemTo = bnpDivRemTo;
    BigInteger.prototype.invDigit = bnpInvDigit;
    BigInteger.prototype.isEven = bnpIsEven;
    BigInteger.prototype.exp = bnpExp;

    // public
    BigInteger.prototype.toString = bnToString;
    BigInteger.prototype.negate = bnNegate;
    BigInteger.prototype.abs = bnAbs;
    BigInteger.prototype.compareTo = bnCompareTo;
    BigInteger.prototype.bitLength = bnBitLength;
    BigInteger.prototype.mod = bnMod;
    BigInteger.prototype.modPowInt = bnModPowInt;

    // "constants"
    BigInteger.ZERO = nbv(0);
    BigInteger.ONE = nbv(1);

    // Copyright (c) 2005-2009  Tom Wu
    // All Rights Reserved.
    // See "LICENSE" for details.

    // Extended JavaScript BN functions, required for RSA private ops.

    // Version 1.1: new BigInteger("0", 10) returns "proper" zero

    // (public)
    function bnClone() { var r = nbi(); this.copyTo(r); return r; }

    // (public) return value as integer
    function bnIntValue() {
	if(this.s < 0) {
	    if(this.t == 1) return this[0]-this.DV;
	    else if(this.t == 0) return -1;
	}
	else if(this.t == 1) return this[0];
	else if(this.t == 0) return 0;
	// assumes 16 < DB < 32
	return ((this[1]&((1<<(32-this.DB))-1))<<this.DB)|this[0];
    }

    // (public) return value as byte
    function bnByteValue() { return (this.t==0)?this.s:(this[0]<<24)>>24; }

    // (public) return value as short (assumes DB>=16)
    function bnShortValue() { return (this.t==0)?this.s:(this[0]<<16)>>16; }

    // (protected) return x s.t. r^x < DV
    function bnpChunkSize(r) { return Math.floor(Math.LN2*this.DB/Math.log(r)); }

    // (public) 0 if this == 0, 1 if this > 0
    function bnSigNum() {
	if(this.s < 0) return -1;
	else if(this.t <= 0 || (this.t == 1 && this[0] <= 0)) return 0;
	else return 1;
    }

    // (protected) convert to radix string
    function bnpToRadix(b) {
	if(b == null) b = 10;
	if(this.signum() == 0 || b < 2 || b > 36) return "0";
	var cs = this.chunkSize(b);
	var a = Math.pow(b,cs);
	var d = nbv(a), y = nbi(), z = nbi(), r = "";
	this.divRemTo(d,y,z);
	while(y.signum() > 0) {
	    r = (a+z.intValue()).toString(b).substr(1) + r;
	    y.divRemTo(d,y,z);
	}
	return z.intValue().toString(b) + r;
    }

    // (protected) convert from radix string
    function bnpFromRadix(s,b) {
	this.fromInt(0);
	if(b == null) b = 10;
	var cs = this.chunkSize(b);
	var d = Math.pow(b,cs), mi = false, j = 0, w = 0;
	for(var i = 0; i < s.length; ++i) {
	    var x = intAt(s,i);
	    if(x < 0) {
		if(s.charAt(i) == "-" && this.signum() == 0) mi = true;
		continue;
	    }
	    w = b*w+x;
	    if(++j >= cs) {
		this.dMultiply(d);
		this.dAddOffset(w,0);
		j = 0;
		w = 0;
	    }
	}
	if(j > 0) {
	    this.dMultiply(Math.pow(b,j));
	    this.dAddOffset(w,0);
	}
	if(mi) BigInteger.ZERO.subTo(this,this);
    }

    // (protected) alternate constructor
    function bnpFromNumber(a,b,c) {
	if("number" == typeof b) {
	    // new BigInteger(int,int,RNG)
	    if(a < 2) this.fromInt(1);
	    else {
		this.fromNumber(a,c);
		if(!this.testBit(a-1))	// force MSB set
		    this.bitwiseTo(BigInteger.ONE.shiftLeft(a-1),op_or,this);
		if(this.isEven()) this.dAddOffset(1,0); // force odd
		while(!this.isProbablePrime(b)) {
		    this.dAddOffset(2,0);
		    if(this.bitLength() > a) this.subTo(BigInteger.ONE.shiftLeft(a-1),this);
		}
	    }
	}
	else {
	    // new BigInteger(int,RNG)
	    var x = [], t = a&7;
	    x.length = (a>>3)+1;
	    b.nextBytes(x);
	    if(t > 0) x[0] &= ((1<<t)-1); else x[0] = 0;
	    this.fromString(x,256);
	}
    }

    // (public) convert to bigendian byte array
    function bnToByteArray() {
	var i = this.t, r = [];
	r[0] = this.s;
	var p = this.DB-(i*this.DB)%8, d, k = 0;
	if(i-- > 0) {
	    if(p < this.DB && (d = this[i]>>p) != (this.s&this.DM)>>p)
		r[k++] = d|(this.s<<(this.DB-p));
	    while(i >= 0) {
		if(p < 8) {
		    d = (this[i]&((1<<p)-1))<<(8-p);
		    d |= this[--i]>>(p+=this.DB-8);
		}
		else {
		    d = (this[i]>>(p-=8))&0xff;
		    if(p <= 0) { p += this.DB; --i; }
		}
		if((d&0x80) != 0) d |= -256;
		if(k == 0 && (this.s&0x80) != (d&0x80)) ++k;
		if(k > 0 || d != this.s) r[k++] = d;
	    }
	}
	return r;
    }

    function bnEquals(a) { return(this.compareTo(a)==0); }
    function bnMin(a) { return(this.compareTo(a)<0)?this:a; }
    function bnMax(a) { return(this.compareTo(a)>0)?this:a; }

    // (protected) r = this op a (bitwise)
    function bnpBitwiseTo(a,op,r) {
	var i, f, m = Math.min(a.t,this.t);
	for(i = 0; i < m; ++i) r[i] = op(this[i],a[i]);
	if(a.t < this.t) {
	    f = a.s&this.DM;
	    for(i = m; i < this.t; ++i) r[i] = op(this[i],f);
	    r.t = this.t;
	}
	else {
	    f = this.s&this.DM;
	    for(i = m; i < a.t; ++i) r[i] = op(f,a[i]);
	    r.t = a.t;
	}
	r.s = op(this.s,a.s);
	r.clamp();
    }

    // (public) this & a
    function op_and(x,y) { return x&y; }
    function bnAnd(a) { var r = nbi(); this.bitwiseTo(a,op_and,r); return r; }

    // (public) this | a
    function op_or(x,y) { return x|y; }
    function bnOr(a) { var r = nbi(); this.bitwiseTo(a,op_or,r); return r; }

    // (public) this ^ a
    function op_xor(x,y) { return x^y; }
    function bnXor(a) { var r = nbi(); this.bitwiseTo(a,op_xor,r); return r; }

    // (public) this & ~a
    function op_andnot(x,y) { return x&~y; }
    function bnAndNot(a) { var r = nbi(); this.bitwiseTo(a,op_andnot,r); return r; }

    // (public) ~this
    function bnNot() {
	var r = nbi();
	for(var i = 0; i < this.t; ++i) r[i] = this.DM&~this[i];
	r.t = this.t;
	r.s = ~this.s;
	return r;
    }

    // (public) this << n
    function bnShiftLeft(n) {
	var r = nbi();
	if(n < 0) this.rShiftTo(-n,r); else this.lShiftTo(n,r);
	return r;
    }

    // (public) this >> n
    function bnShiftRight(n) {
	var r = nbi();
	if(n < 0) this.lShiftTo(-n,r); else this.rShiftTo(n,r);
	return r;
    }

    // return index of lowest 1-bit in x, x < 2^31
    function lbit(x) {
	if(x == 0) return -1;
	var r = 0;
	if((x&0xffff) == 0) { x >>= 16; r += 16; }
	if((x&0xff) == 0) { x >>= 8; r += 8; }
	if((x&0xf) == 0) { x >>= 4; r += 4; }
	if((x&3) == 0) { x >>= 2; r += 2; }
	if((x&1) == 0) ++r;
	return r;
    }

    // (public) returns index of lowest 1-bit (or -1 if none)
    function bnGetLowestSetBit() {
	for(var i = 0; i < this.t; ++i)
	    if(this[i] != 0) return i*this.DB+lbit(this[i]);
	if(this.s < 0) return this.t*this.DB;
	return -1;
    }

    // return number of 1 bits in x
    function cbit(x) {
	var r = 0;
	while(x != 0) { x &= x-1; ++r; }
	return r;
    }

    // (public) return number of set bits
    function bnBitCount() {
	var r = 0, x = this.s&this.DM;
	for(var i = 0; i < this.t; ++i) r += cbit(this[i]^x);
	return r;
    }

    // (public) true iff nth bit is set
    function bnTestBit(n) {
	var j = Math.floor(n/this.DB);
	if(j >= this.t) return(this.s!=0);
	return((this[j]&(1<<(n%this.DB)))!=0);
    }

    // (protected) this op (1<<n)
    function bnpChangeBit(n,op) {
	var r = BigInteger.ONE.shiftLeft(n);
	this.bitwiseTo(r,op,r);
	return r;
    }

    // (public) this | (1<<n)
    function bnSetBit(n) { return this.changeBit(n,op_or); }

    // (public) this & ~(1<<n)
    function bnClearBit(n) { return this.changeBit(n,op_andnot); }

    // (public) this ^ (1<<n)
    function bnFlipBit(n) { return this.changeBit(n,op_xor); }

    // (protected) r = this + a
    function bnpAddTo(a,r) {
	var i = 0, c = 0, m = Math.min(a.t,this.t);
	while(i < m) {
	    c += this[i]+a[i];
	    r[i++] = c&this.DM;
	    c >>= this.DB;
	}
	if(a.t < this.t) {
	    c += a.s;
	    while(i < this.t) {
		c += this[i];
		r[i++] = c&this.DM;
		c >>= this.DB;
	    }
	    c += this.s;
	}
	else {
	    c += this.s;
	    while(i < a.t) {
		c += a[i];
		r[i++] = c&this.DM;
		c >>= this.DB;
	    }
	    c += a.s;
	}
	r.s = (c<0)?-1:0;
	if(c > 0) r[i++] = c;
	else if(c < -1) r[i++] = this.DV+c;
	r.t = i;
	r.clamp();
    }

    // (public) this + a
    function bnAdd(a) { var r = nbi(); this.addTo(a,r); return r; }

    // (public) this - a
    function bnSubtract(a) { var r = nbi(); this.subTo(a,r); return r; }

    // (public) this * a
    function bnMultiply(a) { var r = nbi(); this.multiplyTo(a,r); return r; }

    // (public) this / a
    function bnDivide(a) { var r = nbi(); this.divRemTo(a,r,null); return r; }

    // (public) this % a
    function bnRemainder(a) { var r = nbi(); this.divRemTo(a,null,r); return r; }

    // (public) [this/a,this%a]
    function bnDivideAndRemainder(a) {
	var q = nbi(), r = nbi();
	this.divRemTo(a,q,r);
	return [q,r];
    }

    // (protected) this *= n, this >= 0, 1 < n < DV
    function bnpDMultiply(n) {
	this[this.t] = this.am(0,n-1,this,0,0,this.t);
	++this.t;
	this.clamp();
    }

    // (protected) this += n << w words, this >= 0
    function bnpDAddOffset(n,w) {
	if(n == 0) return;
	while(this.t <= w) this[this.t++] = 0;
	this[w] += n;
	while(this[w] >= this.DV) {
	    this[w] -= this.DV;
	    if(++w >= this.t) this[this.t++] = 0;
	    ++this[w];
	}
    }

    // A "null" reducer
    function NullExp() {}
    function nNop(x) { return x; }
    function nMulTo(x,y,r) { x.multiplyTo(y,r); }
    function nSqrTo(x,r) { x.squareTo(r); }

    NullExp.prototype.convert = nNop;
    NullExp.prototype.revert = nNop;
    NullExp.prototype.mulTo = nMulTo;
    NullExp.prototype.sqrTo = nSqrTo;

    // (public) this^e
    function bnPow(e) { return this.exp(e,new NullExp()); }

    // (protected) r = lower n words of "this * a", a.t <= n
    // "this" should be the larger one if appropriate.
    function bnpMultiplyLowerTo(a,n,r) {
	var i = Math.min(this.t+a.t,n);
	r.s = 0; // assumes a,this >= 0
	r.t = i;
	while(i > 0) r[--i] = 0;
	var j;
	for(j = r.t-this.t; i < j; ++i) r[i+this.t] = this.am(0,a[i],r,i,0,this.t);
	for(j = Math.min(a.t,n); i < j; ++i) this.am(0,a[i],r,i,0,n-i);
	r.clamp();
    }

    // (protected) r = "this * a" without lower n words, n > 0
    // "this" should be the larger one if appropriate.
    function bnpMultiplyUpperTo(a,n,r) {
	--n;
	var i = r.t = this.t+a.t-n;
	r.s = 0; // assumes a,this >= 0
	while(--i >= 0) r[i] = 0;
	for(i = Math.max(n-this.t,0); i < a.t; ++i)
	    r[this.t+i-n] = this.am(n-i,a[i],r,0,0,this.t+i-n);
	r.clamp();
	r.drShiftTo(1,r);
    }

    // Barrett modular reduction
    function Barrett(m) {
	// setup Barrett
	this.r2 = nbi();
	this.q3 = nbi();
	BigInteger.ONE.dlShiftTo(2*m.t,this.r2);
	this.mu = this.r2.divide(m);
	this.m = m;
    }

    function barrettConvert(x) {
	if(x.s < 0 || x.t > 2*this.m.t) return x.mod(this.m);
	else if(x.compareTo(this.m) < 0) return x;
	else { var r = nbi(); x.copyTo(r); this.reduce(r); return r; }
    }

    function barrettRevert(x) { return x; }

    // x = x mod m (HAC 14.42)
    function barrettReduce(x) {
	x.drShiftTo(this.m.t-1,this.r2);
	if(x.t > this.m.t+1) { x.t = this.m.t+1; x.clamp(); }
	this.mu.multiplyUpperTo(this.r2,this.m.t+1,this.q3);
	this.m.multiplyLowerTo(this.q3,this.m.t+1,this.r2);
	while(x.compareTo(this.r2) < 0) x.dAddOffset(1,this.m.t+1);
	x.subTo(this.r2,x);
	while(x.compareTo(this.m) >= 0) x.subTo(this.m,x);
    }

    // r = x^2 mod m; x != r
    function barrettSqrTo(x,r) { x.squareTo(r); this.reduce(r); }

    // r = x*y mod m; x,y != r
    function barrettMulTo(x,y,r) { x.multiplyTo(y,r); this.reduce(r); }

    Barrett.prototype.convert = barrettConvert;
    Barrett.prototype.revert = barrettRevert;
    Barrett.prototype.reduce = barrettReduce;
    Barrett.prototype.mulTo = barrettMulTo;
    Barrett.prototype.sqrTo = barrettSqrTo;

    // (public) this^e % m (HAC 14.85)
    function bnModPow(e,m) {
	var i = e.bitLength(), k, r = nbv(1), z;
	if(i <= 0) return r;
	else if(i < 18) k = 1;
	else if(i < 48) k = 3;
	else if(i < 144) k = 4;
	else if(i < 768) k = 5;
	else k = 6;
	if(i < 8)
	    z = new Classic(m);
	else if(m.isEven())
	    z = new Barrett(m);
	else
	    z = new Montgomery(m);

	// precomputation
	var g = [], n = 3, k1 = k-1, km = (1<<k)-1;
	g[1] = z.convert(this);
	if(k > 1) {
	    var g2 = nbi();
	    z.sqrTo(g[1],g2);
	    while(n <= km) {
		g[n] = nbi();
		z.mulTo(g2,g[n-2],g[n]);
		n += 2;
	    }
	}

	var j = e.t-1, w, is1 = true, r2 = nbi(), t;
	i = nbits(e[j])-1;
	while(j >= 0) {
	    if(i >= k1) w = (e[j]>>(i-k1))&km;
	    else {
		w = (e[j]&((1<<(i+1))-1))<<(k1-i);
		if(j > 0) w |= e[j-1]>>(this.DB+i-k1);
	    }

	    n = k;
	    while((w&1) == 0) { w >>= 1; --n; }
	    if((i -= n) < 0) { i += this.DB; --j; }
	    if(is1) {	// ret == 1, don't bother squaring or multiplying it
		g[w].copyTo(r);
		is1 = false;
	    }
	    else {
		while(n > 1) { z.sqrTo(r,r2); z.sqrTo(r2,r); n -= 2; }
		if(n > 0) z.sqrTo(r,r2); else { t = r; r = r2; r2 = t; }
		z.mulTo(r2,g[w],r);
	    }

	    while(j >= 0 && (e[j]&(1<<i)) == 0) {
		z.sqrTo(r,r2); t = r; r = r2; r2 = t;
		if(--i < 0) { i = this.DB-1; --j; }
	    }
	}
	return z.revert(r);
    }

    // (public) gcd(this,a) (HAC 14.54)
    function bnGCD(a) {
	var x = (this.s<0)?this.negate():this.clone();
	var y = (a.s<0)?a.negate():a.clone();
	if(x.compareTo(y) < 0) { var t = x; x = y; y = t; }
	var i = x.getLowestSetBit(), g = y.getLowestSetBit();
	if(g < 0) return x;
	if(i < g) g = i;
	if(g > 0) {
	    x.rShiftTo(g,x);
	    y.rShiftTo(g,y);
	}
	while(x.signum() > 0) {
	    if((i = x.getLowestSetBit()) > 0) x.rShiftTo(i,x);
	    if((i = y.getLowestSetBit()) > 0) y.rShiftTo(i,y);
	    if(x.compareTo(y) >= 0) {
		x.subTo(y,x);
		x.rShiftTo(1,x);
	    }
	    else {
		y.subTo(x,y);
		y.rShiftTo(1,y);
	    }
	}
	if(g > 0) y.lShiftTo(g,y);
	return y;
    }

    // (protected) this % n, n < 2^26
    function bnpModInt(n) {
	if(n <= 0) return 0;
	var d = this.DV%n, r = (this.s<0)?n-1:0;
	if(this.t > 0)
	    if(d == 0) r = this[0]%n;
	else for(var i = this.t-1; i >= 0; --i) r = (d*r+this[i])%n;
	return r;
    }

    // (public) 1/this % m (HAC 14.61)
    function bnModInverse(m) {
	var ac = m.isEven();
	if((this.isEven() && ac) || m.signum() == 0) return BigInteger.ZERO;
	var u = m.clone(), v = this.clone();
	var a = nbv(1), b = nbv(0), c = nbv(0), d = nbv(1);
	while(u.signum() != 0) {
	    while(u.isEven()) {
		u.rShiftTo(1,u);
		if(ac) {
		    if(!a.isEven() || !b.isEven()) { a.addTo(this,a); b.subTo(m,b); }
		    a.rShiftTo(1,a);
		}
		else if(!b.isEven()) b.subTo(m,b);
		b.rShiftTo(1,b);
	    }
	    while(v.isEven()) {
		v.rShiftTo(1,v);
		if(ac) {
		    if(!c.isEven() || !d.isEven()) { c.addTo(this,c); d.subTo(m,d); }
		    c.rShiftTo(1,c);
		}
		else if(!d.isEven()) d.subTo(m,d);
		d.rShiftTo(1,d);
	    }
	    if(u.compareTo(v) >= 0) {
		u.subTo(v,u);
		if(ac) a.subTo(c,a);
		b.subTo(d,b);
	    }
	    else {
		v.subTo(u,v);
		if(ac) c.subTo(a,c);
		d.subTo(b,d);
	    }
	}
	if(v.compareTo(BigInteger.ONE) != 0) return BigInteger.ZERO;
	if(d.compareTo(m) >= 0) return d.subtract(m);
	if(d.signum() < 0) d.addTo(m,d); else return d;
	if(d.signum() < 0) return d.add(m); else return d;
    }

    var lowprimes = [2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,97,101,103,107,109,113,127,131,137,139,149,151,157,163,167,173,179,181,191,193,197,199,211,223,227,229,233,239,241,251,257,263,269,271,277,281,283,293,307,311,313,317,331,337,347,349,353,359,367,373,379,383,389,397,401,409,419,421,431,433,439,443,449,457,461,463,467,479,487,491,499,503,509];
    var lplim = (1<<26)/lowprimes[lowprimes.length-1];

    // (public) test primality with certainty >= 1-.5^t
    function bnIsProbablePrime(t) {
	var i, x = this.abs();
	if(x.t == 1 && x[0] <= lowprimes[lowprimes.length-1]) {
	    for(i = 0; i < lowprimes.length; ++i)
		if(x[0] == lowprimes[i]) return true;
	    return false;
	}
	if(x.isEven()) return false;
	i = 1;
	while(i < lowprimes.length) {
	    var m = lowprimes[i], j = i+1;
	    while(j < lowprimes.length && m < lplim) m *= lowprimes[j++];
	    m = x.modInt(m);
	    while(i < j) if(m%lowprimes[i++] == 0) return false;
	}
	return x.millerRabin(t);
    }

    // (protected) true if probably prime (HAC 4.24, Miller-Rabin)
    function bnpMillerRabin(t) {
	var n1 = this.subtract(BigInteger.ONE);
	var k = n1.getLowestSetBit();
	if(k <= 0) return false;
	var r = n1.shiftRight(k);
	t = (t+1)>>1;
	if(t > lowprimes.length) t = lowprimes.length;
	var a = nbi();
	for(var i = 0; i < t; ++i) {
	    a.fromInt(lowprimes[i]);
	    var y = a.modPow(r,this);
	    if(y.compareTo(BigInteger.ONE) != 0 && y.compareTo(n1) != 0) {
		var j = 1;
		while(j++ < k && y.compareTo(n1) != 0) {
		    y = y.modPowInt(2,this);
		    if(y.compareTo(BigInteger.ONE) == 0) return false;
		}
		if(y.compareTo(n1) != 0) return false;
	    }
	}
	return true;
    }
    
    

    // protected
    BigInteger.prototype.chunkSize = bnpChunkSize;
    BigInteger.prototype.toRadix = bnpToRadix;
    BigInteger.prototype.fromRadix = bnpFromRadix;
    BigInteger.prototype.fromNumber = bnpFromNumber;
    BigInteger.prototype.bitwiseTo = bnpBitwiseTo;
    BigInteger.prototype.changeBit = bnpChangeBit;
    BigInteger.prototype.addTo = bnpAddTo;
    BigInteger.prototype.dMultiply = bnpDMultiply;
    BigInteger.prototype.dAddOffset = bnpDAddOffset;
    BigInteger.prototype.multiplyLowerTo = bnpMultiplyLowerTo;
    BigInteger.prototype.multiplyUpperTo = bnpMultiplyUpperTo;
    BigInteger.prototype.modInt = bnpModInt;
    BigInteger.prototype.millerRabin = bnpMillerRabin;

    // public
    BigInteger.prototype.clone = bnClone;
    BigInteger.prototype.intValue = bnIntValue;
    BigInteger.prototype.byteValue = bnByteValue;
    BigInteger.prototype.shortValue = bnShortValue;
    BigInteger.prototype.signum = bnSigNum;
    BigInteger.prototype.toByteArray = bnToByteArray;
    BigInteger.prototype.equals = bnEquals;
    BigInteger.prototype.min = bnMin;
    BigInteger.prototype.max = bnMax;
    BigInteger.prototype.and = bnAnd;
    BigInteger.prototype.or = bnOr;
    BigInteger.prototype.xor = bnXor;
    BigInteger.prototype.andNot = bnAndNot;
    BigInteger.prototype.not = bnNot;
    BigInteger.prototype.shiftLeft = bnShiftLeft;
    BigInteger.prototype.shiftRight = bnShiftRight;
    BigInteger.prototype.getLowestSetBit = bnGetLowestSetBit;
    BigInteger.prototype.bitCount = bnBitCount;
    BigInteger.prototype.testBit = bnTestBit;
    BigInteger.prototype.setBit = bnSetBit;
    BigInteger.prototype.clearBit = bnClearBit;
    BigInteger.prototype.flipBit = bnFlipBit;
    BigInteger.prototype.add = bnAdd;
    BigInteger.prototype.subtract = bnSubtract;
    BigInteger.prototype.multiply = bnMultiply;
    BigInteger.prototype.divide = bnDivide;
    BigInteger.prototype.remainder = bnRemainder;
    BigInteger.prototype.divideAndRemainder = bnDivideAndRemainder;
    BigInteger.prototype.modPow = bnModPow;
    BigInteger.prototype.modInverse = bnModInverse;
    BigInteger.prototype.pow = bnPow;
    BigInteger.prototype.gcd = bnGCD;
    BigInteger.prototype.isProbablePrime = bnIsProbablePrime;

    // BigInteger interfaces not implemented in jsbn:

    // BigInteger(int signum, byte[] magnitude)
    // double doubleValue()
    // float floatValue()
    // int hashCode()
    // long longValue()
    // static BigInteger valueOf(long val)



    //////////////////////////////////////////////////////////////////////
    //////////////////////////////////////////////////////////////////////
    //////////////////////////////////////////////////////////////////////
    //////////////////////////////////////////////////////////////////////
    //////////////////////////////////////////////////////////////////////
    // END OF copy-and-paste of jsbn.



    BigInteger.NEGATIVE_ONE = BigInteger.ONE.negate();


    // Other methods we need to add for compatibilty with js-numbers numeric tower.

    // add is implemented above.
    // subtract is implemented above.
    // multiply is implemented above.
    // equals is implemented above.
    // abs is implemented above.
    // negate is defined above.

    // makeBignum: string -> BigInteger
    var makeBignum = function(s) {
	if (typeof(s) === 'number') { s = s + ''; }
	s = expandExponent(s);
	return new BigInteger(s, 10);
    };

    var zerostring = function(n) {
	var buf = [];
	for (var i = 0; i < n; i++) {
	    buf.push('0');
	}
	return buf.join('');
    };


    BigInteger.prototype.level = 0;
    BigInteger.prototype.liftTo = function(target) {
	if (target.level === 1) {
	    return new Rational(this, 1);
	}
	if (target.level === 2) {
	    var fixrep = this.toFixnum();
	    if (fixrep === Number.POSITIVE_INFINITY)
		return TOO_POSITIVE_TO_REPRESENT;
	    if (fixrep === Number.NEGATIVE_INFINITY)
		return TOO_NEGATIVE_TO_REPRESENT;
	    return new FloatPoint(fixrep);
	}
	if (target.level === 3) {
	    return new Complex(this, 0);
	}
	return throwRuntimeError("invalid level for BigInteger lift", this, target);
    };

    BigInteger.prototype.isFinite = function() {
	return true;
    };

    BigInteger.prototype.isInteger = function() {
	return true;
    };

    BigInteger.prototype.isRational = function() {
	return true;
    };

    BigInteger.prototype.isReal = function() {
	return true;
    };

    BigInteger.prototype.isExact = function() {
	return true;
    };

    BigInteger.prototype.isInexact = function() {
	return false;
    };

    BigInteger.prototype.toExact = function() {
	return this;
    };

    BigInteger.prototype.toInexact = function() {
	return FloatPoint.makeInstance(this.toFixnum());
    };

    BigInteger.prototype.toFixnum = function() {
	var result = 0, str = this.toString(), i;
	if (str[0] === '-') {
	    for (i=1; i < str.length; i++) {
		result = result * 10 + Number(str[i]);
	    }
	    return -result;
	} else {
	    for (i=0; i < str.length; i++) {
		result = result * 10 + Number(str[i]);
	    }
	    return result;
	}
    };


    BigInteger.prototype.greaterThan = function(other) {
	return this.compareTo(other) > 0;
    };

    BigInteger.prototype.greaterThanOrEqual = function(other) {
	return this.compareTo(other) >= 0;
    };

    BigInteger.prototype.lessThan = function(other) {
	return this.compareTo(other) < 0;
    };

    BigInteger.prototype.lessThanOrEqual = function(other) {
	return this.compareTo(other) <= 0;
    };

    // divide: scheme-number -> scheme-number
    // WARNING NOTE: we override the old version of divide.
    BigInteger.prototype.divide = function(other) {
	var quotientAndRemainder = bnDivideAndRemainder.call(this, other);
	if (quotientAndRemainder[1].compareTo(BigInteger.ZERO) === 0) {
	    return quotientAndRemainder[0];
	} else {
	    var result = add(quotientAndRemainder[0],
			     Rational.makeInstance(quotientAndRemainder[1], other));
	    return result;
	}
    };

    BigInteger.prototype.numerator = function() {
	return this;
    };

    BigInteger.prototype.denominator = function() {
	return 1;
    };


    (function() {
	// Classic implementation of Newton-Ralphson square-root search,
	// adapted for integer-sqrt.
	// http://en.wikipedia.org/wiki/Newton's_method#Square_root_of_a_number
	    var searchIter = function(n, guess) {
		while(!(lessThanOrEqual(sqr(guess),n) &&
			lessThan(n,sqr(add(guess, 1))))) {
		    guess = floor(divide(add(guess,
					     floor(divide(n, guess))),
					 2));
		}
		return guess;
	    };

	    // integerSqrt: -> scheme-number
	    BigInteger.prototype.integerSqrt = function() {
		var n;
		if(sign(this) >= 0) {
		    return searchIter(this, this);
		} else {
		    n = this.negate();
		    return Complex.makeInstance(0, searchIter(n, n));
		}
	    };
    })();


    (function() {	
	// Get an approximation using integerSqrt, and then start another
	// Newton-Ralphson search if necessary.
	BigInteger.prototype.sqrt = function() {
	    var approx = this.integerSqrt(), fix;
	    if (eqv(sqr(approx), this)) {
		return approx;
	    }
	    fix = toFixnum(this);
	    if (isFinite(fix)) {
		if (fix >= 0) {
		    return FloatPoint.makeInstance(Math.sqrt(fix));
		} else {
		    return Complex.makeInstance(
			0,
			FloatPoint.makeInstance(Math.sqrt(-fix)));
		}
	    } else {
		return approx;
	    }
	};
    })();




    
    // sqrt: -> scheme-number
    // http://en.wikipedia.org/wiki/Newton's_method#Square_root_of_a_number
    // Produce the square root.

    // floor: -> scheme-number
    // Produce the floor.
    BigInteger.prototype.floor = function() {
        return this;
    }

    // ceiling: -> scheme-number
    // Produce the ceiling.
    BigInteger.prototype.ceiling = function() {
        return this;
    }

    // conjugate: -> scheme-number
    // Produce the conjugate.

    // magnitude: -> scheme-number
    // Produce the magnitude.

    // log: -> scheme-number
    // Produce the log.

    // angle: -> scheme-number
    // Produce the angle.

    // atan: -> scheme-number
    // Produce the arc tangent.

    // cos: -> scheme-number
    // Produce the cosine.

    // sin: -> scheme-number
    // Produce the sine.


    // expt: scheme-number -> scheme-number
    // Produce the power to the input.
    BigInteger.prototype.expt = function(n) {
	return bnPow.call(this, n);
    };



    // exp: -> scheme-number
    // Produce e raised to the given power.

    // acos: -> scheme-number
    // Produce the arc cosine.

    // asin: -> scheme-number
    // Produce the arc sine.

    BigInteger.prototype.imaginaryPart = function() {
	    return 0;
    }
    BigInteger.prototype.realPart = function() {
	    return this;
    }

    // round: -> scheme-number
    // Round to the nearest integer.





    //////////////////////////////////////////////////////////////////////
    // toRepeatingDecimal: jsnum jsnum {limit: number}? -> [string, string, string]
    //
    // Given the numerator and denominator parts of a rational,
    // produces the repeating-decimal representation, where the first
    // part are the digits before the decimal, the second are the
    // non-repeating digits after the decimal, and the third are the
    // remaining repeating decimals.
    // 
    // An optional limit on the decimal expansion can be provided, in which
    // case the search cuts off if we go past the limit.
    // If this happens, the third argument returned becomes '...' to indicate
    // that the search was prematurely cut off.
    var toRepeatingDecimal = (function() {
	var getResidue = function(r, d, limit) {
	    var digits = [];
	    var seenRemainders = {};
	    seenRemainders[r] = true;
	    while(true) {	
		if (limit-- <= 0) {
		    return [digits.join(''), '...']
		}

		var nextDigit = quotient(
		    multiply(r, 10), d);
		var nextRemainder = remainder(
		    multiply(r, 10),
		    d);
		digits.push(nextDigit.toString());
		if (seenRemainders[nextRemainder]) {
		    r = nextRemainder;
		    break;
		} else {
		    seenRemainders[nextRemainder] = true;
		    r = nextRemainder;
		}
	    }
	    
	    var firstRepeatingRemainder = r;
	    var repeatingDigits = [];
	    while (true) {
		var nextDigit = quotient(multiply(r, 10), d);
		var nextRemainder = remainder(
		    multiply(r, 10),
		    d);
		repeatingDigits.push(nextDigit.toString());
		if (equals(nextRemainder, firstRepeatingRemainder)) {
		    break;
		} else {
		    r = nextRemainder;
		}
	    };

	    var digitString = digits.join('');
	    var repeatingDigitString = repeatingDigits.join('');

	    while (digitString.length >= repeatingDigitString.length &&
		   (digitString.substring(
		       digitString.length - repeatingDigitString.length)
		    === repeatingDigitString)) {
		digitString = digitString.substring(
		    0, digitString.length - repeatingDigitString.length);
	    }

	    return [digitString, repeatingDigitString];

	};

	return function(n, d, options) {
	    // default limit on decimal expansion; can be overridden
	    var limit = 512;
	    if (options && typeof(options.limit) !== 'undefined') {
		limit = options.limit;
	    }
	    if (! isInteger(n)) {
		throwRuntimeError('toRepeatingDecimal: n ' + n.toString() +
				  " is not an integer.");
	    }
	    if (! isInteger(d)) {
		throwRuntimeError('toRepeatingDecimal: d ' + d.toString() +
				  " is not an integer.");
	    }
	    if (equals(d, 0)) {
		throwRuntimeError('toRepeatingDecimal: d equals 0');
	    }
	    if (lessThan(d, 0)) {
		throwRuntimeError('toRepeatingDecimal: d < 0');
	    }
 	    var sign = (lessThan(n, 0) ? "-" : "");
 	    n = abs(n);
 	    var beforeDecimalPoint = sign + quotient(n, d);
 	    var afterDecimals = getResidue(remainder(n, d), d, limit);
 	    return [beforeDecimalPoint].concat(afterDecimals);
	};
    })();
    //////////////////////////////////////////////////////////////////////




    // External interface of js-numbers:

    Numbers['fromFixnum'] = fromFixnum;
    Numbers['fromString'] = fromString;
    Numbers['makeBignum'] = makeBignum;
    Numbers['makeRational'] = Rational.makeInstance;
    Numbers['makeFloat'] = FloatPoint.makeInstance;
    Numbers['makeComplex'] = Complex.makeInstance;
    Numbers['makeComplexPolar'] = makeComplexPolar;

    Numbers['pi'] = FloatPoint.pi;
    Numbers['e'] = FloatPoint.e;
    Numbers['nan'] = FloatPoint.nan;
    Numbers['negative_inf'] = FloatPoint.neginf;
    Numbers['inf'] = FloatPoint.inf;
    Numbers['negative_one'] = -1;   // Rational.NEGATIVE_ONE;
    Numbers['zero'] = 0;            // Rational.ZERO;
    Numbers['one'] = 1;             // Rational.ONE;
    Numbers['i'] = plusI;
    Numbers['negative_i'] = minusI;
    Numbers['negative_zero'] = NEGATIVE_ZERO;

    Numbers['onThrowRuntimeError'] = onThrowRuntimeError;
    Numbers['isSchemeNumber'] = isSchemeNumber;
    Numbers['isRational'] = isRational;
    Numbers['isReal'] = isReal;
    Numbers['isExact'] = isExact;
    Numbers['isInexact'] = isInexact;
    Numbers['isInteger'] = isInteger;

    Numbers['toFixnum'] = toFixnum;
    Numbers['toExact'] = toExact;
    Numbers['toInexact'] = toInexact;
    Numbers['add'] = add;
    Numbers['subtract'] = subtract;
    Numbers['multiply'] = multiply;
    Numbers['divide'] = divide;
    Numbers['equals'] = equals;
    Numbers['eqv'] = eqv;
    Numbers['approxEquals'] = approxEquals;
    Numbers['greaterThanOrEqual'] = greaterThanOrEqual;
    Numbers['lessThanOrEqual'] = lessThanOrEqual;
    Numbers['greaterThan'] = greaterThan;
    Numbers['lessThan'] = lessThan;
    Numbers['expt'] = expt;
    Numbers['exp'] = exp;
    Numbers['modulo'] = modulo;
    Numbers['numerator'] = numerator;
    Numbers['denominator'] = denominator;
    Numbers['integerSqrt'] = integerSqrt;
    Numbers['sqrt'] = sqrt;
    Numbers['abs'] = abs;
    Numbers['quotient'] = quotient;
    Numbers['remainder'] = remainder;
    Numbers['floor'] = floor;
    Numbers['ceiling'] = ceiling;
    Numbers['conjugate'] = conjugate;
    Numbers['magnitude'] = magnitude;
    Numbers['log'] = log;
    Numbers['angle'] = angle;
    Numbers['tan'] = tan;
    Numbers['atan'] = atan;
    Numbers['cos'] = cos;
    Numbers['sin'] = sin;
    Numbers['tan'] = tan;
    Numbers['acos'] = acos;
    Numbers['asin'] = asin;
    Numbers['cosh'] = cosh;
    Numbers['sinh'] = sinh;
    Numbers['imaginaryPart'] = imaginaryPart;
    Numbers['realPart'] = realPart;
    Numbers['round'] = round;
    Numbers['sqr'] = sqr;
    Numbers['gcd'] = gcd;
    Numbers['lcm'] = lcm;

    Numbers['toRepeatingDecimal'] = toRepeatingDecimal;



    // The following exposes the class representations for easier
    // integration with other projects.
    Numbers['BigInteger'] = BigInteger;
    Numbers['Rational'] = Rational;
    Numbers['FloatPoint'] = FloatPoint;
    Numbers['Complex'] = Complex;   

    Numbers['MIN_FIXNUM'] = MIN_FIXNUM;
    Numbers['MAX_FIXNUM'] = MAX_FIXNUM;

})();
/**
 *
 *  Originally grabbed from:
 *  Base64 encode / decode
 *  http://www.webtoolkit.info/
 *
 *  dyoo: modified to work with arrays of bytes rather than assume
 *  the bytes are strings.
 * 
 *  Provides a Base64 object with two methods:
 *  Base64.encode: [arrayof int] -> string
 *  Base64.decode: string -> [arrayof int]
 **/
var Base64 = (function() {
    'use strict';

    // private property
    var _keyStr = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/=";

    var Base64 = {        
        // public method for encoding
        encode : function (inputBytes) {
	    var output = [];
	    var chr1, chr2, chr3, enc1, enc2, enc3, enc4;
	    var i = 0;
	    while (i < inputBytes.length) {
                
	        chr1 = inputBytes[i++];
	        chr2 = inputBytes[i++];
	        chr3 = inputBytes[i++];
                
	        enc1 = chr1 >> 2;
	        enc2 = ((chr1 & 3) << 4) | (chr2 >> 4);
	        enc3 = ((chr2 & 15) << 2) | (chr3 >> 6);
	        enc4 = chr3 & 63;
                
	        if (isNaN(chr2)) {
		    enc3 = enc4 = 64;
	        } else if (isNaN(chr3)) {
		    enc4 = 64;
	        }
	        output.push(_keyStr.charAt(enc1));
                output.push(_keyStr.charAt(enc2));
		output.push(_keyStr.charAt(enc3));
                output.push(_keyStr.charAt(enc4));
	    }
	    return output.join('');
        },
        
        // public method for decoding
        decode : function (inputString) {
	    var outputBytes = [];
	    var chr1, chr2, chr3;
	    var enc1, enc2, enc3, enc4;
	    var i = 0;
            
	    inputString = inputString.replace(/[^A-Za-z0-9\+\/\=]/g, "");
            
	    while (i < inputString.length) {
	        enc1 = _keyStr.indexOf(inputString.charAt(i++));
	        enc2 = _keyStr.indexOf(inputString.charAt(i++));
	        enc3 = _keyStr.indexOf(inputString.charAt(i++));
	        enc4 = _keyStr.indexOf(inputString.charAt(i++));
                
	        chr1 = (enc1 << 2) | (enc2 >> 4);
	        chr2 = ((enc2 & 15) << 4) | (enc3 >> 2);
	        chr3 = ((enc3 & 3) << 6) | enc4;
                
	        outputBytes.push(chr1);
                
	        if (enc3 !== 64) {
		    outputBytes.push(chr2);
	        }
	        if (enc4 !== 64) {
		    outputBytes.push(chr3);
	        }
	    }
	    return outputBytes;
        }
    };
    return Base64;
}());/*jslint vars: true, plusplus: true, maxerr: 50, indent: 4 */

// Basic library functions.  This will include a few simple functions,
// but be augmented with several namespaces for the other libraries in
// the base library.
if (!(this.plt)) { this.plt = {}; }
(function (plt) {
    'use strict';
    var baselib = {};
    plt.baselib = baselib;



    // Simple object inheritance.
    var heir = function (parentPrototype) {
        var F = function () {};
        F.prototype = parentPrototype;
        return new F();
    };



    var hasOwnProperty = {}.hasOwnProperty;

    // clone: object -> object
    // Copies an object.  The new object should respond like the old
    // object, including to things like instanceof.
    var clone = function (obj) {
        var property;
        var C = function () {};
        C.prototype = obj;
        var c = new C();
        for (property in obj) {
            if (hasOwnProperty.call(obj, property)) {
                c[property] = obj[property];
            }
        }
        return c;
    };


    // Consumes a class and creates a predicate that recognizes subclasses.
    var makeClassPredicate = function (aClass) {
        return function (x) { return x instanceof aClass; };
    };



    // Helper to deal with the argument-passing of primitives.  Call f
    // with arguments bound from MACHINE.e, assuming
    // MACHINE.a has been initialized with the number of
    // arguments on the stack.  vs provides optional values for the
    // arguments that go beyond those of the mandatoryArgCount.
    var withArguments = function (MACHINE, mandatoryArgCount, vs, f) {
        var args = [], i;
        for (i = 0; i < MACHINE.a; i++) {
            if (i < mandatoryArgCount) {
                args.push(MACHINE.e[MACHINE.e.length - 1 - i]);
            } else {
                if (i < MACHINE.a) {
                    args.push(MACHINE.e[MACHINE.e.length - 1 - i]);
                } else {
                    args.push(vs[mandatoryArgCount - i]);
                }
            }
        }
        return f.apply(null, args);
    };



    baselib.heir = heir;
    baselib.clone = clone;
    baselib.makeClassPredicate = makeClassPredicate;
    baselib.withArguments = withArguments;


}(this.plt));
/*jslint unparam: true, vars: true, white: true, newcap: true, nomen: true, plusplus: true, maxerr: 50, indent: 4 */

/*global window*/

// Dictionaries.  We need this due to JS weirdness, since object
// literals have issues with regards to magic properties like
// __proto__.  This is taken from Effective JavaScript, Item 45.
(function (baselib) {
    'use strict';

    var hasOwnProperty = {}.hasOwnProperty;
    
    baselib.Dict = function(elements) {
        this.elements = elements || {};
        this.hasSpecialProto = false;
        this.specialProto = undefined;
    };

    baselib.Dict.prototype.has = function(key) {
        if (key === '__proto__') {
            return this.hasSpecialProto;
        }
        return hasOwnProperty.call(this.elements, key);
    };

    baselib.Dict.prototype.get = function(key) {
        if (key === '__proto__') { 
            return this.specialProto;
        } else if (hasOwnProperty.call(this.elements, key)) {
            return this.elements[key];
        } else {
            return undefined;
        }
    };

    baselib.Dict.prototype.set = function(key, val) {
        if (key === '__proto__') {
            this.hasSpecialProto = true;
            this.specialProto = val;
        } else {
            this.elements[key] = val;
        }
    };

    baselib.Dict.prototype.remove = function(key) {
        if (key === '__proto__') {
            this.hasSpecialProto = false;
            this.specialProto = undefined;
        } else {
            delete this.elements[key];
        }
    };
}(window.plt.baselib));/*jslint unparam: true, sub: true, vars: true, white: true, plusplus: true, maxerr: 50, indent: 4 */

// Frame structures.
(function(baselib) {
    'use strict';
    var exports = {};
    baselib.frames = exports;



    // A generic frame just holds marks.
    var Frame = function() {
	// The set of continuation marks.
	// this.marks = [];
	// When we're in the middle of computing with-cont-mark, we
	// stash the key in here temporarily.
	// this.pendingContinuationMarkKey = undefined;
	// this.pendingApplyValuesProc = undefined;
	// this.pendingBegin0Count = undefined;
	// this.pendingBegin0Values = undefined;
    };
    Frame.prototype.getMarks = function() {
        if (this.marks === void(0)) { this.marks = []; }
        return this.marks;
    };


    // Frames must support marks and the temporary variables necessary to
    // support with-continuation-mark and with-values.

    // Specialized frames support more features:

    // A CallFrame represents a call stack frame, and includes the return address
    // as well as the function being called.
    var CallFrame = function(label, proc) {
	this.label = label;
	this.p = proc;
    };
    CallFrame.prototype = baselib.heir(Frame.prototype);



    // A prompt frame includes a return address, as well as a prompt
    // tag for supporting delimited continuations.  To support abort,
    // we also keep the size of the environment, and the handler
    // to call if an abort happens.
    //
    // If handler is null, handler will be a default closure that
    // accepts any number of values and returns.
    var PromptFrame = function(label, tag, envLength, handler) {
	this.label = label;
	this.tag = tag; // ContinuationPromptTag
        this.envLength = envLength;
        this.handler = handler;
    };
    PromptFrame.prototype = baselib.heir(Frame.prototype);







    //////////////////////////////////////////////////////////////////////
    exports.Frame = Frame;
    exports.CallFrame = CallFrame;
    exports.PromptFrame = PromptFrame;



}(this.plt.baselib));
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

}(this.plt.baselib));/*jslint vars: true, white: true, maxerr: 50, indent: 4 */


// Equality function
/*global jsnums*/
(function (baselib, jsnums) {
    'use strict';
    var exports = {};
    baselib.equality = exports;



    var eqv = function (x, y) {
        if (x === y) { return true; }

        if (baselib.numbers.isNumber(x) && baselib.numbers.isNumber(y)) {
            return jsnums.eqv(x, y);
        } else if (baselib.chars.isChar(x) && baselib.chars.isChar(y)) {
            return x.val === y.val;
        } else {
            return false;
        }
    };




    // equals: X Y -> boolean
    // Returns true if the objects are equivalent; otherwise, returns false.
    var equals = function (x, y, aUnionFind) {
        if (x === y) { return true; }

        if (baselib.numbers.isNumber(x) && baselib.numbers.isNumber(y)) {
            return baselib.numbers.eqv(x, y);
        }

        if (baselib.strings.isString(x) && baselib.strings.isString(y)) {
            return x.toString() === y.toString();
        }

        if (x === void(0) || x === null) {
            return (y === void(0) || y === null);
        }

        if (typeof (x) === 'object' && typeof (y) === 'object' &&
            x.equals && y.equals) {
            if (aUnionFind === void(0)) {
                aUnionFind = new baselib.UnionFind();
            }

            if (aUnionFind.find(x) === aUnionFind.find(y)) {
                return true;
            }
            else {
                aUnionFind.merge(x, y); 
                return x.equals(y, aUnionFind);
            }
        }
        return false;
    };

    exports.eqv = eqv;
    exports.equals = equals;

}(this.plt.baselib, jsnums));/*jslint browser: true, undef: false, unparam: true, sub: true, vars: true, white: true, plusplus: true, maxerr: 50, indent: 4 */

// Formatting library.
// Produces string and DOM representations of values.
//
/*global $*/
(function(baselib, $) {
    'use strict';
    var exports = {};
    baselib.format = exports;


    var replaceUnprintableStringChars = function(s) {
        var ret = [], i;
        for (i = 0; i < s.length; i++) {
            var val = s.charCodeAt(i);
            switch(val) {
            case 7: ret.push('\\a'); break;
            case 8: ret.push('\\b'); break;
            case 9: ret.push('\\t'); break;
            case 10: ret.push('\\n'); break;
            case 11: ret.push('\\v'); break;
            case 12: ret.push('\\f'); break;
            case 13: ret.push('\\r'); break;
            case 34: ret.push('\\"'); break;
            case 92: ret.push('\\\\'); break;
            default: if (val >= 32 && val <= 126) {
                ret.push( s.charAt(i) );
            }
                else {
                    var numStr = val.toString(16).toUpperCase();
                    while (numStr.length < 4) {
                        numStr = '0' + numStr;
                    }
                    ret.push('\\u' + numStr);
                }
                break;
            }
        }
        return ret.join('');
    };

    var escapeString = function(s) {
        return '"' + replaceUnprintableStringChars(s) + '"';
    };


    // toWrittenString: Any Hashtable -> String
    var toWrittenString = function(x, cache) {
        if (! cache) { 
            cache = baselib.hashes.makeLowLevelEqHash();
        }
        if (x === null) {
            return "null";
        }
        if (x === true) { return "true"; }
        if (x === false) { return "false"; }
        if (typeof(x) === 'object') {
            if (cache.containsKey(x)) {
                return "...";
            }
        }
        if (x === void(0)) {
            return "#<undefined>";
        }
        if (typeof(x) === 'string') {
            return escapeString(x.toString());
        }

        if (baselib.functions.isProcedure(x)) {
            return '#<function:' + x.displayName + '>';
        }

        if (typeof(x) !== 'object' && typeof(x) !== 'function') {
            return x.toString();
        }

        var returnVal;
        if (x.toWrittenString) {
            returnVal = x.toWrittenString(cache);
        } else if (x.toDisplayedString) {
            returnVal = x.toDisplayedString(cache);
        } else {
            returnVal = x.toString();
        }
        return returnVal;
    };



    // toDisplayedString: Any Hashtable -> String
    var toDisplayedString = function(x, cache) {
        if (! cache) {
            cache = baselib.hashes.makeLowLevelEqHash();
        }
        if (x === null) {
            return "null";
        }
        if (x === true) { return "true"; }
        if (x === false) { return "false"; }
        if (typeof(x) === 'object') {
            if (cache.containsKey(x)) {
                return "...";
            }
        }
        if (x === void(0) || x === null) {
            return "#<undefined>";
        }
        if (typeof(x) === 'string') {
            return x;
        }

        if (baselib.functions.isProcedure(x)) {
            return '#<function:' + x.displayName + '>';
        }

        if (typeof(x) !== 'object' && typeof(x) !== 'function') {
            return x.toString();
        }

        var returnVal;
        if (x.toDisplayedString) {
            returnVal = x.toDisplayedString(cache);
        } else if (x.toWrittenString) {
            returnVal = x.toWrittenString(cache);
        } else {
            returnVal = x.toString();
        }
        return returnVal;
    };



    var formatRegexp1 = new RegExp('~[sSaA]', 'g');
    var formatRegexp2 = new RegExp("~[sSaAnevE%~]", "g");
    
    // format: string [X ...] string -> string
    // String formatting.  If an exception occurs, throws
    // a plain Error whose message describes the formatting error.
    var format = function(formatStr, args, functionName) {
        var throwFormatError = function() {
            functionName = functionName || 'format';
            var matches = formatStr.match(formatRegexp1);
            var expectedNumberOfArgs = (matches === null ? 0 : matches.length);
            var errorStrBuffer = [functionName + ': format string requires ' + expectedNumberOfArgs
                                  + ' arguments, given ' + args.length + '; arguments were:',
                                  toWrittenString(formatStr)];
            var i;
            for (i = 0; i < args.length; i++) {
                errorStrBuffer.push( toWrittenString(args[i]) );
            }

            throw new Error(errorStrBuffer.join(' '));
        };


        var buffer = args.slice(0);
        var onTemplate = function(s) {
            if (s === "~~") {
                return "~";
            } else if (s === '~n' || s === '~%') {
                return "\n";
            } else if (s === '~s' || s === "~S") {
                if (buffer.length === 0) {
                    throwFormatError();
                }
                return toWrittenString(buffer.shift());
            } else if (s === '~e' || s === "~E") {
                // FIXME: we don't yet have support for the error-print
                // handler, and currently treat ~e just like ~s.
                if (buffer.length === 0) {
                    throwFormatError();
                }
                return toWrittenString(buffer.shift()); 
            }
            else if (s === '~v') {
                if (buffer.length === 0) {
                    throwFormatError();
                }
                // fprintf must do something more interesting here by
                // printing the dom representation directly...
                return toWrittenString(buffer.shift());
            } else if (s === '~a' || s === "~A") {
                if (buffer.length === 0) {
                    throwFormatError();
                }
                return toDisplayedString(buffer.shift());
            } else {
                throw new Error(functionName + 
                                ': string.replace matched invalid regexp');
            }
        };
        var result = formatStr.replace(formatRegexp2, onTemplate);
        if (buffer.length > 0) {
            throwFormatError();
        }
        return result;
    };
    


    var hasOwnProperty = {}.hasOwnProperty;

    var ToDomNodeParameters = function(params) {
        if (! params) { params = {}; }
        var k;
        for (k in params) {
            if (hasOwnProperty.call(params, k)) {
                this[k] = params[k];
            }
        }
        if (this.cache === void(0)) {
            this.cache = baselib.hashes.makeLowLevelEqHash();
        }
        if (this.cycles === void(0)) {
            this.cycles = baselib.hashes.makeLowLevelEqHash();
        }
        if (this.depth === void(0)) {
            this.depth = 0;
        }
        if (this.objectCounter === void(0)) {
            this.objectCounter = 0;
        }
    };


    ToDomNodeParameters.prototype.incrementDepth = function() {
        return new ToDomNodeParameters({ mode : this.mode,
                                         depth: this.depth + 1,
                                         cache: this.cache,
                                         cycles: this.cycles,
                                         objectCounter: this.objectCounter });
    };
    

    // getMode: -> (U "print" "display" "write" "constructor")
    ToDomNodeParameters.prototype.getMode = function() {
        if (this.mode) { 
            return this.mode; 
        }
        return 'print';
    };

    ToDomNodeParameters.prototype.getDepth = function(x) {
        return this.depth;
    };

    ToDomNodeParameters.prototype.containsKey = function(x) {
        return this.cache.containsKey(x);
    };

    ToDomNodeParameters.prototype.seesOldCycle = function(x) {
        return this.cycles.containsKey(x);
    };

    ToDomNodeParameters.prototype.get = function(x) {
        return this.cache.get(x);
    };

    ToDomNodeParameters.prototype.remove = function(x) {
        return this.cache.remove(x);
    };

    ToDomNodeParameters.prototype.put = function(x, v) {
        return this.cache.put(x, v);
    };

    ToDomNodeParameters.prototype.recur = function(x) {
        return toDomNode(x, this.incrementDepth());
    };



    // rationalToDomNode: rational -> dom-node
    var rationalToDomNode = function(n) {
        var repeatingDecimalNode = document.createElement("span");
        var chunks = baselib.numbers.toRepeatingDecimal(baselib.numbers.numerator(n),
                                                        baselib.numbers.denominator(n),
                                                        {limit: 25});
        repeatingDecimalNode.appendChild($('<span/>').text(chunks[0] + '.').get(0));
        repeatingDecimalNode.appendChild($('<span/>').text(chunks[1]).get(0));
        if (chunks[2] === '...') {
            repeatingDecimalNode.appendChild(
                $('<span/>').text(chunks[2]).get(0));
        } else if (chunks[2] !== '0') {
            var overlineSpan = document.createElement("span");
            overlineSpan.style.textDecoration = 'overline';
            overlineSpan.appendChild(document.createTextNode(chunks[2]));
            repeatingDecimalNode.appendChild(overlineSpan);
        }


        var fractionalNode = document.createElement("span");
        var numeratorNode = document.createElement("sup");
        numeratorNode.appendChild(document.createTextNode(String(baselib.numbers.numerator(n))));
        var denominatorNode = document.createElement("sub");
        denominatorNode.appendChild(document.createTextNode(String(baselib.numbers.denominator(n))));
        fractionalNode.appendChild(numeratorNode);
        fractionalNode.appendChild($('<span/>').text('/').get(0));
        fractionalNode.appendChild(denominatorNode);

        
        var numberNode = document.createElement("span");
        numberNode.className = "wescheme-number Rational";
        numberNode.appendChild(repeatingDecimalNode);
        numberNode.appendChild(fractionalNode);
        fractionalNode.style['display'] = 'none';

        var showingRepeating = true;

        numberNode.onclick = function(e) {
            showingRepeating = !showingRepeating;
            repeatingDecimalNode.style['display'] = 
                (showingRepeating ? 'inline' : 'none');
            fractionalNode.style['display'] = 
                (!showingRepeating ? 'inline' : 'none');
        };
        numberNode.style['cursor'] = 'pointer';
        return numberNode;
    };


    // numberToDomNode: jsnum -> dom
    // Given a jsnum, produces a dom-node representation.
    var numberToDomNode = function(n, params) {
        var node;
        if (baselib.numbers.isExact(n)) {
            if (baselib.numbers.isInteger(n)) {
                node = document.createElement("span");
                node.className = "wescheme-number Integer";
                node.appendChild(document.createTextNode(n.toString()));
                return node;
            } else if (baselib.numbers.isRational(n)) {
                return rationalToDomNode(n);
            } else if (baselib.numbers.isComplex(n)) {
                node = document.createElement("span");
                node.className = "wescheme-number Complex";
                node.appendChild(document.createTextNode(n.toString()));
                return node;
            } else {
                node = document.createElement("span");
                node.className = "wescheme-number";
                node.appendChild(document.createTextNode(n.toString()));
                return node;
            }
        } else {
            node = document.createElement("span");
            node.className = "wescheme-number";
            node.appendChild(document.createTextNode(n.toString()));
            return node;
        }
    };


    var coerseToParams = function(params) {
        if (params === 'write') {
            params = new ToDomNodeParameters({'mode' : 'write'});
        } else if (params === 'print') {
            params = new ToDomNodeParameters({'mode' : 'print'});
        } else if (params === 'display') {
            params = new ToDomNodeParameters({'mode' : 'display'});
        } else if (params === 'constructor') {
            params = new ToDomNodeParameters({'mode' : 'constructor'});
        } else {
            params = params || new ToDomNodeParameters({'mode' : 'display'});
        } 
        return params;
    };


    // toDomNode: scheme-value -> dom-node
    var toDomNode = function(x, params) {
        var node, retval;
        params = coerseToParams(params);

        if (x === null) {
            node = document.createElement("span");
            node.appendChild(document.createTextNode("#<null>"));
            $(node).addClass("null");
            return node;
        }

        if (x === void(0)) {
            node = document.createElement("span");
            node.appendChild(document.createTextNode("#<undefined>"));
            $(node).addClass("undefined");
            return node;
        }

        if (baselib.numbers.isSchemeNumber(x)) {
            node = numberToDomNode(x, params);
            $(node).addClass("number");
            return node;
        }

        if (typeof(x) === 'string') {
            var wrapper = document.createElement("span");
            wrapper.style.whiteSpace = "pre";
            if (params.getMode() === 'write' || params.getMode() === 'print' || params.getMode() === 'constructor') {
                node = document.createTextNode(toWrittenString(x));
            } else {
                node = document.createTextNode(toDisplayedString(x));
            }
            wrapper.appendChild(node);
            $(wrapper).addClass("wescheme-string");
            return wrapper;
        }

        if (x === true || x === false) {
            node = document.createElement("span");
            node.appendChild(document.createTextNode(x ? "true" : "false"));
            $(node).addClass("wescheme-boolean");
            return node;
        }

        if (baselib.functions.isProcedure(x)) {
            node = document.createElement("span");
            node.appendChild(document.createTextNode('#<function:' + x.displayName + '>'));
            $(node).addClass("procedure");
            return node;
        }

        if (typeof(x) !== 'object') {
            node = document.createElement("span");
            node.appendChild(document.createTextNode(x.toString()));
            return node;
        }

        if (x.nodeType) {
            return x;
        }



        // Otherwise, we know the value is an object.
        
        // If we're along a print path with a loop, we need to stop
        // and return the key.
        if (params.seesOldCycle(x)) {
            node = document.createElement("span");
            node.appendChild(document.createTextNode("#" + params.cycles.get(x) + "#"));
            $(node).addClass("cycle");
            return node;
        }

        // If we see a fresh cycle, register it.
        if (params.containsKey(x)) {
            $('<span/>').text('#' + params.objectCounter +'=')
                    .prependTo(params.get(x));

            params.cycles.put(x, params.objectCounter);
            params.objectCounter++;

            node = document.createElement("span");
            node.appendChild(document.createTextNode("#" + params.cycles.get(x) + "#"));
            $(node).addClass("cycle");
            return node;
        }

        node = document.createElement("span");
        params.put(x, node);
        if (x.toDomNode) {
            node.appendChild(x.toDomNode(params));
        } else if (params.getMode() === 'write' && x.toWrittenString) {
            node.appendChild(document.createTextNode(
                x.toWrittenString(params)));
        } else if (params.getMode() === 'display' && x.toDisplayedString) {
            node.appendChild(document.createTextNode(
                x.toDisplayedString(params)));
        } else {
            node.appendChild(document.createTextNode(x.toString()));
        }
        params.remove(x);
        return node;
    };



    //////////////////////////////////////////////////////////////////////


    exports.ToDomNodeParameters = ToDomNodeParameters;

    exports.format = format;
    exports.toWrittenString = toWrittenString;
    exports.toDisplayedString = toDisplayedString;
    exports.toDomNode = toDomNode;

    exports.escapeString = escapeString;
}(this.plt.baselib, jQuery));
/*jslint vars: true, maxerr: 50, indent: 4 */


// Other miscellaneous constants
(function (baselib) {
    'use strict';
    var exports = {};
    baselib.constants = exports;


    var VoidValue = function () {};
    VoidValue.prototype.toString = function () {
        return "#<void>";
    };

    var VOID_VALUE = new VoidValue();


    var EofValue = function () {};
    EofValue.prototype.toString = function () {
        return "#<eof>";
    };

    var EOF_VALUE = new EofValue();


    exports.VOID_VALUE = VOID_VALUE;
    exports.EOF_VALUE = EOF_VALUE;
}(this.plt.baselib));/*jslint vars: true, maxerr: 50, indent: 4 */

// Numbers.
/*global jsnums*/
(function (baselib, jsnums) {
    'use strict';
    var exports = {};
    baselib.numbers = exports;



    // Set the numeric tower to raise errors through our mechanism.
    jsnums.onThrowRuntimeError = function(msg, x, y) {
        if (msg === '/: division by zero') {
            baselib.exceptions.raiseDivisionByZeroError(plt.runtime.currentMachine, msg);
        } else {
            baselib.exceptions.raiseContractError(plt.runtime.currentMachine, msg);
        }
    };


    var isNumber = jsnums.isSchemeNumber;
    var isReal = jsnums.isReal;
    var isRational = jsnums.isRational;
    var isComplex = isNumber;
    var isInteger = jsnums.isInteger;


    var isNatural = function (x) {
        return (jsnums.isExact(x) && isInteger(x) 
                && jsnums.greaterThanOrEqual(x, 0));
    };

    var isNonNegativeReal = function (x) {
        return isReal(x) && jsnums.greaterThanOrEqual(x, 0);
    };

    var isByte = function (x) {
        return (isNatural(x) && 
                jsnums.lessThan(x, 256));
    };


    // sign: number -> number
    var sign = function (x) {
        if (jsnums.isInexact(x)) {
            if (jsnums.greaterThan(x, 0)) {
                return jsnums.makeFloat(1);
            } else if (jsnums.lessThan(x, 0)) {
                return jsnums.makeFloat(-1);
            } else {
                return jsnums.makeFloat(0);
            }
        } else {
            if (jsnums.greaterThan(x, 0)) {
                return 1;
            } else if (jsnums.lessThan(x, 0)) {
                return -1;
            } else {
                return 0;
            }
        }
    };




    //////////////////////////////////////////////////////////////////////
    // Exports


    var hasOwnProperty = {}.hasOwnProperty;

    // We first re-export everything in jsnums.
    var prop;
    for (prop in jsnums) {
        if (hasOwnProperty.call(jsnums,prop)) {
            exports[prop] = jsnums[prop];
        }
    }

    exports.isNumber = jsnums.isSchemeNumber;
    exports.isReal = isReal;
    exports.isRational = isRational;
    exports.isComplex = isComplex;
    exports.isInteger = isInteger;
    exports.isNatural = isNatural;
    exports.isByte = isByte;
    exports.isNonNegativeReal = isNonNegativeReal;

    exports.sign = sign;


}(this.plt.baselib, jsnums));/*global $*/
/*jslint browser: true, unparam: true, vars: true, plusplus: true, maxerr: 50, indent: 4 */


// list structures (pairs, empty)
(function (baselib, $) {
    'use strict';
    var exports = {};
    baselib.lists = exports;


    var Empty = function () {
    };
    Empty.EMPTY = new Empty();
    var EMPTY = Empty.EMPTY;



    Empty.prototype.equals = function (other, aUnionFind) {
        return other instanceof Empty;
    };

    Empty.prototype.hashCode = function(depth) {
        return baselib.hashes.getEqualHashCode("empty");
    };


    Empty.prototype.reverse = function () {
        return this;
    };

    Empty.prototype.toWrittenString = function (cache) { return "empty"; };
    Empty.prototype.toDisplayedString = function (cache) { return "empty"; };
    Empty.prototype.toString = function (cache) { return "()"; };

    Empty.prototype.toDomNode = function(params) {
        if (params.getMode() === "display") {
            return $("<span/>").text("()").get(0);
        } else if (params.getMode() === "write") {
            return $("<span/>").text("()").get(0);
        } else if (params.getMode() === "print") {
            if (params.getDepth() === 0) {
                return $("<span/>").text("'()").get(0);
            } else {
                return $("<span/>").text("()").get(0);
            }
        } else if (params.getMode() === "constructor") {
            return $("<span/>").text("(list)").get(0);
        } else {
            return $("<span/>").text("()").get(0);
        }
    };

    // Empty.append: (listof X) -> (listof X)
    Empty.prototype.append = function (b) {
        return b;
    };



    //////////////////////////////////////////////////////////////////////

    // Cons Pairs

    var Cons = function (first, rest) {
        this.first = first;
        this.rest = rest;
    };

    var makePair = function (first, rest) {
        return new Cons(first, rest);
    };

    Cons.prototype.reverse = function () {
        var lst = this;
        var ret = EMPTY;
        while (lst !== EMPTY) {
            ret = makePair(lst.first, ret);
            lst = lst.rest;
        }
        return ret;
    };

    // FIXME: can we reduce the recursion on this?
    Cons.prototype.equals = function (other, aUnionFind) {
        if (!(other instanceof Cons)) {
            return false;
        }
        return (baselib.equality.equals(this.first, other.first, aUnionFind) &&
                baselib.equality.equals(this.rest, other.rest, aUnionFind));
    };

    Cons.prototype.hashCode = function(depth) {
        var k = baselib.hashes.getEqualHashCode("Cons");
        k += baselib.hashes.getEqualHashCode(this.first, depth);
        k = baselib.hashes.hashMix(k);
        k += baselib.hashes.getEqualHashCode(this.rest, depth);
        k = baselib.hashes.hashMix(k);
        return k;
    };



    // Cons.append: (listof X) -> (listof X)
    Cons.prototype.append = function (b) {
        if (b === EMPTY) {
            return this;
        }
        var ret = b;
        var lst = this.reverse();
        while (lst !== EMPTY) {
            ret = makePair(lst.first, ret);
            lst = lst.rest;
        }
        return ret;
    };


    Cons.prototype.toWrittenString = function (cache) {
        cache.put(this, true);
        var texts = [];
        var p = this;
        while (p instanceof Cons) {
            texts.push(baselib.format.toWrittenString(p.first, cache));
            p = p.rest;
            if (typeof (p) === 'object' && cache.containsKey(p)) {
                break;
            }
        }
        if (p !== EMPTY) {
            texts.push('.');
            texts.push(baselib.format.toWrittenString(p, cache));
        }
        return "(" + texts.join(" ") + ")";
    };

    Cons.prototype.toString = Cons.prototype.toWrittenString;

    Cons.prototype.toDisplayedString = function (cache) {
        cache.put(this, true);
        var texts = [];
        var p = this;
        while (p instanceof Cons) {
            texts.push(baselib.format.toDisplayedString(p.first, cache));
            p = p.rest;
            if (typeof (p) === 'object' && cache.containsKey(p)) {
                break;
            }
        }
        if (p !== EMPTY) {
            texts.push('.');
            texts.push(baselib.format.toDisplayedString(p, cache));
        }
        return "(" + texts.join(" ") + ")";
    };



    Cons.prototype.toDomNode = function (params) {
        var node;

        var subelts = [], dottedPair = false, i;
        var p = this;
        while (p instanceof Cons) {
            subelts.push(params.recur(p.first));
            p = p.rest;
            if (typeof (p) === 'object' && params.containsKey(p)) {
                break;
            }
        }
        if (p !== EMPTY) {
            dottedPair = true;
            subelts.push(params.recur(p));
        }

        if (params.getMode() === 'constructor') {
            if (dottedPair) {
                node = subelts[subelts.length - 1];
                for (i = subelts.length - 2; i >= 0; i--) {
                    node = $('<span/>')
                        .append($('<span/>').text("(").addClass('lParen'))
                        .append($('<span/>').text("cons"))
                        .append(" ")
                        .append(subelts[i])
                        .append(" ")
                        .append(node)
                        .append($('<span/>').text(")").addClass('rParen'))
                        .get(0);
                }
                return node;
            } else {
                node = $("<span/>").addClass("wescheme-cons");
                node.append($('<span/>').text("(").addClass('lParen'));
                node.append($('<span/>').text("list"));
                node.append(" ");
                node.append(subelts[0]);
                for (i = 1; i < subelts.length; i++) {
                    node.append(" ");
                    node.append(subelts[i]);
                }
                node.append($('<span/>').text(')').addClass('rParen'));
                return node.get(0);
            }
        }

        node = $('<span/>').addClass("wescheme-cons");
        if (params.getMode() === 'print') {
            node.append($("<span/>").text("'"));
        }
        node.append($('<span/>').text('(').addClass('lParen'))
        node.append(subelts[0]);
        if (subelts.length > 1) {
            for (i = 1; i < subelts.length - 1; i++) {
                node.append(" ");
                node.append(subelts[i]);
            }
            if (dottedPair) {
                node.append(" ");
                node.append(".");
            }
            node.append(" ");
            node.append(subelts[subelts.length - 1]);
        }
        node.append($('<span/>').text(")").addClass('rParen'));
        return node.get(0);
    };


    var isPair = function (x) { return x instanceof Cons; };
    var isEmpty = function (x) { return x === EMPTY; };



    var makeList = function () {
        var result = EMPTY, i;
        for (i = arguments.length - 1; i >= 0; i--) {
            result = makePair(arguments[i], result);
        }
        return result;
    };


    var arrayToList = function (arr) {
        var result = EMPTY, i;
        for (i = arr.length -1; i >= 0; i--) {
            result = makePair(arr[i], result);
        }
        return result;
    };


    // Coerse a list back into a JavaScript array.
    var listToArray = function (lst) {
        var result = [];
        while (lst !== EMPTY) {
            result.push(lst.first);
            lst = lst.rest;
        }
        return result;
    };


    // isList: Any -> Boolean
    // Returns true if x is a list (a chain of pairs terminated by EMPTY).
    var isList = function (x) {
        var tortoise, hare;
        tortoise = hare = x;
        if (hare === EMPTY) { 
            return true; 
        }
        if (!(hare instanceof Cons)) { return false; }
        while (true) {
            // Loop invariant: at the beginning of the loop, both tortoise
            // and hare should be pointing to a cons cell.
            tortoise = tortoise.rest; 
            hare = hare.rest;
            if (hare instanceof Cons) { 
                // optimization to get amortized linear time isList:
                if (hare._isList !== void(0)) { 
                    tortoise._isList = hare._isList; return hare._isList; 
                }
                hare = hare.rest; 
                // optimization to get amortized linear time isList:
                if (hare instanceof Cons && hare._isList !== void(0)) { 
                    tortoise._isList = hare._isList; return hare._isList; 
                }
            }
            if (hare === EMPTY) { 
                // optimization to get amortized linear time isList:
                tortoise._isList = true;
                return true; 
            }
            if (tortoise === hare) {
                tortoise._isList = false;
                return false; 
            }
            if (!(hare instanceof Cons)) { 
                tortoise._isList = false;
                return false; 
            }
        }
    };



    var reverse = function (lst) {
        var rev = EMPTY;
        while (lst !== EMPTY) {
            rev = makePair(lst.first, rev);
            lst = lst.rest;
        }
        return rev;
    };


    var length = function (lst) {
        var len = 0;
        while (lst !== EMPTY) {
            len++;
            lst = lst.rest;
        }
        return len;
    };


    var listRef = function (lst, n) {
        var i;
        for (i = 0; i < n; i++) {
            lst = lst.rest;
        }
        return lst.first;
    };



    //////////////////////////////////////////////////////////////////////

    exports.EMPTY = EMPTY;
    exports.Empty = Empty;
    exports.Cons = Cons;
    exports.isPair = isPair;
    exports.isList = isList;
    exports.isEmpty = isEmpty;
    exports.makePair = makePair;
    exports.makeList = makeList;
    exports.reverse = reverse;
    exports.length = length;
    exports.listRef = listRef;
    exports.listToArray = listToArray;
    exports.arrayToList = arrayToList;

}(this.plt.baselib, jQuery));
// vectors
/*jslint devel: false, browser: true, vars: true, plusplus: true, maxerr: 500, indent: 4 */
(function (baselib, $) {
    "use strict";
    var exports = {};
    baselib.vectors = exports;



    var Vector = function (initialElements) {
        this.elts = initialElements;
        this.mutable = true;
    };

    Vector.makeInstance = function (elts) {
        return new Vector(elts);
    };

    Vector.prototype.length = function () {
        return this.elts.length;
    };

    Vector.prototype.ref = function (k) {
        return this.elts[k];
    };

    Vector.prototype.set = function (k, v) {
        this.elts[k] = v;
    };

    Vector.prototype.equals = function (other, aUnionFind) {
        var i;
        if (other instanceof Vector) {
            if (other.length() !== this.length()) {
                return false;
            }
            for (i = 0; i <  this.length(); i++) {
                if (!(baselib.equality.equals(this.elts[i], other.elts[i], aUnionFind))) {
                    return false;
                }
            }
            return true;
        } else {
            return false;
        }
    };

    Vector.prototype.hashCode = function(depth) {
        var k = baselib.hashes.getEqualHashCode("Vector");
        var i;
        k = baselib.hashes.hashMix(k);
        for (i = 0; i < this.elts.length; i++) {
            k += baselib.hashes.getEqualHashCode(this.elts[i], depth);
            k = baselib.hashes.hashMix(k);
        }
        return k;
    };

    Vector.prototype.toList = function () {
        var ret = baselib.lists.EMPTY, i;
        for (i = this.length() - 1; i >= 0; i--) {
            ret = baselib.lists.makePair(this.elts[i], ret);           
        }       
        return ret;
    };

    Vector.prototype.toWrittenString = function (cache) {
        var texts = [], i;
        cache.put(this, true);
        for (i = 0; i < this.length(); i++) {
            texts.push(baselib.format.toWrittenString(this.ref(i), cache));
        }
        return "#(" + texts.join(" ") + ")";
    };

    Vector.prototype.toDisplayedString = function (cache) {
        var texts = [], i;
        cache.put(this, true);
        for (i = 0; i < this.length(); i++) {
            texts.push(baselib.format.toDisplayedString(this.ref(i), cache));
        }
        return "#(" + texts.join(" ") + ")";
    };

    Vector.prototype.toDomNode = function (params) {
        var node = $('<span/>'), i;
        if (params.getMode() === 'constructor') {
            node.append($('<span/>').text('(').addClass('lParen'));
            node.append($('<span/>').text('vector'));
            for (i = 0; i < this.length(); i++) {
                node.append(" ");
                node.append(params.recur(this.ref(i)));
            }
            node.append($('<span/>').text(')').addClass('rParen'));
        } else {
            node.append($('<span/>').text('#(').addClass('lParen'));
            for (i = 0; i < this.length(); i++) {
                node.append(params.recur(this.ref(i)));
                if (i !== this.length() - 1) {
                    node.append(" ");
                }
            }
            node.append($('<span/>').text(')').addClass('rParen'));
        }
        return node.get(0);
    };


    var isVector = function (x) { return x instanceof Vector; };

    // makeVector: x ... -> vector
    var makeVector = Vector.makeInstance;

    var makeVectorImmutable = function (elts) {
        var v = Vector.makeInstance(elts);
        v.mutable = false;
        return v;
    };



    //////////////////////////////////////////////////////////////////////

    exports.Vector = Vector;
    exports.isVector = isVector;
    exports.makeVector = makeVector;
    exports.makeVectorImmutable = makeVectorImmutable;


}(this.plt.baselib, jQuery));
// Single characters
(function(baselib, $) {
    var exports = {};
    baselib.chars = exports;


    // Chars
    // Char: string -> Char
    var Char = function(val){
        this.val = val;
    };
    // The characters less than 256 must be eq?, according to the
    // documentation:
    // http://docs.racket-lang.org/reference/characters.html
    var _CharCache = {};
    for (var i = 0; i < 256; i++) {
        _CharCache[String.fromCharCode(i)] = new Char(String.fromCharCode(i));
    }
    
    // makeInstance: 1-character string -> Char  
    Char.makeInstance = function(val){
        if (_CharCache[val]) {
	    return _CharCache[val];
        }
        return new Char(val);
    };

    Char.prototype.toString = function(cache) {
	var code = this.val.charCodeAt(0);
	var returnVal;
	switch (code) {
	case 0: returnVal = '#\\nul'; break;
	case 8: returnVal = '#\\backspace'; break;
	case 9: returnVal = '#\\tab'; break;
	case 10: returnVal = '#\\newline'; break;
	case 11: returnVal = '#\\vtab'; break;
	case 12: returnVal = '#\\page'; break;
	case 13: returnVal = '#\\return'; break;
	case 20: returnVal = '#\\space'; break;
	case 127: returnVal = '#\\rubout'; break;
	default: if (code >= 32 && code <= 126) {
	    returnVal = ("#\\" + this.val);
	}
	    else {
		var numStr = code.toString(16).toUpperCase();
		while (numStr.length < 4) {
		    numStr = '0' + numStr;
		}
		returnVal = ('#\\u' + numStr);
	    }
	    break;
	}
	return returnVal;
    };

    Char.prototype.toWrittenString = Char.prototype.toString;

    Char.prototype.toDisplayedString = function (cache) {
        return this.val;
    };

    Char.prototype.toDomNode = function(params) {
        return $('<span/>')
            .text(this.toString())
            .addClass('wescheme-character')
            .get(0);
    };

    Char.prototype.getValue = function() {
        return this.val;
    };

    Char.prototype.equals = function(other, aUnionFind){
        return other instanceof Char && this.val == other.val;
    };

    Char.prototype.hashCode = function(depth) {
        var k = baselib.hashes.getEqualHashCode('Char');
        k += this.val.charCodeAt(0);
        k = baselib.hashes.hashMix(k);
        return k;
    };


    exports.Char = Char;
    exports.makeChar = Char.makeInstance;
    exports.isChar = plt.baselib.makeClassPredicate(Char);


})(this['plt'].baselib, jQuery);
/*jslint devel: false, browser: true, unparam: true, vars: true, plusplus: true, maxerr: 500, indent: 4 */
// Structure types
(function (baselib,$) {
    "use strict";
    var exports = {};
    baselib.symbols = exports;


    //////////////////////////////////////////////////////////////////////
    
    // Symbols

    //////////////////////////////////////////////////////////////////////
    var Symbol = function (val) {
        this.val = val;
    };

    var symbolCache = new baselib.Dict();

    // makeSymbol: string -> Symbol.
    // Interns a symbol.
    var makeSymbol = function (val) {
        // To ensure that we can eq? symbols with equal values.
        if (!(symbolCache.has(val))) {
            symbolCache.set(val, new Symbol(val));
        }
        return symbolCache.get(val);
    };
    
    Symbol.prototype.equals = function (other, aUnionFind) {
        return other instanceof Symbol &&
            this.val === other.val;
    };

    Symbol.prototype.hashCode = function(depth) {
        var k = baselib.hashes.getEqualHashCode("Symbol");
        k = baselib.hashes.hashMix(k);
        k += baselib.hashes.getEqualHashCode(this.val);
        k = baselib.hashes.hashMix(k);
        return k;
    };
    

    Symbol.prototype.toString = function (cache) {
        return this.val;
    };

    Symbol.prototype.toWrittenString = function (cache) {
        return this.val;
    };

    Symbol.prototype.toDisplayedString = function (cache) {
        return this.val;
    };

    Symbol.prototype.toDomNode = function(params) {
        if (params.getMode() === 'write') {
            return $("<span/>").text(this.val).addClass('wescheme-symbol').get(0);
        }
        if (params.getMode() === 'display') {
            return $("<span/>").text(this.val).addClass('wescheme-symbol').get(0);
        }
        if (params.getMode() === 'print') {
            if (params.getDepth() === 0) {
                return $("<span/>").text("'" + this.val).addClass('wescheme-symbol').get(0);
            } else {
                return $("<span/>").text(this.val).addClass('wescheme-symbol').get(0);
            }
        }
        if (params.getMode() === 'constructor') {
            return $("<span/>").text("'" + this.val).addClass('wescheme-symbol').get(0);
        }

        return $("<span/>").text(this.val).addClass('wescheme-symbol').get(0);
    };
    


    var isSymbol = function (x) { return x instanceof Symbol; };




    //////////////////////////////////////////////////////////////////////

    exports.Symbol = Symbol;
    exports.makeSymbol = makeSymbol;
    exports.isSymbol = isSymbol;

}(this.plt.baselib, jQuery));
// Hardcoded parameters.
(function(baselib) {
    'use strict';
    var exports = {};
    baselib.paramz = exports;

    // The parameter keys here must be uninterned symbols, so we explicitly
    // call the symbol constructor here.
    var exceptionHandlerKey = new baselib.symbols.Symbol("exnh");
    var parameterizationKey = new baselib.symbols.Symbol("paramz");
    var breakEnabledKey = new baselib.symbols.Symbol("break-on?");

    exports.exceptionHandlerKey = exceptionHandlerKey;
    exports.parameterizationKey = parameterizationKey;
    exports.breakEnabledKey = breakEnabledKey;
    
})(this['plt'].baselib);
/*jslint browser: false, unparam: true, vars: true, white: true, nomen: true, plusplus: true, maxerr: 50, indent: 4 */


// Strings

// Strings are either mutable or immutable.  immutable strings are represented
// as regular JavaScript strings.  Mutable ones are represented as instances
// of the Str class.

(function (baselib) {
    'use strict';
    var exports = {};

    baselib.strings = exports;


    // chars: arrayof string
    // Precondition: each string must only be 1 character long or bad things
    // happen.
    var Str = function (chars) {
	this.chars = chars;
	this.length = chars.length;
	this.mutable = true;
    };

    Str.makeInstance = function (chars) {
	return new Str(chars);
    };

    Str.fromString = function (s) {
	return Str.makeInstance(s.split(""));
    };

    Str.prototype.toString = function () {
	return this.chars.join("");
    };

    var replaceUnprintableStringChars = function (s) {
	var ret = [], i;
	for (i = 0; i < s.length; i++) {
	    var val = s.charCodeAt(i);
	    switch(val) {
	    case 7: ret.push('\\a'); break;
	    case 8: ret.push('\\b'); break;
	    case 9: ret.push('\\t'); break;
	    case 10: ret.push('\\n'); break;
	    case 11: ret.push('\\v'); break;
	    case 12: ret.push('\\f'); break;
	    case 13: ret.push('\\r'); break;
	    case 34: ret.push('\\"'); break;
	    case 92: ret.push('\\\\'); break;
	    default: 
                if (val >= 32 && val <= 126) {
		    ret.push( s.charAt(i) );
	        }
		else {
		    var numStr = val.toString(16).toUpperCase();
		    while (numStr.length < 4) {
			numStr = '0' + numStr;
		    }
		    ret.push('\\u' + numStr);
		}
		break;
	    }
	}
	return ret.join('');
    };

    var escapeString = function (s) {
        return '"' + replaceUnprintableStringChars(s) + '"';
    };

    Str.prototype.toWrittenString = function (cache) {
        return escapeString(this.toString());
    };

    Str.prototype.toDisplayedString = Str.prototype.toString;

    Str.prototype.toDomNode = function(params) {
        return $("<span/>")
            .text(escapeString(this.toString()))
            .addClass('wescheme-string')
            .get(0);
    };


    Str.prototype.copy = function () {
	return Str.makeInstance(this.chars.slice(0));
    };

    Str.prototype.substring = function (start, end) {
	if (end === null || end === void(0)) {
	    end = this.length;
	}
	return Str.makeInstance( this.chars.slice(start, end) );
    };

    Str.prototype.charAt = function (index) {
	return this.chars[index];
    };

    Str.prototype.charCodeAt = function (index) {
	return this.chars[index].charCodeAt(0);
    };

    Str.prototype.replace = function (expr, newStr) {
	return Str.fromString(this.toString().replace(expr, newStr) );
    };


    Str.prototype.equals = function (other, aUnionFind) {
	if ( !(other instanceof Str || typeof(other) === 'string') ) {
	    return false;
	}
	return this.toString() === other.toString();
    };

    Str.prototype.hashCode = function(depth) {
        return baselib.hashes.getEqualHashCode(this.toString());
    };


    Str.prototype.set = function (i, c) {
	this.chars[i] = c;
    };

    Str.prototype.toUpperCase = function () {
	return Str.fromString(this.chars.join("").toUpperCase() );
    };

    Str.prototype.toLowerCase = function () {
	return Str.fromString(this.chars.join("").toLowerCase() );
    };

    Str.prototype.match = function (regexpr) {
	return this.toString().match(regexpr);
    };



    var isString = function (s) {
	return (typeof s === 'string' || 
                s instanceof Str);
    };

    var isMutableString = baselib.makeClassPredicate(Str);



    exports.Str = Str;
    exports.escapeString = escapeString;
    exports.isString = isString;
    exports.isMutableString = isMutableString;
    exports.makeMutableString = Str.makeInstance;

}(this.plt.baselib));
/*jslint unparam: true, vars: true, white: true, plusplus: true, maxerr: 50, indent: 4 */



(function(baselib) {
    'use strict';
    var exports = {};
    baselib.bytes = exports;

    // Bytes

    var Bytes = function(bts, mutable) {
        // bytes: arrayof [0-255]
        this.bytes = bts;
        this.mutable = (mutable === void(0)) ? false : mutable;
    };

    Bytes.prototype.get = function(i) {
	return this.bytes[i];
    };

    Bytes.prototype.set = function(i, b) {
	if (this.mutable) {
	    this.bytes[i] = b;
	}
    };

    Bytes.prototype.length = function() {
	return this.bytes.length;
    };

    Bytes.prototype.copy = function(mutable) {
	return new Bytes(this.bytes.slice(0), mutable);
    };

    Bytes.prototype.subbytes = function(start, end) {
	if (end === null || end === void(0)) {
	    end = this.bytes.length;
	}
	return new Bytes( this.bytes.slice(start, end), true );
    };


    Bytes.prototype.equals = function(other) {
        if (! (other instanceof Bytes)) {
	    return false;
        }
        if (this.bytes.length !== other.bytes.length) {
	    return false;
        }
        var A = this.bytes;
        var B = other.bytes;
        var n = this.bytes.length;
        var i;
        for (i = 0; i < n; i++) {
	    if (A[i] !== B[i]) {
	        return false;
            }
        }
        return true;
    };

    Bytes.prototype.hashCode = function(depth) {
        var i;
        var k = baselib.hashes.getEqualHashCode('Bytes');
        for (i = 0; i < this.bytes.length; i++) {
            k += this.bytes[i];
            k = baselib.hashes.hashMix(k);
        }
        return k;
    };


    Bytes.prototype.toString = function(cache) {
	var ret = [], i;
	for (i = 0; i < this.bytes.length; i++) {
	    ret.push(String.fromCharCode(this.bytes[i]));
	}

	return ret.join('');
    };

    Bytes.prototype.toDisplayedString = Bytes.prototype.toString;

    var escapeByte = function(aByte) {
	var ret = [];
	var returnVal;
	switch(aByte) {
	case 7: returnVal = '\\a'; break;
	case 8: returnVal = '\\b'; break;
	case 9: returnVal = '\\t'; break;
	case 10: returnVal = '\\n'; break;
	case 11: returnVal = '\\v'; break;
	case 12: returnVal = '\\f'; break;
	case 13: returnVal = '\\r'; break;
	case 34: returnVal = '\\"'; break;
	case 92: returnVal = '\\\\'; break;
	default: if (aByte >= 32 && aByte <= 126) {
	    returnVal = String.fromCharCode(aByte);
	}
	    else {
		ret.push( '\\' + aByte.toString(8) );
	    }
	    break;
	}
	return returnVal;
    };

    Bytes.prototype.toWrittenString = function() {
	var ret = ['#"'], i;
	for (i = 0; i < this.bytes.length; i++) {
	    ret.push(escapeByte(this.bytes[i]));
	}
	ret.push('"');
	return ret.join('');
    };

    var makeBytes = function(chars) {
        return new Bytes(chars);
    };

    var makeBytesFromBase64 = function(byteString) {
        return new Bytes(Base64.decode(byteString));
    };


    var isBytes = baselib.makeClassPredicate(Bytes);


    exports.Bytes = Bytes;
    exports.makeBytes = makeBytes;
    exports.makeBytesFromBase64 = makeBytesFromBase64;
    exports.isBytes = isBytes;


}(this.plt.baselib));(function() {
'use strict';/**
 * Copyright 2010 Tim Down.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

/**
 * jshashtable
 *
 * jshashtable is a JavaScript implementation of a hash table. It creates a single constructor function called Hashtable
 * in the global scope.
 *
 * Author: Tim Down <tim@timdown.co.uk>
 * Version: 2.1
 * Build date: 21 March 2010
 * Website: http://www.timdown.co.uk/jshashtable
 */

var Hashtable = (function() {
	var FUNCTION = "function";

	var arrayRemoveAt = (typeof Array.prototype.splice == FUNCTION) ?
		function(arr, idx) {
			arr.splice(idx, 1);
		} :

		function(arr, idx) {
			var itemsAfterDeleted, i, len;
			if (idx === arr.length - 1) {
				arr.length = idx;
			} else {
				itemsAfterDeleted = arr.slice(idx + 1);
				arr.length = idx;
				for (i = 0, len = itemsAfterDeleted.length; i < len; ++i) {
					arr[idx + i] = itemsAfterDeleted[i];
				}
			}
		};

	function hashObject(obj) {
		var hashCode;
		if (typeof obj == "string") {
			return obj;
		} else if (typeof obj.hashCode == FUNCTION) {
			// Check the hashCode method really has returned a string
			hashCode = obj.hashCode();
			return (typeof hashCode == "string") ? hashCode : hashObject(hashCode);
		} else if (typeof obj.toString == FUNCTION) {
			return obj.toString();
		} else {
			try {
				return String(obj);
			} catch (ex) {
				// For host objects (such as ActiveObjects in IE) that have no toString() method and throw an error when
				// passed to String()
				return Object.prototype.toString.call(obj);
			}
		}
	}

	function equals_fixedValueHasEquals(fixedValue, variableValue) {
		return fixedValue.equals(variableValue);
	}

	function equals_fixedValueNoEquals(fixedValue, variableValue) {
		return (typeof variableValue.equals == FUNCTION) ?
			   variableValue.equals(fixedValue) : (fixedValue === variableValue);
	}

	function createKeyValCheck(kvStr) {
		return function(kv) {
			if (kv === null) {
				throw new Error("null is not a valid " + kvStr);
			} else if (typeof kv == "undefined") {
				throw new Error(kvStr + " must not be undefined");
			}
		};
	}

	var checkKey = createKeyValCheck("key"), checkValue = createKeyValCheck("value");

	/*----------------------------------------------------------------------------------------------------------------*/

	function Bucket(hash, firstKey, firstValue, equalityFunction) {
        this[0] = hash;
		this.entries = [];
		this.addEntry(firstKey, firstValue);

		if (equalityFunction !== null) {
			this.getEqualityFunction = function() {
				return equalityFunction;
			};
		}
	}

	var EXISTENCE = 0, ENTRY = 1, ENTRY_INDEX_AND_VALUE = 2;

	function createBucketSearcher(mode) {
		return function(key) {
			var i = this.entries.length, entry, equals = this.getEqualityFunction(key);
			while (i--) {
				entry = this.entries[i];
				if ( equals(key, entry[0]) ) {
					switch (mode) {
						case EXISTENCE:
							return true;
						case ENTRY:
							return entry;
						case ENTRY_INDEX_AND_VALUE:
							return [ i, entry[1] ];
					}
				}
			}
			return false;
		};
	}

	function createBucketLister(entryProperty) {
		return function(aggregatedArr) {
			var startIndex = aggregatedArr.length;
			for (var i = 0, len = this.entries.length; i < len; ++i) {
				aggregatedArr[startIndex + i] = this.entries[i][entryProperty];
			}
		};
	}

	Bucket.prototype = {
		getEqualityFunction: function(searchValue) {
			return (typeof searchValue.equals == FUNCTION) ? equals_fixedValueHasEquals : equals_fixedValueNoEquals;
		},

		getEntryForKey: createBucketSearcher(ENTRY),

		getEntryAndIndexForKey: createBucketSearcher(ENTRY_INDEX_AND_VALUE),

		removeEntryForKey: function(key) {
			var result = this.getEntryAndIndexForKey(key);
			if (result) {
				arrayRemoveAt(this.entries, result[0]);
				return result[1];
			}
			return null;
		},

		addEntry: function(key, value) {
			this.entries[this.entries.length] = [key, value];
		},

		keys: createBucketLister(0),

		values: createBucketLister(1),

		getEntries: function(entries) {
			var startIndex = entries.length;
			for (var i = 0, len = this.entries.length; i < len; ++i) {
				// Clone the entry stored in the bucket before adding to array
				entries[startIndex + i] = this.entries[i].slice(0);
			}
		},

		containsKey: createBucketSearcher(EXISTENCE),

		containsValue: function(value) {
			var i = this.entries.length;
			while (i--) {
				if ( value === this.entries[i][1] ) {
					return true;
				}
			}
			return false;
		}
	};

	/*----------------------------------------------------------------------------------------------------------------*/

	// Supporting functions for searching hashtable buckets

	function searchBuckets(buckets, hash) {
		var i = buckets.length, bucket;
		while (i--) {
			bucket = buckets[i];
			if (hash === bucket[0]) {
				return i;
			}
		}
		return null;
	}

	function getBucketForHash(bucketsByHash, hash) {
		var bucket = bucketsByHash[hash];

		// Check that this is a genuine bucket and not something inherited from the bucketsByHash's prototype
		return ( bucket && (bucket instanceof Bucket) ) ? bucket : null;
	}

	/*----------------------------------------------------------------------------------------------------------------*/

	function Hashtable(hashingFunctionParam, equalityFunctionParam) {
		var that = this;
		var buckets = [];
		var bucketsByHash = {};

		var hashingFunction = (typeof hashingFunctionParam == FUNCTION) ? hashingFunctionParam : hashObject;
		var equalityFunction = (typeof equalityFunctionParam == FUNCTION) ? equalityFunctionParam : null;

		this.put = function(key, value) {
			checkKey(key);
			checkValue(value);
			var hash = hashingFunction(key), bucket, bucketEntry, oldValue = null;

			// Check if a bucket exists for the bucket key
			bucket = getBucketForHash(bucketsByHash, hash);
			if (bucket) {
				// Check this bucket to see if it already contains this key
				bucketEntry = bucket.getEntryForKey(key);
				if (bucketEntry) {
					// This bucket entry is the current mapping of key to value, so replace old value and we're done.
					oldValue = bucketEntry[1];
					bucketEntry[1] = value;
				} else {
					// The bucket does not contain an entry for this key, so add one
					bucket.addEntry(key, value);
				}
			} else {
				// No bucket exists for the key, so create one and put our key/value mapping in
				bucket = new Bucket(hash, key, value, equalityFunction);
				buckets[buckets.length] = bucket;
				bucketsByHash[hash] = bucket;
			}
			return oldValue;
		};

		this.get = function(key) {
			checkKey(key);

			var hash = hashingFunction(key);

			// Check if a bucket exists for the bucket key
			var bucket = getBucketForHash(bucketsByHash, hash);
			if (bucket) {
				// Check this bucket to see if it contains this key
				var bucketEntry = bucket.getEntryForKey(key);
				if (bucketEntry) {
					// This bucket entry is the current mapping of key to value, so return the value.
					return bucketEntry[1];
				}
			}
			return null;
		};

		this.containsKey = function(key) {
			checkKey(key);
			var bucketKey = hashingFunction(key);

			// Check if a bucket exists for the bucket key
			var bucket = getBucketForHash(bucketsByHash, bucketKey);

			return bucket ? bucket.containsKey(key) : false;
		};

		this.containsValue = function(value) {
			checkValue(value);
			var i = buckets.length;
			while (i--) {
				if (buckets[i].containsValue(value)) {
					return true;
				}
			}
			return false;
		};

		this.clear = function() {
			buckets.length = 0;
			bucketsByHash = {};
		};

		this.isEmpty = function() {
			return !buckets.length;
		};

		var createBucketAggregator = function(bucketFuncName) {
			return function() {
				var aggregated = [], i = buckets.length;
				while (i--) {
					buckets[i][bucketFuncName](aggregated);
				}
				return aggregated;
			};
		};

		this.keys = createBucketAggregator("keys");
		this.values = createBucketAggregator("values");
		this.entries = createBucketAggregator("getEntries");

		this.remove = function(key) {
			checkKey(key);

			var hash = hashingFunction(key), bucketIndex, oldValue = null;

			// Check if a bucket exists for the bucket key
			var bucket = getBucketForHash(bucketsByHash, hash);

			if (bucket) {
				// Remove entry from this bucket for this key
				oldValue = bucket.removeEntryForKey(key);
				if (oldValue !== null) {
					// Entry was removed, so check if bucket is empty
					if (!bucket.entries.length) {
						// Bucket is empty, so remove it from the bucket collections
						bucketIndex = searchBuckets(buckets, hash);
						arrayRemoveAt(buckets, bucketIndex);
						delete bucketsByHash[hash];
					}
				}
			}
			return oldValue;
		};

		this.size = function() {
			var total = 0, i = buckets.length;
			while (i--) {
				total += buckets[i].entries.length;
			}
			return total;
		};

		this.each = function(callback) {
			var entries = that.entries(), i = entries.length, entry;
			while (i--) {
				entry = entries[i];
				callback(entry[0], entry[1]);
			}
		};

		this.putAll = function(hashtable, conflictCallback) {
			var entries = hashtable.entries();
			var entry, key, value, thisValue, i = entries.length;
			var hasConflictCallback = (typeof conflictCallback == FUNCTION);
			while (i--) {
				entry = entries[i];
				key = entry[0];
				value = entry[1];

				// Check for a conflict. The default behaviour is to overwrite the value for an existing key
				if ( hasConflictCallback && (thisValue = that.get(key)) ) {
					value = conflictCallback(key, thisValue, value);
				}
				that.put(key, value);
			}
		};

		this.clone = function() {
			var clone = new Hashtable(hashingFunctionParam, equalityFunctionParam);
			clone.putAll(that);
			return clone;
		};
	}

	return Hashtable;
})();/*jslint plusplus: true, vars: true, white: true, nomen: true, maxerr: 50, indent: 4 */

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
}());/*jslint unparam: true, vars: true, white: true, newcap: true, nomen: true, plusplus: true, maxerr: 50, indent: 4 */

/*global window,Hashtable*/

// Mutable hashtables.


(function (baselib, Hashtable) {
    'use strict';
    var exports = {};

    baselib.hashes = exports;


    
    var _eqHashCodeCounter = 0;
    var makeEqHashCode = function () {
        _eqHashCodeCounter++;
        return String(_eqHashCodeCounter);
    };


    // getEqHashCode: any -> string
    // Given a value, produces a hashcode appropriate for eq.
    var getEqHashCode = function (x) {
        if (typeof (x) === 'string') {
            return x;
        }
        if (typeof (x) === 'number') {
            return String(x);
        }
        if (x && !x._eqHashCode) {
            x._eqHashCode = makeEqHashCode();
        }
        if (x && x._eqHashCode) {
            return x._eqHashCode;
        }
        return '';
    };


    // getEqvHashCode: any -> string
    var getEqvHashCode = function (x) {
        if (baselib.numbers.isNumber(x)) {
            return baselib.numbers.toFixnum(x);
        }
        if (baselib.chars.isChar(x)) {
            return x.val;
        } 
        return getEqHashCode(x);
    };


    var eq = function (x, y) { return x === y; };
    var eqv = baselib.equality.eqv;
    var equal = function (x, y) {
        return baselib.equality.equals(x, y, new baselib.UnionFind()); 
    };


    // Creates a low-level hashtable, following the interface of 
    // http://www.timdown.co.uk/jshashtable/
    var makeLowLevelEqHash = function () {
        return new Hashtable(getEqHashCode,
                             function (x, y) { return x === y; });
    };


    var makeEqHashtable = function() { 
        return new WhalesongHashtable(
            "hasheq",
            getEqHashCode,
            eq,
            new Hashtable(getEqHashCode, eq));
    };

    var makeEqualHashtable = function() {
        return new WhalesongHashtable(
            "hash",
            getEqualHashCode,
            equal,
            new Hashtable(getEqualHashCode, equal));
    };
    
    
    var makeEqvHashtable = function() {
        return new WhalesongHashtable(
            "hasheqv",
            getEqvHashCode,
            eqv,
            new Hashtable(getEqvHashCode, eqv));
    };


    var makeImmutableEqHashtable = function() { 
        return makeEqHashtable().toImmutable();
    };

    var makeImmutableEqualHashtable = function() {
        return makeEqualHashtable().toImmutable();
    };
        
    var makeImmutableEqvHashtable = function() {
        return makeEqvHashtable().toImmutable();
    };


    // When we need to make comparators for the immutable hash tables, use this.
    var makeComparator = function(hash, eq) {
        return function(x, y) {
            var hx = hash(x), hy = hash(y);
            if (hx < hy) { return -1; }
            if (hx > hy) { return 1; }
            
            if (eq(x, y)) { return 0; }

            hx = getEqHashCode(x);
            hy = getEqHashCode(y);
            if (hx < hy) { return -1; }
            if (hx > hy) { return 1; }
            return 0;
        }
    };


    //////////////////////////////////////////////////////////////////////
    // Whalesong's Hashtables are a thin wrapper around the mutable Hashtable
    // class to make it printable and equatable.
    var WhalesongHashtable = function (type, hash_function, equality_function, hash) {
        this.type = type;
        this.hash_function = hash_function;
        this.equality_function = equality_function;
        this.hash = hash;
    };

    WhalesongHashtable.prototype.clone = function() {
        return new WhalesongHashtable(this.type, this.hash_function, this.equality_function, this.hash.clone());
    };

    WhalesongHashtable.prototype.size = function() {
        return this.hash.size();
    };

    WhalesongHashtable.prototype.toWrittenString = function (cache) {
        var keys = this.hash.keys();
        var ret = [], i;
        for (i = 0; i < keys.length; i++) {
            var keyStr = baselib.format.toWrittenString(keys[i], cache);
            var valStr = baselib.format.toWrittenString(this.hash.get(keys[i]), cache);
            ret.push('(' + keyStr + ' . ' + valStr + ')');
        }
        return ('#' + this.type + '(' + ret.join(' ') + ')');
    };
    
    WhalesongHashtable.prototype.toDisplayedString = function (cache) {
        var keys = this.hash.keys();
        var ret = [], i;
        for (i = 0; i < keys.length; i++) {
            var keyStr = baselib.format.toDisplayedString(keys[i], cache);
            var valStr = baselib.format.toDisplayedString(this.hash.get(keys[i]), cache);
            ret.push('(' + keyStr + ' . ' + valStr + ')');
        }
        return ('#' + this.type + '(' + ret.join(' ') + ')');
    };

    WhalesongHashtable.prototype.keys = function() {
        return this.hash.keys();
    };

    WhalesongHashtable.prototype.values = function() {
        return this.hash.values();
    };


    WhalesongHashtable.prototype.equals = function (other, aUnionFind) {
        if (!(other instanceof WhalesongHashtable)) {
            return false; 
        }
        if (other.type !== this.type) { 
            return false;
        }
        if (this.hash.keys().length !== other.hash.keys().length) { 
            return false;
        }

        var keys = this.hash.keys(), i;
        for (i = 0; i < keys.length; i++) {
            if (!(other.hash.containsKey(keys[i]) &&
                  baselib.equality.equals(this.hash.get(keys[i]),
                                          other.hash.get(keys[i]),
                                          aUnionFind))) {
                return false;
            }
        }
        return true;
    };

    WhalesongHashtable.prototype.hashCode = function(depth) {
        var k = getEqualHashCode(this.type);
        var keys = this.hash.keys(), i;
        for (i = 0; i < keys.length; i++) {
            k += hashMix(getEqualHashCode(this.hash.get(keys[i]), depth));
        }
        return hashMix(k);
    };


    WhalesongHashtable.prototype.get = function(key) {
        return this.hash.get(key);
    };

    WhalesongHashtable.prototype.put = function(key, value) {
        this.hash.put(key, value);
    };

    WhalesongHashtable.prototype.functionalPut = function(key, value) {
        return this.toImmutable().functionalPut(key, value);
    };

    WhalesongHashtable.prototype.remove = function(key) {
        this.hash.remove(key);
    };

    WhalesongHashtable.prototype.functionalRemove = function(key) {
        return this.toImmutable().functionalRemove(key);
    };

    WhalesongHashtable.prototype.containsKey = function(key) {
        return this.hash.containsKey(key);
    };

    WhalesongHashtable.prototype.isImmutable = function() {
        return false;
    };

    WhalesongHashtable.prototype.toImmutable = function() {
        var keycmp = makeComparator(this.hash_function, this.equality_function)
        var immutable = new WhalesongImmutableHashtable(
            this.type,
            this.hash_function,
            this.equality_function,
            LLRBTree.makeMap(keycmp));
        var keys = this.hash.keys();
        var i;
        for (i = 0; i < keys.length; i++) {
            immutable = immutable.functionalPut(keys[i], this.hash.get(keys[i]));
        }
        return immutable;
    };


    //////////////////////////////////////////////////////////////////////
    //////////////////////////////////////////////////////////////////////
    // Whalesong's immutable hashtables are a thin wrapper around the
    // llrbtree class to make it printable and equatable.
    // llrbtree comes from: https://github.com/dyoo/js-llrbtree
    var WhalesongImmutableHashtable = function (type,
                                                hash_function,
                                                equality_function,
                                                map) {
        this.type = type;
        this.hash_function = hash_function;
        this.equality_function = equality_function;
        this.map = map;
    };

    WhalesongImmutableHashtable.prototype.size = function() {
        return this.map.items().length;
    };


    WhalesongImmutableHashtable.prototype.toWrittenString = function (cache) {
        var items = this.map.items();
        var ret = [], i;
        for (i = 0; i < items.length; i++) {
            var keyStr = baselib.format.toWrittenString(items[i][0], cache);
            var valStr = baselib.format.toWrittenString(items[i][1], cache);
            ret.push('(' + keyStr + ' . ' + valStr + ')');
        }
        return ('#' + this.type + '(' + ret.join(' ') + ')');
    };
    
    WhalesongImmutableHashtable.prototype.toDisplayedString = function (cache) {
        var items = this.map.keys();
        var ret = [], i;
        for (i = 0; i < items.length; i++) {
            var keyStr = baselib.format.toDisplayedString(items[i][0], cache);
            var valStr = baselib.format.toDisplayedString(items[i][1], cache);
            ret.push('(' + keyStr + ' . ' + valStr + ')');
        }
        return ('#' + this.type + '(' + ret.join(' ') + ')');
    };

    WhalesongImmutableHashtable.prototype.keys = function() {
        return this.map.keys();
    };

    WhalesongImmutableHashtable.prototype.values = function() {
        return this.map.values();
    };

    WhalesongImmutableHashtable.prototype.equals = function (other, aUnionFind) {
        if (!(other instanceof WhalesongImmutableHashtable)) {
            return false; 
        }
        if (other.type !== this.type) { 
            return false;
        }
        var litems = this.map.items();
        var ritems = other.map.items();

        if (litems.length !== ritems.length) { 
            return false;
        }
        var i;
        for (i = 0; i < litems.length; i++) {
            if (!(baselib.equality.equals(litems[i][0], ritems[i][0], aUnionFind))) {
                return false;
            }
            if (!(baselib.equality.equals(litems[i][1], ritems[i][1], aUnionFind))) {
                return false;
            }
        }
        return true;
    };

    WhalesongImmutableHashtable.prototype.hashCode = function(depth) {
        var k = getEqualHashCode(this.type);
        var items = this.map.items(), i;
        for (i = 0; i < items.length; i++) {
            k = getEqualHashCode(items[i][0], depth);
            k = hashMix(k);
            k = getEqualHashCode(items[i][1], depth);
            k = hashMix(k);
        }
        return hashMix(k);
    };


    WhalesongImmutableHashtable.prototype.get = function(key) {
        return this.map.get(key);
    };

    WhalesongImmutableHashtable.prototype.put = function(key, value) {
        throw new Error();
    };

    WhalesongImmutableHashtable.prototype.functionalPut = function(key, value) {
        return new WhalesongImmutableHashtable(this.type,
                                               this.hash_function,
                                               this.equality_function,
                                               this.map.put(key, value));
    };

    WhalesongImmutableHashtable.prototype.remove = function(key) {
        throw new Error();
    };

    WhalesongImmutableHashtable.prototype.functionalRemove = function(key) {
        return new WhalesongImmutableHashtable(this.type,
                                               this.hash_function,
                                               this.equality_function,
                                               this.map.remove(key));
    };

    WhalesongImmutableHashtable.prototype.containsKey = function(key) {
        return this.map.contains(key);
    };

    WhalesongImmutableHashtable.prototype.isImmutable = function() { 
        return true;
    };
    //////////////////////////////////////////////////////////////////////    



    var isHash = function (x) { 
        return (x instanceof WhalesongHashtable || x instanceof WhalesongImmutableHashtable);
    };

    var isHashEqual = function (x) { 
        return (x instanceof WhalesongHashtable || x instanceof WhalesongImmutableHashtable) && x.type === 'hash';
    };

    var isHashEqv = function (x) { 
        return (x instanceof WhalesongHashtable || x instanceof WhalesongImmutableHashtable) && x.type === 'hasheqv';
    };

    var isHashEq = function (x) { 
        return (x instanceof WhalesongHashtable || x instanceof WhalesongImmutableHashtable) && x.type === 'hasheq';
    };



    // Arbitrary magic number.  We have to cut off the hashing at some point.
    var MAX_HASH_DEPTH = 128;

    // Returns a JavaScript number.
    var getEqualHashCode = function (x, depth) {
        var i, t, k = 0;
        if (depth === void(0)) { depth = [0]; }

        if (depth[0] > MAX_HASH_DEPTH) { return 0; }

        if (baselib.numbers.isNumber(x)) {
            return hashMix(baselib.numbers.toFixnum(x));
        }

        if (baselib.strings.isString(x)) {
            t = x.toString();
            for (i = 0; i < t.length; i++) {
                k += t.charCodeAt(i);
                k = hashMix(k);
            }
            return k;
        }

        if (x === void(0) || x === null) {
            return 1;
        }

        if (typeof(x) === 'object' &&
            typeof(x.hashCode) === 'function') {
            depth[0] = depth[0] + 1;
            return x.hashCode(depth);
        }
        return 0;
    };


    // Does some weird math on k.  Grabbed from Racket's implementation of hashes.
    // References to: http://www.burtleburtle.net/bob/hash/doobs.html
    var hashMix = function(k) {
        k += (k << 10);
        k ^= (k >> 6);
        return k;
    };



    //////////////////////////////////////////////////////////////////////

    exports.getEqHashCode = getEqHashCode;
    exports.getEqualHashCode = getEqualHashCode;
    exports.getEqvHashCode = getEqvHashCode;

    exports.hashMix = hashMix;

    exports.makeEqHashCode = makeEqHashCode;
    exports.makeLowLevelEqHash = makeLowLevelEqHash;

    exports.makeEqHashtable = makeEqHashtable;
    exports.makeEqvHashtable = makeEqvHashtable;
    exports.makeEqualHashtable = makeEqualHashtable;

    exports.makeImmutableEqHashtable = makeImmutableEqHashtable;
    exports.makeImmutableEqvHashtable = makeImmutableEqvHashtable;
    exports.makeImmutableEqualHashtable = makeImmutableEqualHashtable;

    exports.isHash = isHash;

    exports.isHashEqual = isHashEqual;
    exports.isHashEqv = isHashEqv;
    exports.isHashEq = isHashEq;
}(window.plt.baselib, Hashtable));}());
/*jslint vars: true, maxerr: 50, indent: 4 */

(function (baselib) {
    'use strict';
    var exports = {};
    baselib.regexps = exports;


    // Regular expressions.

    var RegularExpression = function (pattern) {
        this.pattern = pattern;
    };


    var ByteRegularExpression = function (pattern) {
        this.pattern = pattern;
    };

    //////////////////////////////////////////////////////////////////////

    exports.RegularExpression = RegularExpression;
    exports.ByteRegularExpression = ByteRegularExpression;

}(this.plt.baselib));/*jslint vars: true, maxerr: 50, indent: 4 */


(function (baselib) {
    'use strict';
    var exports = {};
    baselib.paths = exports;

    // Paths

    var Path = function (p) {
        this.path = p;
    };

    Path.prototype.toString = function () {
        return "#<path:" + String(this.path) + ">";
    };


    Path.prototype.equals = function(other, aUnionFind) {
        return (other instanceof Path &&
                this.path === other.path);
    };

    Path.prototype.hashCode = function(depth) {
        var k = baselib.hashes.getEqualHashCode("path");
        k += baselib.hashes.getEqualHashCode(this.path, depth);
        k = baselib.hashes.hashMix(k);
        return k;
    };


    //////////////////////////////////////////////////////////////////////

    var makePath = function (p) {
        return new Path(p);
    };

    var isPath = baselib.makeClassPredicate(Path);



    exports.Path = Path;
    exports.makePath = makePath;
    exports.isPath = isPath;

}(this.plt.baselib));/*jslint browser: true, unparam: true, vars: true, white: true, plusplus: true, maxerr: 50, indent: 4 */


// Exceptions

(function(baselib, $) {
    'use strict';
    var exports = {};
    baselib.boxes = exports;


    //////////////////////////////////////////////////////////////////////
    // Boxes
    
    var Box = function(x, mutable) {
	this.val = x;
	this.mutable = mutable;
    };

    Box.prototype.ref = function() {
        return this.val;
    };

    Box.prototype.set = function(newVal) {
        if (this.mutable) {
	    this.val = newVal;
        }
    };

    Box.prototype.toString = function(cache) {
        cache.put(this, true);
        return "#&" + baselib.format.toWrittenString(this.val, cache);
    };

    Box.prototype.toWrittenString = function(cache) {
        cache.put(this, true);
        return "#&" + baselib.format.toWrittenString(this.val, cache);
    };

    Box.prototype.toDisplayedString = function(cache) {
        cache.put(this, true);
        return "#&" + baselib.format.toDisplayedString(this.val, cache);
    };

    Box.prototype.toDomNode = function(params) {
        var node = $('<span/>');
        if (params.getMode() === 'constructor') {
            node.append($('<span/>').text('(').addClass('lParen'));
            node.append($('<span/>').text('box'));
            node.append(" ");
            node.append(params.recur(this.val));
            node.append($('<span/>').text(')').addClass('rParen'));
        } else {
            node.append($('<span/>').text('#&'));
            node.append(params.recur(this.val));
        }
        return node.get(0);
    };

    Box.prototype.equals = function(other, aUnionFind) {
        return ((other instanceof Box) &&
	        baselib.equality.equals(this.val, other.val, aUnionFind));
    };

    Box.prototype.hashCode = function(depth) {
        var k = baselib.hashes.getEqualHashCode("Box");
        k = baselib.hashes.hashMix(k);
        k += baselib.hashes.getEqualHashCode(this.val, depth);
        k = baselib.hashes.hashMix(k);
        return k;
    };

    
    var makeBox = function(x) { 
        return new Box(x, true); 
    };

    var makeImmutableBox = function(x) {
        return new Box(x, false); 
    };

    var isBox = function(x) {
        return x instanceof Box;
    };

    var isMutableBox = function(x) { 
        return (x instanceof Box && x.mutable); 
    };

    var isImmutableBox = function(x) { 
        return (x instanceof Box && (!x.mutable));
    };




    //////////////////////////////////////////////////////////////////////
    exports.Box = Box;
    exports.isBox = isBox;
    exports.isMutableBox = isMutableBox;
    exports.isImmutableBox = isImmutableBox;
    exports.makeBox = makeBox;
    exports.makeImmutableBox = makeImmutableBox;


}(this.plt.baselib, jQuery));
// Placeholders
/*jslint browser: true, unparam: true, vars: true, maxerr: 50, indent: 4 */
(function (baselib) {
    'use strict';
    var exports = {};
    baselib.placeholders = exports;


    // Placeholders: same thing as boxes.  Distinct type just to support make-reader-graph.

    var Placeholder = function (x, mutable) {
        this.val = x;
    };

    Placeholder.prototype.ref = function () {
        return this.val;
    };

    Placeholder.prototype.set = function (newVal) {
        this.val = newVal;
    };

    Placeholder.prototype.toString = function (cache) {
        return "#<placeholder>";
    };

    Placeholder.prototype.toWrittenString = function (cache) {
        return "#<placeholder>";
    };

    Placeholder.prototype.toDisplayedString = function (cache) {
        return "#<placeholder>";
    };

    Placeholder.prototype.toDomNode = function (cache) {
        var parent = document.createElement("span");
        parent.appendChild(document.createTextNode('#<placeholder>'));
        return parent;
    };

    Placeholder.prototype.equals = function (other, aUnionFind) {
        return ((other instanceof Placeholder) &&
                baselib.equality.equals(this.val, other.val, aUnionFind));
    };

    Placeholder.prototype.hashCode = function(depth) {
        var k = baselib.hashes.hashCode("Placeholder");
        k += baselib.hashes.hashCode(this.val, depth);
        return baselib.hashes.hashMix(k);
    };


    var makePlaceholder = function(v) {
        return new Placeholder(v);
    };

    var isPlaceholder = function (x) { 
        return x instanceof Placeholder; 
    };
    


    //////////////////////////////////////////////////////////////////////
    exports.Placeholder = Placeholder;
    exports.makePlaceholder = makePlaceholder;
    exports.isPlaceholder = isPlaceholder;



}(this.plt.baselib));/*jslint unparam: true, vars: true, maxerr: 50, indent: 4 */

// Keywords

(function (baselib) {
    'use strict';
    var exports = {};
    baselib.keywords = exports;


    var Keyword = function (val) {
        this.val = val;
    };

    var keywordCache = {};

    var hasOwnProperty = {}.hasOwnProperty;
    
    // makeInstance: string -> Keyword.
    Keyword.makeInstance = function (val) {
        // To ensure that we can eq? symbols with equal values.
        if (!(hasOwnProperty.call(keywordCache, val))) {
            keywordCache[val] = new Keyword(val);
        }
        return keywordCache[val];
    };
    
    Keyword.prototype.equals = function (other, aUnionFind) {
        return other instanceof Keyword &&
            this.val === other.val;
    };

    Keyword.prototype.hashCode = function(depth) {
        var k = baselib.hashes.getEqualHashCode("Keyword");
        k += baselib.hashes.getEqualHashCode(this.val, depth);
        k = baselib.hashes.hashMix(k);
        return k;
    };
    

    Keyword.prototype.toString = function (cache) {
        return this.val;
    };

    Keyword.prototype.toWrittenString = function (cache) {
        return this.val;
    };

    Keyword.prototype.toDisplayedString = function (cache) {
        return this.val;
    };


    exports.Keyword = Keyword;

}(this.plt.baselib));/*jslint browser: true, unparam: true, vars: true, white: true, nomen: true, plusplus: true, maxerr: 50, indent: 4 */
/*globals $*/
(function (baselib, $) {
    "use strict";
    var exports = {};
    baselib.structs = exports;



    //////////////////////////////////////////////////////////////////////

    var Struct = function (constructorName, fields, structType) {
        this._constructorName = constructorName; // Symbol
        this._fields = [];
        this.structType = structType;
    };

    Struct.prototype.toWrittenString = function (cache) { 
        var buffer = [], i;
        cache.put(this, true);
        buffer.push("(");
        buffer.push(this._constructorName);
        for(i = 0; i < this._fields.length; i++) {
            buffer.push(" ");
            buffer.push(baselib.format.toWrittenString(this._fields[i], cache));
        }
        buffer.push(")");
        return buffer.join("");
    };

    Struct.prototype.toDisplayedString = function (cache) {
        return baselib.format.toWrittenString(this, cache); 
    };

    Struct.prototype.toDomNode = function (params) {
        var node = $('<span/>');
        var i;
        node.append($('<span/>').text("(").addClass('lParen'));
        node.append($('<span/>').text(this._constructorName+''));
        for(i = 0; i < this._fields.length; i++) {
            node.append(" ");
            node.append(params.recur(this._fields[i]));
        }
        node.append($('<span/>').text(")").addClass('rParen'));
        return node.get(0);
    };


    Struct.prototype.equals = function (other, aUnionFind) {
        var i;
        if (!(other instanceof this.type)) {
            return false;
        }
        for (i = 0; i < this._fields.length; i++) {
            if (! baselib.equality.equals(this._fields[i],
                                          other._fields[i],
                                          aUnionFind)) {
                return false;
            }
        }
        return true;
    };

    Struct.prototype.hashCode = function(depth) {
        var k = baselib.hashes.getEqualHashCode(this.name);
        var i;
        k = baselib.hashes.hashMix(k);
        for (i = 0; i < this._fields.length; i++) {
            k += baselib.hashes.getEqualHashCode(this._fields[i], depth);
            k = baselib.hashes.hashMix(k);
        }
        return k;
    };


    Struct.prototype.type = Struct;


    //////////////////////////////////////////////////////////////////////


    var StructType = function (name,             // string
                               type,             // StructType
                               numberOfArgs,     // number
                               numberOfFields,   // number
                               firstField,
                               applyGuard,
                               constructor,
                               predicate, 
                               accessor,
                               mutator,
                               propertiesList) {
        this.name = name;
        this.type = type;
        this.numberOfArgs = numberOfArgs;
        this.numberOfFields = numberOfFields;
        this.firstField = firstField;

        this.applyGuard = applyGuard;
        this.constructor = constructor;
        this.predicate = predicate;
        this.accessor = accessor;
        this.mutator = mutator;
        this.propertiesList = propertiesList;
    };


    StructType.prototype.toString = function (cache) {
        return '#<struct-type:' + this.name + '>';
    };


    StructType.prototype.equals = function (other, aUnionFind) {
        return this === other;
    };

    StructType.prototype.hashCode = function(depth) {
        var k = baselib.hashes.getEqualHashCode("StructType");
        k = baselib.hashes.hashMix(k);
        k += baselib.hashes.getEqualHashCode(this.name);
        k = baselib.hashes.hashMix(k);
        return k;
    };




    // guard-function: array string (array -> value)





    // Default structure guard just calls the continuation argument.
    var DEFAULT_GUARD = function (args, name, k) { 
        return k(args); 
    };


    // The default parent type refers to the toplevel Struct.
    var DEFAULT_PARENT_TYPE = { type: Struct,
                                numberOfArgs: 0,
                                numberOfFields: 0,
                                firstField: 0,
                                applyGuard: DEFAULT_GUARD };



    // makeStructureType: string StructType number number boolean
    //                    guard-function -> StructType
    //
    // Creates a new structure type.

    var makeStructureType = function (theName,
                                      parentType, 
                                      initFieldCnt, 
                                      autoFieldCnt, 
                                      autoV, 
                                      guard,
                                      propertiesList) {


        // Defaults
        autoFieldCnt = autoFieldCnt || 0;
        parentType = parentType || DEFAULT_PARENT_TYPE;
        guard = guard || DEFAULT_GUARD;


        // RawConstructor creates a new struct type inheriting from
        // the parent, with no guard checks.
        var RawConstructor = function (name, args, structType) {
            var i;
            parentType.type.call(this, name, args, structType);
            for (i = 0; i < initFieldCnt; i++) {
                this._fields.push(args[i+parentType.numberOfArgs]);
            }
            for (i = 0; i < autoFieldCnt; i++) {
                this._fields.push(autoV);
            }
        };
        RawConstructor.prototype = baselib.heir(parentType.type.prototype);

        var theNameSymbol = 
            baselib.symbols.makeSymbol(theName);

        // Set type, necessary for equality checking
        RawConstructor.prototype.type = RawConstructor;


        var constructAfterGuard =  function (res) { 
            return new RawConstructor(theName, res, newType); 
        };

        // The structure type consists of the name, its constructor, a
        // record of how many argument it and its parent type contains,
        // the list of autofields, the guard, and functions corresponding
        // to the constructor, the predicate, the accessor, and mutators.
        var newType = new StructType(
            theName,
            RawConstructor,
            initFieldCnt + parentType.numberOfArgs,
            initFieldCnt + autoFieldCnt,
            parentType.firstField + parentType.numberOfFields,
            function (args, name, k) {
                return guard(args, name,
                             function (result) {
                                 var parentArgs = result.slice(0, parentType.numberOfArgs);
                                 var restArgs = result.slice(parentType.numberOfArgs);
                                 return parentType.applyGuard(
                                     parentArgs, name,
                                     function (parentRes) {
                                         return k( parentRes.concat(restArgs) ); });
                             });
            },
            // constructor
            function (args) {
                return newType.applyGuard(
                    args,
                    theNameSymbol,
                    constructAfterGuard);
            },

            // predicate
            function (x) { 
                return x instanceof RawConstructor; 
            },

            // accessor
            function (x, i) { return x._fields[i + this.firstField]; },

            // mutator
            function (x, i, v) { x._fields[i + this.firstField] = v; },

            // structure properties list
            propertiesList);
        return newType;
    };




    var StructTypeProperty = function(name, guards, supers) {
        this.name = name;
        this.guards = guards;
        this.supers = supers;
    };



    // supportsStructureTypeProperty: StructType StructureTypeProperty -> boolean
    // Produces true if the structure type provides a binding for the
    // given structure property.
    var supportsStructureTypeProperty = function(structType, property) {
        var propertiesList = structType.propertiesList;
        if (! propertiesList) {
            return false;
        }
        while (propertiesList !== baselib.lists.EMPTY) {
            if (propertiesList.first.first === property) {
                return true;
            }
            propertiesList = propertiesList.rest;
        }
        return false;
    };


    // lookupStructureTypeProperty: StructType StructureTypeProperty -> any
    // Returns the binding associated to this particular structure type propery.
    var lookupStructureTypeProperty = function(structType, property) {
        var propertiesList = structType.propertiesList;
        if (! propertiesList) {
            return void(0);
        }
        while (propertiesList !== baselib.lists.EMPTY) {
            if (propertiesList.first.first === property) {
                return propertiesList.first.rest;
            }
            propertiesList = propertiesList.rest;
        }
        return void(0);
    };


    // A structure type property for noting if an exception supports
    var propExnSrcloc = new StructTypeProperty("prop:exn:srcloc");



    var isStruct = baselib.makeClassPredicate(Struct);
    var isStructType = baselib.makeClassPredicate(StructType);
    var isStructTypeProperty = baselib.makeClassPredicate(StructTypeProperty);



    //////////////////////////////////////////////////////////////////////


    exports.StructType = StructType;
    exports.Struct = Struct;
    exports.makeStructureType = makeStructureType;

    exports.StructTypeProperty = StructTypeProperty;
    exports.supportsStructureTypeProperty = supportsStructureTypeProperty;
    exports.lookupStructureTypeProperty = lookupStructureTypeProperty;

    exports.propExnSrcloc = propExnSrcloc;

    exports.isStruct = isStruct;
    exports.isStructType = isStructType;
    exports.isStructTypeProperty = isStructTypeProperty;
}(this.plt.baselib, jQuery));
/*jslint vars: true, white: true, plusplus: false, maxerr: 50, indent: 4 */
(function(baselib) {
    'use strict';

    var exports = {};
    baselib.srclocs = exports;

    // (define-struct srcloc (source line column position span))
    var srcloc = baselib.structs.makeStructureType(
        'srcloc', false, 5, 0, false, false);
    
    var makeSrcloc = function() { 
        var args = [].slice.call(arguments);
        return srcloc.constructor(args);
    };

    var isSrcloc = srcloc.predicate;
    var srclocSource = function(x) { return srcloc.accessor(x, 0); };
    var srclocLine = function(x) { return srcloc.accessor(x, 1); };
    var srclocColumn = function(x) { return srcloc.accessor(x, 2); };
    var srclocPosition = function(x) { return srcloc.accessor(x, 3); };
    var srclocSpan = function(x) { return srcloc.accessor(x, 4); };

    //////////////////////////////////////////////////////////////////////
    exports.makeSrcloc = makeSrcloc;
    exports.isSrcloc = isSrcloc;
    exports.srclocSource = srclocSource;
    exports.srclocLine = srclocLine;
    exports.srclocColumn = srclocColumn;
    exports.srclocPosition = srclocPosition;
    exports.srclocSpan = srclocSpan;

}(this.plt.baselib));// Arity structure
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
        // Force a trampoline to send control back to the browser
        // for rendering.
        MACHINE.cbt=0;
    };

    var StandardErrorPort = function () {
        OutputPort.call(this);
    };
    StandardErrorPort.prototype = baselib.heir(OutputPort.prototype);
    StandardErrorPort.prototype.writeDomNode = function (MACHINE, domNode) {
        MACHINE.params['currentErrorDisplayer'](MACHINE, domNode);
        $(domNode).trigger({type : 'afterAttach'});
        $('*', domNode).trigger({type : 'afterAttach'});
        // Force a trampoline to send control back to the browser
        // for rendering.
        MACHINE.cbt=0;
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
        readLine.focus();
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


}(this.plt.baselib, jQuery));
/*jslint unparam: true, sub: true, vars: true, white: true, plusplus: true, maxerr: 50, indent: 4 */

// Procedures

// For historical reasons, this module is called 'functions' instead of 'procedures'.
// This may change soon.

/*global plt*/

(function (baselib, plt) {
    'use strict';
    var exports = {};
    baselib.functions = exports;
    
    // Procedure types: a procedure is either a Primitive or a Closure.

    // A Primitive is a function that's expected to return.  It is not
    // allowed to call into Closures.  Its caller is expected to pop off
    // its argument stack space.
    //






    // A Closure is a function that takes on more responsibilities: it is
    // responsible for popping off stack space before it finishes, and it
    // is also explicitly responsible for continuing the computation by 
    // popping off the control stack and doing the jump.  Because of this,
    // closures can do pretty much anything to the machine.

    // A closure consists of its free variables as well as a label
    // into its text segment.
    var Closure = function (label, arity, closedVals, displayName) {
        this.label = label;              // (MACHINE -> void)
        this.racketArity = arity;              // number
        this.closedVals = closedVals;    // arrayof number
        this.displayName = displayName;  // string
    };


    // Finalize the return from a closure.  This is a helper function
    // for those who implement Closures by hand.
    //
    // If used in the body of a Closure, it must be in tail
    // position.  This finishes the closure call, and does the following:
    //
    //     * Clears out the existing arguments off the stack frame
    //     * Sets up the return value
    //     * Jumps either to the single-value return point, or the multiple-value
    //       return point.
    //
    // I'd personally love for this to be a macro and avoid the
    // extra function call here.
    var finalizeClosureCall = function (MACHINE) {
        MACHINE.cbt--;
        var returnArgs = [].slice.call(arguments, 1);

        // clear out stack space
        MACHINE.e.length -= MACHINE.a;

        if (returnArgs.length === 1) {
            MACHINE.v = returnArgs[0];
            return MACHINE.c.pop().label(MACHINE);
        } else if (returnArgs.length === 0) {
            MACHINE.a = 0;
            return (MACHINE.c.pop().label.mvr || plt.runtime.si_context_expected_1)(MACHINE);
        } else {
            MACHINE.a = returnArgs.length;
            MACHINE.v = returnArgs.shift();
            MACHINE.e.push.apply(MACHINE.e, returnArgs.reverse());
            return (MACHINE.c.pop().label.mvr || plt.runtime.si_context_expected_1)(MACHINE);
        }
    };


    var isClosure = function (x) {
        return x instanceof Closure;
    };


    var isProcedure = function (x) {
        return (typeof (x) === 'function' || x instanceof Closure);
    };


    var coerseClosureToJavaScript = function (v, MACHINE) {
        var f = function (succ, fail) {
            var args = [];
            var i;
            for (i = 0; i < arguments.length; i++) {
                args.push(arguments[i]);
            }

            MACHINE.exclusiveLock.acquire(
                "js-as-closure",
                function(releaseLock) {
                    var wrappedSucc = function() { 
                        releaseLock(); 
                        (succ || function () {}).apply(null, arguments); 
                    };
                    var wrappedFail = function(err) {
                        (fail || function () {})(err);
                    };
                    if (!(baselib.arity.isArityMatching(v.racketArity, args.length - 2))) {
                        var msg = baselib.format.format(
                            "arity mismatch: ~s expected ~s argument(s) but received ~s",
                            [v.displayName, v.racketArity, args.length - 2]);
                        releaseLock();
                        return wrappedFail(new baselib.exceptions.RacketError(
                            msg,
                            baselib.exceptions.makeExnFailContractArity(msg,
                                                                        MACHINE.captureContinuationMarks())));
                    }

                    var oldVal = MACHINE.v;
                    var oldArgcount = MACHINE.a;
                    var oldProc = MACHINE.p;
                    var oldErrorHandler = MACHINE.params['currentErrorHandler'];

                    var afterGoodInvoke = function (MACHINE) { 
                        plt.runtime.PAUSE(
                            function (restart) {
                                MACHINE.params['currentErrorHandler'] = oldErrorHandler;
                                var returnValue = MACHINE.v;
                                MACHINE.v = oldVal;
                                MACHINE.a = oldArgcount;
                                MACHINE.p = oldProc;
                                wrappedSucc(returnValue);
                            });
                    };
                    afterGoodInvoke.mvr = function (MACHINE) {
                        plt.runtime.PAUSE(
                            function (restart) {
                                MACHINE.params['currentErrorHandler'] = oldErrorHandler;
                                var returnValues = [MACHINE.v], i;
                                for (i = 0; i < MACHINE.a - 1; i++) {
                                    returnValues.push(MACHINE.e.pop());
                                }
                                MACHINE.v = oldVal;
                                MACHINE.a = oldArgcount;
                                MACHINE.p = oldProc;
                                wrappedSucc.apply(null, returnValues);
                            });
                    };

                    MACHINE.c.push(
                        new baselib.frames.CallFrame(afterGoodInvoke, v));
                    MACHINE.a = args.length - 2;
                    var i;
                    for (i = 0; i < args.length - 2; i++) {
                        MACHINE.e.push(args[args.length - 1 - i]);
                    }
                    MACHINE.p = v;
                    MACHINE.params['currentErrorHandler'] = function (MACHINE, e) {
                        MACHINE.params['currentErrorHandler'] = oldErrorHandler;
                        MACHINE.v = oldVal;
                        MACHINE.a = oldArgcount;
                        MACHINE.p = oldProc;
                        // The lock is released by _trampoline in this case.
                        wrappedFail(e);
                    };

                    MACHINE._trampoline(v.label, false, releaseLock);
                });
        };
        return f;
    };

    // coerseToJavaScript: racket function -> JavaScript function
    // Given a closure or primitive, produces an
    // asynchronous JavaScript function.
    // The function will run on the provided MACHINE.
    //
    // It assumes that it must begin its own trampoline.
    var asJavaScriptFunction = function (v, MACHINE) {
        MACHINE = MACHINE || plt.runtime.currentMachine;
        if (isClosure(v)) {
            return coerseClosureToJavaScript(v, MACHINE);
        } else {
            baselib.exceptions.raise(MACHINE,
                                     baselib.exceptions.makeExnFailContract(
                                         baselib.format.format(
                                             "not a procedure: ~e",
                                             [v]),
                                         MACHINE.captureContinuationMarks()));
        }
    };


    // internallCallDuringPause: call a Racket procedure and get its results.
    // The use assumes the machine is in a running-but-paused state, where the
    // lock is still in effect.  The lock will continue to be in effect
    // after coming back from the internal call.
    var internalCallDuringPause = function (MACHINE, proc, success, fail) {
        var args = [];
        var i;
        for (i = 0; i < arguments.length; i++) {
            args.push(arguments[i]);
        }

        var i;

        if (MACHINE.breakScheduled) {
            return fail(baselib.exceptions.makeExnBreak(
                "User break.",
                MACHINE.captureContinuationMarks(),
                // FIXME: capture the continuation as well,
                // rather than just hold false.
                false));
        }

        var oldArgcount, oldVal, oldProc, oldErrorHandler, oldControlLength, oldEnvLength;
        if (! baselib.arity.isArityMatching(proc.racketArity, args.length - 4)) {
            var msg = baselib.format.format("arity mismatch: ~s expected ~s arguments, but received ~s",
                                            [proc.displayName, proc.racketArity, args.length - 4]);
            return fail(baselib.exceptions.makeExnFailContractArity(msg,
                                                                    MACHINE.captureContinuationMarks()));
        }

        if (! isClosure(proc)) {
            return fail(baselib.exceptions.makeExnFail(
                baselib.format.format(
                    "Not a procedure: ~e",
                    [proc]),
                MACHINE.captureContinuationMarks()));
        }

        oldVal = MACHINE.v;
        oldArgcount = MACHINE.a;
        oldProc = MACHINE.p;
        oldControlLength = MACHINE.c.length;
        oldEnvLength = MACHINE.e.length;

        oldErrorHandler = MACHINE.params['currentErrorHandler'];
        var afterGoodInvoke = function (MACHINE) { 
            plt.runtime.PAUSE(function (restart) {
                MACHINE.params['currentErrorHandler'] = oldErrorHandler;
                var returnValue = MACHINE.v;
                MACHINE.v = oldVal;
                MACHINE.a = oldArgcount;
                MACHINE.p = oldProc;
                return success(returnValue);
            });
        };
        afterGoodInvoke.mvr = function (MACHINE) {
            plt.runtime.PAUSE(function (restart) {
                MACHINE.params['currentErrorHandler'] = oldErrorHandler;
                var returnValues = [MACHINE.v];
                var i;
                for (i = 0; i < MACHINE.a - 1; i++) {
                    returnValues.push(MACHINE.e.pop());
                }
                MACHINE.v = oldVal;
                MACHINE.a = oldArgcount;
                MACHINE.p = oldProc;
                return success.apply(null, returnValues);
            });
        };

        MACHINE.c.push(
            new baselib.frames.CallFrame(afterGoodInvoke, proc));
        MACHINE.a = args.length - 4;
        for (i = 0; i < args.length - 4; i++) {
            MACHINE.e.push(args[args.length - 1 - i]);
        }
        MACHINE.p = proc;
        MACHINE.params['currentErrorHandler'] = function (MACHINE, e) {
            MACHINE.params['currentErrorHandler'] = oldErrorHandler;
            MACHINE.v = oldVal;
            MACHINE.a = oldArgcount;
            MACHINE.p = oldProc;
            MACHINE.c.length = oldControlLength;
            MACHINE.e.length = oldEnvLength;
            return fail(e);
        };
        MACHINE._trampoline(proc.label, 
                            false, 
                            function() {
                                // The lock should still being held, so we don't
                                // automatically unlock control.
                            });
    };







    var makeClosure = function (name, arity, f, closureArgs) {
        if (! closureArgs) { closureArgs = []; }
        return new Closure(f,
                           arity,
                           closureArgs,
                           name);
    };


    var makePrimitiveProcedure = function (name, arity, f) {
        var impl = function(M) {
            if(--M.cbt < 0) {
                throw impl;
            }
            M.v = f(M);
            M.e.length -= M.a;
            return M.c.pop().label(M);
        };
        var proc = makeClosure(name, arity, impl, []);
        // Also, record the raw implementation of the function.
        proc._i = f;
        return proc;
    };








    var renameProcedure = function (f, name) {
        return makeClosure(name, f.racketArity, f.label, f.closedVals);
    };



    // Applying a procedure.
    // Assumptions: the procedure register has been assigned, as has
    // the argcount and environment.
    // Must be running in the context of a trampoline.
    var rawApply = function(M) {
        M.cbt--;
        if (baselib.arity.isArityMatching(M.p.racketArity, M.a)) {
            return M.p.label(M);
        } else {
            baselib.exceptions.raiseArityMismatchError(M, M.p, M.a);
        }
    };



    //////////////////////////////////////////////////////////////////////
    exports.Closure = Closure;
    exports.internalCallDuringPause = internalCallDuringPause;
    exports.finalizeClosureCall = finalizeClosureCall;

    exports.makePrimitiveProcedure = makePrimitiveProcedure;
    exports.makeClosure = makeClosure;

    exports.isClosure = isClosure;

    exports.isProcedure = isProcedure;


    exports.renameProcedure = renameProcedure;

    exports.asJavaScriptFunction = asJavaScriptFunction;
    exports.rawApply = rawApply;


}(this.plt.baselib, this.plt));
/*jslint sub: true, vars: true, nomen: true, plusplus: true, maxerr: 50, indent: 4 */
/*global plt*/

// Modules
(function (baselib, plt) {
    'use strict';
    var exports = {};
    baselib.modules = exports;

    var hasOwnProperty = {}.hasOwnProperty;

    var Namespace = function(modrec) {
        this.modrec = modrec;

        // Returns the key/value pairs of the prefix.
        // mapping: string -> value
        this.mapping = {}; 
    };

    Namespace.prototype.get = function(name) {
        return this.mapping[name];
    };

    Namespace.prototype.hasKey = function(name) {
        return hasOwnProperty.call(this.mapping, name);
    };

    Namespace.prototype.set = function(name, value) {
        this.mapping[name] = value;
    };

    var ModuleRecord = function (name, label) {
        this.name = name;
        this.label = label;
        this.isInvoked = false;
        this.prefix = false;
        this.exports = new Namespace(this);
        this.externalExports = new Namespace(this);

        // JavaScript-implemented code will assign privateExports
        // with all of the exported identifiers.
        this.privateExports = {};
    };

    // Returns the offset into the prefix in which the value will be stored.
    ModuleRecord.prototype.getPrefixOffset = function(externalName) {
        var i;
        for (i = 0; i < this.prefix.names.length; i++) {
            if (this.prefix.names[i] === externalName) {
                return i;
            }
        }
        return void(0);
    };

    // Returns access to the names provided in the module.
    // Note that the names are the names internal to the module.
    ModuleRecord.prototype.getExports = function () {
        return this.exports;
    };    

    // Returns access to the names defined with their external names.
    ModuleRecord.prototype.getExternalExports = function() {
        return this.externalExports;
    };

    // External invokation of a module.
    ModuleRecord.prototype.invoke = function (MACHINE, succ, fail) {
        this._invoke(false, MACHINE, succ, fail);
    };

    // Internal invokation of a module.
    ModuleRecord.prototype.internalInvoke = function (MACHINE, succ, fail) {
        this._invoke(true, MACHINE, succ, fail);
    };

    // Private: general invokation of a module
    ModuleRecord.prototype._invoke = function (isInternal, MACHINE, succ, fail) {
        var that = this;
        MACHINE = MACHINE || plt.runtime.currentMachine;
        succ = succ || function () {};
        fail = fail || function () {};

        var oldErrorHandler = MACHINE.params['currentErrorHandler'];
        var afterGoodInvoke = function (MACHINE) { 
            MACHINE.params['currentErrorHandler'] = oldErrorHandler;
            if (isInternal) { succ(); }
            else {
                throw new plt.runtime.HaltError(succ)
            }
        };

        if (this.isInvoked) {
            succ();
        } else {
            if (isInternal) {
                MACHINE.params['currentErrorHandler'] = function (MACHINE, anError) {
                    MACHINE.params['currentErrorHandler'] = oldErrorHandler;
                    fail(MACHINE, anError);
                };
                MACHINE.c.push(new plt.baselib.frames.CallFrame(afterGoodInvoke, null));
                throw that.label;
            } else {
                MACHINE.exclusiveLock.acquire(
                    void(0),
                    function(release) {
                        MACHINE.params['currentErrorHandler'] = function (MACHINE, anError) {
                            MACHINE.params['currentErrorHandler'] = oldErrorHandler;
                            fail(MACHINE, anError);
                        };
                        MACHINE.c.push(new plt.baselib.frames.CallFrame(afterGoodInvoke, null));
                        MACHINE._trampoline(that.label, false, release);
                    });
            }
        }
    };
    


    exports.ModuleRecord = ModuleRecord;


}(this.plt.baselib, this.plt));
/*global plt*/
/*jslint browser: true, unparam: true, vars: true, white: true, maxerr: 50, indent: 4 , plusplus: true */

// Continuation marks
(function(baselib) {
    'use strict';
    var exports = {};
    baselib.contmarks = exports;


    var ContinuationMarkSet = function(kvlists) {
        this.kvlists = kvlists;
    };


    ContinuationMarkSet.prototype.shift = function() {
        this.kvlists.shift();
    };

    ContinuationMarkSet.prototype.toDomNode = function(params) {
        var dom = document.createElement("span");
        dom.appendChild(document.createTextNode('#<continuation-mark-set>'));
        return dom;
    };

    ContinuationMarkSet.prototype.toWrittenString = function(cache) {
        return '#<continuation-mark-set>';
    };

    ContinuationMarkSet.prototype.toDisplayedString = function(cache) {
        return '#<continuation-mark-set>';
    };

    ContinuationMarkSet.prototype.ref = function(key, promptTag) {
        // FIXME: ref needs to watch the promptTag as well and capture up to it.
        var i, j;
        var result = [];
        var kvlist;
        for (i = 0; i < this.kvlists.length; i++) {
            kvlist = this.kvlists[i];
            for (j = 0; j < kvlist.length; j++) {
                if (baselib.equality.equals(kvlist[j][0], key)) {
                    result.push(kvlist[j][1]);
                }
            }
        }
        return baselib.lists.arrayToList(result);
    };


    ContinuationMarkSet.prototype.refFirst = function(key, promptTag) {
        // FIXME: refFirst needs to watch the promptTag as well and capture up to it.
        var i, j;
        var result = [];
        var kvlist;
        for (i = 0; i < this.kvlists.length; i++) {
            kvlist = this.kvlists[i];
            for (j = 0; j < kvlist.length; j++) {
                if (baselib.equality.equals(kvlist[j][0], key)) {
                    return (kvlist[j][1]);
                }
            }
        }
        return undefined;
    };




    // Returns an approximate stack trace.
    // getContext: MACHINE -> (arrayof (U Procedure (Vector source line column position span)))
    ContinuationMarkSet.prototype.getContext = function(MACHINE) {
        var i, j;
        var result = [];
        var kvlist;

        var tracedAppKey = plt.runtime.getTracedAppKey(MACHINE);
        var tracedCalleeKey = plt.runtime.getTracedCalleeKey(MACHINE);
        var proc, locationVector;

        for (i = 0; i < this.kvlists.length; i++) {
            kvlist = this.kvlists[i];
            for (j = 0; j < kvlist.length; j++) {
                if (kvlist[j][0] === tracedAppKey) {
                    locationVector = kvlist[j][1];
                    result.push(locationVector);
                } else if (kvlist[j][0] === tracedCalleeKey) {
                    proc = kvlist[j][1];
                    if (proc !== null) {
                        result.push(proc);
                    }
                }
            }
        }
        return result;
    };


    var isContinuationMarkSet = baselib.makeClassPredicate(ContinuationMarkSet);





    // A continuation prompt tag labels a prompt frame.
    var ContinuationPromptTag = function(name) {
	this.name = name;         // (U String false)

    };

    ContinuationPromptTag.prototype.toDomNode = function(params) {
        var dom = document.createElement("span");
        if (this.name) {
            dom.appendChild(document.createTextNode('#<continuation-prompt-tag:' 
                                                    + this.name + '>'));
        } else {
            dom.appendChild(document.createTextNode('#<continuation-prompt-tag>'));
        }
        return dom;
    };

    ContinuationPromptTag.prototype.toWrittenString = function(cache) {
        if (this.name) {
            return '#<continuation-prompt-tag' + this.name + '>';
        } else {
            return '#<continuation-prompt-tag>';
        }
    };

    ContinuationPromptTag.prototype.toDisplayedString = function(cache) {
        if (this.name) {
            return '#<continuation-prompt-tag' + this.name + '>';
        } else {
            return '#<continuation-prompt-tag>';
        }
    };



    var isContinuationPromptTag = baselib.makeClassPredicate(ContinuationPromptTag);

    var DEFAULT_CONTINUATION_PROMPT_TAG =
        new ContinuationPromptTag("default");


    exports.ContinuationMarkSet = ContinuationMarkSet;
    exports.isContinuationMarkSet = isContinuationMarkSet;
    exports.ContinuationPromptTag = ContinuationPromptTag;

    exports.isContinuationPromptTag = isContinuationPromptTag;
    exports.DEFAULT_CONTINUATION_PROMPT_TAG = DEFAULT_CONTINUATION_PROMPT_TAG;
}(this.plt.baselib));
/*jslint browser: false, unparam: true, vars: true, white: true, plusplus: true, maxerr: 50, indent: 4 */

// Arity structure
(function(baselib) {
    'use strict';
    var exports = {};
    baselib.arity = exports;



    var ArityAtLeast = baselib.structs.makeStructureType(
        'arity-at-least', false, 1, 0, false, false);


    // An arity is either a primitive number, an ArityAtLeast instance,
    // or a list of either primitive numbers or ArityAtLeast instances.



    var isArityAtLeast = ArityAtLeast.predicate;
    var arityAtLeastValue = function(x) { 
        var val = ArityAtLeast.accessor(x, 0);
        return val;
    };


    ArityAtLeast.type.prototype.toString = function() {
        return '#<arity-at-least ' + arityAtLeastValue(this) + '>';
    };



    // isArityMatching: arity natural -> boolean
    // Produces true if n satisfies the arity.
    var isArityMatching = function(arity, n) {
	if (typeof(arity) === 'number') {
	    return arity === n;
	} else if (isArityAtLeast(arity)) {
	    return n >= arityAtLeastValue(arity);
	} else {
	    while (arity !== baselib.lists.EMPTY) {
		if (typeof(arity.first) === 'number') {
		    if (arity.first === n) { return true; }
		} else if (isArityAtLeast(arity.first)) {
		    if (n >= arityAtLeastValue(arity.first)) { return true; }
		}
		arity = arity.rest;
	    }
	    return false;
	}
    };





    //////////////////////////////////////////////////////////////////////

    exports.ArityAtLeast = ArityAtLeast;
    exports.makeArityAtLeast = function() { 
        var args = [].slice.call(arguments);
        return ArityAtLeast.constructor(args);
    };
    exports.isArityAtLeast = isArityAtLeast;
    exports.isArityMatching = isArityMatching;
    exports.arityAtLeastValue = arityAtLeastValue;

}(this.plt.baselib));/*jslint vars: true, maxerr: 50, indent: 4 */

// Structure types

(function (baselib) {
    'use strict';
    var exports = {};
    baselib.inspectors = exports;


    var Inspector = function () {
    };
    var DEFAULT_INSPECTOR = new Inspector();

    Inspector.prototype.toString = function () {
        return "#<inspector>";
    };

    var isInspector = baselib.makeClassPredicate(Inspector);



    exports.Inspector = Inspector;
    exports.DEFAULT_INSPECTOR = DEFAULT_INSPECTOR;

    exports.isInspector = isInspector;


}(this.plt.baselib));/*jslint browser: true, undef: false, unparam: true, sub: true, vars: true, white: true, plusplus: true, maxerr: 50, indent: 4 */

// Exceptions

(function(baselib) {
    'use strict';
    var exceptions = {};
    baselib.exceptions = exceptions;



    var RacketError = function(message, racketError) {
        Error.call(this, message);
        this.message = message;
        this.racketError = racketError;
    };
    RacketError.prototype = baselib.heir(Error.prototype);
    var isRacketError = baselib.makeClassPredicate(RacketError);



    // (define-struct exn (message continuation-mark-set))
    var Exn = baselib.structs.makeStructureType(
        'exn', false, 2, 0, false, false);


    // (define-struct (exn:break exn) (continuation))
    var ExnBreak = baselib.structs.makeStructureType(
        'exn:break', Exn, 1, 0, false, false);


    var ExnFail = baselib.structs.makeStructureType(
        'exn:fail', Exn, 0, 0, false, false);

    var ExnFailContract = baselib.structs.makeStructureType(
        'exn:fail:contract', ExnFail, 0, 0, false, false);

    var ExnFailContractArity = baselib.structs.makeStructureType(
        'exn:fail:contract:arity', ExnFailContract, 0, 0, false, false);

    // exn:fail:contract (id)
    var ExnFailContractVariable = baselib.structs.makeStructureType(
        'exn:fail:contract:variable', ExnFailContract, 1, 0, false, false);

    var ExnFailContractDivisionByZero = baselib.structs.makeStructureType(
        'exn:fail:contract:divide-by-zero', ExnFailContract, 0, 0, false, false);





    var exceptionHandlerKey = new baselib.symbols.Symbol("exnh");





    //////////////////////////////////////////////////////////////////////

    // Raise error to the toplevel.

    // If the error is of an exception type, make sure e.message holds the string
    // value to allow integration with systems that don't recognize Racket error 
    // structures.
    var raise = function(MACHINE, e) { 
        if (isRacketError(e) && Exn.predicate(e.racketError)) {
            e.message = Exn.accessor(e.racketError, 0);
        } else if (Exn.predicate(e)) {
            e = new RacketError(Exn.accessor(e, 0), e);
        }

        // if (window.console !== void(0) &&
        //     typeof(window.console.log) === 'function') {
        //     window.console.log(MACHINE);
        //     if (e.stack) { window.console.log(e.stack); }
        //     else { window.console.log(e); }
        // } 

        throw e; 
    };


    var raiseExnBreak = function(MACHINE, msg) {
        var contMarks = MACHINE.captureContinuationMarks();
        var k = false;
        // FIXME: capture the current continuation and stuff it into
        // k, to allow restart if possible.
        raise(MACHINE, ExnBreak.constructor([msg, contMarks, k]));
    };



    var raiseFailure = function(MACHINE, msg) {
        var contMarks = MACHINE.captureContinuationMarks();
        raise(MACHINE, ExnFail.constructor([msg, contMarks]));
    };

    var raiseContractError = function(MACHINE, msg) {
        var contMarks = MACHINE.captureContinuationMarks();
        raise(MACHINE, ExnFailContract.constructor([msg, contMarks]));
    };

    var raiseDivisionByZeroError = function(MACHINE, msg) {
        var contMarks = MACHINE.captureContinuationMarks();
        raise(MACHINE, ExnFailContractDivisionByZero.constructor([msg, contMarks]));
    };


    var raiseUnboundToplevelError = function(MACHINE, name) {
        var message = baselib.format.format("Not bound: ~a", [name]);
        var contMarks = MACHINE.captureContinuationMarks();
        raise(MACHINE, 
              ExnFailContractVariable.constructor([message, 
                                                   contMarks, 
                                                   baselib.symbols.makeSymbol(name)])); 
    };


    var raiseArgumentTypeError = function(MACHINE, 
                                          callerName,
                                          expectedTypeName,
                                          argumentOffset,
                                          actualValue) {
        var message;
        var contMarks = MACHINE.captureContinuationMarks();
        if (argumentOffset !== void(0)) {
            message = baselib.format.format(
                          "~a: expected ~a as argument ~e but received ~e",
                          [callerName,
                           expectedTypeName,
                           (argumentOffset + 1),
                           actualValue]);
            raise(MACHINE, ExnFailContract.constructor([message, contMarks]));
        } else {
            message = baselib.format.format(
                          "~a: expected ~a but received ~e",
                          [callerName,
                           expectedTypeName,
                           actualValue]);
            raise(MACHINE, ExnFailContract.constructor([message, contMarks]));
        }
    };

    var raiseContextExpectedValuesError = function(MACHINE, expected) {
        var message = baselib.format.format("expected ~e values, received ~e values",
                                            [expected, MACHINE.a]);
        var contMarks = MACHINE.captureContinuationMarks();
        raise(MACHINE, ExnFailContract.constructor([message, contMarks]));
    };

    var raiseArityMismatchError = function(MACHINE, proc, received) {
        raise(MACHINE, makeArityMismatchError(MACHINE, proc, received));
    };

    var makeArityMismatchError = function(MACHINE, proc, received) {
        var message = baselib.format.format("~a: expected ~e value(s), received ~e value(s)",
                                            [proc.displayName,
                                             proc.racketArity,
                                             received]);
        var contMarks = MACHINE.captureContinuationMarks();
        return ExnFailContractArity.constructor([message, contMarks]);
    };


    var raiseOperatorApplicationError = function(MACHINE, operator) {
        var message = baselib.format.format("not a procedure: ~e",
                                            [operator]);
        var contMarks = MACHINE.captureContinuationMarks();
        raise(MACHINE, 
              ExnFailContract.constructor([message, contMarks]));
    };


    var raiseOperatorIsNotPrimitiveProcedure = function(MACHINE, operator) {
        var message = baselib.format.format("not a primitive procedure: ~e",
                                            [operator]);
        var contMarks = MACHINE.captureContinuationMarks();
        raise(MACHINE,
              ExnFailContract.constructor([message, contMarks]));
    };


    var raiseUnimplementedPrimitiveError = function(MACHINE, name) {
        var message = "unimplemented kernel procedure: " + name;
        var contMarks = MACHINE.captureContinuationMarks();
        raise(MACHINE,
              ExnFailContract.constructor([message, contMarks]));
    };


    var raiseModuleLoadingError = function(MACHINE, name) {
        var message = "unable to dynamically load module: " + name;
        var contMarks = MACHINE.captureContinuationMarks();
        raise(MACHINE,
              ExnFail.constructor([message, contMarks]));
    };








    //////////////////////////////////////////////////////////////////////
    // Exports



    exceptions.RacketError = RacketError;
    exceptions.isRacketError = isRacketError;

    exceptions.Exn = Exn;
    exceptions.makeExn = function(msg, marks) { return Exn.constructor([msg, marks]); };
    exceptions.isExn = Exn.predicate;
    exceptions.exnMessage = function(exn) { return Exn.accessor(exn, 0); };
    exceptions.exnContMarks = function(exn) { return Exn.accessor(exn, 1); };
    exceptions.exnSetContMarks = function(exn, v) { Exn.mutator(exn, 1, v); };

    exceptions.ExnBreak = ExnBreak;
    exceptions.makeExnBreak = function(msg, marks, k) { return ExnBreak.constructor([msg, marks, k]); };
    exceptions.isExnBreak = ExnBreak.predicate;
    exceptions.exnBreakContinuation = 
        function(exn) { return ExnBreak.accessor(exn, 0); };

    exceptions.ExnFail = ExnFail;
    exceptions.makeExnFail = function(msg, marks) { return ExnFail.constructor([msg, marks]); };
    exceptions.isExnFail = ExnFail.predicate;

    exceptions.ExnFailContract = ExnFailContract;
    exceptions.makeExnFailContract = function(msg, marks) { return ExnFailContract.constructor([msg, marks]); };
    exceptions.isExnFailContract = ExnFailContract.predicate;

    exceptions.ExnFailContractArity = ExnFailContractArity;
    exceptions.makeExnFailContractArity = function(msg, marks) { return ExnFailContractArity.constructor([msg, marks]); };
    exceptions.isExnFailContractArity = ExnFailContractArity.predicate;

    exceptions.ExnFailContractVariable = ExnFailContractVariable;
    exceptions.makeExnFailContractVariable = function(msg, marks) { return ExnFailContractVariable.constructor([msg, marks]); };
    exceptions.isExnFailContractVariable = ExnFailContractVariable.predicate;
    exceptions.exnFailContractVariableId = 
        function(exn) { return ExnFailContractVariable.accessor(exn, 0); };


    exceptions.ExnFailContractDivisionByZero = ExnFailContractDivisionByZero;
    exceptions.makeExnFailContractDivisionByZero = 
        function(msg, marks) { return ExnFailContractDivisionByZero.constructor([msg, marks]); };
    exceptions.isExnFailContractDivisionByZero = ExnFailContractDivisionByZero.predicate;


    exceptions.exceptionHandlerKey = exceptionHandlerKey;




    exceptions.raise = raise;
    exceptions.raiseExnBreak = raiseExnBreak;
    exceptions.raiseFailure = raiseFailure;
    exceptions.raiseContractError = raiseContractError;
    exceptions.raiseDivisionByZeroError = raiseDivisionByZeroError;
    exceptions.raiseUnboundToplevelError = raiseUnboundToplevelError;
    exceptions.raiseArgumentTypeError = raiseArgumentTypeError;
    exceptions.raiseContextExpectedValuesError = raiseContextExpectedValuesError;
    exceptions.raiseArityMismatchError = raiseArityMismatchError;
    exceptions.makeArityMismatchError = makeArityMismatchError;
    exceptions.raiseOperatorApplicationError = raiseOperatorApplicationError;
    exceptions.raiseOperatorIsNotPrimitiveProcedure = raiseOperatorIsNotPrimitiveProcedure;
    exceptions.raiseUnimplementedPrimitiveError = raiseUnimplementedPrimitiveError;
    exceptions.raiseModuleLoadingError = raiseModuleLoadingError;


}(this.plt.baselib));
/*jslint vars: true, nomen: true, plusplus: true, maxerr: 50, indent: 4 */
// Arity structure
(function (baselib) {
    'use strict';
    var exports = {};
    baselib.readergraph = exports;


    var readerGraph = function (x, objectHash, n) {
        var i;
        if (typeof (x) === 'object' && objectHash.containsKey(x)) {
            return objectHash.get(x);
        }

        if (baselib.lists.isPair(x)) {
            var consPair = baselib.lists.makePair(x.first, x.rest);
            objectHash.put(x, consPair);
            consPair.first = readerGraph(x.first, objectHash, n + 1);
            consPair.rest = readerGraph(x.rest, objectHash, n + 1);
            return consPair;
        }

        if (baselib.vectors.isVector(x)) {
            var len = x.length();
            var aVector = baselib.vectors.makeVector(x.elts.slice(0));
            objectHash.put(x, aVector); 
            for (i = 0; i < len; i++) {
                aVector.elts[i] = readerGraph(aVector.elts[i], objectHash, n + 1);
            }
            return aVector;
        }

        if (baselib.boxes.isBox(x)) {
            var aBox = baselib.boxes.makeBox(x.ref());
            objectHash.put(x, aBox);
            aBox.val = readerGraph(x.ref(), objectHash, n + 1);
            return aBox;
        }

        if (baselib.hashes.isHash(x)) {
            throw new Error("make-reader-graph of hash not implemented yet");
        }

        if (baselib.structs.isStruct(x)) {
            var aStruct = baselib.clone(x);
            objectHash.put(x, aStruct);
            for (i = 0; i < x._fields.length; i++) {
                x._fields[i] = readerGraph(x._fields[i], objectHash, n + 1);
            }
            return aStruct;
        }

        if (baselib.placeholders.isPlaceholder(x)) {
            return readerGraph(x.ref(), objectHash, n + 1);
        }

        return x;
    };

    exports.readerGraph = readerGraph;

}(this.plt.baselib));/*jslint vars: true, white: true, plusplus: true, maxerr: 50, indent: 4 */


// Helper functions for argument checking.

(function (baselib) {
    'use strict';
    var exports = {};
    baselib.check = exports;

    var EMPTY = baselib.lists.EMPTY;
    var isPair = baselib.lists.isPair;
    var isList = baselib.lists.isList;
    var makeLowLevelEqHash = baselib.hashes.makeLowLevelEqHash;


    //////////////////////////////////////////////////////////////////////

    // testArgument: (X -> boolean) X number string string -> boolean
    // Produces the argument value the predicate is true, and otherwise raises an error.
    var testArgument = function (MACHINE,
                                 expectedTypeName,
                                 predicate,                          
                                 val, 
                                 index, 
                                 callerName) {
        if (predicate(val)) {
            return val;
        } else {
            if (typeof(expectedTypeName) === 'function') { 
                expectedTypeName = expectedTypeName(); 
            }
            baselib.exceptions.raiseArgumentTypeError(MACHINE, 
                                                      callerName,
                                                      expectedTypeName,
                                                      index,
                                                      val);
        }
    };


    var makeCheckArgumentType = function (predicate, predicateName) {
        return function (MACHINE, callerName, position) {
            return testArgument(
                MACHINE,
                predicateName,
                predicate,
                MACHINE.e[MACHINE.e.length - 1 - position],
                position,
                callerName);
        };
    };



    var makeCheckListofArgumentType = function (predicate, predicateName) {
        var listPredicate = function (x) {
            if (! isList(x)) { return false; }
            while (true) {
                if (x === EMPTY){
                    return true;
                }
                if (! predicate(x.first)) {
                    return false;
                }
                x = x.rest;
            }
        };
        return function (MACHINE, callerName, position) {
            return testArgument(
                MACHINE,
                'list of ' + predicateName,
                listPredicate,
                MACHINE.e[MACHINE.e.length - 1 - position],
                position,
                callerName);
        };
    };







    var testArity = function (MACHINE, callerName, observed, minimum, maximum) {
        if (observed < minimum || observed > maximum) {
            baselib.exceptions.raise(
                MACHINE, 
                baselib.exceptions.ExnFailContractArity.constructor(
                    [callerName + ": expected at least " + minimum
                     + " arguments "
                     + " but received " + observed,
                     MACHINE.captureContinuationMarks()]));
        }
    };




    var checkOutputPort = makeCheckArgumentType(
        baselib.ports.isOutputPort,
        'output port');

    var checkInputPort = makeCheckArgumentType(
        baselib.ports.isInputPort,
        'input port');

    var checkSymbol = makeCheckArgumentType(
        baselib.symbols.isSymbol,
        'symbol');

    var checkString = makeCheckArgumentType(
        baselib.strings.isString,
        'string');

    var checkSymbolOrString = makeCheckArgumentType(
        function(x) { return (baselib.symbols.isSymbol(x) || 
                              baselib.strings.isString(x)); },
        'symbol or string');

    var checkMutableString = makeCheckArgumentType(
        baselib.strings.isMutableString,
        'mutable string');

    var checkChar = makeCheckArgumentType(
        baselib.chars.isChar,
        'character');

    var checkProcedure = makeCheckArgumentType(
        baselib.functions.isProcedure,
        'procedure');

    var checkNumber = makeCheckArgumentType(
        baselib.numbers.isNumber,
        'number');

    var checkReal = makeCheckArgumentType(
        baselib.numbers.isReal,
        'real');

    var checkNatural = makeCheckArgumentType(
        baselib.numbers.isNatural,
        'natural');

    var checkNaturalOrFalse = makeCheckArgumentType(
        function(x) { return (baselib.numbers.isNatural(x) || 
                              x === false); },
        'natural or false');

    var checkByte = makeCheckArgumentType(
        baselib.numbers.isByte,
        'byte');

    var checkBytes = makeCheckArgumentType(
        baselib.bytes.isBytes,
        'bytes');

    var checkNaturalInRange = function(M, callerName, index, a, b) {
        var expectedTypeName;
        var x = M.e[M.e.length - 1 - index];
        // fast path: if a, b, and x are numbers
        if (typeof(x) === 'number' && typeof(a) === 'number' && typeof(b) === 'number') {
            if (a <= x && x < b) { 
                return x; 
            }
            else {
                expectedTypeName = baselib.format.format('natural between ~a and ~a', [a, b]);
                return baselib.exceptions.raiseArgumentTypeError(m, 
                                                                 callerName,
                                                                 expectedTypeName,
                                                                 index,
                                                                 x);
            }
        } else {
            if (baselib.numbers.lessThanOrEqual(a, x) && baselib.numbers.lessThan(x, b)) {
                return x;
            } else {
                expectedTypeName = baselib.format.format('natural between ~a and ~a', [a, b]);
                return baselib.exceptions.raiseArgumentTypeError(m, 
                                                                 callerName,
                                                                 expectedTypeName,
                                                                 index,
                                                                 x);
            }
        }
    };


    var checkInteger = makeCheckArgumentType(
        baselib.numbers.isInteger,
        'integer');

    var checkRational = makeCheckArgumentType(
        baselib.numbers.isRational,
        'rational');

    var checkNonNegativeReal = makeCheckArgumentType(
        baselib.numbers.isNonNegativeReal,
        'non-negative real');

    var checkPair = makeCheckArgumentType(
        baselib.lists.isPair,
        'pair');

    var checkList = makeCheckArgumentType(
        isList,
        'list');

    var checkVector = makeCheckArgumentType(
        baselib.vectors.isVector,
        'vector');

    var checkBoolean = makeCheckArgumentType(
        function (x) { return x === true || x === false; },
        'boolean');

    var checkBox = makeCheckArgumentType(
        baselib.boxes.isBox,
        'box');

    var checkMutableBox = makeCheckArgumentType(
        baselib.boxes.isMutableBox,
        'mutable box');

    var checkInspector = makeCheckArgumentType(
        baselib.inspectors.isInspector,
        'inspector');


    var checkPlaceholder = makeCheckArgumentType(
        baselib.placeholders.isPlaceholder,
        'placeholder');


    var checkSrcloc = makeCheckArgumentType(
        baselib.srclocs.isSrcloc,
        'srcloc');

    var checkContinuationMarkSet = makeCheckArgumentType(
        baselib.contmarks.isContinuationMarkSet,
        'continuation mark set');

    var checkContinuationPromptTag = makeCheckArgumentType(
        baselib.contmarks.isContinuationPromptTag,
        'continuation prompt tag');

    var checkExn = makeCheckArgumentType(
        baselib.exceptions.isExn,
        'exn');

    var checkHash = makeCheckArgumentType(
        baselib.hashes.isHash,
        'hash');
    var checkHasheq = makeCheckArgumentType(
        baselib.hashes.isHasheq,
        'hash');
    var checkHasheqv = makeCheckArgumentType(
        baselib.hashes.isHasheqv,
        'hash');
    var checkMutableHash = makeCheckArgumentType(
        function(x) { return baselib.hashes.isHash(x) && ! x.isImmutable()},
        'mutable hash');
    var checkImmutableHash = makeCheckArgumentType(
        function(x) { return baselib.hashes.isHash(x) && x.isImmutable()},
        'immutable hash');






    //////////////////////////////////////////////////////////////////////


    exports.testArgument = testArgument;
    exports.testArity = testArity;
    exports.makeCheckArgumentType = makeCheckArgumentType;
    exports.makeCheckListofArgumentType = makeCheckListofArgumentType;
    exports.checkOutputPort = checkOutputPort;
    exports.checkInputPort = checkInputPort;
    exports.checkSymbol = checkSymbol;
    exports.checkString = checkString;
    exports.checkSymbolOrString = checkSymbolOrString;
    exports.checkMutableString = checkMutableString;
    exports.checkChar = checkChar;
    exports.checkProcedure = checkProcedure;
    exports.checkNumber = checkNumber;
    exports.checkReal = checkReal;
    exports.checkNonNegativeReal = checkNonNegativeReal;
    exports.checkNatural = checkNatural;
    exports.checkNaturalOrFalse = checkNaturalOrFalse;
    exports.checkNaturalInRange = checkNaturalInRange;
    exports.checkByte = checkByte;
    exports.checkBytes = checkBytes;
    exports.checkInteger = checkInteger;
    exports.checkRational = checkRational;
    exports.checkPair = checkPair;
    exports.checkList = checkList;
    exports.checkVector = checkVector;
    exports.checkBox = checkBox;
    exports.checkMutableBox = checkMutableBox;
    exports.checkInspector = checkInspector;
    exports.checkByte = checkByte;
    exports.checkBoolean = checkBoolean;
    exports.checkPlaceholder = checkPlaceholder;
    exports.checkSrcloc = checkSrcloc;
    exports.checkContinuationMarkSet = checkContinuationMarkSet;
    exports.checkContinuationPromptTag = checkContinuationPromptTag;
    exports.checkExn = checkExn;
    exports.checkHash = checkHash;
    exports.checkImmutableHash = checkImmutableHash;
    exports.checkMutableHash = checkMutableHash;

}(this.plt.baselib));
/*global plt*/
/*jslint unparam: true, sub: true, vars: true, white: true, nomen: true, plusplus: true, maxerr: 50, indent: 4 */

// Arity structure
(function (baselib) {
    'use strict';
    var exports = {};
    baselib.primitives = exports;


    //////////////////////////////////////////////////////////////////////
    //////////////////////////////////////////////////////////////////////
    // We try to isolate the effect of external modules: all the identifiers we
    // pull from external modules should be listed here, and should otherwise not
    // show up outside this section!
    var isNumber = baselib.numbers.isNumber;

    var isProcedure = baselib.functions.isProcedure;
    var isReal = baselib.numbers.isReal;
    var isInexact = baselib.numbers.isInexact;
    var isComplex = baselib.numbers.isComplex;
    var isRational = baselib.numbers.isRational;
    var isBytes = baselib.bytes.isBytes;

    var isNatural = baselib.numbers.isNatural;
    var isPair = baselib.lists.isPair;
    var isList = baselib.lists.isList;
    var isChar = baselib.chars.isChar;
    var isVector = baselib.vectors.isVector;
    var isString = baselib.strings.isString;
    var isSymbol = baselib.symbols.isSymbol;
    var isBox = baselib.boxes.isBox;
    var isStruct = baselib.structs.isStruct;
    var isStructType = baselib.structs.isStructType;
    var equals = baselib.equality.equals;

    var NULL = baselib.lists.EMPTY;
    var VOID = baselib.constants.VOID_VALUE;

    var makeFloat = baselib.numbers.makeFloat;
    var makeComplex = baselib.numbers.makeComplex;
    var makeComplexPolar = baselib.numbers.makeComplexPolar;

    var makeSymbol = baselib.symbols.makeSymbol;

    var makeBox = baselib.boxes.makeBox;

    var makeVector = baselib.vectors.makeVector;
    var makeList = baselib.lists.makeList;
    var makePair = baselib.lists.makePair;

    var finalizeClosureCall = baselib.functions.finalizeClosureCall;
    var makePrimitiveProcedure = baselib.functions.makePrimitiveProcedure;
    var makeClosure = baselib.functions.makeClosure;


    // Other helpers
    var withArguments = baselib.withArguments;
    var toDomNode = baselib.format.toDomNode;



    // Exceptions and error handling.
    var raise = baselib.exceptions.raise;
    var raiseContractError = baselib.exceptions.raiseContractError;
    var raiseDivisionByZeroError = baselib.exceptions.raiseDivisionByZeroError;
    var raiseArgumentTypeError = baselib.exceptions.raiseArgumentTypeError;
    var raiseArityMismatchError = baselib.exceptions.raiseArityMismatchError;



    var testArgument = baselib.check.testArgument;

    var checkOutputPort = baselib.check.checkOutputPort;
    var checkInputPort = baselib.check.checkInputPort;
    var checkString = baselib.check.checkString;
    var checkSymbolOrString = baselib.check.checkSymbolOrString;
    var checkMutableString = baselib.check.checkMutableString;
    var checkSymbol = baselib.check.checkSymbol;
    var checkByte = baselib.check.checkByte;
    var checkChar = baselib.check.checkChar;
    var checkProcedure = baselib.check.checkProcedure;
    var checkNumber = baselib.check.checkNumber;
    var checkReal = baselib.check.checkReal;
    var checkNonNegativeReal = baselib.check.checkNonNegativeReal;
    var checkNatural = baselib.check.checkNatural;
    var checkNaturalOrFalse = baselib.check.checkNaturalOrFalse;
    var checkNaturalInRange = baselib.check.checkNaturalInRange;
    var checkInteger = baselib.check.checkInteger;
    var checkIntegerForChar = baselib.check.makeCheckArgumentType(
        function(x) {
            return (baselib.numbers.isInteger(x) &&
                    ((baselib.numbers.lessThanOrEqual(0, x) &&
                      baselib.numbers.lessThanOrEqual(x, 55295))
                     ||
                     (baselib.numbers.lessThanOrEqual(57344, x) &&
                      baselib.numbers.lessThanOrEqual(x, 1114111))));
        },
        'integer'
    );
    var checkRational = baselib.check.checkRational;
    var checkPair = baselib.check.checkPair;
    var checkList = baselib.check.checkList;
    var checkListofChars = baselib.check.makeCheckListofArgumentType(isChar, 'character');
    var checkListofPairs = baselib.check.makeCheckListofArgumentType(isPair, 'pair');
    var checkVector = baselib.check.checkVector;
    var checkBox = baselib.check.checkBox;
    var checkMutableBox = baselib.check.checkMutableBox;
    var checkInspector = baselib.check.checkInspector;
    var checkPlaceholder = baselib.check.checkPlaceholder;
    var checkSrcloc = baselib.check.checkSrcloc;
    var checkContinuationPromptTag = baselib.check.checkContinuationPromptTag;
    var checkContinuationMarkSet = baselib.check.checkContinuationMarkSet;
    var checkExn = baselib.check.checkExn;
    var checkHash = baselib.check.checkHash;
    var checkMutableHash = baselib.check.checkMutableHash;
    var checkImmutableHash = baselib.check.checkImmutableHash;

    // Just for consistency with the other names, we provide checkAny, which
    // doesn't really do any checking.
    var checkAny = function(M, name, offset) {
        return M.e[M.e.length-1-offset];
    };

    var checkPromptTag = baselib.check.makeCheckArgumentType(
        baselib.contmarks.isContinuationPromptTag,
        'prompt tag');

    var PromptFrame = baselib.frames.PromptFrame;

    //////////////////////////////////////////////////////////////////////





    // Primitives are the set of primitive values.  Not all primitives
    // are coded here; several of them (including call/cc) are injected by
    // the bootstrapping code in compiler/boostrapped-primitives.rkt
    var Primitives = {};

    var installPrimitiveProcedure = function (name, arity, f) {
        Primitives[name] = makePrimitiveProcedure(name, arity, f);
    };

    var installPrimitiveClosure = function (name, arity, f) {
        Primitives[name] = makeClosure(name, arity, f, []);
    };


    var installPrimitiveConstant = function (name, v) {
        Primitives[name] = v;
    };



    installPrimitiveConstant('pi', baselib.numbers.pi);
    installPrimitiveConstant('e', baselib.numbers.e);
    installPrimitiveConstant('null', NULL);
    installPrimitiveConstant('true', true);
    installPrimitiveConstant('false', false);
    installPrimitiveConstant('eof', baselib.constants.EOF_VALUE);


    installPrimitiveConstant('exception-handler-key',
                             baselib.paramz.exceptionHandlerKey);
    installPrimitiveConstant('parameterization-key',
                             baselib.paramz.parameterizationKey);
    installPrimitiveConstant('break-enabled-key',
                             baselib.paramz.breakEnabledKey);


    var gensymCounter = 0;
    installPrimitiveProcedure(
        'gensym',
        makeList(0, 1),
        function(M) {
            var baseName = "g";
            if (M.a === 1) {
                baseName = checkSymbolOrString(M, 'gensym', 0).toString();
            }
            gensymCounter++;
            return new baselib.symbols.Symbol(baseName + gensymCounter);
        });


    installPrimitiveProcedure(
        'display',
        makeList(1, 2),
        function (M) {
            var firstArg = M.e[M.e.length - 1];
            var outputPort = M.params.currentOutputPort;
            if (M.a === 2) {
                outputPort = checkOutputPort(M, 'display', 1);
            }
            outputPort.writeDomNode(M, toDomNode(firstArg, 'display'));
            return VOID;
        });

    installPrimitiveProcedure(
        'write',
        makeList(1, 2),
        function (M) {
            var firstArg = M.e[M.e.length - 1];
            var outputPort = M.params.currentOutputPort;
            if (M.a === 2) {
                outputPort = checkOutputPort(M, 'write', 1);
            }
            outputPort.writeDomNode(M, toDomNode(firstArg, 'write'));
            return VOID;
        });

    installPrimitiveProcedure(
        'write-byte',
        makeList(1, 2),
        function (M) {
            var firstArg = checkByte(M, 'write-byte', 0);
            var outputPort = M.params.currentOutputPort;
            if (M.a === 2) {
                outputPort = checkOutputPort(M, 'display', 1);
            }
            outputPort.writeDomNode(M, toDomNode(String.fromCharCode(firstArg), 'display'));
            return VOID;
        });


    installPrimitiveProcedure(
        'newline', makeList(0, 1),
        function (M) {
            var outputPort = M.params.currentOutputPort;
            if (M.a === 1) {
                outputPort = checkOutputPort(M, 'newline', 1);
            }
            outputPort.writeDomNode(M, toDomNode("\n", 'display'));
            return VOID;
        });

    installPrimitiveProcedure(
        'displayln',
        makeList(1, 2),
        function (M){
            var firstArg = M.e[M.e.length-1];
            var outputPort = M.params.currentOutputPort;
            if (M.a === 2) {
                outputPort = checkOutputPort(M, 'displayln', 1);
            }
            outputPort.writeDomNode(M, toDomNode(firstArg, 'display'));
            outputPort.writeDomNode(M, toDomNode("\n", 'display'));
            return VOID;
        });



    installPrimitiveProcedure(
        'format',
        baselib.arity.makeArityAtLeast(1),
        function (M) {
            var args = [], i, formatString;
            formatString = checkString(M, 'format', 0).toString();
            for(i = 1; i < M.a; i++) {
                args.push(M.e[M.e.length - 1 - i]);
            }
            return baselib.format.format(formatString, args, 'format');
        });


    installPrimitiveProcedure(
        'printf',
        baselib.arity.makeArityAtLeast(1),
        function (M) {
            var args = [], i, formatString, result, outputPort;
            formatString = checkString(M, 'printf', 0).toString();
            for(i = 1; i < M.a; i++) {
                args.push(M.e[M.e.length - 1 - i]);
            }
            result = baselib.format.format(formatString, args, 'format');
            outputPort = M.params.currentOutputPort;
            outputPort.writeDomNode(M, toDomNode(result, 'display'));
            return VOID;
        });


    installPrimitiveProcedure(
        'fprintf',
        baselib.arity.makeArityAtLeast(2),
        function (M) {
            var args = [], i, formatString, outputPort, result;
            outputPort = checkOutputPort(M, 'fprintf', 0);
            formatString = checkString(M, 'fprintf', 1).toString();
            for(i = 2; i < M.a; i++) {
                args.push(M.e[M.e.length - 1 - i]);
            }
            result = baselib.format.format(formatString, args, 'format');
            outputPort.writeDomNode(M, toDomNode(result, 'display'));
            return VOID;
        });



    installPrimitiveProcedure(
        'current-print',
        makeList(0, 1),
        function (M) {
            if (M.a === 1) {
                M.params['currentPrint'] =
                    checkProcedure(M, 'current-print', 0);
                return VOID;
            } else {
                return M.params['currentPrint'];
            }
        });


    installPrimitiveProcedure(
        'current-print-mode',
        makeList(0, 1),
        function (M) {
            if (M.a === 1) {
                M.params['print-mode'] = checkString(M, 'print-mode', 0);
                return VOID;
            } else {
                return M.params['print-mode'];
            }
        });


    installPrimitiveProcedure(
        'current-output-port',
        makeList(0, 1),
        function (M) {
            if (M.a === 1) {
                M.params['currentOutputPort'] =
                    checkOutputPort(M, 'current-output-port', 0);
                return VOID;
            } else {
                return M.params['currentOutputPort'];
            }
        });



    installPrimitiveProcedure(
        'current-error-port',
        makeList(0, 1),
        function (M) {
            if (M.a === 1) {
                M.params['currentErrorPort'] =
                    checkOutputPort(M, 'current-output-port', 0);
                return VOID;
            } else {
                return M.params['currentOutputPort'];
            }
        });



    installPrimitiveProcedure(
        'current-input-port',
        makeList(0, 1),
        function (M) {
            if (M.a === 1) {
                M.params['currentInputPort'] =
                    checkInputPort(M, 'current-input-port', 0);
                return VOID;
            } else {
                return M.params['currentInputPort'];
            }
        });



    installPrimitiveClosure(
        'read-byte',
        makeList(0, 1),
        function(M) {
            var inputPort = M.params['currentInputPort'];
            if (M.a === 1) {
                inputPort = checkInputPort(M, 'read-byte', 0);
            }
            return plt.runtime.PAUSE(function(restart) {
                inputPort.callWhenReady(M, function() {
                    restart(function(MACHINE) {
                        plt.runtime.finalizeClosureCall(MACHINE,
                                                        inputPort.readByte(MACHINE));
                    });
                });
            });
        });



    installPrimitiveProcedure(
        '=',
        baselib.arity.makeArityAtLeast(2),
        function (M) {
	    var i;
            var firstArg = checkNumber(M, '=', 0), secondArg;
            for (i = 1; i < M.a; i++) {
                secondArg = checkNumber(M, '=', i);
                if (! (baselib.numbers.equals(firstArg, secondArg))) {
                    return false;
                }
            }
            return true;
        });


    installPrimitiveProcedure(
        '=~',
        3,
        function (M) {
            var x = checkReal(M, '=~', 0);
            var y = checkReal(M, '=~', 1);
            var range = checkNonNegativeReal(M, '=~', 2);
            return baselib.numbers.lessThanOrEqual(
                baselib.numbers.abs(baselib.numbers.subtract(x, y)),
                range);
        });



    var makeChainingBinop = function (predicate, name) {
        return function (M) {
            var firstArg = checkNumber(M, name, 0), secondArg, i;
            for (i = 1; i < M.a; i++) {
                secondArg = checkNumber(M, name, i);
                if (! (predicate(firstArg, secondArg))) {
                    return false;
                }
                firstArg = secondArg;
            }
            return true;
        };
    };

    installPrimitiveProcedure(
        '<',
        baselib.arity.makeArityAtLeast(2),
        makeChainingBinop(baselib.numbers.lessThan, '<'));


    installPrimitiveProcedure(
        '>',
        baselib.arity.makeArityAtLeast(2),
        makeChainingBinop(baselib.numbers.greaterThan, '>'));


    installPrimitiveProcedure(
        '<=',
        baselib.arity.makeArityAtLeast(2),
        makeChainingBinop(baselib.numbers.lessThanOrEqual, '<='));


    installPrimitiveProcedure(
        '>=',
        baselib.arity.makeArityAtLeast(2),
        makeChainingBinop(baselib.numbers.greaterThanOrEqual, '>='));


    installPrimitiveProcedure(
        '+',
        baselib.arity.makeArityAtLeast(0),
        function (M) {
            var result = 0;
            var i = 0;
            for (i = 0; i < M.a; i++) {
                result = baselib.numbers.add(
                    result,
                    checkNumber(M, '+', i));
            }
            return result;
        });


    installPrimitiveProcedure(
        '*',
        baselib.arity.makeArityAtLeast(0),
        function (M) {
            var result = 1;
            var i = 0;
            for (i=0; i < M.a; i++) {
                result = baselib.numbers.multiply(
                    result,
                    checkNumber(M, '*', i));
            }
            return result;
        });

    installPrimitiveProcedure(
        '-',
        baselib.arity.makeArityAtLeast(1),
        function (M) {
            if (M.a === 1) {
                return baselib.numbers.subtract(
                    0,
                    checkNumber(M, '-', 0));
            }
            var result = checkNumber(M, '-', 0), i;
            for (i = 1; i < M.a; i++) {
                result = baselib.numbers.subtract(
                    result,
                    checkNumber(M, '-', i));
            }
            return result;
        });

    installPrimitiveProcedure(
        '/',
        baselib.arity.makeArityAtLeast(1),
        function (M) {
            var result = checkNumber(M, '/', 0), i;
            for (i = 1; i < M.a; i++) {
                result = baselib.numbers.divide(
                    result,
                    checkNumber(M, '/', i));
            }
            return result;
        });

    installPrimitiveProcedure(
        'add1',
        1,
        function (M) {
            var firstArg = checkNumber(M, 'add1', 0);
            return baselib.numbers.add(firstArg, 1);
        });


    installPrimitiveProcedure(
        'sub1',
        1,
        function (M) {
            var firstArg = checkNumber(M, 'sub1', 0);
            return baselib.numbers.subtract(firstArg, 1);
        });


    installPrimitiveProcedure(
        'zero?',
        1,
        function (M) {
            var firstArg = M.e[M.e.length-1];
            return baselib.numbers.equals(firstArg, 0);
        });


    installPrimitiveProcedure(
        'cons',
        2,
        function (M) {
            var firstArg = M.e[M.e.length-1];
            var secondArg = M.e[M.e.length-2];
            return makePair(firstArg, secondArg);
        });


    installPrimitiveProcedure(
        'list',
        baselib.arity.makeArityAtLeast(0),
        function (M) {
            var result = NULL, i;
            for (i = 0; i < M.a; i++) {
                result = makePair(M.e[M.e.length - (M.a - i)],
                                  result);
            }
            return result;
        });

    installPrimitiveProcedure(
        'list*',
        baselib.arity.makeArityAtLeast(1),
        function (M) {
            var result = checkAny(M, 'list*', M.a - 1), i;
            for (i = M.a - 2; i >= 0; i--) {
                result = makePair(M.e[M.e.length - 1 - i],
                                  result);
            }
            return result;
        });


    installPrimitiveProcedure(
        'list-ref',
        2,
        function (M) {
            var lst = checkList(M, 'list-ref', 0);
            var index = checkNaturalInRange(M, 'list-ref', 1,
                                            0, baselib.lists.length(lst));
            return baselib.lists.listRef(lst, baselib.numbers.toFixnum(index));
        });

    installPrimitiveProcedure(
        'unsafe-car',
        1,
        function (M) {
            var firstArg = checkAny(M, 'unsafe-car', 0);
            return firstArg.first;
        });
    installPrimitiveProcedure(
        'unsafe-cdr',
        1,
        function (M) {
            var firstArg = checkAny(M, 'unsafe-cdr', 0);
            return firstArg.rest;
        });
    installPrimitiveProcedure(
        'car',
        1,
        function (M) {
            var firstArg = checkPair(M, 'car', 0);
            return firstArg.first;
        });
    installPrimitiveProcedure(
        'cdr',
        1,
        function (M) {
            var firstArg = checkPair(M, 'cdr', 0);
            return firstArg.rest;
        });

    //////////////////////////////////////////////////////////////////////
    //////////////////////////////////////////////////////////////////////
    // The code from this point up to the end of the c**r functions
    // is autogenerated by whalesong/generate-c-star-d.rkt.
    //
    // Don't modify this code manually: rather, edit the generator and
    // inject the content back in here.
    // Too bad we don't have macros in JavaScript...

    installPrimitiveProcedure(
        'caar',
        1,
        function(M) {
            var x = M.e[M.e.length-1];
            if (isPair(x)&&isPair(x.first)) {
                return x.first.first;
            } else {
                raiseArgumentTypeError(M, "caar", "caarable value", 0, x);
            }
        });
    installPrimitiveProcedure(
        'cdar',
        1,
        function(M) {
            var x = M.e[M.e.length-1];
            if (isPair(x)&&isPair(x.first)) {
                return x.first.rest;
            } else {
                raiseArgumentTypeError(M, "cdar", "cdarable value", 0, x);
            }
        });
    installPrimitiveProcedure(
        'cadr',
        1,
        function(M) {
            var x = M.e[M.e.length-1];
            if (isPair(x)&&isPair(x.rest)) {
                return x.rest.first;
            } else {
                raiseArgumentTypeError(M, "cadr", "cadrable value", 0, x);
            }
        });
    installPrimitiveProcedure(
        'cddr',
        1,
        function(M) {
            var x = M.e[M.e.length-1];
            if (isPair(x)&&isPair(x.rest)) {
                return x.rest.rest;
            } else {
                raiseArgumentTypeError(M, "cddr", "cddrable value", 0, x);
            }
        });
    installPrimitiveProcedure(
        'caaar',
        1,
        function(M) {
            var x = M.e[M.e.length-1];
            if (isPair(x)&&isPair(x.first)&&isPair(x.first.first)) {
                return x.first.first.first;
            } else {
                raiseArgumentTypeError(M, "caaar", "caaarable value", 0, x);
            }
        });
    installPrimitiveProcedure(
        'cdaar',
        1,
        function(M) {
            var x = M.e[M.e.length-1];
            if (isPair(x)&&isPair(x.first)&&isPair(x.first.first)) {
                return x.first.first.rest;
            } else {
                raiseArgumentTypeError(M, "cdaar", "cdaarable value", 0, x);
            }
        });
    installPrimitiveProcedure(
        'cadar',
        1,
        function(M) {
            var x = M.e[M.e.length-1];
            if (isPair(x)&&isPair(x.first)&&isPair(x.first.rest)) {
                return x.first.rest.first;
            } else {
                raiseArgumentTypeError(M, "cadar", "cadarable value", 0, x);
            }
        });
    installPrimitiveProcedure(
        'cddar',
        1,
        function(M) {
            var x = M.e[M.e.length-1];
            if (isPair(x)&&isPair(x.first)&&isPair(x.first.rest)) {
                return x.first.rest.rest;
            } else {
                raiseArgumentTypeError(M, "cddar", "cddarable value", 0, x);
            }
        });
    installPrimitiveProcedure(
        'caadr',
        1,
        function(M) {
            var x = M.e[M.e.length-1];
            if (isPair(x)&&isPair(x.rest)&&isPair(x.rest.first)) {
                return x.rest.first.first;
            } else {
                raiseArgumentTypeError(M, "caadr", "caadrable value", 0, x);
            }
        });
    installPrimitiveProcedure(
        'cdadr',
        1,
        function(M) {
            var x = M.e[M.e.length-1];
            if (isPair(x)&&isPair(x.rest)&&isPair(x.rest.first)) {
                return x.rest.first.rest;
            } else {
                raiseArgumentTypeError(M, "cdadr", "cdadrable value", 0, x);
            }
        });
    installPrimitiveProcedure(
        'caddr',
        1,
        function(M) {
            var x = M.e[M.e.length-1];
            if (isPair(x)&&isPair(x.rest)&&isPair(x.rest.rest)) {
                return x.rest.rest.first;
            } else {
                raiseArgumentTypeError(M, "caddr", "caddrable value", 0, x);
            }
        });
    installPrimitiveProcedure(
        'cdddr',
        1,
        function(M) {
            var x = M.e[M.e.length-1];
            if (isPair(x)&&isPair(x.rest)&&isPair(x.rest.rest)) {
                return x.rest.rest.rest;
            } else {
                raiseArgumentTypeError(M, "cdddr", "cdddrable value", 0, x);
            }
        });
    installPrimitiveProcedure(
        'caaaar',
        1,
        function(M) {
            var x = M.e[M.e.length-1];
            if (isPair(x)&&isPair(x.first)&&isPair(x.first.first)&&isPair(x.first.first.first)) {
                return x.first.first.first.first;
            } else {
                raiseArgumentTypeError(M, "caaaar", "caaaarable value", 0, x);
            }
        });
    installPrimitiveProcedure(
        'cdaaar',
        1,
        function(M) {
            var x = M.e[M.e.length-1];
            if (isPair(x)&&isPair(x.first)&&isPair(x.first.first)&&isPair(x.first.first.first)) {
                return x.first.first.first.rest;
            } else {
                raiseArgumentTypeError(M, "cdaaar", "cdaaarable value", 0, x);
            }
        });
    installPrimitiveProcedure(
        'cadaar',
        1,
        function(M) {
            var x = M.e[M.e.length-1];
            if (isPair(x)&&isPair(x.first)&&isPair(x.first.first)&&isPair(x.first.first.rest)) {
                return x.first.first.rest.first;
            } else {
                raiseArgumentTypeError(M, "cadaar", "cadaarable value", 0, x);
            }
        });
    installPrimitiveProcedure(
        'cddaar',
        1,
        function(M) {
            var x = M.e[M.e.length-1];
            if (isPair(x)&&isPair(x.first)&&isPair(x.first.first)&&isPair(x.first.first.rest)) {
                return x.first.first.rest.rest;
            } else {
                raiseArgumentTypeError(M, "cddaar", "cddaarable value", 0, x);
            }
        });
    installPrimitiveProcedure(
        'caadar',
        1,
        function(M) {
            var x = M.e[M.e.length-1];
            if (isPair(x)&&isPair(x.first)&&isPair(x.first.rest)&&isPair(x.first.rest.first)) {
                return x.first.rest.first.first;
            } else {
                raiseArgumentTypeError(M, "caadar", "caadarable value", 0, x);
            }
        });
    installPrimitiveProcedure(
        'cdadar',
        1,
        function(M) {
            var x = M.e[M.e.length-1];
            if (isPair(x)&&isPair(x.first)&&isPair(x.first.rest)&&isPair(x.first.rest.first)) {
                return x.first.rest.first.rest;
            } else {
                raiseArgumentTypeError(M, "cdadar", "cdadarable value", 0, x);
            }
        });
    installPrimitiveProcedure(
        'caddar',
        1,
        function(M) {
            var x = M.e[M.e.length-1];
            if (isPair(x)&&isPair(x.first)&&isPair(x.first.rest)&&isPair(x.first.rest.rest)) {
                return x.first.rest.rest.first;
            } else {
                raiseArgumentTypeError(M, "caddar", "caddarable value", 0, x);
            }
        });
    installPrimitiveProcedure(
        'cdddar',
        1,
        function(M) {
            var x = M.e[M.e.length-1];
            if (isPair(x)&&isPair(x.first)&&isPair(x.first.rest)&&isPair(x.first.rest.rest)) {
                return x.first.rest.rest.rest;
            } else {
                raiseArgumentTypeError(M, "cdddar", "cdddarable value", 0, x);
            }
        });
    installPrimitiveProcedure(
        'caaadr',
        1,
        function(M) {
            var x = M.e[M.e.length-1];
            if (isPair(x)&&isPair(x.rest)&&isPair(x.rest.first)&&isPair(x.rest.first.first)) {
                return x.rest.first.first.first;
            } else {
                raiseArgumentTypeError(M, "caaadr", "caaadrable value", 0, x);
            }
        });
    installPrimitiveProcedure(
        'cdaadr',
        1,
        function(M) {
            var x = M.e[M.e.length-1];
            if (isPair(x)&&isPair(x.rest)&&isPair(x.rest.first)&&isPair(x.rest.first.first)) {
                return x.rest.first.first.rest;
            } else {
                raiseArgumentTypeError(M, "cdaadr", "cdaadrable value", 0, x);
            }
        });
    installPrimitiveProcedure(
        'cadadr',
        1,
        function(M) {
            var x = M.e[M.e.length-1];
            if (isPair(x)&&isPair(x.rest)&&isPair(x.rest.first)&&isPair(x.rest.first.rest)) {
                return x.rest.first.rest.first;
            } else {
                raiseArgumentTypeError(M, "cadadr", "cadadrable value", 0, x);
            }
        });
    installPrimitiveProcedure(
        'cddadr',
        1,
        function(M) {
            var x = M.e[M.e.length-1];
            if (isPair(x)&&isPair(x.rest)&&isPair(x.rest.first)&&isPair(x.rest.first.rest)) {
                return x.rest.first.rest.rest;
            } else {
                raiseArgumentTypeError(M, "cddadr", "cddadrable value", 0, x);
            }
        });
    installPrimitiveProcedure(
        'caaddr',
        1,
        function(M) {
            var x = M.e[M.e.length-1];
            if (isPair(x)&&isPair(x.rest)&&isPair(x.rest.rest)&&isPair(x.rest.rest.first)) {
                return x.rest.rest.first.first;
            } else {
                raiseArgumentTypeError(M, "caaddr", "caaddrable value", 0, x);
            }
        });
    installPrimitiveProcedure(
        'cdaddr',
        1,
        function(M) {
            var x = M.e[M.e.length-1];
            if (isPair(x)&&isPair(x.rest)&&isPair(x.rest.rest)&&isPair(x.rest.rest.first)) {
                return x.rest.rest.first.rest;
            } else {
                raiseArgumentTypeError(M, "cdaddr", "cdaddrable value", 0, x);
            }
        });
    installPrimitiveProcedure(
        'cadddr',
        1,
        function(M) {
            var x = M.e[M.e.length-1];
            if (isPair(x)&&isPair(x.rest)&&isPair(x.rest.rest)&&isPair(x.rest.rest.rest)) {
                return x.rest.rest.rest.first;
            } else {
                raiseArgumentTypeError(M, "cadddr", "cadddrable value", 0, x);
            }
        });
    installPrimitiveProcedure(
        'cddddr',
        1,
        function(M) {
            var x = M.e[M.e.length-1];
            if (isPair(x)&&isPair(x.rest)&&isPair(x.rest.rest)&&isPair(x.rest.rest.rest)) {
                return x.rest.rest.rest.rest;
            } else {
                raiseArgumentTypeError(M, "cddddr", "cddddrable value", 0, x);
            }
        });

    // End autogenerated code.
    //////////////////////////////////////////////////////////////////////
    //////////////////////////////////////////////////////////////////////


    installPrimitiveProcedure(
        'pair?',
        1,
        function (M) {
            var firstArg = M.e[M.e.length-1];
            return isPair(firstArg);
        });


    installPrimitiveProcedure(
        'list?',
        1,
        function (M) {
            return isList(M.e[M.e.length -1]);
        });


    installPrimitiveProcedure(
        'set-car!',
        2,
        function (M) {
            var firstArg = checkPair(M, 'set-car!', 0);
            var secondArg = M.e[M.e.length-2];
            firstArg.first = secondArg;
            return VOID;
        });


    installPrimitiveProcedure(
        'set-cdr!',
        2,
        function (M) {
            var firstArg = checkPair(M, 'set-car!', 0);
            var secondArg = M.e[M.e.length-2];
            firstArg.rest = secondArg;
            return VOID;
        });

    installPrimitiveProcedure(
        'not',
        1,
        function (M) {
            var firstArg = M.e[M.e.length-1];
            return (firstArg === false);
        });


    installPrimitiveProcedure(
        'null?',
        1,
        function (M) {
            var firstArg = M.e[M.e.length-1];
            return firstArg === NULL;
        });


    installPrimitiveProcedure(
        'vector?',
        1,
        function (M) {
            var firstArg = M.e[M.e.length-1];
            return isVector(firstArg);
        });

    installPrimitiveProcedure(
        'vector',
        baselib.arity.makeArityAtLeast(0),
        function (M) {
            var i;
            var result = [];
            for (i = 0; i < M.a; i++) {
                result.push(M.e[M.e.length-1-i]);
            }
            var newVector = makeVector(result);
            return newVector;
        });


    installPrimitiveProcedure(
        'make-vector',
        makeList(1, 2),
        function (M) {
            var value = 0;
            var length = baselib.numbers.toFixnum(
                checkNatural(M, 'make-vector', 0));
            if (M.a === 2) {
                value = M.e[M.e.length - 2];
            }
            var arr = [];
	    var i;
            for(i = 0; i < length; i++) {
                arr[i] = value;
            }
            return makeVector(arr);
        });

    installPrimitiveProcedure(
        'vector->list',
        1,
        function (M) {
            var elts = checkVector(M, 'vector->list', 0).elts;
            var i;
            var result = NULL;
            for (i = 0; i < elts.length; i++) {
                result = makePair(elts[elts.length - 1 - i], result);
            }
            return result;
        });

    installPrimitiveProcedure(
        'list->vector',
        1,
        function (M) {
            var firstArg = checkList(M, 'list->vector', 0);
            var result = [];
            while (firstArg !== NULL) {
                result.push(firstArg.first);
                firstArg = firstArg.rest;
            }
            return makeVector(result);
        });


    installPrimitiveProcedure(
        'vector-ref',
        2,
        function (M) {
            var elts = checkVector(M, 'vector-ref', 0).elts;
            var index = baselib.numbers.toFixnum(
                checkNaturalInRange(M, 'vector-ref', 1, 0, elts.length));
            return elts[index];
        });


    installPrimitiveProcedure(
        'vector-set!',
        3,
        function (M) {
            var elts = checkVector(M, 'vector-set!', 0).elts;
            // FIXME: check out-of-bounds vector
            var index = baselib.numbers.toFixnum(
                checkNaturalInRange(M, 'vector-set!', 1,
                                    0, elts.length));
            var val = M.e[M.e.length - 1 - 2];
            elts[index] = val;
            return VOID;
        });


    installPrimitiveProcedure(
        'vector-length',
        1,
        function (M) {
            return checkVector(M, 'vector-length', 0).elts.length;
        });



    installPrimitiveProcedure(
        'make-string',
        makeList(1, 2),
        function (M) {
            var value = String.fromCharCode(0);
            var length = baselib.numbers.toFixnum(
                checkNatural(M, 'make-string', 0));
            if (M.a === 2) {
                value = checkChar(M, 'make-string', 1).val;
            }
            var arr = [];
	    var i;
            for(i = 0; i < length; i++) {
                arr[i] = value;
            }
            return baselib.strings.makeMutableString(arr);
        });

    installPrimitiveProcedure(
        'substring',
        makeList(2, 3),
        function(M) {
            var str = checkString(M, 'substring', 0).toString();
            var start = baselib.numbers.toFixnum(checkNatural(M, 'substring', 1));
            var end = str.length;
            if (M.a === 3) {
                end = baselib.numbers.toFixnum(checkNatural(M, 'substring', 2));
            }
            return baselib.strings.makeMutableString((str.substring(start, end)).split(""));
        });

    installPrimitiveProcedure(
        'string-copy',
        1,
        function(M) {
            var str = checkString(M, 'substring', 0).toString();
            return baselib.strings.makeMutableString(str.substring(0, str.length).split(""));
        });


    installPrimitiveProcedure(
        'list->string',
        1,
        function (M) {
            var firstArg = checkListofChars(M, 'list->string', 0);
            var result = [];
            while (firstArg !== NULL) {
                result.push(firstArg.first.val);
                firstArg = firstArg.rest;
            }
            return result.join('');
        });


    installPrimitiveProcedure(
        'string',
        baselib.arity.makeArityAtLeast(0),
        function (M) {
            var i;
            var chars = [];
            for (i = 0; i < M.a; i++) {
                chars.push(checkChar(M, 'string', i).val);
            }
            return chars.join('');
        });


    installPrimitiveProcedure(
        'string->list',
        1,
        function (M) {
            var str = checkString(M, 'string->list', 0).toString();
            var i;
            var result = NULL;
            for (i = str.length - 1; i >= 0; i--) {
                result = makePair(baselib.chars.makeChar(str.charAt(i)), result);
            }
            return result;
        });



    installPrimitiveProcedure(
        'string-set!',
        3,
        function (M) {
            var str = checkMutableString(M, 'string-set!', 0);
            var k = checkNatural(M, 'string-set!', 1);
            var ch = checkChar(M, 'string-set!', 2);
	    str.set(baselib.numbers.toFixnum(k), ch.val);
            return VOID;
        });





    installPrimitiveProcedure(
        'symbol?',
        1,
        function (M) {
            var firstArg = M.e[M.e.length-1];
            return isSymbol(firstArg);
        });

    installPrimitiveProcedure(
        'symbol->string',
        1,
        function (M) {
            var firstArg = checkSymbol(M, 'symbol->string', 0);
            return firstArg.toString();
        });


    installPrimitiveProcedure(
        'string=?',
        baselib.arity.makeArityAtLeast(1),
        function (M) {
            var s = checkString(M, 'string=?', 0).toString();
	    var i;
            for (i = 1; i < M.a; i++) {
                if (s !== checkString(M, 'string=?', i).toString()) {
                    return false;
                }
            }
            return true;
        });


    installPrimitiveProcedure(
        'string<=?',
        baselib.arity.makeArityAtLeast(1),
        function (M) {
            var s = checkString(M, 'string<=?', 0).toString();
	    var i;
            for (i = 1; i < M.a; i++) {
                if ((s <= checkString(M, 'string<=?', i).toString()) === false) {
                    return false;
                }
            }
            return true;
        });

    installPrimitiveProcedure(
        'string<?',
        baselib.arity.makeArityAtLeast(1),
        function (M) {
            var s = checkString(M, 'string<?', 0).toString();
	    var i;
            for (i = 1; i < M.a; i++) {
                if ((s < checkString(M, 'string<?', i).toString()) === false) {
                    return false;
                }
            }
            return true;
        });

    installPrimitiveProcedure(
        'string>=?',
        baselib.arity.makeArityAtLeast(1),
        function (M) {
            var s = checkString(M, 'string>=?', 0).toString();
	    var i;
            for (i = 1; i < M.a; i++) {
                if ((s >= checkString(M, 'string>=?', i).toString()) === false) {
                    return false;
                }
            }
            return true;
        });

    installPrimitiveProcedure(
        'string>?',
        baselib.arity.makeArityAtLeast(1),
        function (M) {
            var s = checkString(M, 'string>?', 0).toString();
	    var i;
            for (i = 1; i < M.a; i++) {
                if ((s > checkString(M, 'string>?', i).toString()) === false) {
                    return false;
                }
            }
            return true;
        });


    installPrimitiveProcedure(
        'string-ci=?',
        baselib.arity.makeArityAtLeast(1),
        function (M) {
            var s = checkString(M, 'string-ci=?', 0).toString().toUpperCase();
	    var i;
            for (i = 1; i < M.a; i++) {
                if (s !== checkString(M, 'string-ci=?', i).toString().toUpperCase()) {
                    return false;
                }
            }
            return true;
        });


    installPrimitiveProcedure(
        'string-ci<=?',
        baselib.arity.makeArityAtLeast(1),
        function (M) {
            var s = checkString(M, 'string-ci<=?', 0).toString().toUpperCase();
	    var i;
            for (i = 1; i < M.a; i++) {
                if ((s <= checkString(M, 'string-ci<=?', i).toString().toUpperCase()) === false) {
                    return false;
                }
            }
            return true;
        });

    installPrimitiveProcedure(
        'string-ci<?',
        baselib.arity.makeArityAtLeast(1),
        function (M) {
            var s = checkString(M, 'string-ci<?', 0).toString().toUpperCase();
	    var i;
            for (i = 1; i < M.a; i++) {
                if ((s < checkString(M, 'string-ci<?', i).toString().toUpperCase()) === false) {
                    return false;
                }
            }
            return true;
        });

    installPrimitiveProcedure(
        'string-ci>=?',
        baselib.arity.makeArityAtLeast(1),
        function (M) {
            var s = checkString(M, 'string-ci>=?', 0).toString().toUpperCase();
	    var i;
            for (i = 1; i < M.a; i++) {
                if ((s >= checkString(M, 'string-ci>=?', i).toString().toUpperCase()) === false) {
                    return false;
                }
            }
            return true;
        });

    installPrimitiveProcedure(
        'string-ci>?',
        baselib.arity.makeArityAtLeast(1),
        function (M) {
            var s = checkString(M, 'string-ci>?', 0).toString().toUpperCase();
	    var i;
            for (i = 1; i < M.a; i++) {
                if ((s > checkString(M, 'string-ci>?', i).toString().toUpperCase()) === false) {
                    return false;
                }
            }
            return true;
        });


    installPrimitiveProcedure(
        'string-append',
        baselib.arity.makeArityAtLeast(0),
        function (M) {
            var buffer = [];
            var i;
            for (i = 0; i < M.a; i++) {
                buffer.push(checkString(M, 'string-append', i).toString());
            }
            return buffer.join('');
        });

    installPrimitiveProcedure(
        'string-length',
        1,
        function (M) {
            var firstArg = checkString(M, 'string-length', 0).toString();
            return firstArg.length;
        });


    installPrimitiveProcedure(
        'string-ref',
        2,
        function (M) {
            var firstArg = checkString(M, 'string-ref', 0).toString();
            var index = baselib.numbers.toFixnum(
                checkNaturalInRange(M, 'string-ref', 1,
                                    0, firstArg.length));
            return baselib.chars.makeChar(firstArg.charAt(index));
        });



    installPrimitiveProcedure(
        'string?',
        1,
        function (M) {
            return isString(M.e[M.e.length - 1]);
        });


    installPrimitiveProcedure(
        'number->string',
        1,
        function (M) {
            return checkNumber(M, 'number->string', 0).toString();
        });


    installPrimitiveProcedure(
        'string->symbol',
        1,
        function (M) {
            return makeSymbol(checkString(M, 'string->symbol', 0).toString());
        });


    installPrimitiveProcedure(
        'string->number',
        1,
        function (M) {
            return baselib.numbers.fromString(
                checkString(M, 'string->number', 0).toString());
        });


    installPrimitiveProcedure(
        'boolean?',
        1,
        function(M) {
            var v = M.e[M.e.length - 1];
            return (v === true || v === false);
        });


    installPrimitiveProcedure(
        'char?',
        1,
        function(M) {
            return isChar(M.e[M.e.length -1 ]);
        });


    var makeCharComparator = function(name, cmp) {
        return function(M) {
            var s = checkChar(M, name, 0).val;
	    var i;
            for (i = 1; i < M.a; i++) {
                if (!(cmp(s, checkChar(M, name, i).val))) {
                    return false;
                }
            }
            return true;
        };
    };

    installPrimitiveProcedure(
        'char>?',
        baselib.arity.makeArityAtLeast(2),
        makeCharComparator('char>?',
                           function(x, y) {
                               return x > y;
                           }));

    installPrimitiveProcedure(
        'char>=?',
        baselib.arity.makeArityAtLeast(2),
        makeCharComparator('char>=?',
                           function(x, y) {
                               return x >= y;
                           }));

    installPrimitiveProcedure(
        'char<?',
        baselib.arity.makeArityAtLeast(2),
        makeCharComparator('char<?',
                           function(x, y) {
                               return x < y;
                           }));

    installPrimitiveProcedure(
        'char<=?',
        baselib.arity.makeArityAtLeast(2),
        makeCharComparator('char<=?',
                           function(x, y) {
                               return x <= y;
                           }));

    installPrimitiveProcedure(
        'char=?',
        baselib.arity.makeArityAtLeast(2),
        makeCharComparator('char=?',
                           function(x, y) {
                               return x === y;
                           }));

    installPrimitiveProcedure(
        'char-ci>?',
        baselib.arity.makeArityAtLeast(2),
        makeCharComparator('char-ci>?',
                           function(x, y) {
                               return x.toUpperCase() > y.toUpperCase();
                           }));

    installPrimitiveProcedure(
        'char-ci>=?',
        baselib.arity.makeArityAtLeast(2),
        makeCharComparator('char-ci>=?',
                           function(x, y) {
                               return x.toUpperCase() >= y.toUpperCase();
                           }));

    installPrimitiveProcedure(
        'char-ci<?',
        baselib.arity.makeArityAtLeast(2),
        makeCharComparator('char-ci<?',
                           function(x, y) {
                               return x.toUpperCase() < y.toUpperCase();
                           }));

    installPrimitiveProcedure(
        'char-ci<=?',
        baselib.arity.makeArityAtLeast(2),
        makeCharComparator('char-ci<=?',
                           function(x, y) {
                               return x.toUpperCase() <= y.toUpperCase();
                           }));

    installPrimitiveProcedure(
        'char-ci=?',
        baselib.arity.makeArityAtLeast(2),
        makeCharComparator('char-ci=?',
                           function(x, y) {
                               return x.toUpperCase() === y.toUpperCase();
                           }));



    installPrimitiveProcedure(
        'char->integer',
        1,
        function(M) {
            return checkChar(M, 'char->integer', 0).val.charCodeAt(0);
        });

    installPrimitiveProcedure(
        'integer->char',
        1,
        function(M) {
            var ch = baselib.numbers.toFixnum(checkIntegerForChar(M, 'integer->char', 0));
            return baselib.chars.makeChar(String.fromCharCode(ch));
        });

    installPrimitiveProcedure(
        'char-upcase',
        1,
        function(M) {
            var ch = checkChar(M, 'char=?', 0).val;
            return baselib.chars.makeChar(ch.toUpperCase());
        });

    installPrimitiveProcedure(
        'char-downcase',
        1,
        function(M) {
            var ch = checkChar(M, 'char=?', 0).val;
            return baselib.chars.makeChar(ch.toLowerCase());
        });

    installPrimitiveProcedure(
        'char-numeric?',
        1,
        function(M) {
            var val = checkChar(M, 'char-numeric?', 0).val;
            return val >= '0' && val <= '9';
        });

    installPrimitiveProcedure(
        'char-alphabetic?',
        1,
        function(M) {
            var val = checkChar(M, 'char-alphabetic?', 0).val;
            return ((val >= 'a' && val <= 'z') ||
                    (val >= 'A' && val <= 'Z'));
        });

    var whitespaceRegexp = new RegExp("^\\s*$");
    installPrimitiveProcedure(
        'char-whitespace?',
        1,
        function(M) {
            var val = checkChar(M, 'char-whitespace?', 0).val;
            return val.match(whitespaceRegexp ? true : false);
      });


    installPrimitiveProcedure(
        'char-upper-case?',
        1,
        function(M) {
            var val = checkChar(M, 'char-upper-case?', 0).val;
            return val === val.toUpperCase();
      });

    installPrimitiveProcedure(
        'char-lower-case?',
        1,
        function(M) {
            var val = checkChar(M, 'char-lower-case?', 0).val;
            return val === val.toLowerCase();
      });


    installPrimitiveProcedure(
        'box',
        1,
        function (M) {
            var firstArg = M.e[M.e.length-1];
            return makeBox(firstArg);
        });

    installPrimitiveProcedure(
        'unbox',
        1,
        function (M) {
            var firstArg = checkBox(M, 'unbox', 0);
            return firstArg.ref();
        });

    installPrimitiveProcedure(
        'set-box!',
        2,
        function (M) {
            var firstArg = checkMutableBox(M, 'set-box!', 0);
            var secondArg = M.e[M.e.length-2];
            firstArg.set(secondArg);
            return VOID;
        });

    installPrimitiveProcedure(
        'void',
        baselib.arity.makeArityAtLeast(0),
        function (M) {
            return VOID;
        });


    installPrimitiveProcedure(
        'random',
        makeList(0, 1),
        function (M) {
            if (M.a === 0) {
                return makeFloat(Math.random());
            } else {
                var n = checkNatural(M, 'random', 0);
                return Math.floor(Math.random() * baselib.numbers.toFixnum(n));
            }
        });


    installPrimitiveProcedure(
        'eq?',
        2,
        function (M) {
            var firstArg = M.e[M.e.length-1];
            var secondArg = M.e[M.e.length-2];
            return firstArg === secondArg;
        });

    installPrimitiveProcedure(
        'eqv?',
        2,
        function (M) {
            var firstArg = M.e[M.e.length-1];
            var secondArg = M.e[M.e.length-2];
            return baselib.equality.eqv(firstArg, secondArg);
        });



    installPrimitiveProcedure(
        'equal?',
        2,
        function (M) {
            var firstArg = M.e[M.e.length-1];
            var secondArg = M.e[M.e.length-2];
            return equals(firstArg, secondArg);
        });


    // This definition of apply will take precedence over the
    // implementation of apply in the boostrapped-primitives.rkt,
    // since it provides nicer error handling.
    var applyImplementation = function (M) {
        if(--M.cbt < 0) {
            throw applyImplementation;
        }
        var proc = checkProcedure(M, 'apply', 0);
        M.e.pop();
        M.a--;
        checkList(M, 'apply', M.a - 1);
        M.spliceListIntoStack(M.a - 1);
        M.p = proc;
        if (baselib.arity.isArityMatching(proc.racketArity, M.a)) {
            return proc.label(M);
        } else {
            raiseArityMismatchError(M, proc, M.a);
        }
    };

    installPrimitiveClosure(
        'apply',
        baselib.arity.makeArityAtLeast(2),
        applyImplementation);


    // FIXME: The definition of call-with-values is in
    // bootstrapped-primitives.rkt.  We may want to replace it with an
    // explicitly defined one here.





    installPrimitiveProcedure(
        'procedure?',
        1,
        function (M) {
            return isProcedure(M.e[M.e.length - 1]);
        });

    installPrimitiveProcedure(
        'procedure-arity-includes?',
        2,
        function (M) {
            var proc = checkProcedure(M, 'procedure-arity-includes?', 0);
            var a = checkNatural(M, 'procedure-arity-includes?', 1);
            return baselib.arity.isArityMatching(proc.racketArity, a);
        });

    installPrimitiveProcedure(
        'procedure-arity',
        1,
        function (M) {
            var proc = checkProcedure(M, 'procedure-arity-includes?', 0);
            return proc.racketArity;
        });


    installPrimitiveProcedure(
        'procedure-rename',
        2,
        function (M) {
            var proc = checkProcedure(M, 'procedure-rename', 0);
            var name = checkSymbol(M, 'procedure-rename', 1);
            return baselib.functions.renameProcedure(proc, name);
        });



    installPrimitiveProcedure(
        'member',
        2,
        function (M) {
            var x = M.e[M.e.length-1];
            var lst = M.e[M.e.length-2];
            while (true) {
                if (lst === NULL) {
                    return false;
                }
                if (! isPair(lst)) {
                    raiseArgumentTypeError(M,
                                           'member',
                                           'list',
                                           1,
                                           M.e[M.e.length - 1 - 1]);
                }
                if (equals(x, (lst.first))) {
                    return lst;
                }
                lst = lst.rest;
            }
        });


    installPrimitiveProcedure(
        'reverse',
        1,
        function (M) {
            var rev = NULL;
            var lst = M.e[M.e.length-1];
            while(lst !== NULL) {
                rev = makePair(testArgument(M, 'pair', isPair, lst, 0, 'reverse').first,
                               rev);
                lst = lst.rest;
            }
            return rev;
        });

    installPrimitiveProcedure(
        'void?',
        1,
        function(M) {
            return M.e[M.e.length -1] === VOID;
        });


    installPrimitiveProcedure(
        'box?',
        1,
        function(M) {
            return isBox(M.e[M.e.length -1]);
        });


    installPrimitiveProcedure(
        'eof-object?',
        1,
        function(M) {
            return M.e[M.e.length -1] === baselib.constants.EOF_VALUE;
        });

    installPrimitiveProcedure(
	'number?',
	1,
	function(M) {
	    return isNumber(M.e[M.e.length - 1]);
	});

    installPrimitiveProcedure(
	'real?',
	1,
	function(M) {
	    return isReal(M.e[M.e.length - 1]);
	});

    installPrimitiveProcedure(
	'inexact?',
	1,
	function(M) {
	    return isInexact(M.e[M.e.length - 1]);
	});

    installPrimitiveProcedure(
	'complex?',
	1,
	function(M) {
	    return isComplex(M.e[M.e.length - 1]);
	});

    installPrimitiveProcedure(
        'bytes?',
        1,
        function(M) {
            return isBytes(M.e[M.e.length-1]);
        });

    installPrimitiveProcedure(
	'byte?',
	1,
	function(M) {
	    var v = M.e[M.e.length - 1];
            if(!isNatural(v)) { return false; }
            v = baselib.numbers.toFixnum(v);
            return v >= 0 && v < 256;
	});

    installPrimitiveProcedure(
	'rational?',
	1,
	function(M) {
	    return isRational(M.e[M.e.length - 1]);
	});

    installPrimitiveProcedure(
        'even?',
        1,
        function(M) {
            var n = checkInteger(M, 'even?', 0);
            return baselib.numbers.equals(0, baselib.numbers.modulo(n, 2));
        });

    installPrimitiveProcedure(
        'odd?',
        1,
        function(M) {
            var n = checkInteger(M, 'odd?', 0);
            return baselib.numbers.equals(1, baselib.numbers.modulo(n, 2));
        });


    installPrimitiveProcedure(
        'positive?',
        1,
        function(M) {
            var n = checkReal(M, 'positive?', 0);
            return baselib.numbers.greaterThan(n, 0);
        });

    installPrimitiveProcedure(
        'negative?',
        1,
        function(M) {
            var n = checkReal(M, 'negative?', 0);
            return baselib.numbers.lessThan(n, 0);
        });


    installPrimitiveProcedure(
        'inexact->exact',
        1,
        function (M) {
            return baselib.numbers.toExact(
                checkNumber(M, 'inexact->exact', 0));
        });

    installPrimitiveProcedure(
        'exact->inexact',
        1,
        function (M) {
            return baselib.numbers.toInexact(
                checkNumber(M, 'exact->inexact', 0));
        });

    installPrimitiveProcedure(
        'abs',
        1,
        function (M) {
            return baselib.numbers.abs(
                checkNumber(M, 'abs', 0));
        });

    installPrimitiveProcedure(
        'acos',
        1,
        function (M) {
            return baselib.numbers.acos(
                checkNumber(M, 'acos', 0));
        });


    installPrimitiveProcedure(
        'asin',
        1,
        function (M) {
            return baselib.numbers.asin(
                checkNumber(M, 'asin', 0));
        });

    installPrimitiveProcedure(
        'sin',
        1,
        function (M) {
            return baselib.numbers.sin(
                checkNumber(M, 'sin', 0));
        });



    installPrimitiveProcedure(
        'sinh',
        1,
        function (M) {
            return baselib.numbers.sinh(
                checkNumber(M, 'sinh', 0));
        });


    installPrimitiveProcedure(
        'tan',
        1,
        function (M) {
            return baselib.numbers.tan(
                checkNumber(M, 'tan', 0));
        });


    installPrimitiveProcedure(
        'atan',
        makeList(1, 2),
        function (M) {
            if (M.a === 1) {
                return baselib.numbers.atan(
                    checkNumber(M, 'atan', 0));
            } else {
                return makeFloat(
                    Math.atan2(
                        baselib.numbers.toFixnum(checkNumber(M, 'atan', 0)),
                        baselib.numbers.toFixnum(checkNumber(M, 'atan', 1))));
            }
        });


    installPrimitiveProcedure(
        'angle',
        1,
        function (M) {
            return baselib.numbers.angle(
                checkNumber(M, 'angle', 0));
        });

    installPrimitiveProcedure(
        'magnitude',
        1,
        function (M) {
            return baselib.numbers.magnitude(
                checkNumber(M, 'magnitude', 0));
        });

    installPrimitiveProcedure(
        'conjugate',
        1,
        function (M) {
            return baselib.numbers.conjugate(
                checkNumber(M, 'conjugate', 0));
        });




    installPrimitiveProcedure(
        'cos',
        1,
        function (M) {
            return baselib.numbers.cos(
                checkNumber(M, 'cos', 0));
        });


    installPrimitiveProcedure(
        'cosh',
        1,
        function (M) {
            return baselib.numbers.cosh(
                checkNumber(M, 'cosh', 0));
        });

    installPrimitiveProcedure(
        'gcd',
        baselib.arity.makeArityAtLeast(1),
        function (M) {
            var args = [], i, x;
            for (i = 0; i < M.a; i++) {
                args.push(checkNumber(M, 'gcd', i));
            }
            x = args.shift();
            return baselib.numbers.gcd(x, args);
        });

    installPrimitiveProcedure(
        'lcm',
        baselib.arity.makeArityAtLeast(1),
        function (M) {
            var args = [], i, x;
            for (i = 0; i < M.a; i++) {
                args.push(checkNumber(M, 'lcm', i));
            }
            x = args.shift();
            return baselib.numbers.lcm(x, args);
        });




    installPrimitiveProcedure(
        'exp',
        1,
        function (M) {
            return baselib.numbers.exp(
                checkNumber(M, 'exp', 0));
        });


    installPrimitiveProcedure(
        'expt',
        2,
        function (M) {
            return baselib.numbers.expt(
                checkNumber(M, 'expt', 0),
                checkNumber(M, 'expt', 1));
        });

    installPrimitiveProcedure(
        'exact?',
        1,
        function (M) {
            return baselib.numbers.isExact(
                checkNumber(M, 'exact?', 0));
        });


    installPrimitiveProcedure(
        'integer?',
        1,
        function (M) {
            return baselib.numbers.isInteger(M.e[M.e.length - 1]);
        });

    installPrimitiveProcedure(
        'exact-integer?',
        1,
        function (M) {
            return (baselib.numbers.isInteger(M.e[M.e.length - 1]) &&
                    baselib.numbers.isExact(M.e[M.e.length - 1]));
        });

    installPrimitiveProcedure(
        'exact-nonnegative-integer?',
        1,
        function (M) {
            return isNatural(M.e[M.e.length - 1]);
        });



    installPrimitiveProcedure(
        'imag-part',
        1,
        function (M) {
            return baselib.numbers.imaginaryPart(
                checkNumber(M, 'imag-part', 0));
        });


    installPrimitiveProcedure(
        'real-part',
        1,
        function (M) {
            return baselib.numbers.realPart(
                checkNumber(M, 'real-part', 0));
        });


    installPrimitiveProcedure(
        'make-polar',
        2,
        function (M) {
            return makeComplexPolar(
                checkReal(M, 'make-polar', 0),
                checkReal(M, 'make-polar', 1));
        });


    installPrimitiveProcedure(
        'make-rectangular',
        2,
        function (M) {
            return makeComplex(
                checkReal(M, 'make-rectangular', 0),
                checkReal(M, 'make-rectangular', 1));
        });

    installPrimitiveProcedure(
        'modulo',
        2,
        function (M) {
            return baselib.numbers.modulo(
                checkInteger(M, 'modulo', 0),
                checkInteger(M, 'modulo', 1));
        });


    installPrimitiveProcedure(
        'remainder',
        2,
        function (M) {
            return baselib.numbers.remainder(
                checkInteger(M, 'remainder', 0),
                checkInteger(M, 'remainder', 1));
        });


    installPrimitiveProcedure(
        'quotient',
        2,
        function (M) {
            return baselib.numbers.quotient(
                checkInteger(M, 'quotient', 0),
                checkInteger(M, 'quotient', 1));
        });



    installPrimitiveProcedure(
        'floor',
        1,
        function (M) {
            return baselib.numbers.floor(
                checkReal(M, 'floor', 0));
        });


    installPrimitiveProcedure(
        'ceiling',
        1,
        function (M) {
            return baselib.numbers.ceiling(
                checkReal(M, 'ceiling', 0));
        });


    installPrimitiveProcedure(
        'round',
        1,
        function (M) {
            return baselib.numbers.round(
                checkReal(M, 'round', 0));
        });


    installPrimitiveProcedure(
        'truncate',
        1,
        function (M) {
            var n = checkReal(M, 'truncate', 0);
            if (baselib.numbers.lessThan(n, 0)) {
                return baselib.numbers.ceiling(n);
            } else {
                return baselib.numbers.floor(n);
            }
        });


    installPrimitiveProcedure(
        'numerator',
        1,
        function (M) {
            return baselib.numbers.numerator(
                checkRational(M, 'numerator', 0));
        });


    installPrimitiveProcedure(
        'denominator',
        1,
        function (M) {
            return baselib.numbers.denominator(
                checkRational(M, 'denominator', 0));
        });


    installPrimitiveProcedure(
        'log',
        1,
        function (M) {
            return baselib.numbers.log(
                checkNumber(M, 'log', 0));
        });


    installPrimitiveProcedure(
        'sqr',
        1,
        function (M) {
            return baselib.numbers.sqr(
                checkNumber(M, 'sqr', 0));
        });




    installPrimitiveProcedure(
        'sqrt',
        1,
        function (M) {
            return baselib.numbers.sqrt(
                checkNumber(M, 'sqrt', 0));
        });



    installPrimitiveProcedure(
        'integer-sqrt',
        1,
        function (M) {
            return baselib.numbers.integerSqrt(
                checkInteger(M, 'integer-sqrt', 0));
        });



    installPrimitiveProcedure(
        'sgn',
        1,
        function (M) {
            return baselib.numbers.sign(
                checkInteger(M, 'sgn', 0));
        });


    installPrimitiveProcedure(
        'min',
        baselib.arity.makeArityAtLeast(1),
        function(M) {
            var i;
            var next;
            var currentMin = checkReal(M, 'min', 0);
            for (i = 1; i < M.a; i++) {
                next = checkReal(M, 'min', i);
                if (baselib.numbers.lessThan(next, currentMin)) {
                    currentMin = next;
                }
            }
            return currentMin;
        });

    installPrimitiveProcedure(
        'max',
        baselib.arity.makeArityAtLeast(1),
        function(M) {
            var i;
            var next;
            var currentMax = checkReal(M, 'min', 0);
            for (i = 1; i < M.a; i++) {
                next = checkReal(M, 'min', i);
                if (baselib.numbers.greaterThan(next, currentMax)) {
                    currentMax = next;
                }
            }
            return currentMax;
        });






    installPrimitiveProcedure(
        'error',
        baselib.arity.makeArityAtLeast(1),
        function (M) {
	    var i;
            if (M.a === 1) {
                var sym = checkSymbol(M, 'error', 1);
                raise(M, baselib.exceptions.makeExnFail(sym.toString(),
                                                        M.captureContinuationMarks()));
            }

            if (isString(M.e[M.e.length - 1])) {
                var vs = [];
                for (i = 1; i < M.a; i++) {
                    vs.push(baselib.format.format("~e", [M.e[M.e.length - 1 - i]]));
                }
                raise(M, baselib.exceptions.makeExnFail(M.e[M.e.length - 1].toString() +
                                                        ": " +
                                                        vs.join(' '),
                                                        M.captureContinuationMarks()));
            }

            if (isSymbol(M.e[M.e.length - 1])) {
                var fmtString = checkString(M, 'error', 1);
                var args = [M.e[M.e.length - 1]];
                for (i = 2; i < M.a; i++) {
                    args.push(M.e[M.e.length - 1 - i]);
                }
                raise(M, baselib.exceptions.makeExnFail(
                    baselib.format.format('~s: ' + fmtString.toString(),
                                          args),
                    M.captureContinuationMarks()));
            }

            // Fall-through
            raiseArgumentTypeError(M, 'error', 'symbol or string', 0, M.e[M.e.length - 1]);
        });


    installPrimitiveProcedure(
        'raise',
        makeList(1, 2),
        function(M) {
            var v = M.e[M.e.length - 1];
            // At the moment, not using the continuation barrier yet.
            // var withBarrier = M.e[M.e.length - 2];
            raise(M, v);
        });



    installPrimitiveProcedure(
        'raise-mismatch-error',
        3,
        function (M) {
            var name = checkSymbol(M, 'raise-mismatch-error', 0);
            var message = checkString(M, 'raise-mismatch-error', 0);
            var val = M.e[M.e.length - 1 - 2];
            raise(M, baselib.exceptions.makeExnFail(
		baselib.format.format("~a: ~a~e",
                                      [name,
                                       message,
                                       val]),
                M.captureContinuationMarks()));
        });


    installPrimitiveProcedure(
        'raise-type-error',
        baselib.arity.makeArityAtLeast(3),
        function (M) {
            var name = checkSymbol(M, 'raise-type-error', 0);
            var expected = checkString(M, 'raise-type-error', 1);
            if (M.a === 3) {
                raiseArgumentTypeError(M,
                                       name,
                                       expected,
                                       void(0),
                                       M.e[M.e.length - 1 - 2]);
            } else {
                raiseArgumentTypeError(M,
                                       name,
                                       expected,
                                       checkNatural(M, 'raise-type-error', 2),
                                       M.e[M.e.length - 1 - 2]);
            }
        });



    installPrimitiveProcedure(
        'make-exn',
        2,
        function(M) {
            var message = checkString(M, 'make-exn', 0);
            var marks = checkContinuationMarkSet(M, 'make-exn', 1);
            return baselib.exceptions.makeExn(message, marks);
        });


    installPrimitiveConstant(
        'struct:exn:fail',
        baselib.exceptions.ExnFail);


    installPrimitiveConstant(
        'prop:exn:srclocs',
        baselib.structs.propExnSrcloc);


    installPrimitiveProcedure(
        'make-exn:fail',
        2,
        function(M) {
            var message = checkString(M, 'make-exn:fail', 0);
            var marks = checkContinuationMarkSet(M, 'make-exn:fail', 1);
            return baselib.exceptions.makeExnFail(message, marks);
        });


    installPrimitiveProcedure(
        'make-exn:fail:contract',
        2,
        function(M) {
            var message = checkString(M, 'make-exn:fail:contract', 0);
            var marks = checkContinuationMarkSet(M, 'make-exn:fail:contract', 1);
            return baselib.exceptions.makeExnFailContract(message, marks);
        });


    installPrimitiveProcedure(
        'make-exn:fail:contract:arity',
        2,
        function(M) {
            var message = checkString(M, 'make-exn:fail:contract:arity', 0);
            var marks = checkContinuationMarkSet(M, 'make-exn:fail:contract:arity', 1);
            return baselib.exceptions.makeExnFailContractArity(message, marks);
        });

    installPrimitiveProcedure(
        'make-exn:fail:contract:variable',
        2,
        function(M) {
            var message = checkString(M, 'make-exn:fail:contract:variable', 0);
            var marks = checkContinuationMarkSet(M, 'make-exn:fail:contract:variable', 1);
            return baselib.exceptions.makeExnFailContractVariable(message, marks);
        });

    installPrimitiveProcedure(
        'make-exn:fail:contract:divide-by-zero',
        2,
        function(M) {
            var message = checkString(M, 'make-exn:fail:contract:divide-by-zero', 0);
            var marks = checkContinuationMarkSet(M, 'make-exn:fail:contract:divide-by-zero', 1);
            return baselib.exceptions.makeExnFailContractDivisionByZero(message, marks);
        });

    installPrimitiveProcedure(
        'exn:fail?',
        1,
        function(M) {
            return baselib.exceptions.isExnFail(M.e[M.e.length-1]);
        });

    installPrimitiveProcedure(
        'exn:fail:contract?',
        1,
        function(M) {
            return baselib.exceptions.isExnFailContract(M.e[M.e.length-1]);
        });

    installPrimitiveProcedure(
        'exn:fail:contract:arity?',
        1,
        function(M) {
            return baselib.exceptions.isExnFailContractArity(M.e[M.e.length-1]);
        });


    installPrimitiveProcedure(
        'exn-message',
        1,
        function(M) {
            var exn = checkExn(M, 'exn-message', 0);
            return baselib.exceptions.exnMessage(exn);
        });

    installPrimitiveProcedure(
        'exn-continuation-marks',
        1,
        function(M) {
            var exn = checkExn(M, 'exn-continuation-marks', 0);
            return baselib.exceptions.exnContMarks(exn);
        });


    installPrimitiveProcedure(
        'current-continuation-marks',
        makeList(0, 1),
        function(M) {
            var promptTag = baselib.contmarks.DEFAULT_CONTINUATION_PROMPT_TAG;
            if (M.a === 1) {
                promptTag = checkContinuationPromptTag(M, 'current-continuation-marks', 0);
            }
            var contMarks = M.captureContinuationMarks(promptTag);
            return contMarks;
        });

    installPrimitiveProcedure(
        'continuation-mark-set->list',
        makeList(2, 3),
        function(M) {
            var marks = checkContinuationMarkSet(M, 'continuation-mark-set->list', 0);
            var key = checkAny(M, 'continuation-mark-set->list', 1);
            var promptTag = baselib.contmarks.DEFAULT_CONTINUATION_PROMPT_TAG;
            if (M.a === 3) {
                promptTag = checkContinuationPromptTag(M, 'current-continuation-marks', 2);
            }
            return marks.ref(key, promptTag);
        });



    installPrimitiveClosure(
        'make-struct-type',
        makeList(4, 5, 6, 7, 8, 9, 10, 11),
        function (M) {
            withArguments(
                M,
                4,
                [false,
                 NULL,
                 false,
                 false,
                 NULL,
                 false,
                 false],
                function (name,
                          superType,
                          initFieldCount,
                          autoFieldCount,
                          autoV,
                          props,
                          inspector,  // FIXME: currently ignored
                          procSpec,   // FIXME: currently ignored
                          immutables, // FIXME: currently ignored
                          guard,      // FIXME: currently ignored
                          constructorName
                         ) {
                    var structType = baselib.structs.makeStructureType(
                        name,
                        superType,
                        initFieldCount,
                        autoFieldCount,
                        autoV,
                        //inspector,
                        //procSpec,
                        //immutables,
                        guard,
                        props);

                    var constructorValue =
                        makePrimitiveProcedure(
                            constructorName,
                            initFieldCount + (superType ? superType.numberOfArgs : 0),
                            function (M) {
                                var args = M.e.slice(M.e.length - M.a).reverse();
                                return structType.constructor(args);
                            });

                    var predicateValue =
                        makePrimitiveProcedure(
                            name.toString() + "?",
                            1,
                            function (M) {
                                return structType.predicate(M.e[M.e.length - 1]);
                            });

                    var accessorValue =
                        makePrimitiveProcedure(
                            name.toString() + "-accessor",
                            2,
                            function (M) {
                                return structType.accessor(
                                    M.e[M.e.length - 1],
                                    baselib.numbers.toFixnum(M.e[M.e.length - 2]));
                            });
                    accessorValue.structType = structType;

                    var mutatorValue =
                        makePrimitiveProcedure(
                            name.toString() + "-mutator",
                            3,
                            function (M) {
                                return structType.mutator(
                                    M.e[M.e.length - 1],
                                    baselib.numbers.toFixnum(M.e[M.e.length - 2]),
                                    M.e[M.e.length - 3]);
                            });
                    mutatorValue.structType = structType;


                    finalizeClosureCall(M,
                                        structType,
                                        constructorValue,
                                        predicateValue,
                                        accessorValue,
                                        mutatorValue);
                });
        });

    installPrimitiveProcedure(
        'struct?',
        1,
        function(M) {
            return isStruct(M.e[M.e.length - 1]);
        });

    installPrimitiveProcedure(
        'struct-type?',
        1,
        function(M) {
            return isStructType(M.e[M.e.length - 1]);
        });

    installPrimitiveProcedure(
        'current-inspector',
        makeList(0, 1),
        function (M) {
            if (M.a === 1) {
                M.params['currentInspector'] =
                    checkInspector(M, 'current-inspector', 0);
                return VOID;
            } else {
                return M.params['currentInspector'];
            }
        }
    );


    installPrimitiveProcedure(
        'make-struct-field-accessor',
        makeList(2, 3),
        function (M){
            var structType = M.e[M.e.length - 1].structType;
            var index = M.e[M.e.length - 2];
            var name;
            if (M.a === 3) {
                name = structType.name + "-" + M.e[M.e.length - 3].toString();
            } else {
                name = structType.name + "-" + 'field' + index;
            }
            var checkStruct = baselib.check.makeCheckArgumentType(structType.predicate,
                                                                  structType.name);
            return makePrimitiveProcedure(
                name,
                1,
                function (M) {
                    var aStruct = checkStruct(M, name, 0);
                    return structType.accessor(
                        aStruct,
                        baselib.numbers.toFixnum(index));
                });
        });


    installPrimitiveProcedure(
        'make-struct-field-mutator',
        makeList(2, 3),
        function (M){
            var structType = M.e[M.e.length - 1].structType;
            var index = M.e[M.e.length - 2];
            var name;
            if (M.a === 3) {
                name = "set-" + structType.name + "-" + M.e[M.e.length - 3].toString() + "!";
            } else {
                name = "set-" + structType.name + "-" + 'field' + index + "!";
            }
            var checkStruct = baselib.check.makeCheckArgumentType(structType.predicate,
                                                                  structType.name);
            return makePrimitiveProcedure(
                name,
                2,
                function (M) {
                    var aStruct = checkStruct(M, name, 0);
                    structType.mutator(
                        aStruct,
                        baselib.numbers.toFixnum(index),
                        M.e[M.e.length - 2]);
                    return VOID;
                });
        });


    installPrimitiveProcedure(
        'make-placeholder',
        1,
        function(M) {
            var v = M.e[M.e.length - 1];
            return baselib.placeholders.makePlaceholder(v);
        });


    installPrimitiveProcedure(
        'placeholder-set!',
        2,
        function(M) {
            var placeholder = checkPlaceholder(M, 'placeholder-set!', 0);
            var val = M.e[M.e.length - 2];
            placeholder.set(val);
            return VOID;
        });


    installPrimitiveProcedure(
        'make-reader-graph',
        1,
        function(M) {
            var x = M.e[M.e.length - 1];
            return baselib.readergraph.readerGraph(x,
                                                   baselib.hashes.makeLowLevelEqHash(),
                                                   0);
        });




    installPrimitiveProcedure(
        'srcloc',
        5,
        function(M) {
            var source = M.e[M.e.length - 1];
            var line = checkNaturalOrFalse(M, 'srcloc', 1);
            var column = checkNaturalOrFalse(M, 'srcloc', 2);
            var position = checkNaturalOrFalse(M, 'srcloc', 3);
            var span = checkNaturalOrFalse(M, 'srcloc', 4);
            return baselib.srclocs.makeSrcloc(source, line, column, position, span);
        });

    installPrimitiveProcedure(
        'make-srcloc',
        5,
        function(M) {
            var source = M.e[M.e.length - 1];
            var line = checkNaturalOrFalse(M, 'make-srcloc', 1);
            var column = checkNaturalOrFalse(M, 'make-srcloc', 2);
            var position = checkNaturalOrFalse(M, 'make-srcloc', 3);
            var span = checkNaturalOrFalse(M, 'make-srcloc', 4);
            return baselib.srclocs.makeSrcloc(source, line, column, position, span);
        });

    installPrimitiveProcedure(
        'srcloc?',
        1,
        function(M) {
            return baselib.srclocs.isSrcloc(M.e[M.e.length - 1]);
        });

    installPrimitiveProcedure(
        'srcloc-source',
        1,
        function(M) {
            return baselib.srclocs.srclocSource(checkSrcloc(M, 'srcloc-source', 0));
        });

    installPrimitiveProcedure(
        'srcloc-line',
        1,
        function(M) {
            return baselib.srclocs.srclocLine(checkSrcloc(M, 'srcloc-line', 0));
        });

    installPrimitiveProcedure(
        'srcloc-column',
        1,
        function(M) {
            return baselib.srclocs.srclocColumn(checkSrcloc(M, 'srcloc-column', 0));
        });


    installPrimitiveProcedure(
        'srcloc-position',
        1,
        function(M) {
            return baselib.srclocs.srclocPosition(checkSrcloc(M, 'srcloc-position', 0));
        });


    installPrimitiveProcedure(
        'srcloc-span',
        1,
        function(M) {
            return baselib.srclocs.srclocSpan(checkSrcloc(M, 'srcloc-span', 0));
        });



    installPrimitiveProcedure(
        'make-continuation-prompt-tag',
        makeList(0, 1),
        function(M) {
            var sym;
            if (M.a === 1) {
                sym = checkSymbol(M, "make-continuation-prompt-tag", 0);
                return new baselib.contmarks.ContinuationPromptTag(sym.toString());
            }
            return new baselib.contmarks.ContinuationPromptTag(false);
        });

    installPrimitiveProcedure(
        'continuation-prompt-tag?',
        1,
        function(M) {
            return baselib.contmarks.isContinuationPromptTag(M.e[M.e.length - 1]);
        });



    installPrimitiveProcedure(
        'default-continuation-prompt-tag',
        0,
        function(M) {
            return baselib.contmarks.DEFAULT_CONTINUATION_PROMPT_TAG;
        });

    installPrimitiveProcedure(
        'current-inexact-milliseconds',
        0,
        function(M) {
            return makeFloat((new Date()).valueOf());
        });


    installPrimitiveProcedure(
        'current-seconds',
        0,
        function() {
            return Math.floor( (new Date()).getTime() / 1000 );
        });


    // initializeHash: (listof pair) WhalesongHashtable -> WhalesongHashtable
    var initializeHash = function(lst, hash) {
	while (lst !== NULL) {
	    hash.put(lst.first.first, lst.first.rest);
	    lst = lst.rest;
	}
	return hash;
    };

    var initializeImmutableHash = function(lst, hash) {
	while (lst !== NULL) {
	    hash = hash.functionalPut(lst.first.first, lst.first.rest);
	    lst = lst.rest;
	}
	return hash;
    };


    installPrimitiveProcedure(
        'hash?',
        1,
        function(M) {
            return baselib.hashes.isHash(checkAny(M, 'hash?', 0));
        });
    installPrimitiveProcedure(
        'hash-equal?',
        1,
        function(M) {
            return baselib.hashes.isHashEqual(checkAny(M, 'hash-equal?', 0));
        });
    installPrimitiveProcedure(
        'hash-eq?',
        1,
        function(M) {
            return baselib.hashes.isHashEq(checkAny(M, 'hash-eq?', 0));
        });
    installPrimitiveProcedure(
        'hash-eqv?',
        1,
        function(M) {
            return baselib.hashes.isHashEqv(checkAny(M, 'hash-eqv?', 0));
        });


    installPrimitiveProcedure(
        'make-hasheq',
        makeList(0, 1),
        function(M) {
            var lst = NULL;
            if (M.a === 1) {
                lst = checkListofPairs(M, 'make-hasheq', 0);
            }
            return initializeHash(lst, plt.baselib.hashes.makeEqHashtable());
        });

    installPrimitiveProcedure(
        'make-hasheqv',
        makeList(0, 1),
        function(M) {
            var lst = NULL;
            if (M.a === 1) {
                lst = checkListofPairs(M, 'make-hasheqv', 0);
            }
            return initializeHash(lst, plt.baselib.hashes.makeEqvHashtable());
        });

    installPrimitiveProcedure(
        'make-hash',
        makeList(0, 1),
        function(M) {
            var lst = NULL;
            if (M.a === 1) {
                lst = checkListofPairs(M, 'make-hash', 0);
            }
            return initializeHash(lst, plt.baselib.hashes.makeEqualHashtable());
        });

    installPrimitiveProcedure(
        'hash-copy',
        1,
        function(M) {
            var hash = checkMutableHash(M, 'hash-copy', 0);
            return hash.clone();
        });
    
    installPrimitiveProcedure(
        'hash-count',
        1,
        function(M) {
            return checkHash(M, 'hash-count', 0).size();
        });

    installPrimitiveProcedure(
        'hash',
        baselib.arity.makeArityAtLeast(0),
        function(M) {
            var lst = NULL, i;
            for(i = 0; i < M.a; i+=2) {
                if (i+1 < M.a) {
                    lst = makePair(makePair(checkAny(M, 'hash', i), checkAny(M, 'hash', i + 1)),
                                   lst);
                } else {
                    raiseContractError(
                        M,
                        baselib.format.format(
                            "hash: key does not have a value (i.e., an odd number of arguments were provided): ~e",
                            [checkAny(M, 'hash', i)]));
                }
            }
            return initializeImmutableHash(lst, plt.baselib.hashes.makeImmutableEqualHashtable());
        });

    installPrimitiveProcedure(
        'hasheq',
        baselib.arity.makeArityAtLeast(0),
        function(M) {
            var lst = NULL, i;
            for(i = 0; i < M.a; i+=2) {
                if (i+1 < M.a) {
                    lst = makePair(makePair(checkAny(M, 'hasheq', i), checkAny(M, 'hasheq', i + 1)),
                                   lst);
                } else {
                    raiseContractError(
                        M,
                        baselib.format.format(
                            "hasheq: key does not have a value (i.e., an odd number of arguments were provided): ~e",
                            [checkAny(M, 'hasheq', i)]));
                }
            }
            return initializeImmutableHash(lst, plt.baselib.hashes.makeImmutableEqHashtable());
        });

    installPrimitiveProcedure(
        'hasheqv',
        baselib.arity.makeArityAtLeast(0),
        function(M) {
            var lst = NULL, i;
            for(i = 0; i < M.a; i+=2) {
                if (i+1 < M.a) {
                    lst = makePair(makePair(checkAny(M, 'hasheqv', i), checkAny(M, 'hasheqv', i + 1)),
                                   lst);
                } else {
                    raiseContractError(
                        M,
                        baselib.format.format(
                            "hasheqv: key does not have a value (i.e., an odd number of arguments were provided): ~e",
                            [checkAny(M, 'hasheqv', i)]));
                }
            }
            return initializeImmutableHash(lst, plt.baselib.hashes.makeImmutableEqvHashtable());
        });

    installPrimitiveProcedure(
        'make-immutable-hasheq',
        makeList(0, 1),
        function(M) {
            var lst = NULL;
            if (M.a === 1) {
                lst = checkListofPairs(M, 'make-immutable-hasheq', 0);
            }
            return initializeImmutableHash(lst, plt.baselib.hashes.makeImmutableEqHashtable());
        });

    installPrimitiveProcedure(
        'make-immutable-hasheqv',
        makeList(0, 1),
        function(M) {
            var lst = NULL;
            if (M.a === 1) {
                lst = checkListofPairs(M, 'make-immutable-hasheqv', 0);
            }
            return initializeImmutableHash(lst, plt.baselib.hashes.makeImmutableEqvHashtable());
        });

    installPrimitiveProcedure(
        'make-immutable-hash',
        makeList(0, 1),
        function(M) {
            var lst = NULL;
            if (M.a === 1) {
                lst = checkListofPairs(M, 'make-immutable-hash', 0);
            }
            return initializeImmutableHash(lst, plt.baselib.hashes.makeImmutableEqualHashtable());
        });

    installPrimitiveClosure(
        'hash-ref',
        makeList(2, 3),
        function(M) {
            var hash = checkHash(M, 'hash-ref', 0);
            var key = checkAny(M, 'hash-ref', 1);
            var thunkOrFailVal;
            if (M.a === 3) {
                thunkOrFailVal = checkAny(M, 'hash-ref', 2);
            }
            if (hash.containsKey(key)) {
                finalizeClosureCall(M, hash.get(key));
            } else {
                if (M.a === 2) {
                    raiseContractError(
                        M,
                        baselib.format.format("hash-ref: no value found for key: ~e",
                                           [key]));
                } else {
                    if (isProcedure(thunkOrFailVal)) {
                        M.p = thunkOrFailVal;
                        M.e.length -= M.a;
                        M.a = 0;
                        baselib.functions.rawApply(M);
                    } else {
                        finalizeClosureCall(M, thunkOrFailVal);
                    }
                }
            }
        });

    installPrimitiveProcedure(
        'hash-has-key?',
        2,
        function(M) {
            var hash = checkHash(M, 'hash-ref', 0);
            var key = checkAny(M, 'hash-ref', 1);
            return hash.containsKey(key);
        });

    installPrimitiveProcedure(
        'hash-set!',
        3,
        function(M){ 
            var hash = checkMutableHash(M, 'hash-set!', 0);
            var key = checkAny(M, 'hash-set!', 1);
            var value = checkAny(M, 'hash-set!', 2);
            hash.put(key, value);
            return VOID;
        });

    installPrimitiveProcedure(
        'hash-set',
        3,
        function(M){ 
            var hash = checkImmutableHash(M, 'hash-set', 0);
            var key = checkAny(M, 'hash-set', 1);
            var value = checkAny(M, 'hash-set', 2);
            return hash.functionalPut(key, value);
        });


    installPrimitiveProcedure(
        'hash-remove!',
        2,
        function(M){ 
            var hash = checkMutableHash(M, 'hash-remove!', 0);
            var key = checkAny(M, 'hash-remove!', 1);
            hash.remove(key);
            return VOID;
        });


    installPrimitiveProcedure(
        'hash-remove',
        2,
        function(M){ 
            var hash = checkImmutableHash(M, 'hash-remove', 0);
            var key = checkAny(M, 'hash-remove', 1);
            return hash.functionalRemove(key);
        });

    installPrimitiveProcedure(
        'hash-keys',
        1,
        function(M) {
            var hash = checkHash(M, 'hash-keys', 0);
            return baselib.lists.arrayToList(hash.keys());
        });

    installPrimitiveProcedure(
        'hash-values',
        1,
        function(M) {
            var hash = checkHash(M, 'hash-values', 0);
            return baselib.lists.arrayToList(hash.values());
        });

    installPrimitiveProcedure(
        'hash-has-key?',
        2,
        function(M){
            var hash = checkHash(M, 'hash-has-key?', 0);
            var key = checkAny(M, 'hash-has-key?', 1);
            return hash.containsKey(key);
        });

    installPrimitiveProcedure(
        'equal-hash-code',
        1,
        function(M) {
            return baselib.hashes.getEqualHashCode(checkAny(M, 'equal-hash-code', 0));
        });


    // The default prompt handler for a given prompt tag will assume
    // it's consuming a zero-argument thunk, and will call it in a
    // context where that prompt has been reestablished.
    var makeDefaultPromptHandler = function(promptTag) {
        return makeClosure(
            "default-prompt-handler",
            1,
            function(M) {
                var proc = checkProcedure(M, 'default-prompt-tag', 0);
                M.e.pop();
                M.p = proc;
                M.a = 0;
                M.addPrompt(promptTag, false, M.e.length);
                baselib.functions.rawApply(M);
            },
            []);
    };


    // FIXME: we should be able to take in an arbitrary continuation
    // as an optional second argument!
    //
   // I need to change the representation of continuations to be able to
    // detect this at runtime.
    installPrimitiveProcedure(
        'continuation-prompt-available?',
        1,
        function(M) {
            var promptTag = checkPromptTag(M, 'continuation-prompt-available?', 0);
            var i;
            for (i = 0; i < M.c.length; i++) {
                var frame = M.c[i];
                if (frame instanceof PromptFrame && frame.tag === promptTag) {
                    return true;
                }
            }
            return false;
        });



    // The default abort prompt handler consumes a thunk and applies
    // it, in a context where a new prompt has been initialized.
    var defaultPromptHandler =
        makeDefaultPromptHandler(baselib.contmarks.DEFAULT_CONTINUATION_PROMPT_TAG);
        

    installPrimitiveClosure(
        'abort-current-continuation',
        baselib.arity.makeArityAtLeast(1),
        function(M) {
            var promptTag = checkPromptTag(M, 'abort-current-continuation', 0);
            var vals = [];
            var frame;
            var i;
            for(i = 1; i < M.a; i++) {
                vals.push(M.e[M.e.length - 1 - i]);
            }
            
            // First, find the continuation prompt.
            while(true) {
                frame = M.c.pop();
                if (frame instanceof PromptFrame && frame.tag === promptTag) {
                    break;
                } else if (M.c.length === 0) {
                    raiseContractError(
                        M,
                        baselib.format.format("continuation includes no prompt with the given tag", []));
                }
            }
            // Shrink the environment to what was observed when the
            // PromptFrame was installed, and then set up the call
            // to the prompt's abort handler.
            M.e.length = frame.envLength;
            M.p = frame.handler || defaultPromptHandler;
            M.a = vals.length;
            for (i = 0; i < vals.length; i++) {
                M.e.push(vals[vals.length - i - 1]);
            }
            baselib.functions.rawApply(M);
        });

 
    installPrimitiveClosure(
        'call-with-continuation-prompt',
        baselib.arity.makeArityAtLeast(1),
        function(M) {
            var proc, promptTag, handler, i;
            proc = checkProcedure(M, 'call-with-continuation-prompt', 0);
            if (M.a >= 2) {
                promptTag = checkPromptTag(M, 'call-with-continuation-prompt', 1);
            } else {
                promptTag = baselib.contmarks.DEFAULT_CONTINUATION_PROMPT_TAG;
            }
            if (M.a >= 3) {
                if (M.e[M.e.length - 1 - 3] === false) {
                    handler = false;
                } else {
                    handler = checkProcedure(M, 'call-with-continuation-prompt', 2);
                }
            } else {
                if (promptTag === baselib.contmarks.DEFAULT_CONTINUATION_PROMPT_TAG) {
                    handler = defaultPromptHandler;
                } else {
                    handler = makeDefaultPromptHandler(promptTag);
                }
            }
            M.p = proc;
            if (M.a >= 1) { M.e.pop(); } // the test is redundant, but I want the parallelism.
            if (M.a >= 2) { M.e.pop(); }
            if (M.a >= 3) { M.e.pop(); }
            M.a = Math.max(M.a - 3, 0);

            // subtle: the prompt's environment is the one _after_ the current call!
            // That's why we need to do M.e.length - M.a: the environment currently
            // has extra values due to us calling the prompt's procedure here.
            M.addPrompt(promptTag, handler, M.e.length - M.a); 

            baselib.functions.rawApply(M);
        });




    exports['Primitives'] = Primitives;
    exports['installPrimitiveProcedure'] = installPrimitiveProcedure;
    exports['installPrimitiveClosure'] = installPrimitiveClosure;
    exports['installPrimitiveConstant'] = installPrimitiveConstant;

}(this.plt.baselib));
/*jslint browser: true, undef: true, unparam: true, sub: true, vars: true, white: true, plusplus: true, maxerr: 50, indent: 4 */


// runtime.js: the main runtime library for whalesong.
//

// All of the values here are namespaced under "plt.runtime".
/*global $*/
(function(plt, $) {
    'use strict';
    var runtime = {};
    plt.runtime = runtime;


    // abbreviation, since we'll be using the basic library a lot.
    var baselib = plt.baselib;


    //////////////////////////////////////////////////////////////////////
    //////////////////////////////////////////////////////////////////////
    // We try to isolate the effect of external modules: all the identifiers we
    // pull from external modules should be listed here, and should otherwise not
    // show up outside this section!
    var isNumber = baselib.numbers.isNumber;
    var isNatural = baselib.numbers.isNatural;
    var isReal = baselib.numbers.isReal;
    var isPair = baselib.lists.isPair;
    var isCaarPair = function(x) { return isPair(x) && isPair(x.first); };
    var isList = baselib.lists.isList;
    var isVector = baselib.vectors.isVector;
    var isString = baselib.strings.isString;
    var isSymbol = baselib.symbols.isSymbol;
    var isPath = baselib.paths.isPath;

    var equals = baselib.equality.equals;

    var NULL = baselib.lists.EMPTY;
    var VOID = baselib.constants.VOID_VALUE;

    var NEGATIVE_ZERO = baselib.numbers.negative_zero;
    var INF = baselib.numbers.inf;
    var NEGATIVE_INF = baselib.numbers.negative_inf;
    var NAN = baselib.numbers.nan;

    var makeFloat = baselib.numbers.makeFloat;
    var makeRational = baselib.numbers.makeRational;
    var makeBignum = baselib.numbers.makeBignum;
    var makeComplex = baselib.numbers.makeComplex;


    var makeSymbol = baselib.symbols.makeSymbol;
    var makePath = baselib.paths.makePath;
    var makeBytes = baselib.bytes.makeBytes;
    var makeBytesFromBase64 = baselib.bytes.makeBytesFromBase64;

    var makeBox = baselib.boxes.makeBox;
    var isBox = baselib.boxes.isBox;

    var makeVector = baselib.vectors.makeVector;
    var makeList = baselib.lists.makeList;
    var makePair = baselib.lists.makePair;
    var makeChar = baselib.chars.makeChar;

    var makeStructureType = baselib.structs.makeStructureType;


    var Struct = baselib.structs.Struct;
    var StructType = baselib.structs.StructType;

    var Closure = baselib.functions.Closure;
    var finalizeClosureCall = baselib.functions.finalizeClosureCall;
    var makePrimitiveProcedure = baselib.functions.makePrimitiveProcedure;
    var makeClosure = baselib.functions.makeClosure;

    var ContinuationPromptTag = baselib.contmarks.ContinuationPromptTag;


    // Other helpers
    var heir = baselib.heir;
    var makeClassPredicate = baselib.makeClassPredicate;
    var toDomNode = baselib.format.toDomNode;
    var toWrittenString = baselib.format.toWrittenString;
    var toDisplayedString = baselib.format.toDisplayedString;



    // Frame structures.
    var Frame = baselib.frames.Frame;
    var CallFrame = baselib.frames.CallFrame;
    var PromptFrame = baselib.frames.PromptFrame;

    // Module structure
    var ModuleRecord = baselib.modules.ModuleRecord;



    // Ports
    var isOutputPort = baselib.ports.isOutputPort;
    var StandardOutputPort = baselib.ports.StandardOutputPort;
    var StandardErrorPort = baselib.ports.StandardErrorPort;
    var StandardInputPort = baselib.ports.StandardInputPort;
    var isOutputStringPort = baselib.ports.isOutputStringPort;




    // Exceptions and error handling.
    var raise = baselib.exceptions.raise;
    var makeExnBreak = baselib.exceptions.makeExnBreak; 
    var raiseUnboundToplevelError = baselib.exceptions.raiseUnboundToplevelError;
    var raiseArgumentTypeError = baselib.exceptions.raiseArgumentTypeError;
    var raiseContextExpectedValuesError = baselib.exceptions.raiseContextExpectedValuesError;
    var raiseArityMismatchError = baselib.exceptions.raiseArityMismatchError;
    var raiseOperatorApplicationError = baselib.exceptions.raiseOperatorApplicationError;
    var raiseOperatorIsNotPrimitiveProcedure = baselib.exceptions.raiseOperatorIsNotPrimitiveProcedure;
    var raiseUnimplementedPrimitiveError = baselib.exceptions.raiseUnimplementedPrimitiveError;
    var raiseModuleLoadingError = baselib.exceptions.raiseModuleLoadingError;



    var ArityAtLeast = baselib.arity.ArityAtLeast;
    var makeArityAtLeast = baselib.arity.makeArityAtLeast;
    var isArityMatching = baselib.arity.isArityMatching;


    var testArgument = baselib.check.testArgument;
    var testArity = baselib.check.testArity;
    var makeCheckArgumentType = baselib.check.makeCheckArgumentType;


    var Primitives = baselib.primitives.Primitives;
    var installPrimitiveProcedure = baselib.primitives.installPrimitiveProcedure;



    // This value used to be dynamically determined, but something on iOS5
    // breaks badly when I try this.
    // We're very conservative now.
     var STACK_LIMIT_ESTIMATE = 200;



    //////////////////////////////////////////////////////////////////////



    var defaultCurrentPrintImplementation = function (MACHINE) {
        if(--MACHINE.cbt < 0) {
            throw defaultCurrentPrintImplementation;
        }
        var oldArgcount = MACHINE.a;

	var elt = MACHINE.e[MACHINE.e.length - 1];
	var outputPort =
	    MACHINE.params.currentOutputPort;
	if (elt !== VOID) {
	    outputPort.writeDomNode(
                MACHINE,
                toDomNode(elt, MACHINE.params['print-mode']));
	    outputPort.writeDomNode(MACHINE, toDomNode("\n", 'display'));
	}
        MACHINE.a = oldArgcount;
        return finalizeClosureCall(MACHINE, VOID);
    };
    var defaultCurrentPrint = makeClosure(
	"default-printer",
	1,
	defaultCurrentPrintImplementation);



    // makeRandomNonce: -> string
    // Creates a randomly-generated nonce.
    var makeRandomNonce = function() {
        var chars = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXTZabcdefghiklmnopqrstuvwxyz";
        var LEN = 32;
        var result = [];
        var i;
        for (i = 0; i < LEN; i++) {
            result.push(chars.charAt(Math.floor(Math.random() * chars.length)));
        }
        return result.join('');
    };


    //////////////////////////////////////////////////////////////////////

    // Exclusive Locks.  Even though JavaScript is a single-threaded
    // evaluator, we still have a need to create exclusive regions
    // of evaluation, since we might inadvertantly access some state
    // with two computations, with use of setTimeout.
    var ExclusiveLock = function() {
        this.locked = false;  // (U false string)
        this.alreadyReleased = false;
        this.waiters = [];
    };


    ExclusiveLock.prototype.acquire = function(id, onAcquire) {
        var that = this;
        if (!id) {
            id = makeRandomNonce();
        }

        this.alreadyReleased = false;

        if (this.locked === false) {
            this.locked = id;
            onAcquire.call(
                that,
                // releaseLock
                function() {
                    var waiter;
                    if (that.alreadyReleased) {
                        throw new Error(
                            "Internal error: trying to release the lock, but already released");
                    }
                    if (that.locked === false) {
                        throw new Error(
                            "Internal error: trying to unlock the lock, but already unlocked");
                    }
                    that.locked = false;
                    that.alreadyReleased = true;
                    if (that.waiters.length > 0) {
                        waiter = that.waiters.shift();
                        setTimeout(
                            function() {
                                that.acquire(waiter.id, waiter.onAcquire);
                            },
                            0);
                    }
                });
        } else {
            this.waiters.push({ id: id,
                                onAcquire: onAcquire } );
        }
    };
    //////////////////////////////////////////////////////////////////////





    //////////////////////////////////////////////////////////////////////]
    // The MACHINE

    var Machine = function() {
        // These are the modules that have been installed.  They are not
        // necessarily invoked yet.
        this.installedModules = {};  // String -> (-> ModuleRecord)
        this.reset();
    };


    // Resets the state of the machine.  Almost all of the system's
    // state is reset, except the installedModules, which should
    // persist as an optimization to reduce loading code over and
    // over.
    Machine.prototype.reset = function() {
	this.cbt = STACK_LIMIT_ESTIMATE;  // calls before trampoline
	this.v = void(0);         // value register
	this.p = void(0);        // procedure register
	this.a = void(0);           // argument count
	this.e = [];                // environment
	this.c = [];            // control: Arrayof (U Frame CallFrame PromptFrame)
	this.running = false;

        // We do not initialize installedModules here because
        // we want that to persist.

        // These are the modules that have been invoked.
	this.modules = {};     // String -> ModuleRecord
        this.mainModules = []; // Arrayof String
	this.params = {

            // print-as-expression: boolean
            'print-as-expression' : false,

            // print-mode: (one-of "write" "print" "constructor")
            'print-mode' : 'write',


	    // currentDisplayer: DomNode -> Void
	    // currentDisplayer is responsible for displaying to the browser.
	    'currentDisplayer': function(MACHINE, domNode) {
		$(domNode).appendTo(document.body);
	    },

	    // currentErrorDisplayer: DomNode -> Void
	    // currentErrorDisplayer is responsible for displaying errors to the browser.
	    'currentErrorDisplayer': function(MACHINE, domNode) {
                $(domNode).appendTo(document.body);
	    },

            'currentInspector': baselib.inspectors.DEFAULT_INSPECTOR,

	    'currentOutputPort': new StandardOutputPort(),
	    'currentErrorPort': new StandardErrorPort(),
            'currentInputPort': new StandardInputPort(),
	    'currentErrorHandler': function(MACHINE, exn) {
                MACHINE.params.currentErrorDisplayer(
                    MACHINE,
                    toDomNode(exn, MACHINE.params['print-mode']));
            },

	    'currentNamespace': { get: function() {}, 
                                  set : function() {}, 
                                  hasKey : function() { return false; }
                                },

	    // These parameters control how often
	    // control yields back to the browser
	    // for response.  The implementation is a
	    // simple PID controller.
	    //
	    // To tune this, adjust desiredYieldsPerSecond.
	    // Do no touch numBouncesBeforeYield or
	    // maxNumBouncesBeforeYield, because those
	    // are adjusted automatically by the
	    // recomputeMaxNumBouncesBeforeYield
	    // procedure.
	    'desiredYieldsPerSecond': 5,
	    'numBouncesBeforeYield': 2000,   // self-adjusting
	    'maxNumBouncesBeforeYield': 2000, // self-adjusting

	    'currentPrint': defaultCurrentPrint
	};
        this.globals = {};
	this.primitives = Primitives;
        this.exclusiveLock = new ExclusiveLock();
        this.breakScheduled = false;      // (U boolean function)
    };

    // Schedule a break the next time the trampoline begins.
    Machine.prototype.scheduleBreak = function(afterBreak) {
        if (typeof afterBreak === 'function') {
            this.breakScheduled = afterBreak;
        } else {
            this.breakScheduled = true;
        }
    };

    
    Machine.prototype.loadAndInvoke = function(moduleName, success, fail) {
        var that = this;
        runtime.currentModuleLoader(
            that, 
            moduleName,
            function() {
                that.modules[moduleName] = that.installedModules[moduleName]();
                that.modules[moduleName].invoke(that,
                                             function() {
                                                 success();
                                             },
                                             function(M, err) {
                                                 fail(err);
                                             });
            },
            function(err) {
                fail(err);
            });
    };


    // Try to get the continuation mark key used for procedure application tracing.
    var getTracedAppKey = function(MACHINE) {
        if (MACHINE.modules['whalesong/lang/private/traced-app.rkt']) {
            return MACHINE.modules['whalesong/lang/private/traced-app.rkt'].getExports().get('traced-app-key') || 'traced-app-key';
        }
        return void(0);
    };

    var getTracedCalleeKey = function(MACHINE) {
        if (MACHINE.modules['whalesong/lang/private/traced-app.rkt']) {
            return MACHINE.modules['whalesong/lang/private/traced-app.rkt'].getExports().get('traced-callee-key') || 'traced-callee-key';
        }
        return void(0);
    };



    // captureControl implements the continuation-capturing part of
    // call/cc.  It grabs the control frames up to (but not including) the
    // prompt tagged by the given tag.
    Machine.prototype.captureControl = function(skip, tag) {
	var MACHINE = this;
	var i;
	for (i = MACHINE.c.length - 1 - skip; i >= 0; i--) {
	    if (MACHINE.c[i].tag === tag) {
		return MACHINE.c.slice(i + 1,
					     MACHINE.c.length - skip);
	    }
	}
	raise(MACHINE, new Error("captureControl: unable to find tag " + tag));
    };



    // restoreControl clears the control stack (up to, but not including the
    // prompt tagged by tag), and then appends the rest of the control frames.
    // At the moment, the rest of the control frames is assumed to be in the
    // top of the environment.
    Machine.prototype.restoreControl = function(tag) {
	var MACHINE = this;
	var i;
	for (i = MACHINE.c.length - 1; i >= 0; i--) {
	    if (MACHINE.c[i].tag === tag) {
		MACHINE.c =
		    MACHINE.c.slice(0, i+1).concat(
			MACHINE.e[MACHINE.e.length - 1]);
		return;
	    }
	}
	raise(MACHINE, new Error("restoreControl: unable to find tag " + tag));

    };


    // Splices the list argument in the environment.  Adjusts MACHINE.a
    // appropriately.
    Machine.prototype.spliceListIntoStack = function(depth) {
	var MACHINE = this;
	var lst = MACHINE.e[MACHINE.e.length - 1 - depth];
	var vals = [];
	while(lst !== NULL) {
	    vals.push(lst.first);
	    lst = lst.rest;
	}
	vals.reverse();
	MACHINE.e.splice.apply(MACHINE.e,
				 [MACHINE.e.length - 1 - depth, 1].concat(vals));
	MACHINE.a = MACHINE.a + vals.length - 1;
    };


    // Unsplices a list from the MACHINE stack.
    Machine.prototype.unspliceRestFromStack = function(depth, length) {
	var MACHINE = this;
	var lst = NULL;
	var i;
	for (i = 0; i < length; i++) {
	    lst = makePair(MACHINE.e[MACHINE.e.length - depth - length + i],
                           lst);
	}
	MACHINE.e.splice(MACHINE.e.length - depth - length,
			   length,
			   lst);
	MACHINE.a = MACHINE.a - length + 1;
    };


    // Save the continuation mark on the top control frame.
    Machine.prototype.installContinuationMarkEntry = function(key, value) {
        var frame = this.c[this.c.length - 1];
        var marks = frame.getMarks();
        var i;
        var l = marks.length;
        for (i = 0; i < l; i++) {
            if (key === marks[i][0]) {
                marks[i][1] = value;
                return;
            }
        }
        marks.push([key, value]);
    };


    Machine.prototype.captureContinuationMarks = function(promptTag) {
        var kvLists = [];
        var i;
        var control = this.c;
        var tracedCalleeKey = getTracedCalleeKey(this);
        for (i = control.length-1; i >= 0; i--) {
            if (promptTag !== null &&
                control[i] instanceof PromptFrame && control[i].tag === promptTag) {
                break;
            }
            if (control[i].getMarks().length !== 0) {
                kvLists.push(control[i].getMarks());
            }

            if (tracedCalleeKey !== null &&
                control[i] instanceof CallFrame &&
                control[i].p !== null) {
                kvLists.push([[tracedCalleeKey, control[i].p]]);
            }
        }
        return new baselib.contmarks.ContinuationMarkSet(kvLists);
    };

    
    var justReturn = function(M) {
        M.p=M.c[M.c.length-1].label;
        M.c.pop();
        return(M.p)(M);
    };
    justReturn.mvr = function(M) {
        M.p=M.c[M.c.length-1].label;
        M.c.pop();
        return(M.p.mvr)(M);
    };

    Machine.prototype.addPrompt = function(promptTag, abortHandlerClosure, envLength) {
        this.c.push(new PromptFrame(justReturn,
                                    promptTag,
                                    envLength,
                                    abortHandlerClosure));
    };






    //////////////////////////////////////////////////////////////////////
    //////////////////////////////////////////////////////////////////////
    //////////////////////////////////////////////////////////////////////
    //////////////////////////////////////////////////////////////////////
    // The toplevel trampoline.
    //
    //
    // trampoline: MACHINE (MACHINE -> void) -> void
    //
    // All evaluation in Racketland happens in the context of this
    // trampoline.
    //
    var recomputeMaxNumBouncesBeforeYield;


    // Checks to see if we need to handle break.  If so, returns true.
    // Otherwise, returns false.
    //
    // FIXME: This logic is duplicated within one of the catch blocks
    // in the trampoline.  Would be nice to refactor.
    var maybeHandleBreakOutsideTrampoline = function(MACHINE) {
        var breakScheduled = MACHINE.breakScheduled;
        if (breakScheduled) {
            MACHINE.breakScheduled = false;
            MACHINE.running = false;
            MACHINE.params.currentErrorHandler(
                MACHINE,
                new baselib.exceptions.RacketError(
                    "User break",
                    makeExnBreak("User break.",
                                 MACHINE.captureContinuationMarks(),
                                 // FIXME: capture the continuation as well,
                                 // rather than just hold false.
                                 false)));
            if (typeof breakScheduled === 'function') {
                setTimeout(breakScheduled, 0);
            }
            return true;
        }
        return false;
    };

    var scheduleTrampoline = function(MACHINE, f, release) {
        setTimeout(
	    function() {
                if (maybeHandleBreakOutsideTrampoline(MACHINE)) {
                    release(); 
                    return; 
                }
                MACHINE._trampoline(f, false, release);
            },
            0);
    };

    // Creates a restarting function, that reschedules f in a context
    // with the old argcount in place.
    // Meant to be used only by the trampoline.
    var makeRestartFunction = function(MACHINE, release, pauseLock) {
        var oldArgcount = MACHINE.a;
        return function(f) {
            pauseLock.acquire(
                void(0),
                function(pauseReleaseLock) {
                    MACHINE.a = oldArgcount;
                    MACHINE._trampoline(f, false, release);
                    pauseReleaseLock();
                });
        };
    };


    // These are exception values that are treated specially in the context
    // of the trampoline.

    var HaltError = function(onHalt) {
        // onHalt: MACHINE -> void
        this.onHalt = onHalt || function(MACHINE) {};
    };


    var Pause = function(onPause) {
        // onPause: MACHINE -> void
        this.onPause = onPause;
    };
    var THE_SINGLETON_PAUSE = new Pause();
    var PAUSE = function(onPause) {
        THE_SINGLETON_PAUSE.onPause = onPause;
        throw(THE_SINGLETON_PAUSE);
    };


    // WARNING WARNING WARNING
    //
    // Make sure to get an exclusive lock before jumping into trampoline.
    // Otherwise, Bad Things will happen.
    //
    // e.g. machine.lock.acquire('id', function(release) { machine.trampoline... release();});
    Machine.prototype.trampoline = function(initialJump, noJumpingOff) {
        var that = this;

        that.exclusiveLock.acquire(
            'trampoline',
            function(release) {
                that._trampoline(initialJump, noJumpingOff, release);
            });
    };

    Machine.prototype._trampoline = function(initialJump, noJumpingOff, release) {
        var that = this;
        var thunk = initialJump;
        var startTime = (new Date()).valueOf();
        that.params.numBouncesBeforeYield =
            that.params.maxNumBouncesBeforeYield;
        that.running = true;

        while(true) {
            try {
                thunk(that);
                break;
            } catch (e) {
                // There are a few kinds of things that can get thrown
                // during racket evaluation:
                //
                // functions: this gets thrown if the Racket code
                // realizes that the number of bounces has grown too
                // large.  The thrown function represents a restarter
                // function.  The running flag remains true.
                //
                // Pause: causes the machine evaluation to pause, with
                // the expectation that it will restart momentarily.
                // The running flag on the machine will remain true.
                //
                // HaltError: causes evaluation to immediately halt.
                // We schedule the onHalt function of the HaltError to
                // call afterwards.  The running flag on the machine
                // is set to false.
                //
                // Everything else: otherwise, we send the exception value
                // to the current error handler and exit.
                // The running flag is set to false.
                if (typeof(e) === 'function') {
                    thunk = e;
                    that.cbt = STACK_LIMIT_ESTIMATE;


                    // If we're running an a model that prohibits
                    // jumping off the trampoline, continue.
                    if (noJumpingOff) {
                        continue;
                    }

                    if (that.params.numBouncesBeforeYield-- < 0) {
                        recomputeMaxNumBouncesBeforeYield(
                            that,
                            (new Date()).valueOf() - startTime);
                        scheduleTrampoline(that, thunk, release);
                        return;
                    }
                } else if (e === THE_SINGLETON_PAUSE) {
                    var pauseLock = new ExclusiveLock();
                    var oldArgcount = that.a;
                    var restarted = false;
                    var restart = function(f) {
                        pauseLock.acquire(
                            void(0),
                            function(releasePauseLock) {
                                restarted = true;
                                that.a = oldArgcount;
                                that._trampoline(f, false, release);
                                releasePauseLock();
                            });
                    };
                    var internalCall = function(proc, success, fail) {
                        var i;
                        if (restarted) {
                            return;
                        }
                        var args = [];
                        for (i = 3; i < arguments.length; i++) {
                            args.push(arguments[i]);
                        }
                        pauseLock.acquire(
                            void(0),
                            function(release) {
                                var newSuccess = function() {
                                    success.apply(null, arguments);
                                    release();
                                };
                                var newFail = function() {
                                    fail.apply(null, arguments);
                                    release();
                                };
                                baselib.functions.internalCallDuringPause.apply(
                                    null, [that, proc, newSuccess, newFail].concat(args));
                            });
                    };
                    e.onPause(restart, internalCall);
                    return;
                } else if (e instanceof HaltError) {
                    that.running = false;
                    that.breakScheduled = false;
                    e.onHalt(that);
                    release();
                    return;
                } else if (e instanceof baselib.exceptions.RacketError &&
                           baselib.exceptions.isExnBreak(e.racketError)) {
                    var oldBreakScheduled = that.breakScheduled;
                    if (typeof oldBreakScheduled === 'function') {
                        setTimeout(oldBreakScheduled, 0);
                    }
                    that.running = false;
                    that.breakScheduled = false;
                    release();
                    that.params.currentErrorHandler(that, e);
                    return;
                } else {
                    // General error condition: if there's a exception
                    // handler, use it.  Otherwise, just exit out of
                    // the trampoline and call the current error
                    // handler.
                    var exceptionHandlerFunction = getDynamicExceptionHandlerFunction(that);
                    if (e instanceof baselib.exceptions.RacketError && 
                        exceptionHandlerFunction !== false) {
                        that.p = exceptionHandlerFunction;
                        that.a = 1;
                        that.e.push(e.racketError);
                        thunk = baselib.functions.rawApply;
                    } else {
                        that.running = false;
                        that.breakScheduled = false;
                        release();
                        that.params.currentErrorHandler(that, e);
                        return;
                    }
                }
            }
        }

        that.running = false;
        that.breakScheduled = false;
        release();
        return;

    };

    // getDynamicExceptionHandlerFunction: machine -> (U procedure #f)
    //
    // Scans for the nearest continuation mark value associated to
    // baselib.paramz.exceptionHandlerKey.
    //
    // If it exists and the value is a closure, then return that closure's
    // label.  Otherwise, return false.
    var getDynamicExceptionHandlerFunction = function(M) {
        var contMarks = M.captureContinuationMarks(promptTag);
        var promptTag = baselib.contmarks.DEFAULT_CONTINUATION_PROMPT_TAG;
        var procOrUndefined = contMarks.refFirst(baselib.paramz.exceptionHandlerKey,
                                                 promptTag);
        if (baselib.functions.isProcedure(procOrUndefined)) {
            return procOrUndefined;
        } else {
            return false;
        }
    };



    // recomputeGas: state number -> number
    recomputeMaxNumBouncesBeforeYield = function(MACHINE, observedDelay) {
	// We'd like to see a delay of DESIRED_DELAY_BETWEEN_BOUNCES so
	// that we get MACHINE.params.desiredYieldsPerSecond bounces per
	// second.
	var DESIRED_DELAY_BETWEEN_BOUNCES =
	    (1000 / MACHINE.params.desiredYieldsPerSecond);
	var ALPHA = 50;
	var delta = (ALPHA * ((DESIRED_DELAY_BETWEEN_BOUNCES -
			       observedDelay) /
			      DESIRED_DELAY_BETWEEN_BOUNCES));
	MACHINE.params.maxNumBouncesBeforeYield =
            Math.max(Math.floor(MACHINE.params.maxNumBouncesBeforeYield + delta),
                     1);
    };










    //////////////////////////////////////////////////////////////////////
    //////////////////////////////////////////////////////////////////////
    //////////////////////////////////////////////////////////////////////









    // There is a single, distinguished default continuation prompt tag
    // that's used to wrap around toplevel prompts.
    var DEFAULT_CONTINUATION_PROMPT_TAG =
        baselib.contmarks.DEFAULT_CONTINUATION_PROMPT_TAG;








    //////////////////////////////////////////////////////////////////////
    //////////////////////////////////////////////////////////////////////
    //////////////////////////////////////////////////////////////////////

    var VariableReference = function(prefix, pos) {
        this.prefix = prefix;
        this.pos = pos;
    };








    //////////////////////////////////////////////////////////////////////
    //////////////////////////////////////////////////////////////////////
    //////////////////////////////////////////////////////////////////////

    // Implementation of the ready function.  This will fire off when
    // setReadyTrue is called.
    var ready, setReadyTrue, setReadyFalse;
    (function() {
        var runtimeIsReady = true;
        var readyWaiters = [];
        var notifyWaiter = function(w) {
            w();
        };

        ready = function(f) {
            if (runtimeIsReady) {
                notifyWaiter(f);
            } else {
                readyWaiters.push(f);
            }
        };

        setReadyTrue = function() {
            runtimeIsReady = true;
            while(runtimeIsReady && readyWaiters.length > 0) {
                notifyWaiter(readyWaiters.shift());
            }
        };

        setReadyFalse = function() {
            runtimeIsReady = false;
        };

    }());


    //////////////////////////////////////////////////////////////////////
    //////////////////////////////////////////////////////////////////////
    //////////////////////////////////////////////////////////////////////

    // Executes all programs that have been labeled as a main module
    var invokeMains = function(machine, succ, fail) {
        runtime.ready(function () {
            setReadyFalse();
            machine = machine || runtime.currentMachine;
            var wrappedSucc = function() { if (succ) { succ.apply(null, arguments); } setReadyTrue(); }
            var wrappedFail = function() { if (fail) { fail.apply(null, arguments); } setReadyTrue(); }
            var mainModules = machine.mainModules.slice();
            var loop = function() {
                if (mainModules.length > 0) {
                    var nextModuleName = mainModules.shift();
                    machine.loadAndInvoke(nextModuleName, loop, wrappedFail);
                } else {
                    wrappedSucc();
                }
            };
            setTimeout(loop, 0);
        });
    };

    // Looks up a name in any of the machine's main modules.
    var lookupInMains = function(name, machine) {
        var i;
        machine = machine || runtime.currentMachine;
        for (i = 0; i < machine.mainModules.length; i++) {
            var ns = machine.modules[machine.mainModules[i]].getExternalExports();
            if(ns.hasKey(name)) {
                return ns.get(name);
            }
        }
    };



    var checkClosureAndArity = function(M) {
        if(!(M.p instanceof Closure)){
            raiseOperatorApplicationError(M,M.p);
        }
        if(!isArityMatching(M.p.racketArity,M.a)) {
            raiseArityMismatchError(M,M.p,M.a);
        }
    };

    var checkPrimitiveArity = function(M) {
        if(!isArityMatching(M.p.racketArity,M.a)) {
            raiseArityMismatchError(M,M.p,M.a);
        }
    };


    //////////////////////////////////////////////////////////////////////
    // Superinstructions to try to reduce code size.
    var si_context_expected = function(n) {
        if (n === 1) { return si_context_expected_1; }
        var f = function(M) { raiseContextExpectedValuesError(M, n); };
        return f;
    };
    var si_context_expected_1 = function(M) { raiseContextExpectedValuesError(M, 1); }

    // A block that omits the multiple values returned on the stack and
    // continues on with the target function f.
    var si_pop_multiple_values_and_continue = function(target) {
        var f = function(M) {
            if(--M.cbt<0) { throw f; }
            M.e.length -= (M.a-1);
            return target(M);
        };
        return f;
    };


    //////////////////////////////////////////////////////////////////////
    var checkedIsZero = function(M, n) {
        if (typeof(n) === 'number') { return n===0; }
        return plt.baselib.numbers.equals(
            testArgument(M, 'number', isNumber, n, 0, 'zero?'),
            0);
    };

    var checkedAdd1 = function(M, n) {
        if (typeof(n) === 'number' && n < 9e15) { return n+1; }
        return plt.baselib.numbers.add(
            testArgument(M, 'number', isNumber, n, 0, 'add1'),
            1);
    };

    var checkedSub1 = function(M, n) {
        if (typeof(n) === 'number' && n > -9e15) { return n-1; }
        return plt.baselib.numbers.subtract(
            testArgument(M, 'number', isNumber, n, 0, 'sub1'),
            1);
    };

    var checkedNegate = function(M, n) {
        if (typeof(n) === 'number') { return -n; }
        return plt.baselib.numbers.subtract(
            0,
            testArgument(M, 'number', isNumber, n, 0, '-'));
    };

    var checkedAdd = function(M, x, y) {
        var sum, i;
        // fast path optimization: binop addition on fixnums
        if (arguments.length === 3) {
            if (typeof(x) === 'number' && typeof(y) === 'number') {
                sum = x + y;
                if (sum < -9e15 || sum > 9e15) {
                    return plt.baselib.numbers.add(x, y);
                }
                return sum;
            } else {
                if (! isNumber(x)) {
                    raiseArgumentTypeError(M, '+', 'number', 0, x);
                }
                if (! isNumber(y)) {
                    raiseArgumentTypeError(M, '+', 'number', 1, y);
                }
                return plt.baselib.numbers.add(x, y);
            }
        }
        // Secondary path: if everything is a fixnum...
        sum = 0;
        for (i = 1; i < arguments.length; i++) {
            if (typeof(arguments[i]) === 'number') {
                sum += arguments[i];
                if (sum < -9e15 || sum > 9e15) {
                    return checkedAddSlowPath(M, Array.prototype.slice.call(arguments, 1));
                }
            } else {
                return checkedAddSlowPath(M, Array.prototype.slice.call(arguments, 1));
            }
        }
        return sum;
    };

    var checkedAddSlowPath = function(M, args) {
        var i;
        var sum = 0;
        for (i = 0; i < args.length; i++) {
            if (! isNumber(args[i])) {
                raiseArgumentTypeError(M, '+', 'number', i, args[i]);
            }
            sum = plt.baselib.numbers.add(sum, args[i]);
        }
        return sum;
    };

    var checkedMul = function(M, x, y) {
        var prod, i;
        // fast path optimization: binop addition on fixnums
        if (arguments.length === 3) {
            if (typeof(x) === 'number' && typeof(y) === 'number') {
                prod = x * y;
                if (prod < -9e15 || prod > 9e15) {
                    return plt.baselib.numbers.multiply(x, y);
                }
                return prod;
            } else {
                if (! isNumber(x)) {
                    raiseArgumentTypeError(M, '*', 'number', 0, x);
                }
                if (! isNumber(y)) {
                    raiseArgumentTypeError(M, '*', 'number', 1, y);
                }
                return plt.baselib.numbers.multiply(x, y);
            }
        }
        // Secondary path: if everything is a fixnum...
        prod = 1;
        for (i = 1; i < arguments.length; i++) {
            if (typeof(arguments[i]) === 'number') {
                prod *= arguments[i];
                if (prod < -9e15 || prod > 9e15) {
                    return checkedMulSlowPath(M, Array.prototype.slice.call(arguments, 1));
                }
            } else {
                return checkedMulSlowPath(M, Array.prototype.slice.call(arguments, 1));
            }
        }
        return prod;
    };

    var checkedMulSlowPath = function(M, args) {
        var i, prod;
        if (! isNumber(args[0])) {
            raiseArgumentTypeError(M, '*', 'number', 0, args[0]);
        }
        var prod = args[0];
        for (i = 1; i < args.length; i++) {
            if (! isNumber(args[i])) {
                raiseArgumentTypeError(M, '*', 'number', i, args[i]);
            }
            prod = plt.baselib.numbers.multiply(prod, args[i]);
        }
        return prod;
    };


    var checkedSub = function(M, x, y) {
        // Assumption: at least two arguments to subtract.
        var sum;
        // fast path optimization: binop subtraction on fixnums
        if (arguments.length === 3) {
            if (typeof(x) === 'number' && typeof(y) === 'number') {
                sum = x - y;
                if (sum < -9e15 || sum > 9e15) {
                    return checkedSubSlowPath(M, Array.prototype.slice.call(arguments, 1));
                }
                return sum;
            } else {
                if (! isNumber(x)) {
                    raiseArgumentTypeError(M, '-', 'number', 0, x);
                }
                if (! isNumber(y)) {
                    raiseArgumentTypeError(M, '-', 'number', 1, y);
                }
                return plt.baselib.numbers.subtract(x, y);
            }
        }
        return checkedSubSlowPath(M, Array.prototype.slice.call(arguments, 1));
    };

    var checkedSubSlowPath = function(M, args) {
        var i;
        if (! isNumber(args[0])) {
            raiseArgumentTypeError(M, '-', 'number', 0, args[0]);
        }
        var sum = args[0];
        for (i = 1; i < args.length; i++) {
            if (! isNumber(args[i])) {
                raiseArgumentTypeError(M, '-', 'number', i, args[i]);
            }
            sum = plt.baselib.numbers.subtract(sum, args[i]);
        }
        return sum;
    };

    var checkedGreaterThan = function(M, x, y) {
        // fast path optimization: binop comparison on fixnums
        if (arguments.length === 3) {
            if (typeof(x) === 'number' && typeof(y) === 'number') {
                return x > y;
            } else {
                if (! isNumber(x)) {
                    raiseArgumentTypeError(M, '>', 'number', 0, x);
                }
                if (! isNumber(y)) {
                    raiseArgumentTypeError(M, '>', 'number', 1, y);
                }
                return plt.baselib.numbers.greaterThan(x, y);
            }
        }
        return checkedGreaterThanSlowPath(M, Array.prototype.slice.call(arguments, 1));
    };

    var checkedGreaterThanSlowPath = function(M, args) {
        var i;
        if (! isNumber(args[0])) {
            raiseArgumentTypeError(M, '>', 'number', 0, args[0]);
        }
        for (i = 1; i < args.length ; i++) {
            if (! isNumber(args[i])) {
                raiseArgumentTypeError(M, '>', 'number', i, args[i]);
            }
            if (! plt.baselib.numbers.greaterThan(args[i-1], args[i])) {
                return false;
            }
        }
        return true;
    };


    var checkedNumEquals = function(M, x, y) {
        // Assumption: at least two arguments to compare
        var i;

        // fast path optimization: binop comparison on fixnums
        if (arguments.length === 3) {
            if (typeof(x) === 'number' && typeof(y) === 'number') {
                return x === y;
            } else {
                if (! isNumber(x)) {
                    raiseArgumentTypeError(M, '=', 'number', 0, x);
                }
                if (! isNumber(y)) {
                    raiseArgumentTypeError(M, '=', 'number', 1, y);
                }
                return plt.baselib.numbers.equals(x, y);
            }
        }
        if (typeof(arguments[1]) !== 'number') {
            return checkedNumEqualsSlowPath(M, Array.prototype.slice.call(arguments, 1));
        }
        var n = arguments[1];
        for (i = 2; i < arguments.length; i++) {
            if (typeof(arguments[i]) === 'number') {
                if (n !== arguments[i]) { return false; }
            } else {
                return checkedNumEqualsSlowPath(M, Array.prototype.slice.call(arguments, 1));
            }
        }
        return true;
    };

    var checkedNumEqualsSlowPath = function(M, args) {
        var i;
        if (! isNumber(args[0])) {
            raiseArgumentTypeError(M, '=', 'number', 0, args[0]);
        }
        var n = args[0];
        for (i = 1; i < args.length; i++) {
            if (! isNumber(args[i])) {
                raiseArgumentTypeError(M, '=', 'number', i, args[i]);
            }
            if (! plt.baselib.numbers.equals(n, args[i])) {
                return false;
            }
        }
        return true;
    };

    var checkedCar = function(M, v) {
        if (isPair(v)) { return v.first; }
        raiseArgumentTypeError(M, 'car', 'pair', 0, v);
    };

    var checkedCdr = function(M, v) {
        if (isPair(v)) { return v.rest; }
        raiseArgumentTypeError(M, 'cdr', 'pair', 0, v);
    };

    var checkedVectorRef = function(M, vec, i) {
        var expectedTypeName;
        if (isVector(vec)) {
            if (typeof(i) === 'number') {
                if (i >= 0 && i < vec.elts.length) {
                    return vec.elts[i];
                }                
            } else if (isNatural(i)) {
                i = baselib.numbers.toFixnum(i);
                if (i >= 0 && i < vec.elts.length) {
                    return vec.elts[i];
                }
            }
            expectedTypeName = baselib.format.format('natural between 0 and ~a', 
                                                     [vec.elts.length]);
            raiseArgumentTypeError(M, 'vector-ref', expectedTypeName, 1, i);
        } else {
            raiseArgumentTypeError(M, 'vector-ref', 'vector', 0, vec);
        }
    };


    var checkedVectorSet = function(M, vec, i, val) {
        var expectedTypeName;
        if (isVector(vec)) {
            if (typeof(i) === 'number') {
                if (i >= 0 && i < vec.elts.length) {
                    vec.elts[i] = val;
                    return VOID;
                }                
            } else if (isNatural(i)) {
                i = baselib.numbers.toFixnum(i);
                if (i >= 0 && i < vec.elts.length) {
                    vec.elts[i] = val;
                    return VOID;
                }
            }
            expectedTypeName = baselib.format.format('natural between 0 and ~a', 
                                                     [vec.elts.length]);
            raiseArgumentTypeError(M, 'vector-set!', expectedTypeName, 1, i);
        } else {
            raiseArgumentTypeError(M, 'vector-set!', 'vector', 0, vec);
        }
    };


    // defaultModuleLoader: Machine string (-> any) (-> any) -> void
    //
    // The default module loader currently doesn't do anything dynamic.
    // 
    // Other module loader implementations may do more interesting
    // things here, such as loading off the disk, or from the network.
    var defaultModuleLoader = function(M, moduleName, success, fail) {
        if (M.installedModules[moduleName] !== undefined) {
            return success();
        } else {
            return fail();
        }
    };

    
    // makeLocalFileModuleLoader: (hashof string string) -> moduleLoader
    // Given the manifest mapping module names to the files that implement them,
    // produces a module loader that uses loadscript to get these modules
    // into memory.
    var makeLocalFileModuleLoader = function(moduleManifest) {
        var loadScript = baselib.loadscript.loadScript;
        return function(M, moduleName, success, fail) {

            if (M.installedModules[moduleName] !== undefined) {
                return success();
            } else {
                // The manifest should map module names to 
                // their files.
                if (moduleManifest[moduleName]) {
                    var modulePath = moduleManifest[moduleName];
                    return loadScript(modulePath+"?gensym="+Math.random(), success, fail);
                }
                return fail();
            }
        };
    };





    //////////////////////////////////////////////////////////////////////
    //////////////////////////////////////////////////////////////////////
    //////////////////////////////////////////////////////////////////////


    // Exports
    var exports = runtime;
    exports['currentMachine'] = new Machine();

    exports['currentModuleLoader'] = defaultModuleLoader;
    exports['makeLocalFileModuleLoader'] = makeLocalFileModuleLoader;

    exports['invokeMains'] = invokeMains;
    exports['lookupInMains'] = lookupInMains;


    // installing new primitives
    exports['installPrimitiveProcedure'] = installPrimitiveProcedure;
    exports['makePrimitiveProcedure'] = makePrimitiveProcedure;
    exports['Primitives'] = Primitives;

    exports['ready'] = ready;
    // Private: the runtime library will set this flag to true when
    // the library has finished loading.
    exports['setReadyTrue'] = setReadyTrue;
    exports['setReadyFalse'] = setReadyFalse;




    exports['Machine'] = Machine;
    exports['Frame'] = Frame;
    exports['CallFrame'] = CallFrame;
    exports['PromptFrame'] = PromptFrame;
    exports['Closure'] = Closure;
    exports['ModuleRecord'] = ModuleRecord;
    exports['VariableReference'] = VariableReference;
    exports['ContinuationPromptTag'] = ContinuationPromptTag;
    exports['DEFAULT_CONTINUATION_PROMPT_TAG'] =
	DEFAULT_CONTINUATION_PROMPT_TAG;
    exports['NULL'] = NULL;
    exports['VOID'] = VOID;

    exports['NEGATIVE_ZERO'] = NEGATIVE_ZERO;
    exports['INF'] = INF;
    exports['NEGATIVE_INF'] = NEGATIVE_INF;
    exports['NAN'] = NAN;





    exports['testArgument'] = testArgument;
    exports['testArity'] = testArity;
    exports['makeCheckArgumentType'] = makeCheckArgumentType;


    exports['raise'] = raise;
    exports['raiseUnboundToplevelError'] = raiseUnboundToplevelError;
    exports['raiseArgumentTypeError'] = raiseArgumentTypeError;
    exports['raiseContextExpectedValuesError'] = raiseContextExpectedValuesError;
    exports['raiseArityMismatchError'] = raiseArityMismatchError;
    exports['raiseOperatorApplicationError'] = raiseOperatorApplicationError;
    exports['raiseOperatorIsNotPrimitiveProcedure'] = raiseOperatorIsNotPrimitiveProcedure;
    exports['raiseUnimplementedPrimitiveError'] = raiseUnimplementedPrimitiveError;
    exports['raiseModuleLoadingError'] = raiseModuleLoadingError;


    exports['finalizeClosureCall'] = finalizeClosureCall;


    //////////////////////////////////////////////////////////////////////


    // Type constructors

    // numbers
    exports['makeList'] = makeList;
    exports['makePair'] = makePair;
    exports['makeChar'] = makeChar;
    exports['makeVector'] = makeVector;
    exports['makeBox'] = makeBox;
    exports['makeFloat'] = makeFloat;
    exports['makeRational'] = makeRational;
    exports['makeBignum'] = makeBignum;
    exports['makeComplex'] = makeComplex;
    exports['makeSymbol'] = makeSymbol;
    exports['makePath'] = makePath;
    exports['makeBytes'] = makeBytes;
    exports['makeBytesFromBase64'] = makeBytesFromBase64;


    exports['checkPair'] = baselib.check.checkPair;
    exports['checkNumber'] = baselib.check.checkNumber;
    exports['checkString'] = baselib.check.checkString;



    // Type predicates
    exports['isPair'] = isPair;
    exports['isCaarPair'] = isCaarPair;
    exports['isList'] = isList;
    exports['isVector'] = isVector;
    exports['isOutputPort'] = isOutputPort;
    exports['isOutputStringPort'] = isOutputStringPort;
    exports['isBox'] = isBox;
    exports['isString'] = isString;
    exports['isSymbol'] = isSymbol;
    exports['isPath'] = isPath;
    exports['isNumber'] = isNumber;
    exports['isNatural'] = isNatural;
    exports['isReal'] = isReal;
    exports['isProcedure'] = plt.baselib.functions.isProcedure;
    exports['equals'] = equals;

    exports['toDomNode'] = toDomNode;
    exports['toWrittenString'] = toWrittenString;
    exports['toDisplayedString'] = toDisplayedString;

    exports['ArityAtLeast'] = ArityAtLeast;
    exports['makeArityAtLeast'] = makeArityAtLeast;
    exports['isArityMatching'] = isArityMatching;

    exports['heir'] = heir;
    exports['makeClassPredicate'] = makeClassPredicate;

    exports['PAUSE'] = PAUSE;
    exports['HaltError'] = HaltError;



    exports['makeStructureType'] = makeStructureType;
    exports['Struct'] = Struct;
    exports['StructType'] = StructType;

    exports['getTracedAppKey'] = getTracedAppKey;
    exports['getTracedCalleeKey'] = getTracedCalleeKey;

    exports['si_context_expected'] = si_context_expected;
    exports['si_context_expected_1'] = si_context_expected_1;
    exports['checkClosureAndArity'] = checkClosureAndArity;
    exports['checkPrimitiveArity'] = checkPrimitiveArity;

    exports['checkedIsZero'] = checkedIsZero;
    exports['checkedAdd1'] = checkedAdd1;
    exports['checkedSub1'] = checkedSub1;
    exports['checkedNegate'] = checkedNegate;
    exports['checkedAdd'] = checkedAdd;
    exports['checkedAddSlowPath'] = checkedAddSlowPath;
    exports['checkedMul'] = checkedMul;
    exports['checkedMulSlowPath'] = checkedMulSlowPath;
    exports['checkedSub'] = checkedSub;
    exports['checkedSubSlowPath'] = checkedSubSlowPath;
    exports['checkedNumEquals'] = checkedNumEquals;
    exports['checkedNumEqualsSlowPath'] = checkedNumEqualsSlowPath;
    exports['checkedGreaterThan'] = checkedGreaterThan;
    exports['checkedGreaterThanSlowPath'] = checkedGreaterThanSlowPath;
    exports['checkedCar'] = checkedCar;
    exports['checkedCdr'] = checkedCdr;
    exports['checkedVectorRef'] = checkedVectorRef;
    exports['checkedVectorSet'] = checkedVectorSet;


    exports['makeRandomNonce'] = makeRandomNonce;
}(this.plt, jQuery));

(function(M, SUCCESS, FAIL, PARAMS) {(function(M, success, fail, params) {
"use strict";
var param;
var RT = plt.runtime;
var _247=function(M){if(--M.cbt<0){throw _247;}

//"lambda body for memq"

M.e.push(M.p.closedVals[0]);
M.v=(M.e[M.e.length-3]===RT.NULL);
if(M.v===false){return(_251)(M);}else{M.v=false;
M.e.length-=3;
M.p=M.c[M.c.length-1].label;
M.c.pop();
return(M.p)(M);}};

var _248=function(M){M.c.push(new RT.CallFrame(_250,M.p)); M.addPrompt(RT.DEFAULT_CONTINUATION_PROMPT_TAG,false,M.e.length);
M.e.push([]);M.e[M.e.length-1].names=[];
M.e.push(void(0));
M.e[M.e.length-1]=new RT.Closure(_247,2,void(0),"memq");
M.e[M.e.length-1].closedVals=[M.e[M.e.length-1]];
M.v=M.e[M.e.length-1];
M.a=1;
M.e.splice(M.e.length-((M.a-1)+1),1);
M.v=M.v;
M.e.splice(M.e.length-((M.a-1)+1),1);
M.p=M.c[M.c.length-1].label;
M.c.pop();
return(M.p)(M);};


var _184=function(M){if(--M.cbt<0){throw _184;}

//"lambda body for loop"

M.e.push(M.p.closedVals[0]);
M.e.push(M.e[M.e.length-3]);
M.p=_185_c;
M.a=1;
M.c.push(new RT.CallFrame(_199,M.p));
return(_185)(M);};

var _306=function(M){M.c.push(new RT.CallFrame(_308,M.p)); M.addPrompt(RT.DEFAULT_CONTINUATION_PROMPT_TAG,false,M.e.length);
M.e.push([]);M.e[M.e.length-1].names=[];
M.e.push(void(0));
M.e[M.e.length-1]=new RT.Closure(_305,2,void(0),"assv");
M.e[M.e.length-1].closedVals=[M.e[M.e.length-1]];
M.v=M.e[M.e.length-1];
M.a=1;
M.e.splice(M.e.length-((M.a-1)+1),1);
M.v=M.v;
M.e.splice(M.e.length-((M.a-1)+1),1);
M.p=M.c[M.c.length-1].label;
M.c.pop();
return(M.p)(M);};

var _32=function(M){M.p=_3_c;
M.a=2;
M.c.push(new RT.CallFrame(_37,M.p));
return(_3)(M);};

var _243=function(M){if(--M.cbt<0){throw _243;}

//"lambda body for unknown"

M.e.push(void(0));
M.e[M.e.length-1]=RT.checkedCar(M, M.e[M.e.length-2]);
M.v=RT.checkedCar(M, M.e[M.e.length-1]);
M.e.length-=2;
M.p=M.c[M.c.length-1].label;
M.c.pop();
return(M.p)(M);};

var _244=function(M){M.c.push(new RT.CallFrame(_246,M.p)); M.addPrompt(RT.DEFAULT_CONTINUATION_PROMPT_TAG,false,M.e.length);
M.e.push([]);M.e[M.e.length-1].names=[];
M.v=_243_c;
M.a=1;
M.v=M.v;
M.e.splice(M.e.length-((M.a-1)+1),1);
M.p=M.c[M.c.length-1].label;
M.c.pop();
return(M.p)(M);};

var _321=function(M){if(--M.cbt<0){throw _321;}
M.e.length-=(M.a-1);
return(_322)(M);};

var _325=function(M){M.e.push(void(0),void(0));
M.e[M.e.length-1]=M.e[M.e.length-4];
M.e[M.e.length-2]=RT.checkedCdr(M, M.e[M.e.length-5]);
M.p=M.e[M.e.length-3];
M.a=2;
M.e.splice(M.e.length-5,3);
M.c[M.c.length-1].p=M.p;
return(_319)(M);};

var _323=function(M){M.e.push(void(0),void(0));
M.e[M.e.length-1]=M.e[M.e.length-4];
M.e[M.e.length-2]=(RT.testArgument(M,"caarpair",RT.isCaarPair,M.e[M.e.length-5],0,"caar")).first.first;
M.v=M.primitives["equal?"]._i(M);
M.e.length-=2;
if(M.v===false){return(_325)(M);}else{M.v=RT.checkedCar(M, M.e[M.e.length-3]);
M.e.length-=3;
M.p=M.c[M.c.length-1].label;
M.c.pop();
return(M.p)(M);}};

var _319=function(M){if(--M.cbt<0){throw _319;}

//"lambda body for assoc"

M.e.push(M.p.closedVals[0]);
M.v=(M.e[M.e.length-3]===RT.NULL);
if(M.v===false){return(_323)(M);}else{M.v=false;
M.e.length-=3;
M.p=M.c[M.c.length-1].label;
M.c.pop();
return(M.p)(M);}};

var _320=function(M){M.c.push(new RT.CallFrame(_322,M.p)); M.addPrompt(RT.DEFAULT_CONTINUATION_PROMPT_TAG,false,M.e.length);
M.e.push([]);M.e[M.e.length-1].names=[];
M.e.push(void(0));
M.e[M.e.length-1]=new RT.Closure(_319,2,void(0),"assoc");
M.e[M.e.length-1].closedVals=[M.e[M.e.length-1]];
M.v=M.e[M.e.length-1];
M.a=1;
M.e.splice(M.e.length-((M.a-1)+1),1);
M.v=M.v;
M.e.splice(M.e.length-((M.a-1)+1),1);
M.p=M.c[M.c.length-1].label;
M.c.pop();
return(M.p)(M);};

var _384=function(M){RT.Primitives["values"]=RT.Primitives["values"]||_385_c;
return(_388)(M);};

var _292=function(M){M.c.push(new RT.CallFrame(_294,M.p)); M.addPrompt(RT.DEFAULT_CONTINUATION_PROMPT_TAG,false,M.e.length);
M.e.push([]);M.e[M.e.length-1].names=[];
M.e.push(void(0));
M.e[M.e.length-1]=new RT.Closure(_291,2,void(0),"assq");
M.e[M.e.length-1].closedVals=[M.e[M.e.length-1]];
M.v=M.e[M.e.length-1];
M.a=1;
M.e.splice(M.e.length-((M.a-1)+1),1);
M.v=M.v;
M.e.splice(M.e.length-((M.a-1)+1),1);
M.p=M.c[M.c.length-1].label;
M.c.pop();
return(M.p)(M);};

var _9=function(M){if(--M.cbt<0){throw _9;}
M.e.length-=(M.a-1);
return(_10)(M);};

var _59=function(M){M.v=RT.makePair(M.e[M.e.length-1],M.e[M.e.length-2]);
M.e.length-=3;
M.p=M.c[M.c.length-1].label;
M.c.pop();
return(M.p)(M);};


var _57=function(M){M.e.push(void(0),void(0));
M.e.push(void(0));
M.e[M.e.length-1]=RT.checkedCar(M, M.e[M.e.length-4]);
M.e[M.e.length-2]=RT.checkedCdr(M, M.e[M.e.length-1]);
M.e.pop();
M.e.push(void(0));
M.e.push(M.e[M.e.length-4]);
M.v=M.primitives["unsafe-cdr"]._i(M);
M.e.pop();
M.e[M.e.length-1]=M.v;
M.p=_7_c;
M.a=1;
M.c.push(new RT.CallFrame(_61,M.p));
return(_7)(M);};

var _54=function(M){M.v=RT.makePair(M.e[M.e.length-1],M.e[M.e.length-2]);
M.e.length-=3;
M.p=M.c[M.c.length-1].label;
M.c.pop();
return(M.p)(M);};


var _52=function(M){M.e.push(void(0),void(0));
M.e.push(void(0));
M.e[M.e.length-1]=RT.checkedCar(M, M.e[M.e.length-4]);
M.e[M.e.length-2]=RT.checkedCar(M, M.e[M.e.length-1]);
M.e.pop();
M.e.push(void(0));
M.e.push(M.e[M.e.length-4]);
M.v=M.primitives["unsafe-cdr"]._i(M);
M.e.pop();
M.e[M.e.length-1]=M.v;
M.p=_6_c;
M.a=1;
M.c.push(new RT.CallFrame(_56,M.p));
return(_6)(M);};

var _195=function(M){M.e.push(M.e[M.e.length-3]);
M.p=_186_c;
M.a=1;
M.c.push(new RT.CallFrame(_204,M.p));
return(_186)(M);};

var _47=function(M){M.e.push(void(0));
M.e.push(M.e[M.e.length-2]);
M.v=M.primitives["unsafe-cdr"]._i(M);
M.e.pop();
M.e[M.e.length-1]=M.v;
M.p=_5_c;
M.a=1;
M.e.splice(M.e.length-2,1);
M.c[M.c.length-1].p=M.p;
return(_5)(M);};

var _45=function(M){M.e.push(void(0));
M.e[M.e.length-1]=RT.checkedCar(M, M.e[M.e.length-2]);
M.v=(M.e[M.e.length-1]===RT.NULL);
M.e.pop();
if(M.v===false){return(_47)(M);}else{M.v=true;
M.e.pop();
M.p=M.c[M.c.length-1].label;
M.c.pop();
return(M.p)(M);}};

var _309=function(M){M.e.push(void(0),void(0));
M.e[M.e.length-1]=M.e[M.e.length-4];
M.e[M.e.length-2]=(RT.testArgument(M,"caarpair",RT.isCaarPair,M.e[M.e.length-5],0,"caar")).first.first;
M.v=M.primitives["eqv?"]._i(M);
M.e.length-=2;
if(M.v===false){return(_311)(M);}else{M.v=RT.checkedCar(M, M.e[M.e.length-3]);
M.e.length-=3;
M.p=M.c[M.c.length-1].label;
M.c.pop();
return(M.p)(M);}};

var _40=function(M){M.v=false;
M.e.pop();
M.p=M.c[M.c.length-1].label;
M.c.pop();
return(M.p)(M);};

var _38=function(M){M.e.push(void(0));
M.e[M.e.length-1]=RT.checkedCar(M, M.e[M.e.length-2]);
M.v=(M.e[M.e.length-1]===RT.NULL);
M.e.pop();
if(M.v===false){return(_40)(M);}else{M.e.push(void(0));
M.e.push(M.e[M.e.length-2]);
M.v=M.primitives["unsafe-cdr"]._i(M);
M.e.pop();
M.e[M.e.length-1]=M.v;
M.p=_4_c;
M.a=1;
M.e.splice(M.e.length-2,1);
M.c[M.c.length-1].p=M.p;
return(_4)(M);}};

var _35=function(M){M.v=RT.makePair(M.e[M.e.length-1],M.e[M.e.length-2]);
M.e.length-=4;
M.p=M.c[M.c.length-1].label;
M.c.pop();
return(M.p)(M);};


var _245=function(M){if(--M.cbt<0){throw _245;}
M.e.length-=(M.a-1);
return(_246)(M);};


var _7=function(M){if(--M.cbt<0){throw _7;}

//"lambda body for rest-lists"

M.v=(M.e[M.e.length-1]===RT.NULL);
if(M.v===false){return(_57)(M);}else{M.v=RT.NULL;
M.e.pop();
M.p=M.c[M.c.length-1].label;
M.c.pop();
return(M.p)(M);}};


var _27=function(M){M.p=M.e[M.e.length-2];
M.e[M.e.length-2]=M.v;
M.a=2;
RT.checkClosureAndArity(M);
M.c.push(new RT.CallFrame(_31,M.p));
return((M.p).label)(M);};


var _6=function(M){if(--M.cbt<0){throw _6;}

//"lambda body for first-tuple"

M.v=(M.e[M.e.length-1]===RT.NULL);
if(M.v===false){return(_52)(M);}else{M.v=RT.NULL;
M.e.pop();
M.p=M.c[M.c.length-1].label;
M.c.pop();
return(M.p)(M);}};

var _22=function(M){M.e.push(void(0),void(0));
M.e.push(void(0),void(0));
M.e[M.e.length-2]=M.primitives["apply"];
M.e[M.e.length-1]=M.e[M.e.length-5];
M.e.push(M.e[M.e.length-6]);
M.p=_6_c;
M.a=1;
M.c.push(new RT.CallFrame(_29,M.p));
return(_6)(M);};

var _24=function(M){if(M.v===false){return(_22)(M);}else{M.v=M.e[M.e.length-2];
M.v=M.e[M.e.length-1];
M.e.push(void(0),void(0));
M.e[M.e.length-1]=sym74;
M.e[M.e.length-2]="all lists must have the same size";
M.a=2;
M.v=M.primitives["error"]._i(M);
M.e.length-=4;
M.p=M.c[M.c.length-1].label;
M.c.pop();
return(M.p)(M);}};


var _5=function(M){if(--M.cbt<0){throw _5;}

//"lambda body for some-empty?"

M.v=(M.e[M.e.length-1]===RT.NULL);
if(M.v===false){return(_45)(M);}else{M.v=false;
M.e.pop();
M.p=M.c[M.c.length-1].label;
M.c.pop();
return(M.p)(M);}};

var _17=function(M){M.e.push(M.e[M.e.length-2]);
M.p=_5_c;
M.a=1;
M.c.push(new RT.CallFrame(_26,M.p));
return(_5)(M);};

var _19=function(M){if(M.v===false){return(_17)(M);}else{M.v=RT.NULL;
M.e.length-=2;
M.p=M.c[M.c.length-1].label;
M.c.pop();
return(M.p)(M);}};


var _4=function(M){if(--M.cbt<0){throw _4;}

//"lambda body for all-empty?"

M.v=(M.e[M.e.length-1]===RT.NULL);
if(M.v===false){return(_38)(M);}else{M.v=true;
M.e.pop();
M.p=M.c[M.c.length-1].label;
M.c.pop();
return(M.p)(M);}};

var _3=function(M){if(--M.cbt<0){throw _3;}

//"lambda body for loop"

M.e.push(M.e[M.e.length-2]);
M.p=_4_c;
M.a=1;
M.c.push(new RT.CallFrame(_21,M.p));
return(_4)(M);};

var _2=function(M){if(--M.cbt<0){throw _2;}

//"lambda body for do-it"

M.e.push(void(0),void(0));
M.e[M.e.length-1]=M.e[M.e.length-3];
M.e[M.e.length-2]=M.e[M.e.length-4];
M.p=_3_c;
M.a=2;
M.e.splice(M.e.length-4,2);
M.c[M.c.length-1].p=M.p;
return(_3)(M);};

var _1=function(M){if(--M.cbt<0){throw _1;}

//"lambda body for unknown"

M.unspliceRestFromStack(1,(M.a-1));
M.e.push(void(0),void(0));
M.e[M.e.length-1]=M.e[M.e.length-3];
M.e[M.e.length-2]=M.e[M.e.length-4];
M.p=_2_c;
M.a=2;
M.e.splice(M.e.length-4,2);
M.c[M.c.length-1].p=M.p;
return(_2)(M);};

var _8=function(M){M.c.push(new RT.CallFrame(_10,M.p)); M.addPrompt(RT.DEFAULT_CONTINUATION_PROMPT_TAG,false,M.e.length);
M.e.push([]);M.e[M.e.length-1].names=[];
M.v=_1_c;
M.a=1;
M.v=M.v;
M.e.splice(M.e.length-((M.a-1)+1),1);
M.p=M.c[M.c.length-1].label;
M.c.pop();
return(M.p)(M);};

var _191=function(M){if(--M.cbt<0){throw _191;}
M.e.length-=(M.a-1);
return(_192)(M);};

var _189=function(M){if(--M.cbt<0){throw _189;}

//"lambda body for unknown"

M.unspliceRestFromStack(1,(M.a-1));
M.e.push(M.p.closedVals[0]);
M.e.push(void(0),void(0));
//"Constant toplevel ref: ?"

M.p=M.e[M.e.length-3][2];
M.e[M.e.length-1]=M.e[M.e.length-4];
M.e[M.e.length-2]=M.e[M.e.length-5];
M.a=2;
RT.checkClosureAndArity(M);
M.e.splice(M.e.length-5,3);
M.c[M.c.length-1].p=M.p;
return((M.p).label)(M);};

var _238=function(M){M.v=RT.makePair(M.e[M.e.length-1],M.e[M.e.length-2]);
M.e.length-=3;
M.p=M.c[M.c.length-1].label;
M.c.pop();
return(M.p)(M);};


var _236=function(M){M.e.push(void(0),void(0));
M.e.push(void(0));
M.e[M.e.length-1]=RT.checkedCar(M, M.e[M.e.length-4]);
M.e[M.e.length-2]=RT.checkedCdr(M, M.e[M.e.length-1]);
M.e.pop();
M.e.push(void(0));
M.e.push(M.e[M.e.length-4]);
M.v=M.primitives["unsafe-cdr"]._i(M);
M.e.pop();
M.e[M.e.length-1]=M.v;
M.p=_188_c;
M.a=1;
M.c.push(new RT.CallFrame(_240,M.p));
return(_188)(M);};

var _233=function(M){M.v=RT.makePair(M.e[M.e.length-1],M.e[M.e.length-2]);
M.e.length-=3;
M.p=M.c[M.c.length-1].label;
M.c.pop();
return(M.p)(M);};


var _231=function(M){M.e.push(void(0),void(0));
M.e.push(void(0));
M.e[M.e.length-1]=RT.checkedCar(M, M.e[M.e.length-4]);
M.e[M.e.length-2]=RT.checkedCar(M, M.e[M.e.length-1]);
M.e.pop();
M.e.push(void(0));
M.e.push(M.e[M.e.length-4]);
M.v=M.primitives["unsafe-cdr"]._i(M);
M.e.pop();
M.e[M.e.length-1]=M.v;
M.p=_187_c;
M.a=1;
M.c.push(new RT.CallFrame(_235,M.p));
return(_187)(M);};

var _226=function(M){M.e.push(void(0));
M.e.push(M.e[M.e.length-2]);
M.v=M.primitives["unsafe-cdr"]._i(M);
M.e.pop();
M.e[M.e.length-1]=M.v;
M.p=_186_c;
M.a=1;
M.e.splice(M.e.length-2,1);
M.c[M.c.length-1].p=M.p;
return(_186)(M);};

var _224=function(M){M.e.push(void(0));
M.e[M.e.length-1]=RT.checkedCar(M, M.e[M.e.length-2]);
M.v=(M.e[M.e.length-1]===RT.NULL);
M.e.pop();
if(M.v===false){return(_226)(M);}else{M.v=true;
M.e.pop();
M.p=M.c[M.c.length-1].label;
M.c.pop();
return(M.p)(M);}};

var _219=function(M){M.v=false;
M.e.pop();
M.p=M.c[M.c.length-1].label;
M.c.pop();
return(M.p)(M);};

var _217=function(M){M.e.push(void(0));
M.e[M.e.length-1]=RT.checkedCar(M, M.e[M.e.length-2]);
M.v=(M.e[M.e.length-1]===RT.NULL);
M.e.pop();
if(M.v===false){return(_219)(M);}else{M.e.push(void(0));
M.e.push(M.e[M.e.length-2]);
M.v=M.primitives["unsafe-cdr"]._i(M);
M.e.pop();
M.e[M.e.length-1]=M.v;
M.p=_185_c;
M.a=1;
M.e.splice(M.e.length-2,1);
M.c[M.c.length-1].p=M.p;
return(_185)(M);}};


var _210=function(M){M.p=M.e[M.e.length-2];
M.e[M.e.length-2]=M.v;
M.a=2;
RT.checkClosureAndArity(M);
M.c.push(new RT.CallFrame(_214,M.p));
return((M.p).label)(M);};


var _188=function(M){if(--M.cbt<0){throw _188;}

//"lambda body for rest-lists"

M.v=(M.e[M.e.length-1]===RT.NULL);
if(M.v===false){return(_236)(M);}else{M.v=RT.NULL;
M.e.pop();
M.p=M.c[M.c.length-1].label;
M.c.pop();
return(M.p)(M);}};


var _205=function(M){M.p=M.e[M.e.length-2];
M.e[M.e.length-2]=M.v;
M.a=2;
RT.checkClosureAndArity(M);
M.c.push(new RT.CallFrame(_209,M.p));
return((M.p).label)(M);};


var _187=function(M){if(--M.cbt<0){throw _187;}

//"lambda body for first-tuple"

M.v=(M.e[M.e.length-1]===RT.NULL);
if(M.v===false){return(_231)(M);}else{M.v=RT.NULL;
M.e.pop();
M.p=M.c[M.c.length-1].label;
M.c.pop();
return(M.p)(M);}};

var _200=function(M){M.e.push(void(0),void(0));
if (M.globals["or"]===void(0)&&M.params.currentNamespace.get("or")===void(0)){ RT.raiseUnboundToplevelError(M,"or"); }
M.e[M.e.length-2]=(M.globals["or"]!==void(0)?M.globals["or"]:M.params.currentNamespace.get("or"));
M.e.push(void(0),void(0));
M.e[M.e.length-2]=M.primitives["apply"];
M.e[M.e.length-1]=M.e[M.e.length-6];
M.e.push(M.e[M.e.length-7]);
M.p=_187_c;
M.a=1;
M.c.push(new RT.CallFrame(_207,M.p));
return(_187)(M);};

var _202=function(M){if(M.v===false){return(_200)(M);}else{M.e.push(void(0),void(0));
M.e[M.e.length-1]=sym75;
M.e[M.e.length-2]="all lists must have the same size";
M.a=2;
M.v=M.primitives["error"]._i(M);
M.e.length-=5;
M.p=M.c[M.c.length-1].label;
M.c.pop();
return(M.p)(M);}};


var _186=function(M){if(--M.cbt<0){throw _186;}

//"lambda body for some-empty?"

M.v=(M.e[M.e.length-1]===RT.NULL);
if(M.v===false){return(_224)(M);}else{M.v=false;
M.e.pop();
M.p=M.c[M.c.length-1].label;
M.c.pop();
return(M.p)(M);}};

var _307=function(M){if(--M.cbt<0){throw _307;}
M.e.length-=(M.a-1);
return(_308)(M);};

var _197=function(M){if(M.v===false){return(_195)(M);}else{M.v=RT.NULL;
M.e.length-=3;
M.p=M.c[M.c.length-1].label;
M.c.pop();
return(M.p)(M);}};

var _311=function(M){M.e.push(void(0),void(0));
M.e[M.e.length-1]=M.e[M.e.length-4];
M.e[M.e.length-2]=RT.checkedCdr(M, M.e[M.e.length-5]);
M.p=M.e[M.e.length-3];
M.a=2;
M.e.splice(M.e.length-5,3);
M.c[M.c.length-1].p=M.p;
return(_305)(M);};

var _185=function(M){if(--M.cbt<0){throw _185;}

//"lambda body for all-empty?"

M.v=(M.e[M.e.length-1]===RT.NULL);
if(M.v===false){return(_217)(M);}else{M.v=true;
M.e.pop();
M.p=M.c[M.c.length-1].label;
M.c.pop();
return(M.p)(M);}};

var _305=function(M){if(--M.cbt<0){throw _305;}

//"lambda body for assv"

M.e.push(M.p.closedVals[0]);
M.v=(M.e[M.e.length-3]===RT.NULL);
if(M.v===false){return(_309)(M);}else{M.v=false;
M.e.length-=3;
M.p=M.c[M.c.length-1].label;
M.c.pop();
return(M.p)(M);}};

var _183=function(M){if(--M.cbt<0){throw _183;}

//"lambda body for do-it"

M.e.push(M.p.closedVals[0]);
M.e.push(void(0),void(0));
//"Constant toplevel ref: ?"

M.p=M.e[M.e.length-3][1];
M.e[M.e.length-1]=M.e[M.e.length-4];
M.e[M.e.length-2]=M.e[M.e.length-5];
M.a=2;
RT.checkClosureAndArity(M);
M.e.splice(M.e.length-5,3);
M.c[M.c.length-1].p=M.p;
return((M.p).label)(M);};

var _190=function(M){M.c.push(new RT.CallFrame(_192,M.p)); M.addPrompt(RT.DEFAULT_CONTINUATION_PROMPT_TAG,false,M.e.length);
M.e.push([M.globals["or"]!==void(0)?M.globals["or"]:M.params.currentNamespace.get("or"),false,false]);M.e[M.e.length-1].names=["or",false,false];
M.v=new RT.Closure(_183,2,[M.e[M.e.length-1]],"do-it");
M.e[M.e.length-1][2]=M.v;
M.v=RT.VOID;
M.v=new RT.Closure(_184,2,[M.e[M.e.length-1]],"loop");
M.e[M.e.length-1][1]=M.v;
M.v=RT.VOID;
M.v=new RT.Closure(_189,(RT.makeArityAtLeast(1)),[M.e[M.e.length-1]],"unknown");
M.a=1;
M.v=M.v;
M.e.splice(M.e.length-((M.a-1)+1),1);
M.p=M.c[M.c.length-1].label;
M.c.pop();
return(M.p)(M);};

var _92=function(M){if(--M.cbt<0){throw _92;}
M.e.push(void(0),void(0));
M.e[M.e.length-1]=M.e[M.e.length-3];
M.e.push(M.e[M.e.length-4]);
M.p=_68_c;
M.a=1;
M.c.push(new RT.CallFrame(_95,M.p));
return(_68)(M);};

var _367=function(M){M.v=RT.makePair(M.e[M.e.length-1],M.e[M.e.length-2]);
M.e.length-=4;
M.p=M.c[M.c.length-1].label;
M.c.pop();
return(M.p)(M);};

var _93=function(M){M.p=_64_c;
M.a=2;
M.e.splice(M.e.length-4,2);
M.c[M.c.length-1].p=M.p;
return(_64)(M);};

var _348=function(M){if(--M.cbt<0){throw _348;}

//"lambda body for append-2"

M.v=(M.e[M.e.length-1]===RT.NULL);
if(M.v===false){return(_365)(M);}else{M.v=M.e[M.e.length-2];
M.e.length-=2;
M.p=M.c[M.c.length-1].label;
M.c.pop();
return(M.p)(M);}};

var _359=function(M){M.p=_348_c;
M.a=2;
M.e.splice(M.e.length-3,1);
M.c[M.c.length-1].p=M.p;
return(_348)(M);};

var _88=function(M){M.p=M.e[M.e.length-2];
M.e[M.e.length-2]=M.v;
M.a=2;
RT.checkClosureAndArity(M);
M.c.push(new RT.CallFrame(_92,M.p));
return((M.p).label)(M);};


var _67=function(M){if(--M.cbt<0){throw _67;}

//"lambda body for first-tuple"

M.v=(M.e[M.e.length-1]===RT.NULL);
if(M.v===false){return(_113)(M);}else{M.v=RT.NULL;
M.e.pop();
M.p=M.c[M.c.length-1].label;
M.c.pop();
return(M.p)(M);}};

var _392=function(M){return(_8)(M);};

var _389=function(M){if(--M.cbt<0){throw _389;}
M.p=M.e[M.e.length-1];
M.e.pop();
M.a=(M.a-1);
M.spliceListIntoStack((M.a-1));
RT.checkClosureAndArity(M);
M.e.splice(M.e.length-(M.a+0),0);
M.c[M.c.length-1].p=M.p;
return((M.p).label)(M);};

var _388=function(M){RT.Primitives["apply"]=RT.Primitives["apply"]||_389_c;
};

var _385=function(M){if(--M.cbt<0){throw _385;}
if(M.a===1){return(_387)(M);}else{if(M.a===0){return(_386)(M);}else{M.v=M.e[M.e.length-1];
M.e.pop();
M.p=(M.c[M.c.length-1].label.mvr||RT.si_context_expected_1);
M.c.pop();
return(M.p)(M);}}};

var _386=function(M){M.p=(M.c[M.c.length-1].label.mvr||RT.si_context_expected_1);
M.c.pop();
return(M.p)(M);};

var _387=function(M){M.v=M.e[M.e.length-1];
M.e.pop();
M.p=M.c[M.c.length-1].label;
M.c.pop();
return(M.p)(M);};

var _293=function(M){if(--M.cbt<0){throw _293;}
M.e.length-=(M.a-1);
return(_294)(M);};

var _callCCClosureEntry=function(M){if(--M.cbt<0){throw _callCCClosureEntry;}
M.v=M.e[M.e.length-1];
M.e.push(M.p.closedVals[0],M.p.closedVals[1]);
M.restoreControl(RT.DEFAULT_CONTINUATION_PROMPT_TAG);
M.e=M.e[M.e.length-2].slice(0);
M.p=M.c[M.c.length-1].label;
M.c.pop();
return(M.p)(M);};

var _381=function(M){return(_384)(M);};

var _callCCEntry=function(M){if(--M.cbt<0){throw _callCCEntry;}
M.p=M.e[M.e.length-1];
M.e.push(void(0),void(0));
M.e[M.e.length-1]=M.captureControl(0,RT.DEFAULT_CONTINUATION_PROMPT_TAG);
M.e[M.e.length-2]=M.e.slice(0, M.e.length-3);
M.e[M.e.length-3]=new RT.Closure(_callCCClosureEntry,1,[M.e[M.e.length-2],M.e[M.e.length-1]],"call/cc");
M.e.length-=2;
M.a=1;
RT.checkClosureAndArity(M);
M.e.splice(M.e.length-1,0);
M.c[M.c.length-1].p=M.p;
return((M.p).label)(M);};

var _373=function(M){if(--M.cbt<0){throw _373;}
RT.Primitives["call-with-values"]=RT.Primitives["call-with-values"]||M.v;
RT.Primitives["call/cc"]=RT.Primitives["call/cc"]||_callCCEntry_c;
RT.Primitives["call-with-current-continuation"]=RT.Primitives["call-with-current-continuation"]||_callCCEntry_c;
return(_384)(M);};

var _377=function(M){if(--M.cbt<0){throw _377;}
M.a=1;
return(_378)(M);};

var _351=function(M){if(--M.cbt<0){throw _351;}
RT.Primitives["append"]=RT.Primitives["append"]||M.v;
return(_371)(M);};

var _369=function(M){if(--M.cbt<0){throw _369;}
M.e[M.e.length-2]=M.v;
return(_367)(M);};

var _361=function(M){if(--M.cbt<0){throw _361;}
M.e[M.e.length-2]=M.v;
return(_359)(M);};

var _337=function(M){if(--M.cbt<0){throw _337;}
RT.Primitives["length"]=RT.Primitives["length"]||M.v;
return(_349)(M);};

var _322=function(M){if(--M.cbt<0){throw _322;}
RT.Primitives["assoc"]=RT.Primitives["assoc"]||M.v;
return(_335)(M);};

var _308=function(M){if(--M.cbt<0){throw _308;}
RT.Primitives["assv"]=RT.Primitives["assv"]||M.v;
return(_320)(M);};

var _294=function(M){if(--M.cbt<0){throw _294;}
RT.Primitives["assq"]=RT.Primitives["assq"]||M.v;
return(_306)(M);};

var _278=function(M){if(--M.cbt<0){throw _278;}
RT.Primitives["memf"]=RT.Primitives["memf"]||M.v;
return(_292)(M);};

var _284=function(M){if(--M.cbt<0){throw _284;}
if(M.v===false){return(_281)(M);}else{M.v=M.e[M.e.length-4];
M.e.length-=4;
M.p=M.c[M.c.length-1].label;
M.c.pop();
return(M.p)(M);}};

var _264=function(M){if(--M.cbt<0){throw _264;}
RT.Primitives["memv"]=RT.Primitives["memv"]||M.v;
return(_276)(M);};

var _250=function(M){if(--M.cbt<0){throw _250;}
RT.Primitives["memq"]=RT.Primitives["memq"]||M.v;
return(_262)(M);};

var _246=function(M){if(--M.cbt<0){throw _246;}
RT.Primitives["caar"]=RT.Primitives["caar"]||M.v;
return(_248)(M);};

var _192=function(M){if(--M.cbt<0){throw _192;}
RT.Primitives["ormap"]=RT.Primitives["ormap"]||M.v;
return(_244)(M);};

var _240=function(M){if(--M.cbt<0){throw _240;}
M.e[M.e.length-2]=M.v;
return(_238)(M);};

var _235=function(M){if(--M.cbt<0){throw _235;}
M.e[M.e.length-2]=M.v;
return(_233)(M);};

var _214=function(M){if(--M.cbt<0){throw _214;}
M.p=M.e[M.e.length-2];
M.e[M.e.length-2]=M.v;
M.a=2;
RT.checkClosureAndArity(M);
M.e.splice(M.e.length-5,3);
M.c[M.c.length-1].p=M.p;
return((M.p).label)(M);};

var _212=function(M){if(--M.cbt<0){throw _212;}
return(_210)(M);};

var _209=function(M){if(--M.cbt<0){throw _209;}
M.e[M.e.length-1]=M.v;
M.e.push(void(0),void(0));
//"Constant toplevel ref: ?"

M.e[M.e.length-2]=M.e[M.e.length-5][1];
M.e[M.e.length-1]=M.e[M.e.length-6];
M.e.push(M.e[M.e.length-7]);
M.p=_188_c;
M.a=1;
M.c.push(new RT.CallFrame(_212,M.p));
return(_188)(M);};

var _207=function(M){if(--M.cbt<0){throw _207;}
return(_205)(M);};

var _204=function(M){if(--M.cbt<0){throw _204;}
return(_202)(M);};

var _199=function(M){if(--M.cbt<0){throw _199;}
return(_197)(M);};

var _132=function(M){if(--M.cbt<0){throw _132;}
RT.Primitives["andmap"]=RT.Primitives["andmap"]||M.v;
return(_190)(M);};

var _180=function(M){if(--M.cbt<0){throw _180;}
M.e[M.e.length-2]=M.v;
return(_178)(M);};

var _175=function(M){if(--M.cbt<0){throw _175;}
M.e[M.e.length-2]=M.v;
return(_173)(M);};

var _154=function(M){if(--M.cbt<0){throw _154;}
M.p=M.e[M.e.length-2];
M.e[M.e.length-2]=M.v;
M.a=2;
RT.checkClosureAndArity(M);
M.e.splice(M.e.length-5,3);
M.c[M.c.length-1].p=M.p;
return((M.p).label)(M);};

var _152=function(M){if(--M.cbt<0){throw _152;}
return(_150)(M);};

var _149=function(M){if(--M.cbt<0){throw _149;}
M.e[M.e.length-1]=M.v;
M.e.push(void(0),void(0));
//"Constant toplevel ref: ?"

M.e[M.e.length-2]=M.e[M.e.length-5][1];
M.e[M.e.length-1]=M.e[M.e.length-6];
M.e.push(M.e[M.e.length-7]);
M.p=_128_c;
M.a=1;
M.c.push(new RT.CallFrame(_152,M.p));
return(_128)(M);};

var _147=function(M){if(--M.cbt<0){throw _147;}
return(_145)(M);};

var _144=function(M){if(--M.cbt<0){throw _144;}
return(_142)(M);};

var _139=function(M){if(--M.cbt<0){throw _139;}
return(_137)(M);};

var _71=function(M){if(--M.cbt<0){throw _71;}
RT.Primitives["for-each"]=RT.Primitives["for-each"]||M.v;
return(_130)(M);};

var _122=function(M){if(--M.cbt<0){throw _122;}
M.e[M.e.length-2]=M.v;
return(_120)(M);};

var _117=function(M){if(--M.cbt<0){throw _117;}
M.e[M.e.length-2]=M.v;
return(_115)(M);};

var _95=function(M){if(--M.cbt<0){throw _95;}
M.e[M.e.length-2]=M.v;
return(_93)(M);};

var _101=function(M){M.v=false;
M.e.pop();
M.p=M.c[M.c.length-1].label;
M.c.pop();
return(M.p)(M);};

var _90=function(M){if(--M.cbt<0){throw _90;}
return(_88)(M);};

var _87=function(M){if(--M.cbt<0){throw _87;}
return(_85)(M);};

var _82=function(M){if(--M.cbt<0){throw _82;}
return(_80)(M);};

var _10=function(M){if(--M.cbt<0){throw _10;}
RT.Primitives["map"]=RT.Primitives["map"]||M.v;
return(_69)(M);};

var _61=function(M){if(--M.cbt<0){throw _61;}
M.e[M.e.length-2]=M.v;
return(_59)(M);};

var _56=function(M){if(--M.cbt<0){throw _56;}
M.e[M.e.length-2]=M.v;
return(_54)(M);};

var _37=function(M){if(--M.cbt<0){throw _37;}
M.e[M.e.length-2]=M.v;
return(_35)(M);};

var _34=function(M){if(--M.cbt<0){throw _34;}
M.e[M.e.length-2]=M.v;
return(_32)(M);};

var _31=function(M){if(--M.cbt<0){throw _31;}
M.e[M.e.length-1]=M.v;
M.e.push(void(0),void(0));
M.e[M.e.length-1]=M.e[M.e.length-5];
M.e.push(M.e[M.e.length-6]);
M.p=_7_c;
M.a=1;
M.c.push(new RT.CallFrame(_34,M.p));
return(_7)(M);};

var _29=function(M){if(--M.cbt<0){throw _29;}
return(_27)(M);};

var _347=function(M){if(--M.cbt<0){throw _347;}

//"lambda body for append-many"

M.v=(M.e[M.e.length-1]===RT.NULL);
if(M.v===false){return(_355)(M);}else{M.v=RT.NULL;
M.e.pop();
M.p=M.c[M.c.length-1].label;
M.c.pop();
return(M.p)(M);}};

var _346=function(M){if(--M.cbt<0){throw _346;}

//"lambda body for unknown"

M.unspliceRestFromStack(0,M.a);
M.e.push(M.e[M.e.length-1]);
M.p=_347_c;
M.a=1;
M.e.splice(M.e.length-2,1);
M.c[M.c.length-1].p=M.p;
return(_347)(M);};

var _349=function(M){M.c.push(new RT.CallFrame(_351,M.p)); M.addPrompt(RT.DEFAULT_CONTINUATION_PROMPT_TAG,false,M.e.length);
M.e.push([]);M.e[M.e.length-1].names=[];
M.v=_346_c;
M.a=1;
M.v=M.v;
M.e.splice(M.e.length-((M.a-1)+1),1);
M.p=M.c[M.c.length-1].label;
M.c.pop();
return(M.p)(M);};

var _26=function(M){if(--M.cbt<0){throw _26;}
return(_24)(M);};

var _21=function(M){if(--M.cbt<0){throw _21;}
return(_19)(M);};

var _297=function(M){M.e.push(void(0),void(0));
M.e[M.e.length-1]=M.e[M.e.length-4];
M.e[M.e.length-2]=RT.checkedCdr(M, M.e[M.e.length-5]);
M.p=M.e[M.e.length-3];
M.a=2;
M.e.splice(M.e.length-5,3);
M.c[M.c.length-1].p=M.p;
return(_291)(M);};

var _295=function(M){M.e.push(void(0),void(0));
M.e[M.e.length-1]=M.e[M.e.length-4];
M.e[M.e.length-2]=(RT.testArgument(M,"caarpair",RT.isCaarPair,M.e[M.e.length-5],0,"caar")).first.first;
M.v=(M.e[M.e.length-1]===M.e[M.e.length-2]);
M.e.length-=2;
if(M.v===false){return(_297)(M);}else{M.v=RT.checkedCar(M, M.e[M.e.length-3]);
M.e.length-=3;
M.p=M.c[M.c.length-1].label;
M.c.pop();
return(M.p)(M);}};

var _291=function(M){if(--M.cbt<0){throw _291;}

//"lambda body for assq"

M.e.push(M.p.closedVals[0]);
M.v=(M.e[M.e.length-3]===RT.NULL);
if(M.v===false){return(_295)(M);}else{M.v=false;
M.e.length-=3;
M.p=M.c[M.c.length-1].label;
M.c.pop();
return(M.p)(M);}};

var _372=function(M){if(--M.cbt<0){throw _372;}
M.e.length-=(M.a-1);
return(_373)(M);};

var _375=function(M){M.p=M.c[M.c.length-1].pendingApplyValuesProc;
M.c.pop();
RT.checkClosureAndArity(M);
M.e.splice(M.e.length-(M.a+2),2);
M.c[M.c.length-1].p=M.p;
return((M.p).label)(M);};

var _370=function(M){if(--M.cbt<0){throw _370;}

//"lambda body for unknown"

M.c.push(new RT.Frame());
M.c[M.c.length-1].pendingApplyValuesProc=M.e[M.e.length-2];
M.p=M.e[M.e.length-1];
M.a=0;
RT.checkClosureAndArity(M);
M.c.push(new RT.CallFrame(_377,M.p));
return((M.p).label)(M);};

var _371=function(M){M.c.push(new RT.CallFrame(_373,M.p)); M.addPrompt(RT.DEFAULT_CONTINUATION_PROMPT_TAG,false,M.e.length);
M.e.push([]);M.e[M.e.length-1].names=[];
M.v=_370_c;
M.a=1;
M.v=M.v;
M.e.splice(M.e.length-((M.a-1)+1),1);
M.p=M.c[M.c.length-1].label;
M.c.pop();
return(M.p)(M);};

var _378=function(M){if(--M.cbt<0){throw _378;}
if(M.a===0){return(_375)(M);}else{M.e.push(M.v);
return(_375)(M);}};

var _70=function(M){if(--M.cbt<0){throw _70;}
M.e.length-=(M.a-1);
return(_71)(M);};

var _120=function(M){M.v=RT.makePair(M.e[M.e.length-1],M.e[M.e.length-2]);
M.e.length-=3;
M.p=M.c[M.c.length-1].label;
M.c.pop();
return(M.p)(M);};


var _118=function(M){M.e.push(void(0),void(0));
M.e.push(void(0));
M.e[M.e.length-1]=RT.checkedCar(M, M.e[M.e.length-4]);
M.e[M.e.length-2]=RT.checkedCdr(M, M.e[M.e.length-1]);
M.e.pop();
M.e.push(void(0));
M.e.push(M.e[M.e.length-4]);
M.v=M.primitives["unsafe-cdr"]._i(M);
M.e.pop();
M.e[M.e.length-1]=M.v;
M.p=_68_c;
M.a=1;
M.c.push(new RT.CallFrame(_122,M.p));
return(_68)(M);};

var _115=function(M){M.v=RT.makePair(M.e[M.e.length-1],M.e[M.e.length-2]);
M.e.length-=3;
M.p=M.c[M.c.length-1].label;
M.c.pop();
return(M.p)(M);};


var _113=function(M){M.e.push(void(0),void(0));
M.e.push(void(0));
M.e[M.e.length-1]=RT.checkedCar(M, M.e[M.e.length-4]);
M.e[M.e.length-2]=RT.checkedCar(M, M.e[M.e.length-1]);
M.e.pop();
M.e.push(void(0));
M.e.push(M.e[M.e.length-4]);
M.v=M.primitives["unsafe-cdr"]._i(M);
M.e.pop();
M.e[M.e.length-1]=M.v;
M.p=_67_c;
M.a=1;
M.c.push(new RT.CallFrame(_117,M.p));
return(_67)(M);};

var _108=function(M){M.e.push(void(0));
M.e.push(M.e[M.e.length-2]);
M.v=M.primitives["unsafe-cdr"]._i(M);
M.e.pop();
M.e[M.e.length-1]=M.v;
M.p=_66_c;
M.a=1;
M.e.splice(M.e.length-2,1);
M.c[M.c.length-1].p=M.p;
return(_66)(M);};

var _106=function(M){M.e.push(void(0));
M.e[M.e.length-1]=RT.checkedCar(M, M.e[M.e.length-2]);
M.v=(M.e[M.e.length-1]===RT.NULL);
M.e.pop();
if(M.v===false){return(_108)(M);}else{M.v=true;
M.e.pop();
M.p=M.c[M.c.length-1].label;
M.c.pop();
return(M.p)(M);}};

var _350=function(M){if(--M.cbt<0){throw _350;}
M.e.length-=(M.a-1);
return(_351)(M);};

var _99=function(M){M.e.push(void(0));
M.e[M.e.length-1]=RT.checkedCar(M, M.e[M.e.length-2]);
M.v=(M.e[M.e.length-1]===RT.NULL);
M.e.pop();
if(M.v===false){return(_101)(M);}else{M.e.push(void(0));
M.e.push(M.e[M.e.length-2]);
M.v=M.primitives["unsafe-cdr"]._i(M);
M.e.pop();
M.e[M.e.length-1]=M.v;
M.p=_65_c;
M.a=1;
M.e.splice(M.e.length-2,1);
M.c[M.c.length-1].p=M.p;
return(_65)(M);}};


var _365=function(M){M.e.push(void(0),void(0));
M.e[M.e.length-1]=RT.checkedCar(M, M.e[M.e.length-3]);
M.e.push(void(0),void(0));
M.e.push(M.e[M.e.length-5]);
M.v=M.primitives["unsafe-cdr"]._i(M);
M.e.pop();
M.e[M.e.length-1]=M.v;
M.e[M.e.length-2]=M.e[M.e.length-6];
M.p=_348_c;
M.a=2;
M.c.push(new RT.CallFrame(_369,M.p));
return(_348)(M);};


var _68=function(M){if(--M.cbt<0){throw _68;}

//"lambda body for rest-lists"

M.v=(M.e[M.e.length-1]===RT.NULL);
if(M.v===false){return(_118)(M);}else{M.v=RT.NULL;
M.e.pop();
M.p=M.c[M.c.length-1].label;
M.c.pop();
return(M.p)(M);}};

var _91=function(M){if(--M.cbt<0){throw _91;}
M.e.length-=(M.a-1);
return(_92)(M);};


var _357=function(M){M.e.push(void(0),void(0));
M.e.push(M.e[M.e.length-3]);
M.v=M.primitives["unsafe-car"]._i(M);
M.e.pop();
M.e[M.e.length-1]=M.v;
M.e.push(void(0));
M.e.push(M.e[M.e.length-4]);
M.v=M.primitives["unsafe-cdr"]._i(M);
M.e.pop();
M.e[M.e.length-1]=M.v;
M.p=_347_c;
M.a=1;
M.c.push(new RT.CallFrame(_361,M.p));
return(_347)(M);};

var _355=function(M){M.e.push(void(0));
M.e[M.e.length-1]=RT.checkedCdr(M, M.e[M.e.length-2]);
M.v=(M.e[M.e.length-1]===RT.NULL);
M.e.pop();
if(M.v===false){return(_357)(M);}else{M.e.push(M.e[M.e.length-1]);
M.v=M.primitives["unsafe-car"]._i(M);
M.e.length-=2;
M.p=M.c[M.c.length-1].label;
M.c.pop();
return(M.p)(M);}};

var _83=function(M){M.e.push(void(0),void(0));
M.e[M.e.length-2]=M.primitives["apply"];
M.e[M.e.length-1]=M.e[M.e.length-3];
M.e.push(M.e[M.e.length-4]);
M.p=_67_c;
M.a=1;
M.c.push(new RT.CallFrame(_90,M.p));
return(_67)(M);};

var _85=function(M){if(M.v===false){return(_83)(M);}else{M.v=M.e[M.e.length-2];
M.v=M.e[M.e.length-1];
M.e.push(void(0),void(0));
M.e[M.e.length-1]=sym76;
M.e[M.e.length-2]="all lists must have the same size";
M.a=2;
M.v=M.primitives["error"]._i(M);
M.e.length-=4;
M.p=M.c[M.c.length-1].label;
M.c.pop();
return(M.p)(M);}};


var _66=function(M){if(--M.cbt<0){throw _66;}

//"lambda body for some-empty?"

M.v=(M.e[M.e.length-1]===RT.NULL);
if(M.v===false){return(_106)(M);}else{M.v=false;
M.e.pop();
M.p=M.c[M.c.length-1].label;
M.c.pop();
return(M.p)(M);}};

var _78=function(M){M.e.push(M.e[M.e.length-2]);
M.p=_66_c;
M.a=1;
M.c.push(new RT.CallFrame(_87,M.p));
return(_66)(M);};

var _80=function(M){if(M.v===false){return(_78)(M);}else{M.v=RT.NULL;
M.e.length-=2;
M.p=M.c[M.c.length-1].label;
M.c.pop();
return(M.p)(M);}};


var _65=function(M){if(--M.cbt<0){throw _65;}

//"lambda body for all-empty?"

M.v=(M.e[M.e.length-1]===RT.NULL);
if(M.v===false){return(_99)(M);}else{M.v=true;
M.e.pop();
M.p=M.c[M.c.length-1].label;
M.c.pop();
return(M.p)(M);}};

var _64=function(M){if(--M.cbt<0){throw _64;}

//"lambda body for loop"

M.e.push(M.e[M.e.length-2]);
M.p=_65_c;
M.a=1;
M.c.push(new RT.CallFrame(_82,M.p));
return(_65)(M);};

var _63=function(M){if(--M.cbt<0){throw _63;}

//"lambda body for do-it"

M.e.push(void(0),void(0));
M.e[M.e.length-1]=M.e[M.e.length-3];
M.e[M.e.length-2]=M.e[M.e.length-4];
M.p=_64_c;
M.a=2;
M.e.splice(M.e.length-4,2);
M.c[M.c.length-1].p=M.p;
return(_64)(M);};

var _62=function(M){if(--M.cbt<0){throw _62;}

//"lambda body for unknown"

M.unspliceRestFromStack(1,(M.a-1));
M.e.push(void(0),void(0));
M.e[M.e.length-1]=M.e[M.e.length-3];
M.e[M.e.length-2]=M.e[M.e.length-4];
M.p=_63_c;
M.a=2;
M.e.splice(M.e.length-4,2);
M.c[M.c.length-1].p=M.p;
return(_63)(M);};

var _69=function(M){M.c.push(new RT.CallFrame(_71,M.p)); M.addPrompt(RT.DEFAULT_CONTINUATION_PROMPT_TAG,false,M.e.length);
M.e.push([]);M.e[M.e.length-1].names=[];
M.v=_62_c;
M.a=1;
M.v=M.v;
M.e.splice(M.e.length-((M.a-1)+1),1);
M.p=M.c[M.c.length-1].label;
M.c.pop();
return(M.p)(M);};

var _277=function(M){if(--M.cbt<0){throw _277;}
M.e.length-=(M.a-1);
return(_278)(M);};

var _281=function(M){M.e.push(void(0),void(0),void(0));
M.e[M.e.length-1]=M.e[M.e.length-5];
M.e[M.e.length-2]=M.e[M.e.length-6];
M.e[M.e.length-3]=RT.checkedCdr(M, M.e[M.e.length-7]);
M.p=M.e[M.e.length-4];
M.a=3;
M.e.splice(M.e.length-7,4);
M.c[M.c.length-1].p=M.p;
return(_275)(M);};


var _279=function(M){M.e.push(void(0));
M.p=M.e[M.e.length-4];
M.e[M.e.length-1]=M.e[M.e.length-3];
M.a=1;
RT.checkClosureAndArity(M);
M.c.push(new RT.CallFrame(_284,M.p));
return((M.p).label)(M);};

var _275=function(M){if(--M.cbt<0){throw _275;}

//"lambda body for memf"

M.e.push(M.p.closedVals[0]);
M.v=(M.e[M.e.length-4]===RT.NULL);
if(M.v===false){return(_279)(M);}else{M.v=false;
M.e.length-=4;
M.p=M.c[M.c.length-1].label;
M.c.pop();
return(M.p)(M);}};

var _276=function(M){M.c.push(new RT.CallFrame(_278,M.p)); M.addPrompt(RT.DEFAULT_CONTINUATION_PROMPT_TAG,false,M.e.length);
M.e.push([]);M.e[M.e.length-1].names=[];
M.e.push(void(0));
M.e[M.e.length-1]=new RT.Closure(_275,3,void(0),"memf");
M.e[M.e.length-1].closedVals=[M.e[M.e.length-1]];
M.v=M.e[M.e.length-1];
M.a=1;
M.e.splice(M.e.length-((M.a-1)+1),1);
M.v=M.v;
M.e.splice(M.e.length-((M.a-1)+1),1);
M.p=M.c[M.c.length-1].label;
M.c.pop();
return(M.p)(M);};

var _336=function(M){if(--M.cbt<0){throw _336;}
M.e.length-=(M.a-1);
return(_337)(M);};

var _140=function(M){M.e.push(void(0),void(0));
if (M.globals["and"]===void(0)&&M.params.currentNamespace.get("and")===void(0)){ RT.raiseUnboundToplevelError(M,"and"); }
M.e[M.e.length-2]=(M.globals["and"]!==void(0)?M.globals["and"]:M.params.currentNamespace.get("and"));
M.e.push(void(0),void(0));
M.e[M.e.length-2]=M.primitives["apply"];
M.e[M.e.length-1]=M.e[M.e.length-6];
M.e.push(M.e[M.e.length-7]);
M.p=_127_c;
M.a=1;
M.c.push(new RT.CallFrame(_147,M.p));
return(_127)(M);};

var _334=function(M){if(--M.cbt<0){throw _334;}

//"lambda body for length-iter"

M.v=(M.e[M.e.length-1]===RT.NULL);
if(M.v===false){return(_341)(M);}else{M.v=M.e[M.e.length-2];
M.e.length-=2;
M.p=M.c[M.c.length-1].label;
M.c.pop();
return(M.p)(M);}};

var _333=function(M){if(--M.cbt<0){throw _333;}

//"lambda body for unknown"

M.e.push(void(0),void(0));
M.e[M.e.length-1]=M.e[M.e.length-3];
M.e[M.e.length-2]=0;
M.p=_334_c;
M.a=2;
M.e.splice(M.e.length-3,1);
M.c[M.c.length-1].p=M.p;
return(_334)(M);};

var _335=function(M){M.c.push(new RT.CallFrame(_337,M.p)); M.addPrompt(RT.DEFAULT_CONTINUATION_PROMPT_TAG,false,M.e.length);
M.e.push([]);M.e[M.e.length-1].names=[];
M.v=_333_c;
M.a=1;
M.v=M.v;
M.e.splice(M.e.length-((M.a-1)+1),1);
M.p=M.c[M.c.length-1].label;
M.c.pop();
return(M.p)(M);};

var _263=function(M){if(--M.cbt<0){throw _263;}
M.e.length-=(M.a-1);
return(_264)(M);};

var _267=function(M){M.e.push(void(0),void(0));
M.e[M.e.length-1]=M.e[M.e.length-4];
M.e.push(M.e[M.e.length-5]);
M.v=M.primitives["unsafe-cdr"]._i(M);
M.e.pop();
M.e[M.e.length-2]=M.v;
M.p=M.e[M.e.length-3];
M.a=2;
M.e.splice(M.e.length-5,3);
M.c[M.c.length-1].p=M.p;
return(_261)(M);};

var _265=function(M){M.e.push(void(0),void(0));
M.e[M.e.length-1]=M.e[M.e.length-4];
M.e[M.e.length-2]=RT.checkedCar(M, M.e[M.e.length-5]);
M.v=M.primitives["eqv?"]._i(M);
M.e.length-=2;
if(M.v===false){return(_267)(M);}else{M.v=M.e[M.e.length-3];
M.e.length-=3;
M.p=M.c[M.c.length-1].label;
M.c.pop();
return(M.p)(M);}};

var _261=function(M){if(--M.cbt<0){throw _261;}

//"lambda body for memv"

M.e.push(M.p.closedVals[0]);
M.v=(M.e[M.e.length-3]===RT.NULL);
if(M.v===false){return(_265)(M);}else{M.v=false;
M.e.length-=3;
M.p=M.c[M.c.length-1].label;
M.c.pop();
return(M.p)(M);}};

var _262=function(M){M.c.push(new RT.CallFrame(_264,M.p)); M.addPrompt(RT.DEFAULT_CONTINUATION_PROMPT_TAG,false,M.e.length);
M.e.push([]);M.e[M.e.length-1].names=[];
M.e.push(void(0));
M.e[M.e.length-1]=new RT.Closure(_261,2,void(0),"memv");
M.e[M.e.length-1].closedVals=[M.e[M.e.length-1]];
M.v=M.e[M.e.length-1];
M.a=1;
M.e.splice(M.e.length-((M.a-1)+1),1);
M.v=M.v;
M.e.splice(M.e.length-((M.a-1)+1),1);
M.p=M.c[M.c.length-1].label;
M.c.pop();
return(M.p)(M);};

var _131=function(M){if(--M.cbt<0){throw _131;}
M.e.length-=(M.a-1);
return(_132)(M);};

var _129=function(M){if(--M.cbt<0){throw _129;}

//"lambda body for unknown"

M.unspliceRestFromStack(1,(M.a-1));
M.e.push(M.p.closedVals[0]);
M.e.push(void(0),void(0));
//"Constant toplevel ref: ?"

M.p=M.e[M.e.length-3][2];
M.e[M.e.length-1]=M.e[M.e.length-4];
M.e[M.e.length-2]=M.e[M.e.length-5];
M.a=2;
RT.checkClosureAndArity(M);
M.e.splice(M.e.length-5,3);
M.c[M.c.length-1].p=M.p;
return((M.p).label)(M);};

var _178=function(M){M.v=RT.makePair(M.e[M.e.length-1],M.e[M.e.length-2]);
M.e.length-=3;
M.p=M.c[M.c.length-1].label;
M.c.pop();
return(M.p)(M);};


var _176=function(M){M.e.push(void(0),void(0));
M.e.push(void(0));
M.e[M.e.length-1]=RT.checkedCar(M, M.e[M.e.length-4]);
M.e[M.e.length-2]=RT.checkedCdr(M, M.e[M.e.length-1]);
M.e.pop();
M.e.push(void(0));
M.e.push(M.e[M.e.length-4]);
M.v=M.primitives["unsafe-cdr"]._i(M);
M.e.pop();
M.e[M.e.length-1]=M.v;
M.p=_128_c;
M.a=1;
M.c.push(new RT.CallFrame(_180,M.p));
return(_128)(M);};

var _173=function(M){M.v=RT.makePair(M.e[M.e.length-1],M.e[M.e.length-2]);
M.e.length-=3;
M.p=M.c[M.c.length-1].label;
M.c.pop();
return(M.p)(M);};


var _171=function(M){M.e.push(void(0),void(0));
M.e.push(void(0));
M.e[M.e.length-1]=RT.checkedCar(M, M.e[M.e.length-4]);
M.e[M.e.length-2]=RT.checkedCar(M, M.e[M.e.length-1]);
M.e.pop();
M.e.push(void(0));
M.e.push(M.e[M.e.length-4]);
M.v=M.primitives["unsafe-cdr"]._i(M);
M.e.pop();
M.e[M.e.length-1]=M.v;
M.p=_127_c;
M.a=1;
M.c.push(new RT.CallFrame(_175,M.p));
return(_127)(M);};

var _166=function(M){M.e.push(void(0));
M.e.push(M.e[M.e.length-2]);
M.v=M.primitives["unsafe-cdr"]._i(M);
M.e.pop();
M.e[M.e.length-1]=M.v;
M.p=_126_c;
M.a=1;
M.e.splice(M.e.length-2,1);
M.c[M.c.length-1].p=M.p;
return(_126)(M);};

var _164=function(M){M.e.push(void(0));
M.e[M.e.length-1]=RT.checkedCar(M, M.e[M.e.length-2]);
M.v=(M.e[M.e.length-1]===RT.NULL);
M.e.pop();
if(M.v===false){return(_166)(M);}else{M.v=true;
M.e.pop();
M.p=M.c[M.c.length-1].label;
M.c.pop();
return(M.p)(M);}};

var _159=function(M){M.v=false;
M.e.pop();
M.p=M.c[M.c.length-1].label;
M.c.pop();
return(M.p)(M);};

var _157=function(M){M.e.push(void(0));
M.e[M.e.length-1]=RT.checkedCar(M, M.e[M.e.length-2]);
M.v=(M.e[M.e.length-1]===RT.NULL);
M.e.pop();
if(M.v===false){return(_159)(M);}else{M.e.push(void(0));
M.e.push(M.e[M.e.length-2]);
M.v=M.primitives["unsafe-cdr"]._i(M);
M.e.pop();
M.e[M.e.length-1]=M.v;
M.p=_125_c;
M.a=1;
M.e.splice(M.e.length-2,1);
M.c[M.c.length-1].p=M.p;
return(_125)(M);}};


var _150=function(M){M.p=M.e[M.e.length-2];
M.e[M.e.length-2]=M.v;
M.a=2;
RT.checkClosureAndArity(M);
M.c.push(new RT.CallFrame(_154,M.p));
return((M.p).label)(M);};


var _128=function(M){if(--M.cbt<0){throw _128;}

//"lambda body for rest-lists"

M.v=(M.e[M.e.length-1]===RT.NULL);
if(M.v===false){return(_176)(M);}else{M.v=RT.NULL;
M.e.pop();
M.p=M.c[M.c.length-1].label;
M.c.pop();
return(M.p)(M);}};


var _145=function(M){M.p=M.e[M.e.length-2];
M.e[M.e.length-2]=M.v;
M.a=2;
RT.checkClosureAndArity(M);
M.c.push(new RT.CallFrame(_149,M.p));
return((M.p).label)(M);};


var _127=function(M){if(--M.cbt<0){throw _127;}

//"lambda body for first-tuple"

M.v=(M.e[M.e.length-1]===RT.NULL);
if(M.v===false){return(_171)(M);}else{M.v=RT.NULL;
M.e.pop();
M.p=M.c[M.c.length-1].label;
M.c.pop();
return(M.p)(M);}};

var _341=function(M){M.e.push(void(0),void(0));
M.e[M.e.length-1]=RT.checkedCdr(M, M.e[M.e.length-3]);
M.e[M.e.length-2]=RT.checkedAdd1(M, M.e[M.e.length-4]);
M.p=_334_c;
M.a=2;
M.e.splice(M.e.length-4,2);
M.c[M.c.length-1].p=M.p;
return(_334)(M);};

var _142=function(M){if(M.v===false){return(_140)(M);}else{M.e.push(void(0),void(0));
M.e[M.e.length-1]=sym77;
M.e[M.e.length-2]="all lists must have the same size";
M.a=2;
M.v=M.primitives["error"]._i(M);
M.e.length-=5;
M.p=M.c[M.c.length-1].label;
M.c.pop();
return(M.p)(M);}};


var _126=function(M){if(--M.cbt<0){throw _126;}

//"lambda body for some-empty?"

M.v=(M.e[M.e.length-1]===RT.NULL);
if(M.v===false){return(_164)(M);}else{M.v=false;
M.e.pop();
M.p=M.c[M.c.length-1].label;
M.c.pop();
return(M.p)(M);}};

var _135=function(M){M.e.push(M.e[M.e.length-3]);
M.p=_126_c;
M.a=1;
M.c.push(new RT.CallFrame(_144,M.p));
return(_126)(M);};

var _137=function(M){if(M.v===false){return(_135)(M);}else{M.v=RT.NULL;
M.e.length-=3;
M.p=M.c[M.c.length-1].label;
M.c.pop();
return(M.p)(M);}};


var _125=function(M){if(--M.cbt<0){throw _125;}

//"lambda body for all-empty?"

M.v=(M.e[M.e.length-1]===RT.NULL);
if(M.v===false){return(_157)(M);}else{M.v=true;
M.e.pop();
M.p=M.c[M.c.length-1].label;
M.c.pop();
return(M.p)(M);}};

var _124=function(M){if(--M.cbt<0){throw _124;}

//"lambda body for loop"

M.e.push(M.p.closedVals[0]);
M.e.push(M.e[M.e.length-3]);
M.p=_125_c;
M.a=1;
M.c.push(new RT.CallFrame(_139,M.p));
return(_125)(M);};

var _123=function(M){if(--M.cbt<0){throw _123;}

//"lambda body for do-it"

M.e.push(M.p.closedVals[0]);
M.e.push(void(0),void(0));
//"Constant toplevel ref: ?"

M.p=M.e[M.e.length-3][1];
M.e[M.e.length-1]=M.e[M.e.length-4];
M.e[M.e.length-2]=M.e[M.e.length-5];
M.a=2;
RT.checkClosureAndArity(M);
M.e.splice(M.e.length-5,3);
M.c[M.c.length-1].p=M.p;
return((M.p).label)(M);};

var _130=function(M){M.c.push(new RT.CallFrame(_132,M.p)); M.addPrompt(RT.DEFAULT_CONTINUATION_PROMPT_TAG,false,M.e.length);
M.e.push([M.globals["and"]!==void(0)?M.globals["and"]:M.params.currentNamespace.get("and"),false,false]);M.e[M.e.length-1].names=["and",false,false];
M.v=new RT.Closure(_123,2,[M.e[M.e.length-1]],"do-it");
M.e[M.e.length-1][2]=M.v;
M.v=RT.VOID;
M.v=new RT.Closure(_124,2,[M.e[M.e.length-1]],"loop");
M.e[M.e.length-1][1]=M.v;
M.v=RT.VOID;
M.v=new RT.Closure(_129,(RT.makeArityAtLeast(1)),[M.e[M.e.length-1]],"unknown");
M.a=1;
M.v=M.v;
M.e.splice(M.e.length-((M.a-1)+1),1);
M.p=M.c[M.c.length-1].label;
M.c.pop();
return(M.p)(M);};

var _249=function(M){if(--M.cbt<0){throw _249;}
M.e.length-=(M.a-1);
return(_250)(M);};

var _253=function(M){M.e.push(void(0),void(0));
M.e[M.e.length-1]=M.e[M.e.length-4];
M.e.push(M.e[M.e.length-5]);
M.v=M.primitives["unsafe-cdr"]._i(M);
M.e.pop();
M.e[M.e.length-2]=M.v;
M.p=M.e[M.e.length-3];
M.a=2;
M.e.splice(M.e.length-5,3);
M.c[M.c.length-1].p=M.p;
return(_247)(M);};

var _251=function(M){M.e.push(void(0),void(0));
M.e[M.e.length-1]=M.e[M.e.length-4];
M.e[M.e.length-2]=RT.checkedCar(M, M.e[M.e.length-5]);
M.v=(M.e[M.e.length-1]===M.e[M.e.length-2]);
M.e.length-=2;
if(M.v===false){return(_253)(M);}else{M.v=M.e[M.e.length-3];
M.e.length-=3;
M.p=M.c[M.c.length-1].label;
M.c.pop();
return(M.p)(M);}};

_10.mvr=_9;
_92.mvr=_91;
_71.mvr=_70;
_132.mvr=_131;
_192.mvr=_191;
_246.mvr=_245;
_250.mvr=_249;
_264.mvr=_263;
_278.mvr=_277;
_294.mvr=_293;
_308.mvr=_307;
_322.mvr=_321;
_337.mvr=_336;
_351.mvr=_350;
_377.mvr=_378;
_373.mvr=_372;
var sym74=RT.makeSymbol("map");
var sym75=RT.makeSymbol("ormap");
var sym77=RT.makeSymbol("andmap");
var sym76=RT.makeSymbol("for-each");var _188_c=new RT.Closure(_188,1,void(0),"rest-lists");
var _187_c=new RT.Closure(_187,1,void(0),"first-tuple");
var _186_c=new RT.Closure(_186,1,void(0),"some-empty?");
var _128_c=new RT.Closure(_128,1,void(0),"rest-lists");
var _185_c=new RT.Closure(_185,1,void(0),"all-empty?");
var _127_c=new RT.Closure(_127,1,void(0),"first-tuple");
var _334_c=new RT.Closure(_334,2,void(0),"length-iter");
var _333_c=new RT.Closure(_333,1,void(0),"unknown");
var _243_c=new RT.Closure(_243,1,void(0),"unknown");
var _348_c=new RT.Closure(_348,2,void(0),"append-2");
var _125_c=new RT.Closure(_125,1,void(0),"all-empty?");
var _68_c=new RT.Closure(_68,1,void(0),"rest-lists");
var _6_c=new RT.Closure(_6,1,void(0),"first-tuple");
var _7_c=new RT.Closure(_7,1,void(0),"rest-lists");
var _346_c=new RT.Closure(_346,(RT.makeArityAtLeast(0)),void(0),"unknown");
var _5_c=new RT.Closure(_5,1,void(0),"some-empty?");
var _347_c=new RT.Closure(_347,1,void(0),"append-many");
var _67_c=new RT.Closure(_67,1,void(0),"first-tuple");
var _66_c=new RT.Closure(_66,1,void(0),"some-empty?");
var _4_c=new RT.Closure(_4,1,void(0),"all-empty?");
var _64_c=new RT.Closure(_64,2,void(0),"loop");
var _65_c=new RT.Closure(_65,1,void(0),"all-empty?");
var _2_c=new RT.Closure(_2,2,void(0),"do-it");
var _389_c=new RT.Closure(_389,(RT.makeArityAtLeast(2)),void(0),"apply");
var _126_c=new RT.Closure(_126,1,void(0),"some-empty?");
var _385_c=new RT.Closure(_385,(RT.makeArityAtLeast(0)),void(0),"values");
var _1_c=new RT.Closure(_1,(RT.makeArityAtLeast(1)),void(0),"unknown");
var _63_c=new RT.Closure(_63,2,void(0),"do-it");
var _3_c=new RT.Closure(_3,2,void(0),"loop");
var _callCCEntry_c=new RT.Closure(_callCCEntry,1,void(0),"call/cc");
var _370_c=new RT.Closure(_370,2,void(0),"unknown");
var _62_c=new RT.Closure(_62,(RT.makeArityAtLeast(1)),void(0),"unknown");M.params.currentErrorHandler = fail;
for (param in params) {
    if (Object.hasOwnProperty.call(params, param)) {
        M.params[param] = params[param];
    }
}M.c.push(new RT.CallFrame(function(M){ setTimeout(success, 0); },M.p));
M.trampoline(_392, true); })(M, function() { SUCCESS(); }, FAIL, PARAMS);})(plt.runtime.currentMachine,
function(){ plt.runtime.setReadyTrue(); },
function(){},
{});
