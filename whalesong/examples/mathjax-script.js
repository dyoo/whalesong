<script type="text/javascript" src="http://cdn.mathjax.org/mathjax/latest/MathJax.js">
  MathJax.Hub.Config({
    extensions: ["tex2jax.js"],
    jax: ["input/TeX", "output/HTML-CSS"],
    tex2jax: {
      inlineMath: [ ['$','$'], ["\\(","\\)"] ],
      displayMath: [ ['$$','$$'], ["\\[","\\]"] ],
      processEscapes: true
    },
    "HTML-CSS": { availableFonts: ["TeX"] }
  });
</script>
<script>
var UpdateMath = function () {
                     MathJax.Hub.Queue(["Typeset",MathJax.Hub]); 
                     window.setTimeout(UpdateMath, 1000*1);                                      
                     return true;
                 };
window.setTimeout(UpdateMath, 1000*1);
</script>

