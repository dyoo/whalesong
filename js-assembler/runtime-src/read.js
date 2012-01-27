var readSchemeExpressions;


function tokenize(s) {

  function replaceEscapes(s) {
    return s.replace(/\\./g, function(match, submatch, index) {
      // FIXME: add more escape sequences.
      if (match == '\\n') {
	return "\n";
      }
      else {
	return match.substring(1);
      }
      });
    }



  var tokens = [];

  var PATTERNS = [['whitespace' , /^(\s+)/],
		  ['comment' , /(^;[^\n]*)/],
		  ['(' , /^(\(|\[)/],
		  [')' , /^(\)|\])/],
	          ['\'' , /^(\')/],
		  ['`' , /^(`)/],
		  [',' , /^(,)/],
		  ['char', /^\#\\(newline)/],
                  ['char', /^\#\\(.)/],
		  ['number' , /^([+\-]?(?:\d+\.\d+|\d+\.|\.\d+|\d+))/],
		  ['string' , /^"((?:([^\\"]|(\\.)))*)"/],      // comment (emacs getting confused with quote): " 
		  ['symbol' ,/^([a-zA-Z\:\+\=\~\_\?\!\@\#\$\%\^\&\*\-\/\.\>\<][\w\:\+\=\~\_\?\!\@\#\$\%\^\&\*\-\/\.\>\<]*)/]
		 ];

  while (true) {
    var shouldContinue = false;
    for (var i = 0; i < PATTERNS.length; i++) {
      var patternName = PATTERNS[i][0];
      var pattern = PATTERNS[i][1]
      var result = s.match(pattern);
      if (result != null) {
	if (patternName == 'string') {
	  result[1] = replaceEscapes(result[1]);
        }
	if (patternName != 'whitespace' && patternName != 'comment') {
	  tokens.push([patternName, result[1]]);
	}
	s = s.substring(result[0].length);
	shouldContinue = true;
      }
    }
    if (! shouldContinue) {
      break;
    }
  }
  return [tokens, s];
}




(function(){


  readSchemeExpressions = function(s) {
    var tokensAndError = tokenize(s);
    var tokens = tokensAndError[0];
    if (tokensAndError[1].length > 0) {
	throw new Error("Error while tokenizing: the rest of the stream is: " + tokensAndError[1]);
    }

    var quoteSymbol = plt.types.Symbol.makeInstance("quote");
    var quasiquoteSymbol = plt.types.Symbol.makeInstance("quasiquote");
    var unquoteSymbol = plt.types.Symbol.makeInstance("unquote");
    var empty = plt.types.Empty.EMPTY;

    function isType(type) {
      return (tokens.length > 0 && tokens[0][0] == type);
    }
    
    function eat(expectedType) {
      if (tokens.length == 0)
	throw new Error("token stream exhausted while trying to eat " +
			expectedType);
      var t = tokens.shift();
      if (t[0] == expectedType) {
	return t;
      } else {
	throw new Error("Unexpected token " + t);
      }
    }



    function readExpr() {
      var t;
      if (isType('(')) {
	eat('(');
	var result = readExprs();
	eat(')');
	return result;
      } else if (isType("'")) {
	eat("'");
	var quoted = readExpr();
	return plt.Kernel.cons(quoteSymbol,
				   plt.Kernel.cons(quoted, empty));
      } else if (isType('`')) {
	eat("`");
	return plt.Kernel.cons(quasiquoteSymbol,
				   plt.kernel.cons(quoted, empty));
      } else if (isType(',')) {
	eat(",");
	return plt.Kernel.cons(unquoteSymbol,
				   plt.kernel.cons(quoted, empty));
      } else if (isType('number')) {
	t = eat('number');
	if (t[1].match(/\./)) {
	  return plt.types.FloatPoint.makeInstance(parseFloat(t[1]));
	} else {
	  return plt.types.Rational.makeInstance(parseInt(t[1]), 1);
	}
      } else if (isType('string')) {
	t = eat('string');
	return plt.types.String.makeInstance(t[1]);
      } else if (isType('char')) {
        t = eat('char');
	  if (t[1] == 'newline') {
	      return plt.types.Char.makeInstance('\n');
	  }
          else {
	      return plt.types.Char.makeInstance(t[1]);
	  }
      } else if (isType('symbol')) {
	t = eat('symbol');
	return plt.types.Symbol.makeInstance(t[1]);
      } else {
	throw new Error("Parse broke with token stream " + tokens);
      }
    }


    function readExprs() {
      var result = plt.types.Empty.EMPTY;
      while (true) {
	if (tokens.length == 0 || isType(')')) {
	  break;
	} else {
	  var nextElt = readExpr();
	  result = plt.types.Cons.makeInstance(nextElt, result);
	}
      }
      return plt.Kernel.reverse(result);
    }
    


    return readExprs();
  }
  
}());
