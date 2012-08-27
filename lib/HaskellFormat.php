<?php

// -----------------------------------------------------------------------------
// A generic lexer
// -----------------------------------------------------------------------------

class Lexer {
	private $rules;
	private $str,$offset,$len;
	
	function __construct($rules,$str) {
		$this->rules = $rules;
		$this->str = $str;
		$this->len = strlen($str);
	}
	
	function next() {
		foreach ($this->rules as $name => $rule) {
			if (preg_match($rule,$this->str,$ma,PREG_OFFSET_CAPTURE,$this->offset) && $ma[0][1] == $this->offset) {
				$this->offset += strlen($ma[0][0]);
				return array($name,$ma[0][0]);
			}
		}
		// failed to match any
		return array('',$this->str[$this->offset++]);
	}
	function end() {
		return $this->offset >= $this->len;
	}
}

// -----------------------------------------------------------------------------
// Haskell lexer and syntax highlighter
// -----------------------------------------------------------------------------

class HaskellFormat {
	public static function format($code) {
		// Haskell lexer goes here
		global $haskell_lang;
		$lex = new Lexer($haskell_lang, $code);
		return HaskellFormat::do_format($lex);
    }
    protected function do_format($lex) {
		$out = '';
		while (!$lex->end()) {
			list($type,$match) = $lex->next();
			$is_html = false;
			if ($type == '!!!notation') {
				list($type,$next_match) = $lex->next();
				$match = substr($match,5,-5);
				$is_html = true;
			}
			if ($type == '') {
				$out .= htmlspecialchars($match);
			} elseif ($type == '!!!') {
				$out .= substr($match,3,-3);
			} else {
				if (!$is_html) $match = htmlspecialchars($match);
				if ($type == 'keyword') {
					$match = preg_replace('@^__keyword__@','',$match);
				}
				if ($type == 'varid' || $type == 'varop' || $type == 'conid' || $type == 'comment') {
					$match = preg_replace('@__([[:alnum:]_]+)@','<sub>\\1</sub>',$match);
					$match = preg_replace('@__[{]([^}]*)[}]@','<sub>\\1</sub>',$match);
					$match = preg_replace('@!!!(.*?)!!!@e','htmlspecialchars_decode("\\1")',$match);
				}
				$out .= "<span class=\"$type\">$match</span>";
			}
		}
		return $out;
	}
}

// rules for the haskell lexer
global $haskell_lang;
$haskell_lang = array(
	'!!!' =>      '@!!!.*?!!!@',
	'!!!notation' => '@{-!!!.*!!!-}@',
	'input' =>    '@^(?:[*]?([[:alnum:]]|λ)+>|<[a-zA-Z]+> *>?|> )@i',
	'pragma' =>   '@{-# *[[:upper:]]+ .*?#-}@',
	'comment' =>  '@--.*|{-.*-}@',
	'keyword' =>  '@\b(if|then|else|module|import|qualified|hiding|where|let|in|case|of|newtype|default|infix|infixr|infixl|(?:data|type)(?: family)?|class|instance|forall|exists|deriving|do|__keyword__[[:alnum:]]+)\b@',
	'str' =>      "@\"(?:[^\"\\\\]|\\\\.)*?\"@",
	'chr' =>      "@\'(?:[^\'\\\\]|\\\\.+?)\'@",
	'num' =>      '@-?\b[0-9]+@',
	'listcon' =>  '@[\\[\\]]|\(:\)|(:|\\.\\.)(?=[^-:!\\@#$%^*.|=+<>&~/\\\\])|\\|\\]@',
	'keyglyph' => '@[\\[\\]]|(?:=>|=|->|::|\\\\|<-|\|)(?=\\s|[a-zA-Z(\\@_])@',
	'conop' =>    "@`(?:[[:upper:]][[:alnum:]_']*[.])*[[:upper:]][[:alnum:]_']*`|:([-:\\@#$%^*.|=+<>&~/\\\\]|!!?(?!!))*@u",
	'varop' =>    "@`(?:[[:upper:]][[:alnum:]_']*[.])*[[:lower:]_][[:alnum:]_']*`|([-:\\@#$%^*.|=+<>&~/\\\\\']|!!?(?!!))+(__[{][^}]*[}])?@u",
	'conid' =>    "@\\b(?:[[:upper:]][[:alnum:]_']*[.])*[[:upper:]][[:alnum:]_']*@u",
	'varid' =>    "@\\b(?:[[:upper:]][[:alnum:]_']*[.])*[[:lower:]_][[:alnum:]_']*@u",
);

