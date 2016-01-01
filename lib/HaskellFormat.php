<?php

// -----------------------------------------------------------------------------
// Haskell lexer and syntax highlighter
// -----------------------------------------------------------------------------

class HaskellFormat {
    private $lang;
    public function __construct($lang = null) {
        if ($lang === null) {
		    global $haskell_lang;
	        $this->lang = $haskell_lang;
	    } else {
            $this->lang = $lang;
        }
    }
	public function format($code) {
		$lex = new Lexer($this->lang, $code);
		return HaskellFormat::do_format($lex);
    }
    protected static function do_format($lex) {
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
			} elseif ($type == 'hole') {
				$match = '{' . substr($match,2,-2) . '}';
				$out .= "<span class=\"$type\">$match</span>";
		  } elseif ($type == 'sub') {
				if (!$is_html) $match = htmlspecialchars($match);
			  $match = preg_replace('@[\^_]@u','',$match);
			  $out .= "<sub>$match</sub>";
		  } elseif ($type == 'sup') {
				if (!$is_html) $match = htmlspecialchars($match);
			  $match = preg_replace('@[\^_]@u','',$match);
			  $out .= "<sup>$match</sup>";
			} else {
				if (!$is_html) $match = htmlspecialchars($match);
				if ($type == 'keyword') {
					$match = preg_replace('@^__keyword__@','',$match);
				}
				if ($type == 'varid' || $type == 'varop' || $type == 'conid' || $type == 'comment') {
					$match = preg_replace('@__([[:alnum:]_]+)@u','<sub>\\1</sub>',$match);
					$match = preg_replace('@__[{]([^}]*)[}]@u','<sub>\\1</sub>',$match);
					$match = preg_replace_callback('@!!!(.*?)!!!@u',function($ma){return htmlspecialchars_decode($ma[1]);},$match);
				}
				$out .= "<span class=\"$type\">$match</span>";
			}
		}
		return $out;
	}
	public function modify_rule($which, $change) {
        // "@x@y" becomes "@x change @y"
	    $rule = $this->lang[$which];
	    $after = strrpos($rule, $rule[0],1);
	    $rule = substr($rule,0,$after) . $change . substr($rule,$after);
	    $this->lang[$which] = $rule;
    }
}

// rules for the haskell lexer
global $haskell_lang;
$haskell_lang = array(
	'!!!' =>      '@!!!.*?!!!@',
	'!!!notation' => '@{-!!!.*!!!-}@',
  'sub' => "@(?!)@u", // subscript (disabled)
  'sup' => "@(?!)@u",
	'input' =>    '@^(?:[*]?([[:alnum:]]|λ)+>|<[a-zA-Z]+> *>?|> )@iu',
	'pragma' =>   '@{-# *[[:upper:]]+ .*?#-}@u',
	'comment' =>  '@--.*|{-.*-}@u',
	'keyword' =>  '@\b(if|then|else|module|import|qualified|hiding|where|let|in|case|of|newtype|default|infix|infixr|infixl|(?:data|type)(?: family)?|class|instance|forall|exists|deriving|do|foreign|prim|__keyword__[[:alnum:]]+)\b@',
	'str' =>      "@\"(?:[^\"\\\\]|\\\\.)*?\"@u",
	'chr' =>      "@\'(?:[^\'\\\\]|\\\\.+?)\'@u",
	'num' =>      '@-?\b[0-9]+@u',
	'listcon' =>  '@[\\[\\]]|\(:\)|(:|\\.\\.)(?=[^-:!\\@#$%^*.|=+<>&~/\\\\])|\\|\\]@',
	'keyglyph' => '@[\\[\\]]|(?:=>|=|->|::|\\\\|<-|\|)(?=\\s|[a-zA-Z(\\@_])@',
	'conop' =>    "@`(?:[[:upper:]][[:alnum:]_']*[.])*[[:upper:]][[:alnum:]_']*`|:([-:\\@#$%^*.|=+<>&~/\\\\]|!!?(?!!))*@u",
	'varop' =>    "@`(?:[[:upper:]][[:alnum:]_']*[.])*[[:lower:]_][[:alnum:]_']*`|([-:\\@#$%^*.|=+<>&~/\\\\\']|!!?(?!!))+(__[{][^}]*[}])?@u",
	'conid' =>    "@\\b(?:[[:upper:]][[:alnum:]_']*[.])*[[:upper:]][[:alnum:]_']*@u",
	'varid' =>    "@\\b(?:[[:upper:]][[:alnum:]_']*[.])*[[:lower:]_][[:alnum:]_']*@u",
);

