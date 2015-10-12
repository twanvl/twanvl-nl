<?php

// -----------------------------------------------------------------------------
// C-- lexer and syntax highlighter
// -----------------------------------------------------------------------------

class CmmFormat {
  private $lang = array(
	  'comment' =>  '@//.*|/\*.*\*/@',
	  'keyword' =>  '@\b(if|else|return|bits16|W_|P_|gcptr)\b@',
	  'str' =>      "@\"(?:[^\"\\\\]|\\\\.)*?\"@",
	  'chr' =>      "@\'(?:[^\'\\\\]|\\\\.+?)\'@",
	  'num' =>      '@-?\b[0-9]+@',
	  'listcon' =>  '@\\[|\\]@',
	  'varop' =>    "@`(?:[[:upper:]][[:alnum:]_']*[.])*[[:lower:]_][[:alnum:]_']*`|([-:\\@#$%^*.|=+<>&~/\\\\\']|!!?(?!!))+(__[{][^}]*[}])?@u",
  );
  
	public function format($code) {
    $lex = new Lexer($this->lang, $code);
	  return CmmFormat::do_format($lex);
  }
  protected static function do_format($lex) {
		$out = '';
		while (!$lex->end()) {
			list($type,$match) = $lex->next();
			if ($type == '') {
				$out .= htmlspecialchars($match);
			} else {
				$out .= "<span class=\"$type\">$match</span>";
			}
		}
		return $out;
	}
}

