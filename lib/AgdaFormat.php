<?php

// -----------------------------------------------------------------------------
// Haskell lexer and syntax highlighter
// -----------------------------------------------------------------------------

class AgdaFormat extends HaskellFormat {
	public static function format($code) {
		global $agda_lang;
		$lex = new Lexer($agda_lang, $code);
		return HaskellFormat::do_format($lex);
    }
}


// rules for the agda lexer
require_once('lib/HaskellFormat.php');
global $haskell_lang;
global $agda_lang;
$agda_lang = $haskell_lang;
$agda_lang['keyword'] = '@\b([∀→]|if|then|else|module|import|qualified|hiding|where|let|in|case|of|newtype|default|infix|infixr|infixl|(?:data|type)(?: family)?|class|instance|forall|exists|deriving|do|record|field|open|public|private|renaming|__keyword__[[:alnum:]]+)\b@';
$agda_lang['varop'] = "@([-:\\@#$%^*.|=+<>&~/\\\\\'⟶≈_⟨⟩]|!!?(?!!))+(__[{][^}]*[}])?@u";
