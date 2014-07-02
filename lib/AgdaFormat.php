<?php

// -----------------------------------------------------------------------------
// Haskell lexer and syntax highlighter
// -----------------------------------------------------------------------------

class AgdaFormat extends HaskellFormat {
    public function __construct() {
		global $agda_lang;
        parent::__construct($agda_lang);
    }
}


// rules for the agda lexer
require_once('lib/HaskellFormat.php');
global $haskell_lang;
global $agda_lang;
$agda_lang = array();
$agda_lang['!!!'] = $haskell_lang['!!!'];
$agda_lang['!!!notation'] = $haskell_lang['!!!notation'];
$agda_lang['input'] = $haskell_lang['input'];
$agda_lang['pragma'] = $haskell_lang['pragma'];
$agda_lang['comment'] = $haskell_lang['comment'];
$agda_lang['str'] = $haskell_lang['str'];
$agda_lang['chr'] = $haskell_lang['chr'];
$agda_lang['num'] = $haskell_lang['num'];
$agda_lang['keyglyph'] = '@=>|=|->|:|\\\\|<-|\||[∀→_]@u';
$agda_lang['keyword'] = '@\b(if|then|else|module|open|import|qualified|hiding|where|let|in|case|of|newtype|default|infix|infixr|infixl|data|class|instance|forall|exists|deriving|do|record|field|open|public|private|renaming|postulate|with|__keyword__[[:alnum:]]+)(?=$|[ (){}])@';
$agda_lang['agda-fun']  = "@\b(cong|subst|Set|Eq|Σ|Π|Iso)(?=$|[ (){}])@u";
$agda_lang['agda-ctor'] = "@\b(refl|,|\[\]|0|zero|suc|here|there|acc)(?=$|[ (){}])@u";
$agda_lang['agda-proj'] = "@\b(proj₁|proj₂)(?=$|[ (){}])@u";
$agda_lang['conop'] = "@[⊎ℕ]@u";
$agda_lang['varop'] = "@([-:\\@#$%^*.|=+<>&~/\\\\\'⟶≈⟨⟩⌈⌉⌊⌋≤◂≡×≢?∘\\[\\]]|!!?(?!!))+[^ (){}_]*(__[{][^}]*[}])?@u";
$agda_lang['conid'] = "@\\b(?:[[:upper:]][[:alnum:]']*[.])*[[:upper:]][^ (){}_.]+(?=$|[ (){}])@u";
$agda_lang['varid'] = "@\\b(?:[[:upper:]][[:alnum:]']*[.])*[[:lower:]][^ (){}_.]*(?=$|[ (){}])@u";
$agda_lang['hole'] = "@[{]!(.*?)![}]@u";
unset($agda_lang['listcon']);

