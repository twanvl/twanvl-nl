<?php

// -----------------------------------------------------------------------------
// MediaWiki style markup
// -----------------------------------------------------------------------------

class BaseFormat {
	function text_to_url($url) {
		/*if (preg_match("/^(.*)/i",$url,$matches)) {
			$sub = str_replace(' ','_',strtolower($matches[1]));
			return local_url($sub);
		} else {
			return url($url);
		}*/
		return $url;
	}
	function format_link($url, $text, $if_cat = '') {
		$url = BaseFormat::text_to_url($url);
		$style = '';
		//if ($url == $current_url || $if_cat == $category) {
		//	$style .= ' class="current"';
		//}
		return '<a href="' . htmlspecialchars($url) . '"' . $style . '>' . $text . '</a>';
	}
}

class WikiFormat extends BaseFormat {
	public $html; // output
	public $language; // programming language to use
	private $state = '';
	private $pending = '';
	private $pending_linklist = array();
	public $trusted = false; // do we trust the source of the code? If true we allow file access and php execution
	
	public function __construct() {
		$this->language = new HaskellFormat();
	}
	
	public static function format($lines, $trusted = false) {
		$fmt = new WikiFormat();
		$fmt->trusted = $trusted;
		$fmt->add_lines($lines);
		$fmt->end();
		return $fmt->html;
	}
	
	public function add_lines($lines) {
		if (!is_array($lines)) $lines = explode("\n",$lines);
		foreach ($lines as $line) {
			$this->add_line($line);
		}
	}
	
	public function add_line($line) {
		$len = strlen($line);
		
		// paragraph
		if ($line == '') {
			$this->state_switch('','');
		
		// headings
		} else if (preg_match('@^[-=]{4}\s*([^-=].*)\s*[-=]{4}\s*$@',$line,$ma)) {
			$this->state_switch('','');
			$this->html .= '<h4>' . $this->anchor_for($ma[1]) . $this->format_inline($ma[1]) . "</h4>\n";
		} elseif (preg_match('@^[-=]{3}\s*([^-=].*)\s*[-=]{3}\s*$@',$line,$ma)) {
			$this->state_switch('','');
			$this->html .= '<h3>' . $this->anchor_for($ma[1]) . $this->format_inline($ma[1]) . "</h3>\n";
		} elseif (preg_match('@^[-=]{2}\s*([^-=].*)\s*[-=]{2}\s*$@',$line,$ma)) {
			$this->state_switch('','');
			$this->html .= '<h2>' . $this->anchor_for($ma[1]) . $this->format_inline($ma[1]) . "</h2>\n";
		
		// latex
		} elseif (preg_match('@^(FORMULA): ?(.*)$@',$line,$ma)) {
			// fancy latex
			$this->state_switch('<p>','</p>');
			$this->html .= "<img src='image/$ma[2].png'>\n";
			$this->state = 'FORMULA';
		} elseif ($this->state == 'FORMULA' && preg_match("@^\\t@", $line)) {
			// ignore
		
		// lists etc.
		} elseif (preg_match('@^\s*[*](.*)$@',$line,$ma)) {
			$this->state_switch('<ul>','</ul>');
			$this->html .= '<li>' . $this->format_inline($ma[1]) . "</li>\n";
		} elseif (preg_match('@^\s*[#](.*)$@',$line,$ma)) {
			$this->state_switch('<ol>','</ol>');
			$this->html .= '<li>' . $this->format_inline($ma[1]) . "</li>\n";
		} elseif (preg_match('@^>\s*--\s*(IGNORE|HIDDEN)\s*$@',$line)){
			// ignore the following block
			$this->state_switch('','');
			$this->state = 'IGNORE';
		} elseif (preg_match('@^\]?>\s*--\s*BLOCK[:]?\s*(\S+)$@',$line, $ma)){
			// language of the following block
			$this->state_switch('<pre class="'.htmlspecialchars($ma[1]).'">','</pre>');
		} elseif (preg_match('@^[>] ?(.*)$@',$line,$ma)) {
			// source code
			if ($this->state == 'IGNORE') {
				// ignore this block, for literal Haskell files
				return;
			}
			if (strpos($this->state,'<pre class="haskell-') === false) {
				$this->state_switch('<pre class="haskell">','</pre>');
			}
			if (trim($ma[1]) === '') {
				$this->html .= "<div class='empty-line'></div>\n";
			} else {
				$this->html .= WikiFormat::format_code($ma[1]) . "\n";
			}
		} elseif (preg_match('@^\]> ?(.*)$@',$line,$ma)) {
			// source code, no formating
			if (strpos($this->state,'<pre class="haskell-') !== false) {
			} else {
				$this->state_switch('<pre class="ghci">','</pre>');
			}
			$this->html .= WikiFormat::format_code($ma[1]) . "\n";
		} elseif (preg_match('@^\] ?(.*)$@',$line,$ma)) {
			// source code, no formating
			if ($this->state == '<pre class="ghci">') {
				// stay in state
			} else {
				$this->state_switch('<pre>','</pre>');
			}
			$this->html .= htmlspecialchars($ma[1]) . "\n";
		
		// link lists
		} elseif (preg_match('@^(LINK|TITLE|SUBTITLE|DATE|DETAILS|ICON): ?(.*)$@',$line,$ma)) {
			$this->state_switch('<ul class="link-list">','</ul>');
			if ($ma[1] == 'LINK') $this->render_pending_linklist();
		    if (isset($this->pending_linklist[$ma[1]])) {
    			$this->pending_linklist[$ma[1]] .= "\n<br>" . $ma[2];
		    } else {
	    		$this->pending_linklist[$ma[1]] = $ma[2];
            }
		
		// other
		} elseif (preg_match("@^\s*</?(pre|ul|ol|li|div|blockquote|h2|h3|>)@", $line)) {
			$this->state_switch('','');
			$line = preg_replace("@^<>@","",$line);
			$this->html .= $this->format_inline($line) . "\n";
		} elseif (preg_match("@^//@", $line)) {
			// comment, ignore
        } elseif ($this->trusted && preg_match("@^INCLUDE: ?(.*)$@", $line, $ma)) {
            // include a file
            $lines = file($ma[1]);
            while (!empty($lines) && preg_match("@:@",$lines[0])) {
			    array_shift($lines);
			}
            $this->add_lines($lines);
		} else {
			// body text
			$this->state_switch('<p>','</p>');
			$this->html .= $this->format_inline($line) . "\n";
		}
	}
	private function state_switch($open,$close) {
		if ($this->state == $open) {
			// okay
		} else {
			$this->render_pending_linklist();
			$this->html   .= $this->pending;
			$this->html   .= $open;
			$this->pending = $close;
			$this->state   = $open;
		}
	}
	private function end() {
		$this->render_pending_linklist();
		$this->html .= $this->pending;
	}
	private function render_pending_linklist() {
		$l = $this->pending_linklist;
		if (!$l) return;
		$icon = 'thumbnail';
		if (isset($l['ICON'])) $icon .= ' icon-' . $l['ICON'];
		$this->html .= '<li><a href="' . htmlspecialchars($l['LINK']) . '">';
		$this->html .= '<span class="'.$icon.'"></span>';
		$this->html .= '<span class="details">' . $this->format_inline(@$l['DETAILS']) . '</span>';
		$this->html .= '<span class="title">' . $this->format_inline($l['TITLE']) . '</span>';
		$this->html .= '<span class="desc">' . $this->format_inline(@$l['SUBTITLE']) . '</span>';
		$this->html .= '</a></li>' . "\n";
		$this->pending_linklist = array();
	}
	
	private function format_inline($line) {
		if ($this->trusted) {
			$line = preg_replace_callback('/[$][{](.*?)[}][$]/',  'WikiFormat::exec_code',            $line);
		}
		$line = preg_replace("/'''(.*?)'''/",                 '<strong>\\1</strong>',       $line);
		$line = preg_replace("/''(.*?)''/",                   '<em>\\1</em>',               $line);
		$line = preg_replace_callback('/@(?![^<]*[a-z"]>)(([^@]|@@)*)(?![^<]*[a-z"]>)@/', array($this,'format_inline_code'), $line);
		$line = preg_replace_callback('/[$](.*?)[$]/',        'WikiFormat::format_inline_math',   $line);
		$line = preg_replace_callback('/\[\[(.*?)\|(.*?)]]/', 'WikiFormat::format_inline_link_s', $line);
		$line = preg_replace_callback('/\[\[(.*?)]](s?)/',    'WikiFormat::format_inline_link',   $line);
		$line = preg_replace('@(?<!["\'<\\\\])(https?://[^" )]+)(?<![.])@', '<a href="\\1">\\1</a>',   $line);
		return $line;
	}
	// format ${execute-this}$
	private static function exec_code($matches) {
		$code = $matches[1];
		if ($code[0] == '$') {
			return $GLOBALS[substr($code,1)];
		} else {
			return eval($code);
		}
	}
	// format [[url|text]] or [[url]]
	private static function format_inline_link($matches) {
		return WikiFormat::format_link($matches[1], WikiFormat::url_to_title($matches[1], $matches[2]));
	}
	// format [[url]]s
	private static function format_inline_link_s($matches) {
		return WikiFormat::format_link($matches[1], WikiFormat::url_to_title($matches[2]));
	}
	// format @code@
	private function format_inline_code($matches) {
		$str = $matches[1];
		$code = $this->language->format($str);
		//if (preg_match("@^[^\\(\\)\\[\\]<>!+]*$@",$str)) {
		if (preg_match("@^[_a-zA-Z0-9]*$@",$str)) {
			return "<tt>$code</tt>";
		} else {
			return "<tt class='complex'>$code</tt>";
		}
		//$str = htmlspecialchars($str);
		//$str = preg_replace('@__([a-zA-Z0-9_]+)@','<sub>\\1</sub>',$str);
		//return "<tt>$str</tt>";
	}
	// format $math$
	private static function format_inline_math($matches) {
		$str = $matches[1];
		$str = preg_replace('@\s-->\s@',' &rarr; ',$str);
		$str = preg_replace('@\s==>\s@',' &rArr; ',$str);
		$str = preg_replace('@\s\\\\le\s@',' &le; ',$str);
		
		$str = preg_replace('@\^\{([^}]*)\}@',          '<sup>\\1</sup>',$str);
		$str = preg_replace('@\^(\{[^}]*\}|[0-9]+|.)@', '<sup>\\1</sup>',$str);
		//$str = preg_replace('@__?(\{[^}]*\}|.)@',       '<sub>\\1</sub>',$str);
		$str = preg_replace('@__?([^{])@','<sub>\\1</sub>',$str);
		$str = preg_replace('@__?[{]([^}]*)[}]@','<sub>\\1</sub>',$str);
		return  "<span class=\"math\">$str</span>";
	}
	
	// convert a url to a title
	public function url_to_title($url, $s = '') {
		if (preg_match("/.*:$/",$url)) {
			$url = preg_replace("/:/","",$url);
		} else if (!preg_match("@^http://@",$url)) {
			$url = preg_replace("/.*:/","",$url);
		}
		if ($s == 's' && $url{strlen($url)-1}=='y') {
			$url = substr($url,0,-1) . 'ies';
		} else {
			$url .= $s;
		}
		return $url;
	}

	// add an anchor for headings
	private function anchor_for($text) {
		if (!$this->trusted) return '';
		$text = strtolower($text);
		$text = trim($text);
		$text = preg_replace('@[^a-z0-9_]+@','-',$text);
		return '<a name="'.$text.'"></a>';
	}
	
	private function format_code($code) {
		return $this->language->format($code);
	}
}

class NoFormat {
	public static function format($code) {
		return htmlspecialchars($code);
	}
}


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

class HaskellFormat {
	public static function format($code) {
		// Haskell lexer goes here
		global $haskell_lang;
		$lex = new Lexer($haskell_lang, $code);
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
				if ($type == 'varid' || $type == 'varop' || $type == 'comment') {
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

global $haskell_lang;
$haskell_lang = array(
	'!!!' =>     '@!!!.*?!!!@',
	'!!!notation' => '@{-!!!.*!!!-}@',
	'input' =>   '@^(?:[*]?([[:alnum:]]|λ)+>|<[a-zA-Z]+> *>?|> )@i',
	'pragma' =>  '@{-# *[[:upper:]]+ .*?#-}@',
	'comment' => '@--.*|{-.*-}@',
	'keyword' => '@\b(if|then|else|module|import|qualified|hiding|where|let|in|case|of|newtype|default|infix|infixr|infixl|(?:data|type)(?: family)?|class|instance|forall|exists|deriving|do|__keyword__[[:alnum:]]+)\b@',
	'str' =>     "@\"(?:[^\"\\\\]|\\\\.)*?\"@",
	'chr' =>     "@\'(?:[^\'\\\\]|\\\\.)*?\'@",
	'num' =>      '@-?\b[0-9]+@',
	'listcon' =>  '@[\\[\\]]|\(:\)|(:|\\.\\.)(?=[^-:!\\@#$%^*.|=+<>&~/\\\\])@',
	'keyglyph' => '@[\\[\\]]|(?:=>|=|->|::|\\\\|<-|\|)(?=\\s|[a-zA-Z(\\@_])@',
	'conop' =>    "@`(?:[[:upper:]][[:alnum:]_']*[.])*[[:upper:]][[:alnum:]_']*`|:([-:\\@#$%^*.|=+<>&~/\\\\]|!!?(?!!))*@u",
	'varop' =>    "@`(?:[[:upper:]][[:alnum:]_']*[.])*[[:lower:]_][[:alnum:]_']*`|([-:\\@#$%^*.|=+<>&~/\\\\]|!!?(?!!))+(__[{][^}]*[}])?@u",
	'conid' =>    "@\\b(?:[[:upper:]][[:alnum:]_']*[.])*[[:upper:]][[:alnum:]_']*@u",
	'varid' =>    "@\\b(?:[[:upper:]][[:alnum:]_']*[.])*[[:lower:]_][[:alnum:]_']*@u",
);

