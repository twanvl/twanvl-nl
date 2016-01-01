<?php

// -----------------------------------------------------------------------------
// MediaWiki style markup
// -----------------------------------------------------------------------------

class BaseFormat {
	static function text_to_url($url) {
		/*if (preg_match("/^(.*)/i",$url,$matches)) {
			$sub = str_replace(' ','_',strtolower($matches[1]));
			return local_url($sub);
		} else {
			return url($url);
		}*/
		return $url;
	}
	static function format_link($url, $text, $if_cat = '') {
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
	public $default_block_type;
	private $state = '';
	private $pending = '';
	private $pending_linklist = array();
	public $trusted = false; // do we trust the source of the code? If true we allow file access and php execution
	
	public function __construct($default_block_type = 'haskell') {
		$this->default_block_type = $default_block_type;
		if ($this->default_block_type == 'haskell') {
			$this->language = new HaskellFormat();
		} elseif ($this->default_block_type == 'agda') {
			$this->language = new AgdaFormat();
		} else {
		}
	}
	
	public static function format($lines, $trusted = false, $default_block_type = 'haskell') {
		$fmt = new WikiFormat($default_block_type);
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
		} else if (preg_match('@^[-=]{4}\s*([^-=].*)\s*[-=]{4}\s*$@u',$line,$ma)) {
			$this->state_switch('','');
			$this->html .= '<h4>' . $this->anchor_for($ma[1]) . $this->format_inline($ma[1]) . "</h4>\n";
		} elseif (preg_match('@^[-=]{3}\s*([^-=].*)\s*[-=]{3}\s*$@u',$line,$ma)) {
			$this->state_switch('','');
			$this->html .= '<h3>' . $this->anchor_for($ma[1]) . $this->format_inline($ma[1]) . "</h3>\n";
		} elseif (preg_match('@^[-=]{2}\s*([^-=].*)\s*[-=]{2}\s*$@u',$line,$ma)) {
			$this->state_switch('','');
			$this->html .= '<h2>' . $this->anchor_for($ma[1]) . $this->format_inline($ma[1]) . "</h2>\n";
		
		// latex
		} elseif (preg_match('@^(FORMULA): ?(.*)$@u',$line,$ma)) {
			// fancy latex
			$this->state_switch('<p>','</p>');
			$this->html .= "<img src='image/$ma[2].png'>\n";
			$this->state = 'FORMULA';
		} elseif ($this->state == 'FORMULA' && preg_match("@^\\t@", $line)) {
			// ignore
		
		// lists etc.
		} elseif (preg_match('@^\s*[*](.*)$@u',$line,$ma)) {
			$this->state_switch('<ul>','</ul>');
			$this->html .= '<li>' . $this->format_inline($ma[1]) . "</li>\n";
		} elseif (preg_match('@^\s*[#](.*)$@u',$line,$ma)) {
			$this->state_switch('<ol>','</ol>');
			$this->html .= '<li>' . $this->format_inline($ma[1]) . "</li>\n";
		} elseif (preg_match('@^>\s*--\s*(IGNORE|HIDDEN)(: .*)?\s*$@u',$line)){
			// ignore the following block
			$this->state_switch('','');
			$this->state = 'IGNORE';
		} elseif (preg_match('@^\]>\s*--\s*(?:BLOCK|TYPE|LANGUAGE)[:]?\s*(\S+)-continue$@u',$line, $ma)){
			$this->state = '<pre class="'.$ma[1].'-continue';
		} elseif (preg_match('@^\]?>\s*--\s*(?:BLOCK|TYPE|LANGUAGE)[:]?\s*(\S+)$@u',$line, $ma)){
			// language of the following block
			if ($ma[1] == 'haskell') {
			    $this->language = new HaskellFormat();
			    $this->default_block_type = 'haskell';
		    } elseif ($ma[1] == 'agda') {
			    $this->language = new AgdaFormat();
			    $this->default_block_type = 'agda';
		    } elseif ($ma[1] == 'c--') {
			    $this->language = new CmmFormat();
			    $this->default_block_type = 'c--';
		    }
		} elseif (preg_match('@^\]?>\s*--\s*(?:LEXER)[:]?\s*(\S+)\s*(.*)$@u',$line, $ma)){
			// modify lexer
			$this->language->modify_rule($ma[1],$ma[2]);
		} elseif (preg_match('@^[>] ?(.*)$@u',$line,$ma)) {
			// source code
			if ($this->state == 'IGNORE') {
				// ignore this block, for literal Haskell files
				return;
			}
			if (!preg_match('@<pre class="(haskell|agda)@u',$this->state)) {
				$this->state_switch('<pre class="'.$this->default_block_type.'">','</pre>');
			}
			if (trim($ma[1]) === '') {
				$this->html .= "<div class='empty-line'></div>\n";
			} else {
				$this->html .= WikiFormat::format_code($ma[1]) . "\n";
			}
		} elseif (preg_match('@^\]> ?(.*)$@u',$line,$ma)) {
			// source code, no formating
			if (!preg_match('@<pre class="(haskell|agda)@u',$this->state)) {
				$this->state_switch('<pre class="ghci">','</pre>');
			}
			$this->html .= WikiFormat::format_code($ma[1]) . "\n";
		} elseif (preg_match('@^\] ?(.*)$@u',$line,$ma)) {
			// source code, no formating
			if ($this->state == '<pre class="ghci">') {
				// stay in state
			} else {
				$this->state_switch('<pre>','</pre>');
			}
			$this->html .= htmlspecialchars($ma[1]) . "\n";
		
		// link lists
		} elseif (preg_match('@^(LINK|TITLE|SUBTITLE|DATE|DETAILS|ICON): ?(.*)$@u',$line,$ma)) {
			$this->state_switch('<ul class="link-list">','</ul>');
			if ($ma[1] == 'LINK') $this->render_pending_linklist();
		    if (isset($this->pending_linklist[$ma[1]])) {
    			$this->pending_linklist[$ma[1]] .= "\n<br>" . $ma[2];
		    } else {
	    		$this->pending_linklist[$ma[1]] = $ma[2];
            }
		
		// other
		} elseif (preg_match("@^\s*</?(pre|ul|ol|li|div|blockquote|h2|h3|>)@u", $line)) {
			$this->state_switch('','');
			$line = preg_replace("@^<>@","",$line);
			$this->html .= $this->format_inline($line) . "\n";
		} elseif (preg_match("@^//@", $line)) {
			// comment, ignore
        } elseif ($this->trusted && preg_match("@^INCLUDE: ?(.*)$@u", $line, $ma)) {
            // include a file
            $lines = file($ma[1]);
            while (!empty($lines) && preg_match("@:@u",$lines[0])) {
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
		$line = preg_replace("/'''(.*?)'''/u",                 '<strong>\\1</strong>',       $line);
		$line = preg_replace("/''(.*?)''/u",                   '<em>\\1</em>',               $line);
		$line = preg_replace_callback('/@(?![^<]*[a-z"]>)(([^@]|@@)*)(?![^<]*[a-z"]>)@/u', array($this,'format_inline_code'), $line);
		$line = preg_replace_callback('/[$](.*?)[$]/u',        'WikiFormat::format_inline_math',   $line);
		$line = preg_replace_callback('/\[\[(.*?)\|(.*?)]]/u', 'WikiFormat::format_inline_link_s', $line);
		$line = preg_replace_callback('/\[\[(.*?)]](s?)/u',    'WikiFormat::format_inline_link',   $line);
		$line = preg_replace('@(?<!["\'<\\\\])(https?://[^" )]+)(?<![.])@u', '<a href="\\1">\\1</a>',   $line);
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
		if (preg_match("@^[_a-zA-Z0-9]*$@u",$str)) {
			return "<tt>$code</tt>";
		} else {
			return "<tt class='complex'>$code</tt>";
		}
		//$str = htmlspecialchars($str);
		//$str = preg_replace('@__([a-zA-Z0-9_]+)@u','<sub>\\1</sub>',$str);
		//return "<tt>$str</tt>";
	}
	// format $math$
	private static function format_inline_math($matches) {
		$str = $matches[1];
		$str = preg_replace('@\s-->\s@u',' &rarr; ',$str);
		$str = preg_replace('@\s==>\s@u',' &rArr; ',$str);
		$str = preg_replace('@\s\\\\le\s@u',' &le; ',$str);
		
		$str = preg_replace('@\^\{([^}]*)\}@u',          '<sup>\\1</sup>',$str);
		$str = preg_replace('@\^(\{[^}]*\}|[0-9]+|.)@u', '<sup>\\1</sup>',$str);
		//$str = preg_replace('@__?(\{[^}]*\}|.)@u',       '<sub>\\1</sub>',$str);
		$str = preg_replace('@__?([^{])@u','<sub>\\1</sub>',$str);
		$str = preg_replace('@__?[{]([^}]*)[}]@u','<sub>\\1</sub>',$str);
		return  "<span class=\"math\">$str</span>";
	}
	
	// convert a url to a title
	public static function url_to_title($url, $s = '') {
		if (preg_match("/.*:$/u",$url)) {
			$url = preg_replace("/:/u","",$url);
		} else if (!preg_match("@^http://@u",$url)) {
			$url = preg_replace("/.*:/u","",$url);
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
		$text = preg_replace('@[^a-z0-9_]+@u','-',$text);
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

