<?php

// -----------------------------------------------------------------------------
// Page loaded from a text file
// -----------------------------------------------------------------------------

class TextFilePage extends Page {
	private $path;
	private $loaded;
	public $show_sourcelink;
	
	function __construct($url,$path) {
		if (!$url) {
			// url = path - extension
			$url = $path;
			$url = preg_replace('@[.][^./]*$@','',$url);
			$url = str_replace('\\','/',$url);
		}
		$this->path = $path;
		parent::__construct($url);
		$this->is_blog = preg_match('@^blog/@',$this->url);
		$this->show_comments = $this->is_blog;
		$this->show_date = $this->is_blog;
		$this->show_feedlink = $this->is_blog;
	}
	
	function dependencies() {
		Cache::depend_on_file($this->path);
		Comments::dependencies($this->url);
	}
	
	function do_load() {
		$this->last_modified = filemtime($this->path);
		// load the file
		$data = file($this->path,FILE_IGNORE_NEW_LINES);
		// title, and other attributes
		$language = 'haskell';
		if (preg_match('@.md$@',$this->path)) {
  		$format = 'markdown';
		} else {
		  $format = 'wiki';
		}
		$literate_style = preg_match('@.lagda$@',$this->path) ? 'latex' : 'normal';
		while (!empty($data)) {
			$attr = array_shift($data);
			if (preg_match('@^(title):\s*(.*)@',$attr,$ma)) {
				$this->title = $ma[2];
			} elseif (preg_match('@^(date|time):\s*(.*)@',$attr,$ma)) {
				$this->last_modified = strtotime($ma[2], $this->last_modified);
			} elseif (preg_match('@^(subtitle):\s*(.*)@',$attr,$ma)) {
				$this->subtitle = $ma[2];
			} elseif (preg_match('@^(icon):\s*(.*)@',$attr,$ma)) {
				$this->icon = $ma[2];
			} elseif (preg_match('@^(category|section):\s*(.*)@',$attr,$ma)) {
				$this->category = $ma[2];
			} elseif (preg_match('@^(tags?):\s*(.*)@',$attr,$ma)) {
				$this->tags = preg_split ('@[\s|,]+@',$ma[2],-1,PREG_SPLIT_NO_EMPTY);
			} elseif (preg_match('@^((?:is[ _-]?)?published):\s*(.*)@',$attr,$ma)) {
				$this->is_published = to_bool($ma[2]);
			} elseif (preg_match('@^(source[ _-]?link url):(.*)@',$attr,$ma)) {
				$this->source_link_url = $ma[2];
			} elseif (preg_match('@^(source[ _-]?link):(.*)@',$attr,$ma)) {
				if (isset($this->source_link_url)) {
					$this->source_link = "<a href='$this->source_link_url'>$ma[2]</a>";
				} else {
					$this->source_link = "<a href='$this->path'>$ma[2]</a>";
				}
			} elseif (preg_match('@^(?:show[ _-]?)?(comments):\s*(.*)@',$attr,$ma)) {
				$this->show_comments = true;
			} elseif (preg_match('@^show[ _-]?date:\s*(.*)@',$attr,$ma)) {
				$this->show_date = true;
			} elseif (preg_match('@^literate style: latex@',$attr,$ma)) {
				$literate_style == 'latex';
			} elseif (preg_match('@^language: (.*)@',$attr,$ma)) {
				$language = $ma[1];
			} elseif (preg_match('@^natural language: (.*)@',$attr,$ma)) {
				$this->natural_language = $ma[1];
			} elseif (preg_match('@^license: (.*)@',$attr,$ma)) {
				$license = $ma[1];
			} elseif (preg_match('@^discussion: (.*)@',$attr,$ma)) {
				$this->discussion_link = "Discussion: " . $ma[1];
			} else if ($attr != '') {
				$this->title = $attr;
				break;
			} else {
				break;
			}
		}
		if (count($this->tags) && !$this->icon) {
			foreach ($this->tags as $t) $this->icon .= " icon-" . $t;
		}
		if (isset($license)) {
		    $this->license_link = $license;
	    } else if ($this->is_blog) {
	        $this->license_link = DEFAULT_BLOG_LICENSE;
	    } else {
	        $this->license_link = DEFAULT_PAGE_LICENSE;
	    }
		if ($literate_style == 'latex') {
		  $data = unlit_latex($data);
		}
		// body
		if ($format == 'markdown') {
		  $this->body = MarkdownFormat::format($data, $language);
		} else {
		  $this->body = '';
		  $this->body .= WikiFormat::format($data, true, $language);
		}
	}
	
}

function to_bool($x) {
	return $x=='true' || $x=='yes' || (int)$x;
}

// convert latex style literate code to bird-tack style
function unlit_latex($lines) {
	$in_code = false;
	$out = array();
	foreach ($lines as $l) {
		if ($l == "\\begin{code}" || $l == "\\begin{pseudocode}") {
			$in_code = true;
		} elseif ($l == "\\end{code}" || $l == "\\end{pseudocode}") {
			$in_code = false;
		} elseif ($in_code) {
			$out []= '> ' . $l;
		} else {
			$out []= $l;
		}
	}
	return $out;
}

