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
			} elseif (preg_match('@^(source[ _-]?link):(.*)@',$attr,$ma)) {
				$this->source_link = "<a href='$this->path'>$ma[2]</a>";
			} elseif (preg_match('@^(?:show[ _-]?)?(comments):\s*(.*)@',$attr,$ma)) {
				$this->show_comments = true;
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
		
		// body
		$this->body = '';
		$this->body .= WikiFormat::format($data, true);
	}
	
}

function to_bool($x) {
	return $x=='true' || $x=='yes' || (int)$x;
}
