<?php

// -----------------------------------------------------------------------------
// Atom feeds
// -----------------------------------------------------------------------------

abstract class Feed {
	public $entries = array();
	private $loaded = false;
	
	function sort() {
		usort($this->entries,'compare_date');
	}
	
	function last_modified() {
		$time = 0;
		foreach ($this->entries as $e) {
			if ($time < $e->last_modified) $time = $e->last_modified;
		}
		return $time;
	}
	
	function load() {
		if (!$this->loaded) {
			$this->loaded = true;
			$this->do_load();
		}
	}
	abstract function do_load();
	
	function write() {
		$this->do_load();
		$this->write_atom();
	}
	
	function write_atom() {
		header("Content-Type: application/atom+xml");
		$this->sort();
		echo $this->content_atom();
	}
	
	function content_atom() {
		$updated = $this->last_modified();
		$out =  "<?xml version='1.0' encoding='utf-8'?>\n";
		$out .= "<feed xmlns='http://www.w3.org/2005/Atom'>\n";
		$out .= "  <title>" . BLOG_TITLE . "</title>\n";
		$out .= "  <link href='" . BLOG_URL . "feed' rel='self' />\n";
		$out .= "  <link href='" . BLOG_URL . "blog' />\n";
		$out .= "  <id>" . BLOG_URL . "blog</id>\n";
		if ($updated) {
			$out .= "  <updated>" . iso_timestamp($updated) . "</updated>\n";
		}
		$out .= "  <author>\n";
		$out .= "    <name>" . BLOG_AUTHOR_NAME . "</name>\n";
		$out .= "    <email>" . BLOG_AUTHOR_EMAIL . "</email>\n";
		$out .= "  </author>\n";
		foreach ($this->entries as $e) {
			$out .= Feed::entry_content_atom($e);
		}
		$out .= "</feed>";
		return $out;
	}
	
	function entry_content_atom($page) {
		$url     = $page->full_url();
		$title   = htmlspecialchars($page->title);
		$content = htmlspecialchars($page->body);
		$id      = $url; // TODO: get something better?
		$out =  "  <entry>\n";
		$out .= "    <title>$title</title>\n";
		$out .= "    <link href='$url' />\n";
		$out .= "    <link rel='alternate' type='text/html' href='$url'/>\n";
		$out .= "    <id>$id</id>\n";
		$out .= "    <updated>" . iso_timestamp($page->last_modified) . "</updated>\n";
		foreach ($page->tags as $tag) {
			$out .= "    <category scheme='" . BLOG_URL ."' term='$tag' />\n";
		}
		$out .= "    <content type='html' xml:base='".Util::base_url()."'>$content</content>\n";
		$out .= "  </entry>\n";
		return $out;
	}
}

function iso_timestamp($timestamp) {
	return gmstrftime('%Y-%m-%dT%H:%M:%SZ',$timestamp);
}

function compare_date($a,$b) {
	if ($a->last_modified < $b->last_modified) return +1;
	if ($a->last_modified > $b->last_modified) return -1;
	return 0;
}

