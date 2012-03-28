<?php

require_once('lib/bootstrap.inc');

function compare_date($a,$b) {
	if ($a->last_modified < $b->last_modified) return +1;
	if ($a->last_modified > $b->last_modified) return -1;
	return 0;
}

class BlogPage extends Page {
	function __construct($url) {
		parent::__construct($url);
		
		$this->tag = @$_REQUEST['tag'];
		
		// find all blog items
		$this->pages = Resolver::find_all_pages('blog');
	}
	function dependencies() {
		// we (potentially) depend on all pages
		foreach ($this->pages as $page) {
			$page->dependencies();
		}
	}
	function do_load() {
		foreach ($this->pages as $page) {
			$page->load();
			if (!$page->is_published) continue;
			if ($this->tag && !in_array($this->tag,$page->tags)) continue;
			$this->entries []= $page;
		}
		
		usort($this->entries,'compare_date');
		
		if ($this->tag) {
			$this->title = "Posts tagged '" . htmlspecialchars($this->tag) . "'";
		} else {
			$this->title = BLOG_TITLE;
			$this->show_feedlink = true;
		}
		$this->body  = $this->body();
	}
	function body() {
		if (empty($this->entries)) {
			return "<em>No posts found</em>";
		}
		$out = '<ul class="link-list">';
		foreach($this->entries as $page) {
			$notes = '';
			$notes .= "Date: <span class='date'>" . gmstrftime('%Y-%m-%d',$page->last_modified) . '</span><br>';
			if ($page->tags) $notes .= "Tags: " . implode(', ', $page->tags) . '<br>';
			if ($page->show_comments) {
				$comments = count(Comments::get_all($page->url));
				if     ($comments == 0) $notes .= "0 comments<br>";
				elseif ($comments == 1) $notes .= "1 comment<br>";
				else                    $notes .= "$comments comments<br>";
			}
			
			$out .= "<li>";
			$out .= '<a href="' . htmlspecialchars($page->url) . '">';
			$out .= "<span class='thumbnail $page->icon'></span>";
			$out .= "<span class='details'>" . $notes . "</span>";
			$out .= "<span class='title'>" . htmlspecialchars($page->title) . "</span>";
			$out .= "<span class='desc'>" . htmlspecialchars($page->subtitle) . "</span>";
			$out .= "</a></li>\n";
		}
		$out .= "</ul>";
		if ($this->tag) {
			//$out .= "<p><a href='feed/$this->tag'>Feed for posts tagged '$this->tag'</a>";
		} else {
			$out .= "<p><a href='feed'>Subscribe to my feed</a>";
		}
		return $out;
	}
}
$page = new BlogPage('blog');

Cache::begin($page);
HtmlTemplate::write($page);

