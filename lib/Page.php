<?php

// -----------------------------------------------------------------------------
// (Base) Class for pages
// -----------------------------------------------------------------------------

class Page {
	public $url;
	public $status_code = 200;
	public $title = SITE_TITLE;
	public $subtitle = '';
	public $body;
	public $tags = array();
	public $icon = '';
	public $category = ''; // what part of the site?
	public $is_published = true;
	public $show_comments = false;
	public $show_date = false;
	public $show_feedlink = false;
	public $last_modified = 0;
	public $source_link = '';
	private $loaded = false;
	
	function __construct($url) {
		$this->url = $url;
		$this->title = $url;
		$parts = explode('/',$url);
		$this->category = $parts[0];
		$this->title = $parts[count($parts)-1];
	}
	
	function full_url() {
		//return Util::base_url() . $this->url;
		return "http://twanvl.nl/" . $this->url;
	}
	
	function load() {
		if (!$this->loaded) {
			$this->loaded = true;
			$this->do_load();
		}
	}
	
	function dependencies() {}
	function do_load() {}
	
	// some simple pages
	
	static function error_page_unhandled_exception($e) {
		$page = new Page('');
		$page->title = "Unhandled exception";
		$page->body = $e->getMessage();
		$page->status_code = 500;
		return $page;
	}
	static function error_page_file_not_found($url,$msg='') {
		$page = new Page($url);
		$page->title = "File not found";
		$page->body = "<p class='face file-not-found'>The file <tt>".htmlspecialchars($url)."</tt> could not be found on this server.</p>$msg";
		$page->status_code = 404;
		return $page;
	}
}

