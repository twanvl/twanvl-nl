<?php

// -----------------------------------------------------------------------------
// Feeds that do a dir listing
// -----------------------------------------------------------------------------

class DirFeed extends Feed {
	function __construct($root, $tag=false) {
		$this->pages = Resolver::find_all_pages($root);
		$this->tag = $tag;
	}

	function dependencies() {
		foreach ($this->pages as $page) {
			$page->dependencies(); // TODO: don't depend on comments
		}
	}
	
	function do_load() {	
		foreach ($this->pages as $page) {
			$page->load();
			if (!$page->is_published) continue;
			if ($this->tag && !in_array($this->tag,$page->tags)) continue;
			$this->entries []= $page;
		}
	}
}

