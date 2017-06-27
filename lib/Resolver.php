<?php

// -----------------------------------------------------------------------------
// Path resolver
// -----------------------------------------------------------------------------

class Resolver {
	static function find_page($url = '') {
		// clean up path
		$url = trim($url);
		if ($url[0] == '/') $url = substr($url,1);
		if (preg_match('@//|[.][.]|/[.][^/]*@',$url)) {
			return Page::error_page_file_not_found($url);
		}
		// trim trailing slash
		if (preg_match('@([/]+)$@',$url)) {
			$url = preg_replace('@([/]+)$@','',$url);
			// don't redirect, because apache adds these, and we would loop
		}
		// trim extension
		if (preg_match('@([/]*[.].*|[/]+)$@',$url)) {
			$url = preg_replace('@([/]*[.].*|[/]+)$@','',$url);
			//die($url);
			Util::redirect($url);
		}
		$url = trim($url);
		if ($url == '') {
			$url = 'index';
		}
		
		// options:
		/*
		1. a .php file -> include it
		2. a .txt file -> autoformat it
		3. a .lhs file -> autoformat it
		*/
		if (file_exists("$url.txt")) {
			return new TextFilePage($url,"./$url.txt");
		} else if (file_exists("$url.lhs")) {
			return new TextFilePage($url,"./$url.lhs");
		} else if (file_exists("$url.lagda")) {
			return new TextFilePage($url,"./$url.lagda");
		} else if (file_exists("$url.lagda.md")) {
			return new TextFilePage($url,"./$url.lagda.md");
		} else if (file_exists("$url/index.txt")) {
			return new TextFilePage($url,"./$url/index.txt");
		} else if (file_exists("$url/index.php")) {
			include("$url/index.php");
			return $page;
//		} else if (file_exists("$url.php")) {
//			return "x";
		} else {
			Cache::disable();
			return Page::error_page_file_not_found($url);
		}
	}
	
	static function find_all_pages($path) {
		// clean up path
		if (preg_match('@//|[.][.]|/[.][^/]*@',$path)) {
			return array();
		}
		// trim trailing slash
		if (preg_match('@([/]+)$@',$path)) {
			$path = preg_replace('@([/]+)$@','',$path);
		}
		$pages = array();
		Resolver::find_all_pages_($path,$pages);
		return $pages;
	}
	private static function find_all_pages_($path,&$pages) {
		foreach (new DirectoryIterator($path) as $file) {
			if ($file->isDot()) continue;
			if ($file->isDir()) {
				Resolver::find_all_pages_($file->getPathname(), $pages);
			} elseif (preg_match('@[.](txt|lhs|lagda)$@',$file->getFilename())) {
				$pages []= new TextFilePage('',$file->getPathname(),false);
			}
		}
	}
}
