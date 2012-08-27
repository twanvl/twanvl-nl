<?php

// -----------------------------------------------------------------------------
// Cache of a rendered page
// -----------------------------------------------------------------------------

class Cache {
	private static $last_modified = 0;
	private static $aborted = false;
	
	function disable() {
		Cache::$last_modified = 1e100;
		Cache::$aborted = true;
	}
	function depend_on_file($path) {
		Cache::depend_on_time(@filemtime($path));
	}
	function depend_on_time($time) {
		if ($time > Cache::$last_modified) Cache::$last_modified = $time;
	}
	
	static function begin($page) {
		// should we use caching?
		if (count($_POST) || count($_GET) || ($page->show_comments && count($_COOKIE))) {
			return;
		}
		// check dependencies
		$page->dependencies();
		
		// is there a cached version of this page somewhere?
		$cache_file = Cache::cache_file();
		$mtime = @filemtime($cache_file);

		// check for not-modified header?
		if ($mtime > 0 && $mtime >= Cache::$last_modified) {
			header('Content-Type: text/html; charset=utf-8');
			readfile($cache_file);
			exit();
		} else {
			// capture cache file
			ob_start(array('Cache','do_end'));
		}
	}

	// this function should be private, but it can't be, because it is used as a callback
	static function do_end($buffer,$mode) {
		// don't cache when there were errors
		if (Cache::$aborted) {
			return false;
		}
		$error = error_get_last();
		if ($error && $error['type'] & (E_PARSE | E_COMPILE_ERROR)) {
			return false;
		}
		// write contents to file
		chdir(dirname($_SERVER['SCRIPT_FILENAME']));
		file_put_contents(Cache::cache_file(),$buffer);
		// use gz_handler to output if possible
		if (headers_sent()||true) {
			return false;
		} else {
			return ob_gzhandler($buffer,$mode);
		}
	}
	
	private static function cache_file() {
		$file = Util::current_url();
		$file = str_replace('_','_u',$file);
		$file = str_replace('/','_s',$file);
		$file = str_replace('.','_d',$file);
		return CACHE_DIR . '/' . $file;
	}
}

