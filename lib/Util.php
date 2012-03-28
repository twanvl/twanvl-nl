<?php

// -----------------------------------------------------------------------------
// Utility functions
// -----------------------------------------------------------------------------

class Util {
	// ---------------------------------------------------------------------
	// Redirect and other HTTP control
	// ---------------------------------------------------------------------
	
	// Redirects the user to $url.
	static function redirect($url) {
		if ($url == '') {
			$url = 'index.php';
		}
		if ($url{0} != '/') $url = Util::base_url() . $url;
		//header("HTTP/1.1 301 Moved Permanently");
		header("HTTP/1.1 302 Found");
		header("Location: $url");
		echo "This resource may be found at <a href=\"$url\">$url</a>.";
		exit();
	}
	
	static function set_last_modified($content_date) {
		$if_modified_since = @$_SERVER['HTTP_IF_MODIFIED_SINCE'];
		if ($if_modified_since && $content_date <= strtotime($if_modified_since)) {
			header('x',true,304);
			exit();
		} else {
			header('Last-Modified: ' . gmdate('r', $content_date));
		}
	}
	
	static function die_not_found($url,$msg='') {
		$page = Page::error_page_file_not_found($url,$msg);
		HtmlTemplate::write($page);
		exit();
	}
	
	// ---------------------------------------------------------------------
	// Base url, etc.
	// ---------------------------------------------------------------------
	
	static function current_url() {
		$script = pathinfo($_SERVER['SCRIPT_NAME'],PATHINFO_BASENAME);
		return $script . @$_SERVER['PATH_INFO'];
	}
	
	static function current_script_is($script) {
		return strpos($_SERVER['SCRIPT_NAME'],$script) !== false;
	}
	
	static function base_url() {
		$dirname = pathinfo($_SERVER["SCRIPT_NAME"], PATHINFO_DIRNAME);
		$server = $_SERVER["SERVER_NAME"];
		if ($_SERVER['SERVER_PORT'] != 80) {
			$server .= ':' . $_SERVER['SERVER_PORT'];
		}
		$base = 'http://' . $server . $dirname;
		if (substr($base,-1) != '/') $base .= '/';
		return $base;
	}
	
}
