<?php

// -----------------------------------------------------------------------------
// HTTP Digest authentication
// Based on http://php.net/manual/en/features.http-auth.php
// -----------------------------------------------------------------------------

function http_digest_parse($txt) {
	// protect against missing data
	$needed_parts = array('nonce'=>1, 'nc'=>1, 'cnonce'=>1, 'qop'=>1, 'username'=>1, 'uri'=>1, 'response'=>1);
	$data = array();
	$keys = implode('|', array_keys($needed_parts)) . '|realm';
	
	preg_match_all('@(' . $keys . ')=(?:([\'"])([^\2]+?)\2|([^\s,]+))@', $txt, $matches, PREG_SET_ORDER);
	
	foreach ($matches as $m) {
		$data[$m[1]] = $m[3] ? $m[3] : $m[4];
		unset($needed_parts[$m[1]]);
	}
	
	return $needed_parts ? false : $data;
}

class Authentication {
	// ---------------------------------------------------------------------
	// HTTP Authentication
	// ---------------------------------------------------------------------
	
	// Check authentication
	// if there is no user, return "guest"
	private static $user;
	static function user() {
		if (isset(Authentication::$user)) return Authentication::$user;
		if (!isset($_SERVER['PHP_AUTH_DIGEST'])) {
			return Authentication::$user = "guest";
		}
		if (!($data = http_digest_parse($_SERVER['PHP_AUTH_DIGEST']))) {
			Authentication::forbidden("Invalid authentication");
		}
		if (isset($data['realm']) && $data['realm'] != AUTH_REALM) {
			Authentication::forbidden("Invalid authentication"); // allow re-login
		}
		
		// generate the valid response
		$A1 = Authentication::password_for($data['username']);
		$A2 = md5($_SERVER['REQUEST_METHOD'].':'.$data['uri']);
		$valid_response = md5($A1.':'.$data['nonce'].':'.$data['nc'].':'.$data['cnonce'].':'.$data['qop'].':'.$A2);
		if ($data['response'] != $valid_response) {
			Authentication::forbidden("Incorrect username or password");
		}
		return Authentication::$user = $data['username'];
	}
	
	static function require_admin() {
		if (Authentication::user() == 'guest') {
			Authentication::forbidden();
		}
	}
	
	// set the username forcefully
	static function unsafe_set_user($username) {
		Authentication::$user = $username;
	}
	
	static function password_for($username) {
		$passes = file(AUTH_PASSWORD_FILE);
		foreach($passes as $line) {
			list($uname,$urealm,$upass) = explode(':',$line);
			if ($uname == $username && $urealm == AUTH_REALM) return trim($upass);
		}
		Authentication::forbidden("Incorrect username or password");
	}
	
	// ---------------------------------------------------------------------
	// Pages
	// ---------------------------------------------------------------------
	
	// Ask for authentication
	static function forbidden($msg = '') {
		if (AUTH_METHOD == 'Digest') {
			header('WWW-Authenticate: Digest realm="'.AUTH_REALM.'",qop="auth",nonce="'.uniqid().'",opaque="'.md5(AUTH_REALM).'"');
		} else {
			header('WWW-Authenticate: Basic realm="'.AUTH_REALM.'"');
		}
		$page = new Page('');
		$page->title = "Authentication required";
		$page->body = "Authentication is required to access this page." . ($msg ? "\n$msg" : "");
		$page->status_code = 401;
		HtmlTemplate::write($page);
		exit();
	}
	
}
