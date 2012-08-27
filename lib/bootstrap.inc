<?php

// -----------------------------------------------------------------------------
// Bootstrapping code
// -----------------------------------------------------------------------------

function __autoload($class_name) {
	require_once "lib/$class_name.php";
}

// shut up warnings about the timezone
@date_default_timezone_set(@date_default_timezone_get());

// UTF-8 all the way
mb_internal_encoding("UTF-8");

require_once('./lib/config.php');

// -----------------------------------------------------------------------------
// Fancy templates need fancy error messages
// -----------------------------------------------------------------------------

function fancy_exception_handler($e) {
    Cache::disable();
	HtmlTemplate::write(Page::error_page_unhandled_exception($e));
	exit();
}

set_exception_handler('fancy_exception_handler');
error_reporting(E_ALL);

