<?php

require_once('lib/bootstrap.inc');

// find and show page
$page = Resolver::find_page(@$_SERVER['PATH_INFO']);
Cache::begin($page);
HtmlTemplate::write($page);

