<?php

require_once('lib/bootstrap.inc');

// find all pages

$tag = @$_REQUEST['tag'];

$feed = new DirFeed('blog',$tag);
Cache::begin($feed);
$feed->write();

