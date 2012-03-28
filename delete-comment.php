<?php

require_once('lib/bootstrap.inc');

Authentication::require_admin();

// find page
$page = Resolver::find_page(@$_SERVER['PATH_INFO']);
if (!$page->show_comments) {
	Util::die_not_found('delete comment/' . @$_SERVER['PATH_INFO']);
}

// hide or delete?
$hide_comment = true;

// find comment to delete
$commentid_to_delete = @$_REQUEST['comment'];
$comments = Comments::get_all($page->url,true);
$other_comments = array();
foreach ($comments as $c) {
	if ($c->id == $commentid_to_delete) {
		$comment_to_delete = $c;
		if ($hide_comment) {
			// hide instead
			$c2 = $c;
			$c2->visible = 0;
			$other_comments []= $c2;
		}
	} else {
		$other_comments []= $c;
	}
}
if (!isset($comment_to_delete) || count($other_comments) != count($comments) - ($hide_comment ? 0 : 1)) {
	Util::die_not_found("Comment to delete: $commentid_to_delete on page $page->url","<a href='$page->url#comments'>Take me back</a>");
}

if (!isset($_REQUEST['confirm'])) {
	$p = new Page('');
	$p->title = "Delete comment";
	$p->body = "Are you sure you want to delete this comment?";
	$p->body .= "<pre>" . htmlspecialchars(print_r($comment_to_delete,true)) . '</pre>';
	$p->body .= "<a href='$page->url#comment-$commentid_to_delete'>No, take me back</a> ";
	$p->body .= "<a href='delete-comment/$page->url?comment=".urlencode($commentid_to_delete)."&amp;confirm=1'>Yes, be gone.</a>";
	$p->body .= "<p><a href='delete-spam.php'>Delete all spam</a>";
	HtmlTemplate::write($p);
	exit();

} else {

	// store and done
	Comments::set_all($page->url,$other_comments);
	Util::redirect($page->url . "#new-comment");
}
