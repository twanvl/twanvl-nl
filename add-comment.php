<?php

require_once('lib/bootstrap.inc');

// find page
$page = Resolver::find_page(@$_SERVER['PATH_INFO']);
$page->load();
if (!$page->show_comments) {
	Util::die_not_found('add-comment/' . @$_SERVER['PATH_INFO']);
}

// construct comment
$comment = new Comment();
$comment->id               = uniqid();
$comment->date             = gmstrftime('%Y-%m-%dT%H:%M:%SZ');
$comment->author_ip        = @$_SERVER['REMOTE_ADDR'];
$comment->author_name      = @$_REQUEST['author_name'];
$comment->author_url       = @$_REQUEST['author_url'];
$comment->author_email     = @$_REQUEST['author_email'];
$comment->author_subscribe = isset($_POST['author_subscribe']);
$comment->body             = @$_REQUEST['body'];

// set cookie for future posts
setcookie('author_name', @$_REQUEST['author_name'],  time()+60*60*24*365*2, '/');
setcookie('author_url',  @$_REQUEST['author_url'],   time()+60*60*24*365*2, '/');
setcookie('author_email',@$_REQUEST['author_email'], time()+60*60*24*365*2, '/');
setcookie('author_subscribe',isset($_POST['author_subscribe'])?1:0, time()+60*60*24*365*2, '/');

// is it valid?
global $invalid_fields;
if (strlen(@$_REQUEST['author_name']) < 3) {
	$invalid_fields['author_name'] = "Enter your name";
}
if (strlen(@$_REQUEST['author_email']) > 0 && strpos(@$_REQUEST['author_email'],'@') === false) {
	$invalid_fields['author_email'] = "Enter a valid email address";
}
if (strlen(@$_REQUEST['body']) < 4) {
	$invalid_fields['body'] = "Enter a message";
}
if ($comment->is_spam()) {
	$invalid_fields[''] = "Go away spammer!.";
}
if (!Captcha::is_answered()) {
	$invalid_fields['captcha'] = "Go away spammer!.";
}

$ok = count($invalid_fields) == 0;

// store
if ($ok) {
	$ok = Comments::add_comment($page->url, $comment);
}

// store and done
if ($ok) {
	// send email to subscribers
	$mail_subject = "Reply to blog post '$page->title'";
	$mail_from    = BLOG_TITLE . "<blog@twanvl.nl>";
	$mail_headers = "From: $mail_from\r\nReply-To: $mail_from";
	$mail_body    = "$comment->author_name has replied to a blog post *$page->title*, to which you are subscribed.\n\n";
	$mail_body   .= "Url: " . $page->full_url() . "#comment-".$comment->id."\n\n";
	$mail_body   .= "Message:\n" . $comment->body;
	foreach(Comments::get_subscribers($page->url) as $to) {
		if ($to == $comment->author_email) continue; // don't send mail to self
		mail($to, $mail_subject, $mail_body, $mail_headers);
	}
	
	Util::redirect($page->url . '#comment-' . $comment->id);
} else {
	HtmlTemplate::write($page);
}

