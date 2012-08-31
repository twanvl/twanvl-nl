<?php

require_once('lib/bootstrap.inc');

Authentication::require_admin();

// Show a list of all comments
$p = new Page('');
$p->title = "Re spam filter";
$p->body = '';

// Do actual deletion/hiding
if (isset($_POST['confirm'])) {
	// Delete/hide spam
	$total_change = 0;
	foreach (Resolver::find_all_pages('blog') as $page) {
		$comments = Comments::get_all($page->url,true);
		$change = false;
		foreach ($comments as $comment) {
			if ($comment->is_spam() && $comment->visible) {
				$comment->visible = false;
				$change = true;
				$total_change++;
			}
		}
		Comments::set_all($page->url,$comments);
	}
	$p->body .= "Hidden $total_change comments";
}

// Build HTML table of all comments
$num_new_spam = 0;

$p->body .= '<table class="spam">';
foreach (Resolver::find_all_pages('blog') as $page) {
	$comments = Comments::get_all($page->url,true);
	$com_body = array();
	foreach ($comments as $comment) {
		$is_spam = ($comment->visible ? 'visible' : 'hidden') . ' ' .
		           ($comment->is_spam() ? 'spam' : 'nonspam');
		if ($comment->visible || !$comment->is_spam()) {
			$com_body [] =
				"<td class='$is_spam summary'>" . htmlspecialchars($comment->author_name) . ', ' . htmlspecialchars($comment->author_email) . ', ' . htmlspecialchars($comment->author_url) . ', ' . htmlspecialchars($comment->author_ip) .
				"<td class='$is_spam summary'>" . htmlspecialchars(substr($comment->body,0,100));
				//"<td class='$is_spam summary'>" . $comment->body_html();
		}
        if ($comment->visible && $comment->is_spam()) {
            $num_new_spam++;
        }
	}
	if ($com_body) {
		$p->body .= "<tr class='first'><td rowspan='".count($com_body)."'><a href='".htmlspecialchars($page->url)."'>" . $page->title . '</a>';
		$p->body .= implode('<tr>',$com_body);
	}
}
$p->body .= '</table>';

$p->body .= '<p>';
$p->body .= "<form method='post' action='delete-spam.php'><input type='hidden' name='confirm' value='1'><input type='submit' value='Be gone, all $num_new_spam of you.'></form>";
HtmlTemplate::write($p);

