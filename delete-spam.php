<?php

require_once('lib/bootstrap.inc');

Authentication::require_admin();

// Settings
$max_age = time() - 30 * 24 * 60 * 60; // show only comments no older than this

// Show a list of all comments
$p = new Page('');
$p->title = "Re spam-filter";
$p->body = '';

// Do actual deletion/hiding
if (isset($_POST['confirm'])) {
	// Delete/hide spam
	$total_change = 0;
	foreach (Resolver::find_all_pages('blog') as $page) {
		$comments = Comments::get_all($page->url,true);
		$page_change = false;
		foreach ($comments as $comment) {
			if (isset($_POST[urlencode("del-" . $page->url . '#' . $comment->id)]) && $comment->visible) {
				$comment->visible = false;
				$page_change = true;
				$total_change++;
			}
		}
		if ($page_change) {
			Comments::set_all($page->url,$comments);
		}
	}
	$p->body .= "Hidden $total_change comments";
}

// Build HTML table of all comments
$num_new_spam = 0;
$more = 0;
$coms = array();
foreach (Resolver::find_all_pages('blog') as $page) {
	$comments = Comments::get_all($page->url,true);
	///$com_body = array();
	foreach ($comments as $comment) {
		$is_spam = ($comment->visible ? 'visible' : 'hidden') . ' ' .
		           ($comment->is_spam() ? 'spam' : 'nonspam');
		if (($comment->visible || !$comment->is_spam())) {
		    if ($comment->visible == $comment->is_spam() || strtotime($comment->date) >= $max_age) {
				$coms []= (object)array(
		            'date' => $comment->date,
		            'html' => "<tr>"
			            . "<td><input type='checkbox' title='delete this comment' name='".urlencode("del-" . $page->url . '#' . $comment->id)."'".($comment->is_spam()?" checked":'').">"
			            . "<td><a href='".htmlspecialchars($page->url)."'>" . $page->title . '</a>'
				        . "<td class='$is_spam summary'>" . htmlspecialchars($comment->author_name)
							. ', ' . htmlspecialchars($comment->author_email)
							. ', ' . htmlspecialchars($comment->author_url)
							. ', ' . htmlspecialchars($comment->author_ip)
				        . "<td class='$is_spam summary'>" . htmlspecialchars(substr($comment->body,0,100))
						. "<td class='$is_spam summary'>" . $comment->date
						. "<td class='$is_spam summary'>" . $comment->spam_score()
				);
			} else {
				$more++;
			}
		}
        if ($comment->visible && $comment->is_spam()) {
            $num_new_spam++;
        }
	}
}

function compare_date($a,$b) {
    return $a->date == $b->date ? 0 : $a->date < $b->date ? 1 : -1;
}
usort($coms,'compare_date');

$p->body .= "<form method='post' action='delete-spam.php'>";
$p->body .= '<table class="spam">';
foreach ($coms as $com) {
    $p->body .= $com->html;
}
$p->body .= '</table>';
$p->body .= "Plus $more older comments.";

$p->body .= '<p>';
//$p->body .= "<label><input type='checkbox' name='delete-auto' checked> delete all $num_new_spam that are now classified as spam</label><br>";
$p->body .= "<input type='hidden' name='confirm' value='1'>";
$p->body .= "<input type='submit' value='Be gone'></form>";
HtmlTemplate::write($p);

