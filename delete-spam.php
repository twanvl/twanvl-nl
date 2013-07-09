<?php

require_once('lib/bootstrap.inc');

Authentication::require_admin();
set_time_limit(0);

// Settings
$max_age = time() - 30 * 24 * 60 * 60; // show only comments no older than this

// Show a list of all comments
$p = new Page('');
$p->title = "Re spam-filter";
$p->body = '';

// Do actual deletion/hiding
if (isset($_POST['confirm'])) {
	// Delete/hide spam
	$total_change = array(-1 => 0, 0 => 0, 1 => 0);
	foreach (Resolver::find_all_pages('blog') as $page) {
		$comments = Comments::get_all($page->url,true);
		$page_change = false;
		foreach ($comments as $comment) {
			if (isset($_POST[urlencode("spam_status-" . $page->url . '#' . $comment->id)])) {
				$new_spam_status = (int)$_POST[urlencode("spam_status-" . $page->url . '#' . $comment->id)];
				if ($comment->spam_status != $new_spam_status) {
					$comment->spam_status = $new_spam_status;
					$page_change = true;
					$total_change[$new_spam_status]++;
				}
			}
		}
		if ($page_change) {
			Comments::set_all($page->url,$comments);
		}
	}
	$p->body .= "Hidden ${total_change[1]} comments, ";
	$p->body .= "Revealed ${total_change[-1]} comments.";
}

// Build HTML table of all yet-to-be classified comments
$num_new_spam = 0;
$more = 0;
$coms = array();
foreach (Resolver::find_all_pages('blog') as $page) {
	$comments = Comments::get_all($page->url,true);
	///$com_body = array();
	foreach ($comments as $comment) {
		// show this comment?
		if (isset($_REQUEST['all'])) {
		} elseif (isset($_REQUEST['nonspam'])) {
			if ($comment->spam_status == 1) continue; // show all non-spam
		} else {
			if ($comment->spam_status != 0) continue; // comment has already got a spam status
		}
		
		// current status
		$classified_spam = $comment->is_spam();
		$is_spam = ($comment->spam_status==1 ? 'spam' : ($comment->spam_status==-1 ? 'ham' : 'unknown')) . ' ' .
		           ($classified_spam ? 'classified-spam' : 'classified-ham');
		
		if (isset($_REQUEST['incorrect'])) {
			if (($classified_spam && $comment->spam_status == 1) || (!$classified_spam && $comment->spam_status == -1)) continue;
		}
		
		{
			$id = $page->url . '#' . $comment->id;
			$coms []= (object)array(
				'date' => $comment->date,
				'html' => "<tr>"
					//. "<td><input type='checkbox' title='delete this comment' name='".urlencode("del-$id")."'".($comment->is_spam()?" checked":'').">"
					. "<td><div style='background:#fdd;'><input type='radio' title='spam'      name='".urlencode("spam_status-$id")."' value='1'".($comment->spam_status==1?" checked":'')."></div>"
					.     "<div style='background:#ffd;'><input type='radio' title='undecided' name='".urlencode("spam_status-$id")."' value='0'".($comment->spam_status==0?" checked":'')."></div>"
					.     "<div style='background:#dfd;'><input type='radio' title='not spam'  name='".urlencode("spam_status-$id")."' value='-1'".($comment->spam_status==-1?" checked":'')."></div>"
					. "<td><a href='".htmlspecialchars($page->url)."'>" . $page->title . '</a>'
					. "<td class='$is_spam summary'>" . htmlspecialchars($comment->author_name)
						. ', ' . htmlspecialchars($comment->author_email)
						. ', ' . htmlspecialchars($comment->author_url)
						. ', ' . htmlspecialchars($comment->author_ip)
					. "<td class='$is_spam summary'>" . htmlspecialchars(substr($comment->body,0,100))
					. "<td class='$is_spam summary'>" . $comment->date
					. "<td class='$is_spam summary'>" . $comment->spam_score()
			);
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
$p->body .= "<input type='hidden' name='confirm' value='1'>";
$p->body .= "<input type='submit' value='Be gone'></form>";
HtmlTemplate::write($p);

