<?php

require_once('lib/bootstrap.inc');

Authentication::require_admin();
set_time_limit(0);

// Settings
$max_age = time() - 30 * 24 * 60 * 60; // all recent comments are spam
$min_age = time() - 0.5 * 24 * 60 * 60; // except more recent than this
$num_folds = 5;
$classifier = 'logistic_regression';

// Show a list of all comments
$p = new Page('');
$p->title = "Training/testing of spamfilter";
$p->body = '';

// Get all comments
$comments = array();
foreach (Resolver::find_all_pages('blog') as $page) {
	$page_comments = Comments::get_all($page->url,true);
	foreach ($page_comments as $comment) {
		$comment->attrs = $comment->spamfilter_attributes();
		if (strtotime($comment->date) >= $min_age) {
			$comment->label = 0.5; // unknown
		} elseif (strtotime($comment->date) >= $max_age || !$comment->visible) {
			$comment->label = 1; // spam
		} else {
			$comment->label = 0; // not spam
		}
		$comments []= $comment;
	}
}

// Cross-validation set
srand(12345);
shuffle($comments);
for ($fold = 0 ; $fold < $num_folds ; ++$fold) {
	// train
	$data = array();
	for ($i = 0 ; $i < count($comments) ; ++$i) {
		if ($i % $num_folds == $fold) continue;
		if ($comments[$i]->label == 0.5) continue;
		$data []= $comments[$i];
	}
	$filter = SpamFilter::make($classifier);
	$filter->train($data);
	
	// test
	for ($i = 0 ; $i < count($comments) ; ++$i) {
		if ($i % $num_folds != $fold) continue;
		$comment = $comments[$i];
		$comment->score = $filter->classify($comment->attrs);
		// which features influence the score?
		$influence = array();
		foreach($comment->attrs as $k => $v) {
			if (!$v) continue;
			$w = $filter->weight($k);
			if ($w) $influence[$k] = $w;
		}
		asort($influence);
		$comment->influence = $influence;
	}
}
function compare_score($a,$b) {
    return $a->score == $b->score ? 0 : $a->score < $b->score ? 1 : -1;
}
usort($comments,'compare_score');

// Calculate AUC
$num_spam = 0;
$num_ham  = 0;
$error = 0;
foreach($comments as $comment) {
	if ($comment->label == 1) {
		$num_spam++;
		$error += $num_ham;
	} elseif ($comment->label == 0) {
		$num_ham++;
	}
}
$auc = 1 - ($error) / ($num_spam*$num_ham);
$p->body .= "AUC: $auc,  errors: $error\n";

// Build HTML table of all comments
$p->body .= '<h2>Order by spam score</h2>';
$p->body .= '<table class="spam">';
foreach ($comments as $comment) {
	$is_spam = $comment->label==0 ? 'was-visible' : ($comment->label==0.5 ? 'was-unknown' : 'was-hidden');
	$influence = '';
	foreach($comment->influence as $k => $s) {
		$influence .= htmlspecialchars($k).":$s\n";
	}
	$p->body .=
		"<tr>"
		//. "<td><a href='".htmlspecialchars($page->url)."'>" . $page->title . '</a>'
		. "<td class='$is_spam'>" . htmlspecialchars($comment->author_name)
			. ', ' . htmlspecialchars($comment->author_email)
			. ', ' . htmlspecialchars($comment->author_url)
			. ', ' . htmlspecialchars($comment->author_ip)
		. "<td class='$is_spam'>" . htmlspecialchars(substr($comment->body,0,100))
		. "<td class='$is_spam'>" . $comment->date
		. "<td class='$is_spam'>" . $comment->score
		//. "<td class='$is_spam'>" . $influence
	;
}
$p->body .= '</table>';

// Train on all data
$filter = SpamFilter::make($classifier);
$data = array();
for ($i = 0 ; $i < count($comments) ; ++$i) {
	if ($comments[$i]->label == 0.5) continue;
	$data []= $comments[$i];
}
$filter->train($data);

// Store trained spamfilter
SpamFilter::save($filter);

// Best/worst spam attributes
$attrs = array();
$s = 1; $n = 2;
foreach ($filter->all_weights() as $k => $v) {
	$attrs[]=(object)array(
		'score' => $v,
		'html'  => "<tr><td>".htmlspecialchars($k)."<td>$v"
	);
}

usort($attrs,'compare_score');
$p->body .= '<h2>Spam/ham attributes</h2>';
$p->body .= '<table class="spam">';
foreach ($attrs as $attr) {
	$p->body .= $attr->html;
}
$p->body .= '</table>';



HtmlTemplate::write($p);


