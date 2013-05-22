<?php

require_once('lib/bootstrap.inc');

Authentication::require_admin();
set_time_limit(0);
ini_set('memory_limit', 1024 * 1024 * 800);

// Settings
$max_age = time() - 0 * 24 * 60 * 60; // all recent comments are spam
$min_age = time() - 0 * 24 * 60 * 60; // except more recent than this
$num_folds = 5;
$classifier = 'logistic_regression';
//$classifier = 'naive_bayes';

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
		if (strtotime($comment->date) >= $min_age || $comment->spam_status == 0) {
			$comment->label = 0.5; // unknown
		} elseif (strtotime($comment->date) >= $max_age || $comment->spam_status == 1) {
			$comment->label = 1; // spam
		} else {
			$comment->label = 0; // not spam
		}
		$comments []= $comment;
	}
}

// -----------------------------------------------------------------------------
// Cross validate to estimate performance
// -----------------------------------------------------------------------------

$validate = !isset($_POST['store']);
$show_attributes = $validate;

if ($validate) {
    // Cross-validation set
    srand(123456);
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
    $prev_score = 0;
    foreach ($comments as $comment) {
	    $is_spam = $comment->label==0 ? 'was-ham' : ($comment->label==0.5 ? 'was-unknown' : 'was-spam');
	    $influence = '<table>';
	    foreach($comment->influence as $k => $s) {
		    $influence .= '<tr><td>'.htmlspecialchars($k)."<td>$s\n";
	    }
	    $influence .= '</table>';

	    if ($comment->score <= SPAM_THRESHOLD && $prev_score >= SPAM_THRESHOLD) {
	        $is_spam .= ' first-past-threshold';
	    }
	    $prev_score = $comment->score;
	
	    $p->body .=
		    "<tr class='$is_spam'>"
		    //. "<td><a href='".htmlspecialchars($page->url)."'>" . $page->title . '</a>'
		    . "<td>" . htmlspecialchars($comment->author_name)
			    . ', ' . htmlspecialchars($comment->author_email)
			    . ', ' . htmlspecialchars($comment->author_url)
			    . ', ' . htmlspecialchars($comment->author_ip)
		    . "<td>" . htmlspecialchars(substr($comment->body,0,100))
			    //. '<div class="influence">' . $influence . '</div>' // large!
		    . "<td>" . $comment->date
		    . "<td>" . $comment->score
	    ;
    }
    $p->body .= '</table>';
}

// -----------------------------------------------------------------------------
// Train on all data
// -----------------------------------------------------------------------------

// Train on all data
$filter = SpamFilter::make($classifier);
$data = array();
for ($i = 0 ; $i < count($comments) ; ++$i) {
	if ($comments[$i]->label == 0.5) continue;
	$data []= $comments[$i];
}
$filter->train($data);

// Store trained spamfilter
$p->body .= '<h2>Store spamfilter</h2>';
if (isset($_POST['store'])) {
    SpamFilter::save($filter);
    $p->body .= "Spamfilter was stored successfully";
} else {
    $p->body .= '<form action="traintest-spamfilter.php" method="post">';
    $p->body .= '<input type="hidden" name="store" value="1">';
    $p->body .= '<input type="submit" value="Store spamfilter">';
    $p->body .= '</form>';
}

// List best/worst spam attributes
if ($show_attributes) {
	$counts = array('_bias'=>array(0,0));
	foreach ($data as $item) {
		foreach ($item->attrs as $k => $v) {
			if (!$v) continue;
			if (!isset($counts[$k])) $counts[$k] = array(0,0);
			$counts[$k][$item->label]++;
		}
		$counts['_bias'][$item->label]++;
	}
	
	$attrs = array();
	$s = 1; $n = 2;
	foreach ($filter->all_weights() as $k => $v) {
		$attrs[]=(object)array(
			'score' => $v,
			'html'  => "<tr><td>".htmlspecialchars($k)."<td>$v<td>{$counts[$k][0]}<td>{$counts[$k][1]}"
		);
	}
	
	usort($attrs,'compare_score');
	$p->body .= '<h2>Spam/ham attributes</h2>';
	$p->body .= '<table class="spam">';
	foreach ($attrs as $attr) {
		$p->body .= $attr->html;
	}
	$p->body .= '</table>';
}

// graph of training error
if (isset($filter->train_loss)) {
	$p->body .= '<h2>Training error</h2>';
	$p->body .= <<<EOF
		<script type="text/javascript" src="https://www.google.com/jsapi"></script>
		<script type="text/javascript">
		google.load('visualization', '1.0', {'packages':['corechart']});
		google.setOnLoadCallback(drawChart);
		function drawChart() {
			// Create the data table.
			var data = google.visualization.arrayToDataTable([["Epoch","Loss"]
EOF;
	foreach ($filter->train_loss as $epoch => $loss) {
		$p->body .= ",[$epoch,$loss]";
	}
	$p->body .= <<<EOF
			]);
			var options = {'title':'Training error', 'width':650, 'height':300};
			var chart = new google.visualization.LineChart(document.getElementById('training_loss'));
			chart.draw(data, options);
		}
		</script>
		<div id="training_loss" style="width:650px; height:300px"></div>
EOF;
}

HtmlTemplate::write($p);


