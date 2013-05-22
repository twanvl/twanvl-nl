<?php

// -----------------------------------------------------------------------------
// Comments
// -----------------------------------------------------------------------------

class Comments {
	/// Return all comments
	static function get_all($url, $include_spam = false, $include_unknown = true, $include_ham = true) {
		$lines = @file("comments/$url.wb",FILE_IGNORE_NEW_LINES);
		if (!$lines) return array();
		$comments = array();
		$comment  = null;
		$field    = null;
		foreach ($lines as $l) {
			if (empty($l)) {
				continue;
			} elseif (preg_match('@^--+\s*$@',$l)) {
				$comment = null;
			} elseif (($l[0] == "\t" || $l[0] == " ") && $comment && $field) {
				$comment->$field .= "\n" . substr($l,1);
			} elseif (preg_match('@([a-z _]+): ?(.*)@',$l,$ma)) {
				if (!$comment) {
					$comment = new Comment();
					$comments []= $comment;
				}
				$field = $ma[1];
				$comment->$field = $ma[2];
			}
		}
		foreach ($comments as $k => $c) {
			if (!$c->id) {
				$c->id = 'comment' . $k;
			}
			if (($c->spam_status == 1  && !$include_spam) || 
			    ($c->spam_status == 0  && !$include_unknown) || 
			    ($c->spam_status == -1 && !$include_ham)) {
				unset($comments[$k]);
			}
		}
		return $comments;
	}
	static function dependencies($url) {
		Cache::depend_on_file("comments/$url.wb");
	}
	
	/// Add a new comment
	static function add_comment($url, $comment) {
		$dir = dirname("comments/$url.wb");
		if (!is_dir($dir)) mkdir($dir,0777,true);
		$fp = fopen("comments/$url.wb", "ab");
		if (!$fp) return false;
		Comments::write_comment($fp,$comment);
		fclose($fp);
		return $comment;
	}
	/// Replace the list of comments
	static function set_all($url, $comments) {
		$dir = dirname("comments/$url.wb");
		if (!is_dir($dir)) mkdir($dir,0700,true);
		$fp = fopen("comments/$url.wb", "wb");
		if (!$fp) return false;
		foreach ($comments as $comment) {
			Comments::write_comment($fp,$comment);
		}
		fclose($fp);
		return true;
	}
	
	static function write_comment($fp,$comment) {
		foreach(get_object_vars($comment) as $k => $v) {
//			echo "[$k](",serialize_field($v),")<br>\n";
			fprintf($fp,"%s: %s\n",$k,serialize_field($v));
		}
		fprintf($fp,"----\n");
	}
	
	static function get_subscribers($comments) {
		if (!is_array($comments)) {
			$comments = Comments::get_all($comments);
		}
		$sums = array();
		$subs["blog-comments@twanvl.nl"] = 1;
		foreach ($comments as $c) {
			if ($c->author_email && $c->author_subscribe) {
				$subs[$c->author_email] = 1;
			}
		}
		return array_keys($subs);
	}
}

function serialize_field($x) {
	$x = str_replace("\r","",$x);
	return str_replace("\n","\n\t",$x);
}
