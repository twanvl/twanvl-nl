<?php

// -----------------------------------------------------------------------------
// CAPTCHA generator and checker
// -----------------------------------------------------------------------------

class Captcha {
	public $question; // human readable question
	public $answer;   // hashed answer

	// construct a new captcha, or get one from the request variables
	function __construct() {
		if (Captcha::is_answered()) {
			$this->question = $_REQUEST['captcha_question'];
			$this->answer   = $_REQUEST['captcha_answer'];
		} else {
			$i = rand()%15;
			$j = rand()%15;
			$this->question = "$i + $j = ";
			//$this->question = format_number($i) . " + " . format_number($j) . " = ";
			$this->answer = $i+$j;
			$this->answer = sha1(CAPTCHA_SECRET . $this->answer);
		}
	}

	static function is_answered() {
		return sha1(CAPTCHA_SECRET . @$_REQUEST['captcha']) == @$_REQUEST['captcha_answer'];
	}
}

global $numbers;
$numbers = array('zero','one','two','three','four','five','six','seven','eight','nine','ten','eleven','twelve','thirteen','fourteen','fifteen','sixteen');
function format_number($i) {
	global $numbers;
	return $numbers[$i];
}
