<?php

// -----------------------------------------------------------------------------
// CAPTCHA generator and checker
// 
// We send the plain text question and signed answers to the user.
// his plain text answer has to be among the signed answers.
// -----------------------------------------------------------------------------

class Captcha {
	public $question; // human readable question
	public $answers;  // plain text answers
	public $answer;   // hashed answer

	// construct a new captcha, or get one from the request variables
	function __construct($type = 'en') {
		if (Captcha::is_answered()) {
			$this->question = $_REQUEST['captcha_question'];
			$this->answer   = $_REQUEST['captcha_answer'];
		} else {
			$this->make_question($type);
			$this->answer = '';
			foreach ($this->answers as $a) {
				$this->answer .= Captcha::sign($a) . ',';
			}
		}
	}
	
	private function make_question($type) {
		if ($type == 'math') {
			// a math question
			$i = rand()%15;
			$j = rand()%15;
			$this->question = "$i + $j = ";
			//$this->question = format_number($i) . " + " . format_number($j) . " = ";
			$this->answers = array($i+$j);
		} else {
		  if ($type == 'nl') {
			  $qa = array(
				  array('q'=>'Noem een studentenklimvereniging in Nijmegen'
				       ,'a'=>array('nijsac')),
			  );
			} else {
			  $qa = array(
				  array('q'=>'What greek letter is usually used for anonymous functions?'
				       ,'a'=>array('lambda','lamda','λ')),
				  array('q'=>'Name of the lazy functional programming language I write about:'
				       ,'a'=>array('haskell','haskel','agda','coq','ml','ocaml')),
				  array('q'=>'Name a function of type <tt>(a -> b) -> ([a] -> [b])</tt>:'
				       ,'a'=>array('map','fmap','<$>','(<$>)')),
				  array('q'=>'Name a function of type <tt>[[a]] -> [a]</tt>:'
				       ,'a'=>array('concat','join','msum','mconcat','head','last')),
			  );
			}
			$i = rand()%count($qa);
			$this->question = $qa[$i]['q'];
			$this->answers = $qa[$i]['a'];
		}
	}
	
	static function is_answered() {
		$expected = @$_REQUEST['captcha_answer'];
		$actual = Captcha::sign(@$_REQUEST['captcha']);
		return strpos($expected, $actual) !== false;
	}
	
	static function sign($x) {
		return hash_hmac('sha256', CAPTCHA_SECRET, strtolower($x));
	}
}

global $numbers;
$numbers = array('zero','one','two','three','four','five','six','seven','eight','nine','ten','eleven','twelve','thirteen','fourteen','fifteen','sixteen');
function format_number($i) {
	global $numbers;
	return $numbers[$i];
}
