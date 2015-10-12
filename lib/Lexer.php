<?php

// -----------------------------------------------------------------------------
// A generic lexer
// -----------------------------------------------------------------------------

class Lexer {
	private $rules;
	private $str, $offset = 0, $len;
	
	function __construct($rules,$str) {
		$this->rules = $rules;
		$this->str = $str;
		$this->len = strlen($str);
	}
	
	function next() {
		foreach ($this->rules as $name => $rule) {
			if (preg_match($rule,$this->str,$ma,PREG_OFFSET_CAPTURE,$this->offset) && $ma[0][1] == $this->offset) {
				$this->offset += strlen($ma[0][0]);
				return array($name,$ma[0][0]);
			}
		}
		// failed to match any
		return array('',$this->str[$this->offset++]);
	}
	function end() {
		return $this->offset >= $this->len;
	}
}

