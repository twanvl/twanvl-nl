<?php

// -----------------------------------------------------------------------------
// Naive Bayesian classifier for spam filter
// -----------------------------------------------------------------------------

class NaiveBayesClassifier {
	// $counts[class][attr] = #of items in class that have attribute
	var $counts;
	var $total_counts;
	var $class_counts;
	// parameters
	var $smoothing = 1;

	function __construct() {
		$this->counts = array(array(),array());
		$this->total_counts = array();
		$this->class_counts = array(0,0);
	}

	function train($data) {
		foreach ($data as $item) {
			$this->add($item->attrs, $item->label);
		}
		$this->drop_rare_stuff(0);
	}
	
	function add($attrs, $class) {
		$this->class_counts[$class]++;
		foreach($attrs as $k => $v) {
			if ($v) {
				if (!isset($this->counts[$class][$k])) $this->counts[$class][$k] = 0;
				if (!isset($this->total_counts[$k])) $this->total_counts[$k] = 0;
				$this->counts[$class][$k]++;
				$this->total_counts[$k]++;
			}
		}
	}
	
	function drop_rare_stuff($min_count = 2) {
		$new_tc = array();
		$new_c  = array(array(),array());
		foreach($this->total_counts as $k => $v) {
			if ($v >= $min_count) {
				$new_tc[$k] = $v;
				foreach($this->counts as $class => $cc) {
					$new_c[$class][$k] = isset($this->counts[$class][$k]) ? $this->counts[$class][$k] : 0;
				}
			}
		}
		$this->total_counts = $new_tc;
		$this->counts = $new_c;
	}

	// Return log-likelihoods of message being in a certain class
	function classify_as($attrs, $class = 1) {
		$score = 0.0; // prior, log P(spam)
		$n = count($this->class_counts); // number of classes
		foreach($attrs as $k => $v) {
			if (!$v) continue;
			if (!isset($this->total_counts[$k])) continue;
			$ct = $this->total_counts[$k];
			$cc = isset($this->counts[$class][$k]) ? $this->counts[$class][$k] : 0;
			// P(attr|spam) = #(attr|spam)+s / #(attr)+2s
			$score += log(($cc + $this->smoothing) / ($ct + $n * $this->smoothing));
		}
		return $score; // P(spam|attrs) = P(spam) * P(attrs|spam)
	}
	
	function classify($attrs) {
		$s0 = $this->classify_as($attrs,0);
		$s1 = $this->classify_as($attrs,1);
		return ($s0 - $s1) / ($s1 + $s0);
	}

	function weight($k, $class = 1) {
		if (!isset($this->total_counts[$k])) return 0;
		$n = count($this->class_counts);
		$ct = $this->total_counts[$k];
		$cc = isset($this->counts[$class][$k]) ? $this->counts[$class][$k] : 0;
		$s1 = log(($cc + $this->smoothing) / ($ct + $n * $this->smoothing));
		$s0 = log(($ct - $cc + $this->smoothing) / ($ct + $n * $this->smoothing));
		return ($s0 - $s1);
	}

	function all_weights() {
		$out = array();
		foreach ($this->total_counts as $k => $v) {
			$out[$k] = $this->weight($k);
		}
		return $out;
	}
}

