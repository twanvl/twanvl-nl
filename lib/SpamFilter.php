<?php

// -----------------------------------------------------------------------------
// Naive Bayesian spam filter
// -----------------------------------------------------------------------------

class SpamFilter {
	static function make($type) {
		if ($type == 'naive_bayes') {
			return new NaiveBayesClassifier();
		} elseif ($type == 'svm' || $type == 'logistic_regression') {
			return new LinearClassifier($type);
		} else {
			throw "Unknown classifier type";
		}
	}
	
	static function load() {
		return unserialize(file_get_contents(SPAM_FILTER_FILE));
	}
	
	static function get() {
		static $f;
		if (!isset($f)) $f = SpamFilter::load();
		return $f;
	}
	
	static function save($classifier) {
		file_put_contents(SPAM_FILTER_FILE, serialize($classifier));
	}
}

