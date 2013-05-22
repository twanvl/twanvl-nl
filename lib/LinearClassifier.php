<?php

// -----------------------------------------------------------------------------
// Linear classifier to use for spam filter
// -----------------------------------------------------------------------------

class LinearClassifier {
	var $bias = 0;
	var $weights = array();
	var $weight_mul = 1.0;
	// parameters for training
	var $num_epochs = 100;
	//var $learning_rate = array(0.01,0.01);//array(0.02,0.005);
	var $type;
	var $learning_rate_bias = 0.1;
	var $lambda = 0.001; // regularization parameter
	var $t = 0;
	var $eta0 = 0.01;
	var $learning_rate_decay = 0.1;
	var $min_occur = 15; // minimum occurence count for features to be considered
	var $balance_classes = true;

	function __construct($type = 'svm') {
		$this->type = $type;
	}
	
	function train($data) {
		// feature selection
		$this->feature_selection($data);
		// train with stochastic gradient descend
		$this->train_loss = array();
		for ($i = 0 ; $i < $this->num_epochs ; ++$i) {
			$loss = $this->train_epoch($data);
			$this->train_loss[$i] = $loss;
			//echo "Epoch $i, loss {$loss} <br>\n";
		}
	}
	
	function feature_selection($data) {
		// count occurence of features per class
		$features = array();
		foreach ($data as $item) {
			foreach ($item->attrs as $k => $v) {
				if (!$v) continue;
				if (!isset($features[$k])) $features[$k] = array(0,0);
				$features[$k][$item->label]++;
			}
		}
		// select only features with at least $this->min_occur occurences
		foreach ($features as $k => $v) {
			$n = $v[0] + $v[1];
			$entropy = ($v[0] ? -$v[0]/$n * log($v[0]/$n) : 0) + ($v[1] ? -$v[1]/$n * log($v[1]/$n) : 0);
			//echo "{$v[0]} + {$v[1]} = $n => $entropy<br>\n";
			//if ($n >= 1 && $entropy <= 0.5) {
			if ($n >= $this->min_occur) {
				$this->weights[$k] = 0;
			}
		}
		//echo "Selected ",count($this->weights)," features out of ",count($features),"<br>\n";
	}
	
	function train_epoch($data) {
		if ($this->balance_classes) {
			$label_count = array(array(),array());
			foreach ($data as $i => $item) {
				$label_count[$item->label][] = $i;
			}
			$n0 = count($label_count[0]);
			$n1 = count($label_count[1]);
			while ($n0 < $n1) {
				$data[] = $data[$label_count[0][rand(0,count($label_count[0])-1)]];
				$n0++;
			}
		}
		shuffle($data);
		
		$loss = 0;
		foreach ($data as $item) {
			$lr = $this->eta0 / (1 + $this->learning_rate_decay * $this->lambda * $this->eta0 * $this->t);
			$loss += $this->train1($item, $lr);
			$this->t++;
		}
		return $loss;
	}
	
	function train1($item, $lr) {
		// select features with probability $p
		$p = 1.0;
		$attrs = array();
		foreach ($item->attrs as $k => $v) {
			if ($v && rand(0,1000-1) < 1000*$p) {
				$attrs[$k] = $v;
			}
		}
		$y = $item->label*2 - 1;
		$z = $this->do_classify($attrs);
		// use square hinge loss
		if ($this->type == 'svm') {
			if ($y*$z > 1) {
				$dl = $loss = 0.0;
			} else {
				$a = 1 - $y*$z;
				$loss = 0.5 * $a * $a;
				$dl = $y * $a;
			}
		} elseif ($this->type == 'logistic_regression') {
			$loss = log(1 + exp(-$y*$z));
			$dl = $y / (1 + exp($y*$z));
		} else {
			throw "Unknown type";
		}
		// update weights and bias
		if ($dl != 0) {
			$dw = $lr * $dl / $this->weight_mul;
			foreach ($attrs as $k => $v) {
				if (!$v) continue;
				if (!isset($this->weights[$k])) {
					//$this->weights[$k] = 0.; // add new item
					continue; // feature not used
				}
				$this->weights[$k] += $dw;
			}
			$this->bias += $lr * $this->learning_rate_bias * $dl;
		}
		// regularization
		$this->weight_mul *= 1 - $lr * $this->lambda;
		$this->bias *= 1 - $lr * $this->learning_rate_bias * $this->lambda;
		if ($this->weight_mul < 1e-5 || $this->weight_mul > 1e5) {
			//echo "Renormalizing {$this->weight_mul} <br>\n";
			// renormalize
			foreach ($this->weights as $k => $v) {
				$this->weights[$k] = $v * $this->weight_mul;
			}
			$this->weight_mul = 1.0;
		}
		//
		return $loss;
	}
	
	private function do_classify($attrs) {
		$sum = 0.0;
		foreach ($attrs as $k => $v) {
			if ($v && isset($this->weights[$k])) {
				$sum += $this->weights[$k];
			}
		}
		return $sum * $this->weight_mul + $this->bias;
	}
	function classify($attrs) {
		if ($this->type == 'logistic_regression') {
			return 1 / (1 + exp(-$this->do_classify($attrs)));
		} else {
			return $this->do_classify($attrs);
		}
	}

	function weight($k) {
		return isset($this->weights[$k]) ? $this->weights[$k] * $this->weight_mul : 0;
	}
	function all_weights() {
		$out = array();
		$out['_bias'] = $this->bias;
		foreach ($this->weights as $k => $v) {
			$out[$k] = $v * $this->weight_mul;
		}
		return $out;
	}
}

