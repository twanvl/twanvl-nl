<?php

// -----------------------------------------------------------------------------
// Comments
// -----------------------------------------------------------------------------

function detectUTF8($string){
	// from http://nl.php.net/manual/en/function.iconv.php
	return preg_match('%(?:
	[\xC2-\xDF][\x80-\xBF]             # non-overlong 2-byte
	|\xE0[\xA0-\xBF][\x80-\xBF]        # excluding overlongs
	|[\xE1-\xEC\xEE\xEF][\x80-\xBF]{2} # straight 3-byte
	|\xED[\x80-\x9F][\x80-\xBF]        # excluding surrogates
	|\xF0[\x90-\xBF][\x80-\xBF]{2}     # planes 1-3
	|[\xF1-\xF3][\x80-\xBF]{3}         # planes 4-15
	|\xF4[\x80-\x8F][\x80-\xBF]{2}     # plane 16
	)+%xs', $string);
}

class Comment {
	public $id = 0;
	public $date;
	public $author_name;
	public $author_url;
	public $author_email;
	public $author_ip;
	public $author_subscribe = true;
	public $body_type; // html or wiki
	public $body;
	public $spam_status = 0; // 0 : unknown, -1: not spam, +1: spam
	
	function __construct() {
		$this->body_type = 'wiki';
	}
	
	function is_spam() {
		return $this->is_spam_heuristic();
		//return $this->is_spam_akismet();
	}
	
	function is_spam_akismet() {
		$akismet = new Akismet(BLOG_URL,AKISMET_API_KEY);
		$akismet->setCommentAuthor($this->author_name);
		$akismet->setCommentAuthorEmail($this->author_email);
		$akismet->setCommentAuthorURL($this->author_url);
		$akismet->setCommentContent($this->body);
		return $akismet->isCommentSpam();
	}
	
	// simple spam filter
	function is_spam_heuristic() {
	    // URL blacklist
		if (preg_match('@vibramfivefinger.org|northfaceoutlet.com|jacketsnorthface.com|uggbootsole.com|buyuggbootsuk.com|cheapuggs-uk.com|jerseysusa.com|capssupplier.com|cheapchristianshoes.com|.cheapghdivstyler.com|webstore.com|hairstraighteners.com|napervilleillocksmith.com|medicadeals.com|officialtiffanybracelet.com|thecylinder.net|onlybrandshoes.com|jordanshops.com|trade-shops.com|coach-us-outlet.com|cellphone-case.net|mes-baskets.com|edhardystyle.net|sportshoeonsale.com|admissionsessay.net|levitrapharmacies.com|www.wholesale|shox...com|ds-b.jp|monclerukshops.com|boots-top.com|nike-discount[.]com|onsale[.]com|pronosearch.com|disocuntedhardy.com|nike-gold.com|shop.info|boots-gold.com|tablet-pc.info|monsterstereoheadset.com|monclertopstores.com|topstores.com|topsalesmbt.com|bride-shops.com|monsterbeatsselling.com|storesale.com|relatiefront.nl|topbag2u.com|frmonclerjacket.com|uggs(sale|clearance)(online)?[.](net|com|org)|starjordan.com|coatsmoncler.com|own-jordan.com|onlineshoes[a-z]*.com|diorbag4u.com|ebayhermeskelly.com|onlinemonsterbeatssale.com|findremedyfast.com|mlbjerseys.com|airjordanfrance.net|jerseysdiscount.net|norxonline.com|order.ed-express.info|cheappillsed.us|edgroup.be/vaigra|edpillsrx.us|remroom.ru|(snapbackhats|airmax|oakleysunglass)(bar|outlet|zone).com|isuprashoes.com|servcss.com|playgoogle.ws|sunglasscheapsale.com|best4rus.ru|jerseys4sale.org|vusped.ru|buypills.com|besthyipinvestment.com|www.pkv[.]de|nikeelitenfljersey.com|love-sites-directory.com|www.junocrates.co.uk|direct-snelgeldlenen.nl|personalinjurylawyeraustintexas|easydresses.net|charmclubno.com|thomassabohandle.com|reddigg.com|iermann.com|nantongren.net|louisvuitton.com|miss-smile.com|billigathomassabo2012.com|monsterbeatsstudio|elite-nfl-jerseys.com|monstersolono.com|lancelfrs.com|mulberrywalletsale.com|nflcustomshop.com|celineonlinesaleus.com|celineluggagebagsus.com|louisvuitton.com|miss-smile.com/new|structuresite[.]info|fakeraybans.webgarden.com|www.free-online[.]cu|093game[.]com|oculosoakley.webnode.pt|osut[.]ro/forum|primechoiceautoparts.com|thomassabodeutschlands.com|thomassaboshopfr.com|copicmarkernachfuellen891|(?:oakley|ray-ban|)[-a-z]*[.]webnode|gryyonline.pl|centotorri.biz|[.]fpage[.]biz|Nike-Air-Max|replicastore[.]com|lancelde[.]com|bookdirectory[.]ru|ccleungwebdesign.com|vaporizersftw.com|lanceltaschenkaufen.com|optymalizacja[.]edu[.]pl|telefonski-studio[.]com|mass-fb-traffic[.]com@i',$this->body . ' ' . $this->author_url . ' ' . $this->author_email)) return true;
	    // URL blacklist
		if (preg_match('@[.](com|net|org)/[-_a-z0-9/.]*\b(air-jordan)\b@i',$this->body . ' ' . $this->author_url)) return true;
		// Word blacklist
		if (preg_match('@\b(free online dating|cialis|sex chat|sex dating|purchase adderall|marlboro cigarettes|viagra|car insurance|auto insurance|generic levitra|generic ultram|internet pharmacy|online pharmacy|Tiffany Bracelets|Nike shoes|cheap shox|cheap in india|sex with your partner|Ugg boots|cheap nike|Louis Vuitton Handbags|Drug Rehabilitation|casinos?|xanax|klonopin|cheap\s\S*\spills|adderall|buy cheap generic|buy generic online|aciphex buy online|prednisone online|porno videos|Fur-Collar Puffer Jacket|100% wool collar|Nike.*Addidas.*Reebok|MBT Womans Tunisha|do not iron the sportswear|cheap mbt shoes|Wedding Bride Gown|Limited Edition Sale|"uggs clearance"|Cheap Jordan Shoes|cheap uggs?|(Versace|Carolina Herrera) (?:\s*(Hobo|Khaki|Beige|Black|Gold)\s*)*(Handbag|Bag|Bags)|trouble getting an erection|Cheap Prada Sunglasses|replica oakley sunglasses|The Campaign Movie Stream|Text Your Ex Back|fake oakleys|thomas sabo armband|penis enlargement|search engine optimization|escort girl|escort service|hair-removal-products)\b@i',$this->body . ' ' . $this->author_url . ' ' . $this->author_name)) return true;
		// Heuristic : link count
		$noa = preg_replace('@[<]a[ ].*?[<]/a[^a-z]|http:[-a-zA-Z0-9_/.]+@i','',$this->body);
		$noa = strip_tags($noa);
		$noa = preg_replace('@\s@i','',$noa);
		$bod = preg_replace('@\s@i','',$this->body);
		preg_match_all('@[<]a[ ].*?[<]/a[^a-z]|http:[-a-zA-Z0-9_/.]+@i',$this->body,$links);
		if ((strlen($this->body) > 50 && strlen($noa)*5 < strlen($bod) && count($links[0]) >= 2) || count($links[0]) >= 100) {
            return true;
        }
        // Heuristic: <a>[url][link], with random text, invalid links
		if (preg_match('@<a href.*[.]com/?".*\[url=http.*\[/url].*\[link@i',$this->body)) return true;
		// Heuristic: images
		if (preg_match('@[.]jpg</img>@i',$this->body)) return true;
		// Banned IPs
		$banned_ips = array("64.31.57.19","66.151.61.111","83.238.5.206","91.232.96.10","109.230.216.60","109.230.216.225","178.196.19.108","173.236.190.37","222.77.234.145","217.114.107.109","94.23.1.18","94.23.1.28","91.121.167.147");
		if (in_array($this->author_ip,$banned_ips)) return true;
		// Heuristic: asdfasdf detector
		// idea: words with no vowels? OR just look at submitter
		if (strlen($this->author_url) > 0 && strpos($this->author_url,'/') === false && $this->author_url == $this->author_name && $this->author_email == '') {
			// url == name
			// don't need this fancyness, they are all spammers!
			return true;
			if (preg_match("@^[A-Za-z0-9]+\s+<a href=\"http://([A-Za-z0-9]+)[.]com[/]\">\\1</a>$@",$this->body)) return true;
			if (preg_match("@\\[url=(?:http://)?([A-Za-z0-9]+)[.]com[/]]\\1@",$this->body)) return true;
			if (preg_match("@\\[link=(?:http://)?([A-Za-z0-9]+)[.]com[/]]\\1@",$this->body)) return true;
		}
		// weirdness in author name
		if (preg_match('@<a href=|\[url[=\]]@i',$this->author_name . ' ' . $this->author_url . ' ' . $this->author_email)) return true;
		// Heuristic: valid UTF8
		//if (!mb_detect_encoding($this->body . ' ' . $this->author_name, 'UTF-8', true)) return true;
		//if (strlen($this->body) > iconv_strlen($this->body, 'UTF-8//TRANSLIT')) return true;
		//if (preg_match('@��@',$this->body . ' ' . $this->author_url . ' ' . $this->author_name)) return true;
		// Use trained spamfilter
		if ($this->spam_score() > (double)SPAM_THRESHOLD) return true;
		// Otherwise not spam
		return false;
	}
	
	// slightly less simple spam filter
	function spamfilter_attributes() {
		$all = $this->body;
		// don't double-count <a href=""> urls
		$all = preg_replace('@<a href="([^"]*)">\1</a>@',"\\1",$all);
		//if ($all == 'Isobel') {return array();print_r($attrs); $attrs = array();}
		$attrs = message_attributes($all);
		$attrs2 = message_attributes($this->author_name);
		foreach ($attrs2 as $k => $v) $attrs['authorname-'.$k] = $v;
		$attrs2 = message_attributes($this->author_url);
		foreach ($attrs2 as $k => $v) $attrs['authorurl-'.$k] = $v;
		$attrs2 = message_attributes($this->author_email);
		foreach ($attrs2 as $k => $v) $attrs['authoremail-'.$k] = $v;
		
		$attrs['author-name='.$this->author_name] = 1;
		if ($this->author_url !== '') $attrs['author-url='.$this->author_url] = 1;
		if ($this->author_email !== '') $attrs['author-email='.$this->author_email] = 1;
		$attrs['author-name-eq-url'] = $this->author_name == $this->author_url;
		if (preg_match('@[.]([a-z]+)/?$@',$this->author_url,$ma)) {
			$attrs['author-url$=.'.$ma[1]] = 1;
		}
		if (preg_match('@www@',$this->author_url,$ma)) {
			//$attrs['author-url~=.'.$ma[0]] = 1;
		}
		$attrs['author-email-dots5'] = preg_match_all('@[.]@',$this->author_email,$ignore) >= 5;
		// words of the author name re-appear in the email or url
		$name_words = preg_split('@(?:\s|[\@.,/-_="\'])+@',strtolower($this->author_name));
		foreach ($name_words as $i => $nw) {
			if (strlen($nw) > 2) {
				if (stripos($this->author_email,$nw) !== false && $this->author_name != $this->author_email) {
					$attrs['author-name-in-email'] = 1;
					$attrs['author-name'.$i.'-in-email'] = 1;
				}
				if (stripos($this->author_url,$nw) !== false && $this->author_name != $this->author_url) {
					$attrs['author-name-in-url'] = 1;
					$attrs['author-name'.$i.'-in-url'] = 1;
				}
			}
		}
		
		// expensive attribute: banned ip by stopforumspam.com
		if (!isset($this->is_stopforumspam_banned_ip)) {
			$this->is_stopforumspam_banned_ip = is_stopforumspam_banned_ip($this->author_ip);
		}
		$attrs['banned-ip'] = $this->is_stopforumspam_banned_ip;
		
		// expensive attribute: akismet spam score
		
		return $attrs;
	}
	
	function spam_score() {
		return SpamFilter::get()->classify($this->spamfilter_attributes());
	}
	
	function body_html() {
		if ($this->body_type == 'html') {
			return $this->body;
		} else {
			// TODO: prevent html injection!!!
			return WikiFormat::format($this->body);
		}
	}
}


// for spamfilter
function message_attributes($text) {
	if (is_array($text)) return $text;

    // don't die on invalid utf-8 etc.
    //$text = preg_replace('@[\x00-\x07\x80-\xff]@','?{UTF8}',$text);

	$plain = strip_tags($text);
	//$words = preg_split('@(?:\s|[\@.,/-_="\'])+@',$plain); // this can crash apache!
	$words = preg_split('@\s|[\@.,/-_="\']@',$plain, null, PREG_SPLIT_NO_EMPTY);

	$attrs = array();

	// links
	if (preg_match_all('@<a href=@', $text, $links, PREG_SET_ORDER | PREG_OFFSET_CAPTURE)) {
		$attrs['links-one'] = 1;
		$attrs['links-many2'] = count($links) >= 2;
		$attrs['links-many10'] = count($links) >= 10;
		$attrs['links-start'] = $links[0][0][1] == 0;
	}

	// urls
	if (preg_match_all("@https?://([.0-9a-z-]+)[^\" \n<>]+@", $text, $urls)) {
		$attrs['urls-one'] = 1;
		$attrs['urls-many2'] = count($urls[0]) >= 2;
		$attrs['urls-many10'] = count($urls[0]) >= 10;
		// domain names
		foreach ($urls[1] as $u) {
			$attrs['urls-domain=' . $u] = 1;
		}
		// repeat urls
		sort($urls);
		for ($i = 0 ; $i+1 < count($urls[1]) ; ++$i) {
			if ($urls[1][$i] == $urls[1][$i+1]) {
				$attrs['urls-repeat-2'] = 1;
			}
			if ($i+2 < count($urls[1]) && $urls[1][$i] == $urls[1][$i+2]) {
				$attrs['urls-repeat-3'] = 1;
			}
		}
		// urls that point to a root domain
		// words in urls
		foreach ($urls[0] as $u) {
			foreach (preg_split('@(?:\s|[\@.,/_="\'-])+@',$plain) as $w) {
				if (strlen($w) > 2) {
					$attrs['urlword-'.strtolower($w)] = 1;
				}
			}
		}
	}

	// length
	$attrs['short-1'] = count($words) <= 1;
	$attrs['short-2'] = count($words) <= 2;
	$attrs['short-5'] = count($words) <= 5;
	$attrs['short-10'] = count($words) <= 10;

	// garbage detector
	$attrs['consonants-5'] = preg_match('@[bcdfghjklmnpqrstvwxz]{5}@',strtolower($text));
	$attrs['utf8-bom'] = strpos($text,"\xEF\xBB\xBF") !== false;
	// TODO: more

	// words
	$long_words = 0;
	$junk_words = 0;
	foreach ($words as $w) {
		if (strlen($w) > 20) {
			$long_words++;
        }
		if (strlen($w) > 2 && strlen($w) < 20) {
			//$attrs['word-'.strtolower($w)] = 1;
		}
		if (strlen($w) > 2) {
		    $okchars = strlen(preg_filter('@[a-zA-Z0-9!\\@#$%^&*().,<>/?:;\'"{}-]@','x',$w));
		    if ($okchars < strlen($w)*0.5) {
		        $junk_words++; // less then half of the characters in the word are plain ascii
	        } elseif (preg_match('@[a-z][A-Z]@',$w)) {
				$junk_words++; // there are upper case characters after lower case characters
	        }
        }
	}
	$attrs['longwords-1'] = $long_words >= 1;
	$attrs['longwords-2'] = $long_words >= 2;
	$attrs['longwords-5'] = $long_words >= 5;
	$attrs['longwords-10'] = $long_words >= 10;
	$attrs['longwords-30%'] = $long_words >= count($words)*0.3;
	$attrs['longwords-50%'] = $long_words >= count($words)*0.5;
	$attrs['longwords-80%'] = $long_words >= count($words)*0.8;
	$attrs['junkwords-1'] = $junk_words >= 1;
	$attrs['junkwords-2'] = $junk_words >= 2;
	$attrs['junkwords-5'] = $junk_words >= 5;
	$attrs['junkwords-10'] = $junk_words >= 10;
	$attrs['junkwords-30%'] = $junk_words >= count($words)*0.3;
	$attrs['junkwords-50%'] = $junk_words >= count($words)*0.5;
	$attrs['junkwords-80%'] = $junk_words >= count($words)*0.8;

	// sub-words
	$subwords = array(
		'online','dating','cheap','sunglas','Nike','air','jordan','jacket','jersey','shoes','casino','porno','sex','viagra','vagina','penis','escort','girls',
		'buy','generic','uggs','ugg boot','woman','women','cheap','monster','solo','factory','bag','chanel','vuiton','outlet','consumer','debt','pimples','discount','payment',' sell','replica','urgent','amateur','video','business','promotion','forum','girls','baseball','coupon','oakley','repair','insurance',
		'prijzen','relatie','gratis','kantoor',' twanvl.nl',
		'comonad','haskell','categor','type','combinator','library','hackage','data','tuple','pair','compose','problem',
		'monad','monoid','calcul',
		'<img','[img','[url','[/URL','[link','/',' @','[','<','+','(',')','((','))','>','->',"\n>",'++','??','<ul>','<li>','<tt>','<pre','<img',"\n>",
		'<a href=>',
		'.cs.','.edu','/~',
	);
	foreach($subwords as $w) {
		if (stripos($text,$w) !== false) {
			$attrs['subword-'.$w] = 1;
		}
	}

	// utf8 garbage
	// TODO: write detector for this

	// other stuff
	$attrs['starts-lower'] = !empty($plain) && $plain[0] != strtoupper($plain[0]);
	$attrs['newline-lower'] = preg_match("@\n[a-z]@",$text);
	$attrs['double-newline-lower'] = preg_match("@\n\n[a-z]@",$text);

	return $attrs;
}

function is_stopforumspam_banned_ip($ip) {
	if (empty($ip)) return false;
	static $banned_ips;
	if (!isset($banned_ips)) {
		$banned_ips = "\n" . file_get_contents('comments/bannedips.csv') . "\n";
	}
	return strpos($banned_ips,"\n$ip\n") !== false;
}

