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
	public $visible = 1;
	
	function __construct() {
		$this->body_type = 'wiki';
	}
	
	// simple spam filter
	function is_spam() {
	    // URL blacklist
		if (preg_match('@vibramfivefinger.org|northfaceoutlet.com|jacketsnorthface.com|uggbootsole.com|buyuggbootsuk.com|cheapuggs-uk.com|jerseysusa.com|capssupplier.com|cheapchristianshoes.com|.cheapghdivstyler.com|webstore.com|hairstraighteners.com|napervilleillocksmith.com|medicadeals.com|officialtiffanybracelet.com|thecylinder.net|onlybrandshoes.com|jordanshops.com|trade-shops.com|coach-us-outlet.com|cellphone-case.net|mes-baskets.com|edhardystyle.net|sportshoeonsale.com|admissionsessay.net|levitrapharmacies.com|www.wholesale|shox...com|ds-b.jp|monclerukshops.com|boots-top.com|nike-discount[.]com|onsale[.]com|pronosearch.com|disocuntedhardy.com|nike-gold.com|shop.info|boots-gold.com|tablet-pc.info|monsterstereoheadset.com|monclertopstores.com|topstores.com|topsalesmbt.com|bride-shops.com|monsterbeatsselling.com|storesale.com|relatiefront.nl|topbag2u.com|frmonclerjacket.com|uggs(sale|clearance)(online)?[.](net|com|org)|starjordan.com|coatsmoncler.com|own-jordan.com|onlineshoes[a-z]*.com|diorbag4u.com|ebayhermeskelly.com|onlinemonsterbeatssale.com|findremedyfast.com|mlbjerseys.com|airjordanfrance.net|jerseysdiscount.net|norxonline.com|order.ed-express.info|cheappillsed.us|edgroup.be/vaigra|edpillsrx.us|remroom.ru|(snapbackhats|airmax|oakleysunglass)(bar|outlet|zone).com|isuprashoes.com|servcss.com|playgoogle.ws@i',$this->body . ' ' . $this->author_url . ' ' . $this->author_email)) return true;
	    // URL blacklist
		if (preg_match('@[.](com|net|org)/[-_a-z0-9/.]*\b(air-jordan)\b@i',$this->body . ' ' . $this->author_url)) return true;
		// Word blacklist
		if (preg_match('@\b(free online dating|cialis|sex chat|sex dating|purchase adderall|marlboro cigarettes|viagra|car insurance|auto insurance|generic levitra|generic ultram|internet pharmacy|online pharmacy|Tiffany Bracelets|Nike shoes|cheap shox|cheap in india|sex with your partner|Ugg boots|cheap nike|Louis Vuitton Handbags|Drug Rehabilitation|casinos?|xanax|klonopin|cheap\s\S*\spills|adderall|buy cheap generic|buy generic online|aciphex buy online|prednisone online|porno videos|Fur-Collar Puffer Jacket|100% wool collar|Nike.*Addidas.*Reebok|MBT Womans Tunisha|do not iron the sportswear|cheap mbt shoes|Wedding Bride Gown|Limited Edition Sale|"uggs clearance"|Cheap Jordan Shoes|cheap uggs?|(Versace|Carolina Herrera) (?:\s*(Hobo|Khaki|Beige|Black|Gold)\s*)*(Handbag|Bag|Bags)|trouble getting an erection)\b@i',$this->body . ' ' . $this->author_url . ' ' . $this->author_name)) return true;
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
		// Banned IPs
		$banned_ips = array("64.31.57.19","66.151.61.111","83.238.5.206","109.230.216.60","109.230.216.225","178.196.19.108","173.236.190.37","222.77.234.145","217.114.107.109","94.23.1.18","94.23.1.28","91.121.167.147");
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
		// Otherwise not spam
		return false;
	}
	
	function body_html() {
		if ($this->body_type == 'html') {
			return $this->body;
		} else {
			// TODO: prevent code injection!!!
			return WikiFormat::format($this->body);
		}
	}
}
