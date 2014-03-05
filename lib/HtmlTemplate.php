<?php

// -----------------------------------------------------------------------------
// Page template
// -----------------------------------------------------------------------------

abstract class HtmlTemplate {
	// ---------------------------------------------------------------------
	// Writing
	// ---------------------------------------------------------------------
	
	static function write($page) {
		HtmlTemplate::write_html($page);
	}
	
	static function write_nav_link($cat,$url,$title) {
		$current = $cat == $url ? ' class="current"' : '';
		echo "<li><a href='$url'$current>$title</a></li>";
	}
	
	static function write_html($page) {
		$page->load();
		header('x',true,$page->status_code);
		header('Content-Type: text/html; charset=utf-8');
		$base  = htmlspecialchars(Util::base_url());
		$title = htmlspecialchars($page->title);
		$bodyclass = $page->show_comments ? ' class="with-comments"' : '';
?><!DOCTYPE html>
<html>
  <head>
    <title><?php echo $title ?></title>
    <base href="<?php echo $base;?>">
    <link href='http://fonts.googleapis.com/css?family=Nobile&subset=latin' rel='stylesheet' type='text/css'>
    <link href='http://fonts.googleapis.com/css?family=Crimson+Text&subset=latin' rel='stylesheet' type='text/css'>
    <link href='http://fonts.googleapis.com/css?family=Droid+Serif&subset=latin' rel='stylesheet' type='text/css'>
    <link href='<?php echo $base;?>style/style.css' rel='stylesheet' type='text/css'>
    <link href='<?php echo $base;?>style/flinder-16.png' rel='shortcut icon'>
    <?php if ($page->show_feedlink) {
       echo "<link rel='alternate' type='application/atom+xml' href='feed' title='Blog feed'>";
    } ?>
<script type="text/javascript">

  var _gaq = _gaq || [];
  _gaq.push(['_setAccount', 'UA-23571556-1']);
  _gaq.push(['_trackPageview']);

  (function() {
    var ga = document.createElement('script'); ga.type = 'text/javascript'; ga.async = true;
    ga.src = ('https:' == document.location.protocol ? 'https://ssl' : 'http://www') + '.google-analytics.com/ga.js';
    var s = document.getElementsByTagName('script')[0]; s.parentNode.insertBefore(ga, s);
  })();

</script>
  </head>
  <body<?php if ($page->show_comments) echo ' class="with-comments"'; ?>>
    <div id="around-body"><div id="header">
      <a href="index" id="site-title"><span id="site-logo"></span>Twan van Laarhoven</a>
      <ul>
       <?php
         HtmlTemplate::write_nav_link($page->category,"index","Me");
         HtmlTemplate::write_nav_link($page->category,"research","Research");
         HtmlTemplate::write_nav_link($page->category,"teaching","Teaching");
         HtmlTemplate::write_nav_link($page->category,"software","Software");
         HtmlTemplate::write_nav_link($page->category,"blog","Blog");
       ?>
      </ul>
      <div class="clear"></div>
    </div>
    <div id="header-shadow"></div>
    <div id="body">
      <h1><?php echo $title ?></h1>
      <?php
        HtmlTemplate::write_page_info($page);
        echo $page->body;
      ?>
    </div></div>
    <?php HtmlTemplate::write_comments($page); ?>
  </body>
</html><?php
	}
	
	static function write_page_info($page) {
		$info = '';
		if ($page->show_date) {
			//$info .= "Published: " . strftime('%Y-%m-%d %H:%M:%S',$page->last_modified);
			$info .= "Published: " . gmstrftime('%Y-%m-%d<span class="Z">T</span>%H:%M<span class="Z" title="UTC">Z</span>',$page->last_modified);
		}
		if ($page->tags) {
			$tags = array();
			foreach ($page->tags as $t) {
				$tags [] = "<a href='tag/$t'>" . htmlspecialchars($t) . "</a>";
			}
			$info .= "<div class='tags'>Tags: " . implode(', ', $tags) . '</div>';
		}
		if ($page->license_link) {
			$info .= "<div>" . $page->license_link . "</div>";
		}
		if ($page->source_link) {
			$info .= "<div>" . $page->source_link . "</div>";
		}
		if ($info) {
			echo "<div class='page-info'>$info</div>";
		}
	}
	
	static function write_comments($page) {
		if (!$page->show_comments) return;
		HtmlTemplate::write_comments_wb($page);
	}
	
	static function write_comments_wb($page) {
		$comments = Comments::get_all($page->url);
		// language
		if ($page->natural_language == 'nl') {
			$lang = array(
				'Comment'=>'Reageren',
				'Reply'=>'Reageren',
				'Comments'=>'Reacties',
				'Name'=>'Naam',
				'Homepage'=>'Homepage',
				'Email'=>'E-mail',
				'optional'=>'optioneel',
				'emailnote'=>'optioneel, blijft geheim',
				'Subscribe'=>'Houd mij op de hoogte van reacties',
				'Human'=>'CAPTCHA',
				'codenote'=>'',
			);
		} else {
			$lang = array(
				'Comment'=>'Comment',
				'Comments'=>'Comments',
				'Reply'=>'Reply',
				'Name'=>'Name',
				'Homepage'=>'Homepage',
				'Email'=>'Email',
				'optional'=>'optional',
				'emailnote'=>'optional, will not be revealed',
				'Subscribe'=>'Subscribe to comments on this article',
				'Human'=>'Human?',
				'codenote'=>'Use <tt>&gt; code</tt> for code blocks, <tt>@code@</tt> for inline code. Some html is also allowed.',
			);
		}
		// comments box
		echo "<div id='comments'>\n";
		echo " <div id='comments-body'><a name='comments'></a>\n";
		if (empty($comments)) {
			echo "  <h2>{$lang['Comment']}</h2>\n";
		} else {
			echo "  <h2>{$lang['Comments']}</h2>\n";
			foreach ($comments as $c) {
				echo "<div class='post-comment' id='comment-".htmlspecialchars($c->id)."'>\n";
				echo "<a name='comment-".htmlspecialchars($c->id)."'></a>\n";
				echo "<div class='poster'>";
				echo "<img src='" . htmlspecialchars(gravatar_image($c->author_email)) . "' alt='' class='avatar'>";
				if ($c->author_url) {
				    $url = htmlspecialchars($c->author_url);
				    if (preg_match('@^[a-z0-9]+[.]@',$url)) {
				        $url = 'http://' . $url;
				    }
					echo '<a href="' . $url . '">' . htmlspecialchars($c->author_name) . '</a>';
				} else {
					echo htmlspecialchars($c->author_name);
				}
				if ($c->date) {
					$date = strtotime($c->date);
					//$date = strftime('%Y-%m-%d %H:%M:%S',$date);
					$date = gmstrftime('%Y-%m-%d<span class="Z">T</span>%H:%M<span class="Z" title="UTC">Z</span>',$date);
					echo "<span class='date'>Date: $date</span>";
				}
				echo "<a href='delete-comment/$page->url?comment=".urlencode($c->id)."' class='delete'>x</a>";
				echo "</div>";
				echo $c->body_html();
				echo "</div>";
			}
			echo "  <h2>$lang[Reply]</h2>\n";
		}
		echo "  <form method='post' action='add-comment/$page->url#new-comment'><a name='new-comment'></a>\n";
		echo "   <input type='hidden' name='url' value='",htmlspecialchars($page->url),"'>\n";
		echo "   <table>";
		global $invalid_fields;
		echo "    <tr><td><label for='author_name' >$lang[Name]</label    ><td><input type='text'  name='author_name'  id='author_name'  ".(isset($invalid_fields['author_name']) ?'class="invalid" ':'')."value='",request_var('author_name'),"'>\n";
		echo "    <tr><td><label for='author_url'  >$lang[Homepage]</label><td><input type='text'  name='author_url'   id='author_url'   ".(isset($invalid_fields['author_url'])  ?'class="invalid" ':'')."value='",request_var('author_url'),"'> <span class='help'>($lang[optional])</span>\n";
		echo "    <tr><td><label for='author_email'>$lang[Email]</label   ><td><input type='email' name='author_email' id='author_email' ".(isset($invalid_fields['author_email'])?'class="invalid" ':'')."value='",request_var('author_email'),"'> <span class='help'>($lang[emailnote])</span>\n";
		echo "    <tr><td><td><label><input type='checkbox' name='author_subscribe' ",request_var_check('author_subscribe'),"> $lang[Subscribe]</label>\n";
		// generate captcha
		$captcha = new Captcha($page->natural_language);
		echo "    <tr><td><label for='captcha'>$lang[Human]</label><td>";
		echo "<input type='hidden' name='captcha_answer' value='".htmlspecialchars($captcha->answer)."'>";
		echo "<input type='hidden' name='captcha_question' value='".htmlspecialchars($captcha->question)."'>";
		echo "$captcha->question <input type='text' name='captcha' id='captcha' ".(isset($invalid_fields['captcha'])?'class="invalid" ':'')."value='",request_var('captcha'),"'>\n";
		echo "   </table>";
		echo "   <textarea rows='6' name='body'".(isset($invalid_fields['body'])?'class="invalid" ':'').">",htmlspecialchars(@$_REQUEST['body']),"</textarea>\n";
		echo "   <div class='edit-help'>";
		echo "    $lang[codenote]";
		echo "   </div>";
		echo "   <input type='submit' value='Submit'>\n";
		if (isset($invalid_fields[''])) {
			echo $invalid_fields[''];
		}
		echo "  </form>\n";
		echo " </div>\n";
		echo "</div>";
	}
	
	static function write_comments_disqus($page) {
		$url = '"' . $page->full_url() . '"';
		echo <<< EOF
<div id="comments">

 <div id="comments-shadow"></div>
 <div id="comments-body">
  <h2>Comments</h2>
  <div id="disqus_thread"></div>
  <script type="text/javascript">
    var disqus_shortname = 'twanvl-home';
    var disqus_identifier = $url;
    var disqus_url = $url;
    (function() {
        var dsq = document.createElement('script'); dsq.type = 'text/javascript'; dsq.async = true;
        dsq.src = 'http://' + disqus_shortname + '.disqus.com/embed.js';
        (document.getElementsByTagName('head')[0] || document.getElementsByTagName('body')[0]).appendChild(dsq);
    })();
  </script>
  <noscript>Please enable JavaScript to view the <a href="http://disqus.com/?ref_noscript">comments powered by Disqus.</a></noscript>
  <a href="http://disqus.com" class="boring">blog comments powered by <span class="logo-disqus">Disqus</span></a><?php
 </div>
</div>
EOF;
	}
}

function request_var($var) {
	if (isset($_REQUEST[$var])) return htmlspecialchars($_REQUEST[$var]);
	if (isset($_COOKIE[$var])) return htmlspecialchars($_COOKIE[$var]);
	return "";
}

function request_var_check($var, $default = true) {
	if (count($_POST)) {
		return isset($_POST[$var]) ? 'checked' : '';
	} else if (isset($_COOKIE[$var])) {
		return $_COOKIE[$var] ? 'checked' : '';
	} else {
		return $default ? 'checked' : '';
	}
}

function gravatar_image($email) {
	if (!$email) return "style/no-avatar2.png";
	$hash = md5(strtolower(trim($email)));
	return "http://www.gravatar.com/avatar/$hash?s=20&d=retro";
}
