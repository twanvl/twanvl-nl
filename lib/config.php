<?php

// -----------------------------------------------------------------------------
// Configuration goes here
// -----------------------------------------------------------------------------

define('SITE_TITLE', "Twan van Laarhoven's homepage");

define('BLOG_TITLE', "Twan van Laarhoven's blog");
define('BLOG_URL', "http://twanvl.nl/");
define('BLOG_AUTHOR_NAME', "Twan van Laarhoven");
define('BLOG_AUTHOR_EMAIL', "blog@twanvl.nl");

define('CACHE_DIR', '_cache');

define('CAPTCHA_SECRET', 'my-secret');

define('SPAM_FILTER_FILE', 'comments/spam-filter.dat');
define('SPAM_ATTEMPT_FILE', 'comments/spam-attempt-log.txt');
define('SPAM_THRESHOLD', 0.9);
define('LOG_SPAM_ATTEMPTS', false);

define('AKISMET_API_KEY','d5225efa60de');

define('DEFAULT_PAGE_LICENSE','');
define('DEFAULT_BLOG_LICENSE','License: <a href="http://creativecommons.org/licenses/by/3.0/">CC-BY</a>');

// -----------------------------------------------------------------------------
// Authentication
// -----------------------------------------------------------------------------

define('AUTH_REALM',           'twanvl');
define('AUTH_METHOD',          'Digest');    // "Digest" or "Basic"
define('AUTH_PASSWORD_FILE',   '.htdigest'); // htdigest or htpasswd file, depending on AUTH_METHOD

