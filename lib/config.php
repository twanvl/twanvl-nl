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

// -----------------------------------------------------------------------------
// Authentication
// -----------------------------------------------------------------------------

define('AUTH_REALM',           'twanvl');
define('AUTH_METHOD',          'Digest');    // "Digest" or "Basic"
define('AUTH_PASSWORD_FILE',   '.htdigest'); // htdigest or htpasswd file, depending on AUTH_METHOD

