<!DOCTYPE html>
<!--[if lt IE 7]> <html class="ie6 ie" lang="<?php print $language->language; ?>" dir="<?php print $language->dir; ?>"> <![endif]-->
<!--[if IE 7]>    <html class="ie7 ie" lang="<?php print $language->language; ?>" dir="<?php print $language->dir; ?>"> <![endif]-->
<!--[if IE 8]>    <html class="ie8 ie" lang="<?php print $language->language; ?>" dir="<?php print $language->dir; ?>"> <![endif]-->
<!--[if gt IE 8]> <!--> <html class="" lang="<?php print $language->language; ?>" dir="<?php print $language->dir; ?>"> <!--<![endif]-->
<head>
  <?php print $head; ?>
  <!-- Set the viewport width to device width for mobile -->
  <meta name="viewport" content="width=device-width, initial-scale=1.0" />
  <title><?php print $head_title; ?></title>
  <?php print $styles; ?>
  <!-- IE Fix for HTML5 Tags -->
  <!--[if lt IE 9]>
    <script src="http://html5shiv.googlecode.com/svn/trunk/html5.js"></script>
  <![endif]-->
</head>

<body class="<?php print $body_classes; ?>">

    <div class="navbar navbar-fixed-top">
      <div class="navbar-inner">
        <div class="container">
          <a class="btn btn-navbar" data-toggle="collapse" data-target=".nav-collapse">
            <span class="icon-bar"></span>
            <span class="icon-bar"></span>
            <span class="icon-bar"></span>
          </a>

          <a class="brand" href="/">
            <img alt="Flukso" src="/sites/all/themes/framework/img/flukso.logo.png">
          </a>

          <div class="nav-collapse">
            <ul class="nav main">
              <li class="about"><a href="/about">About</a></li>
              <li class="installation"><a href="/installation">Install</a></li>
              <li class="blog"><a href="/blog">Blog</a></li>
              <li class="forum"><a href="/forum">Forum</a></li>
              <li class="source"><a href="https://github.com/flukso">Source</a></li>
              <li class="dash"><a href="/dash">Dash</a></li>
              <li class="contact"><a href="/contact">Contact</a></li>
              <li class="shop cart checkout"><a href="/shop">Shop</a></li>
           </ul>

            <ul class="nav pull-right icon">
              <?php if ($show_cart_icon) { ?>
              <li><a href="/cart" id="cart" title="my shopping cart"><i class="icon-shopping-cart"></i></a></li>
              <?php } ?>

              <?php if ($show_feed_icon) { ?>
              <li><a href="<?php print $feed; ?>" title="rss feed"><i class="icon-pushpin"></i></a></li>
              <?php } ?>
 
              <?php if ($logged_in) { ?>
              <li><a href="/user/<?php print $user->uid ?>" title="settings for <?php print $user->name ?>"><i class="icon-cogs"></i></a></li>
              <li><a href="/logout" title="log out"><i class="icon-signout"></i></a></li>
              <?php } else { ?>
              <li><a href="/user/register" title="create account"><i class="icon-key"></i></a></li>
              <li><a href="/user" title="log in"><i class="icon-signin"></i></a></li>
              <?php } ?>
            </ul>
          </div>
          <!--/.nav-collapse -->
        </div>
      </div>
    </div>

    <div class="container clearfix">
    <section role="main" class="clearfix">
      <div class="row">
      <?php if (strpos($_GET['q'],    "catalog") === 0
                || strpos($_GET['q'],    "user") === 0 /* includes users */ 
                || strpos($_GET['q'],   "admin") === 0
                || strpos($_GET['q'],   "forum") === 0
                || strpos($_GET['q'],    "dash") === 0) { ?>
        <div class="span10 offset1">
      <?php } elseif (strpos($_GET['q'],  "splash") === 0) { ?>
        <div class="span12">
      <?php } else { ?>
        <div class="span8 offset2">
      <?php }; ?>

      <?php if (!empty($messages)): print $messages; endif; ?>
      <?php if (!empty($mission)): ?><div id="mission"><?php print $mission; ?></div><?php endif; ?>
      <a id="main-content"></a>
      <?php if (!empty($title) && strpos($_GET['q'],  "user") !== 0) { ?>
        <h1 class="title" id="page-title"><?php print $title ?></h1>
      <?php }; ?>

      <?php if (!empty($tabs)): ?><div class="tabs-wrapper clearfix"><?php print $tabs; ?></div><?php endif; ?>
      <?php if (!empty($help)): print $help; endif; ?>
      <?php print $content; ?>

      </div>
      </div>
    </section> <!-- /#main -->

    <footer id="footer" role="contentinfo" class="clearfix">

      <div class="row">
      <div class="span3">

      <?php if ($footer_1): ?>
        <div class="column">
          <?php print $footer_1 ?>
        </div>
      <?php endif; ?>

      </div>
      <div class="span3 offset1">

      <?php if ($footer_2): ?>
        <div class="column">
          <?php print $footer_2 ?>
        </div>
      <?php endif; ?>

      </div>
      <div class="span3 offset1">

      <?php if ($footer_3): ?>
        <div class="column">
          <?php print $footer_3 ?>
        </div>
      <?php endif; ?>

      </div>
      </div>

    </footer> <!-- /#footer -->
    </div>

    <?php print $scripts ?>
    <?php print $closure ?>

</body>
</html>
