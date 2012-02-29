<!DOCTYPE html>
<!--[if lt IE 7]> <html class="ie6 ie" lang="<?php print $language->language; ?>" dir="<?php print $language->dir; ?>"> <![endif]-->
<!--[if IE 7]>    <html class="ie7 ie" lang="<?php print $language->language; ?>" dir="<?php print $language->dir; ?>"> <![endif]-->
<!--[if IE 8]>    <html class="ie8 ie" lang="<?php print $language->language; ?>" dir="<?php print $language->dir; ?>"> <![endif]-->
<!--[if gt IE 8]> <!--> <html class="" lang="<?php print $language->language; ?>" dir="<?php print $language->dir; ?>"> <!--<![endif]-->
<head>
  <?php print $head; ?>
  <!-- Set the viewport width to device width for mobile -->
  <meta name="viewport" content="width=device-width" />
  <title><?php print $head_title; ?></title>
  <?php print $styles; ?>
  <!-- IE Fix for HTML5 Tags -->
  <!--[if lt IE 9]>
    <script src="http://html5shiv.googlecode.com/svn/trunk/html5.js"></script>
  <![endif]-->
</head>

<body class="<?php print $body_classes; ?>">

  <header>
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
            <ul class="nav">
              <li><a href="/about">About</a></li>
              <li><a href="/installation">Install</a></li>
              <li><a href="/blog">Blog</a></li>
              <li><a href="/forum">Forum</a></li>
              <li><a href="https://github.com/flukso">Dev</a></li>
              <li><a href="/dash">Dash</a></li>
              <li><a href="/contact">Contact</a></li>
              <li><a href="/shop">Shop</a></li>
            </ul>

            <form id="searchForm" class="navbar-search pull-right dropdown">
              <input id="searchText" type="text" class="search-query dropdown-toggle" placeholder="Search">
            </form>
          </div>
          <!--/.nav-collapse -->
        </div>
      </div>
    </div>
  </header>

    <section role="main" class="clearfix">
      <div class="container clearfix">
 
      <div class="row">
      <div class="span10 offset1">

      <?php if (!empty($messages)): print $messages; endif; ?>
      <?php if (!empty($mission)): ?><div id="mission"><?php print $mission; ?></div><?php endif; ?>
      <a id="main-content"></a>
      <?php if (!empty($title)): ?><h1 class="title" id="page-title"><?php print $title ?></h1><?php endif; ?>
      <?php if (!empty($tabs)): ?><div class="tabs-wrapper clearfix"><?php print $tabs; ?></div><?php endif; ?>
      <?php if (!empty($help)): print $help; endif; ?>
      <?php print $content; ?>

      </div>
      </div>

      </div>
    </section> <!-- /#main -->

    <footer id="footer" role="contentinfo" class="clearfix">
      <div class="container clearfix">

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

      <?php print $feed_icons ?>

      </div>
    </footer> <!-- /#footer -->

    <?php print $scripts ?>
    <?php print $closure ?>

</body>
</html>
