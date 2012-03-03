<article class="comment<?php print ($comment->new) ? ' comment-new' : ''; print ' '. $status; print ' '. $zebra; ?> clearfix">

  <header><h6>
    <?php print $picture ?>

    <span class="submitted"><?php print $submitted; ?></span>

    <?php if ($comment->new) : ?>
      <span class="new"><?php print $new ?></span>
    <?php endif; ?>
  </h6></header>

  <div class="content">
    <?php print $content ?>
    <?php if ($signature): ?>
      <div class="user-signature clearfix">
        <?php print $signature ?>
      </div>
    <?php endif; ?>
  </div>

</article> <!-- /.comment -->
