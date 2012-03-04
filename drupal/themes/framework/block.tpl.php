<section id="block-<?php print $block->module .'-'. $block->delta; ?>" class="block block-<?php print $block->module ?> clearfix">

  <?php if (!empty($block->subject)): ?>
    <h4><?php print $block->subject ?></h4>
  <?php endif;?>

  <div class="content">
    <?php print $edit_links; ?>
    <?php print $block->content ?>
  </div>

</section> <!-- /.block -->
