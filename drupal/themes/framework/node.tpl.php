<?php if (!$page): ?>
  <article id="node-<?php print $node->nid; ?>" class="node<?php if ($sticky) { print ' sticky'; } ?><?php if (!$status) { print ' node-unpublished'; } ?> clearfix">
<?php endif; ?>

  <?php if ($picture || $submitted || !$page): ?>
    <?php if (!$page): ?>
      <header>
	<?php endif; ?>

      <?php print $picture ?>

	  <?php if (!$page): ?>
        <h1><a href="<?php print $node_url ?>" title="<?php print $title ?>"><?php print $title ?></a></h1>
      <?php endif; ?>

	  <?php if ($submitted): ?>
        <h6 class="submitted"><?php print $submitted; ?></h6>
      <?php endif; ?>

    <?php if (!$page): ?>
      </header>
	<?php endif; ?>
  <?php endif;?>

  <div class="content">
    <?php print $content ?>
  </div>

<?php if (!$page): ?>
  </article> <!-- /.node -->
<?php endif;?>
