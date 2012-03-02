$(function() {
    /* add bootstrap table formatting to each table element
     * but exclude uc's delivery and billing address panes
     */
    $("table")
        .not($("div.address-pane-table")
        .children("table"))
        .addClass("table");

    /* bootstrap submit buttons */
    $("input.form-submit").addClass("btn");

    $("input.form-submit.node-add-to-cart, " +
      "input.form-submit#edit-checkout, "    +
      "input.form-submit#edit-continue, "    +
      "input.form-submit#edit-submit, "      +
      "input.form-submit#edit-save")
        .addClass("btn-primary");

    /* substitute drupal messages for bootstrap alerts
     * since bootstrap.js require jquery 1.7.1, we cannot
     * use bootstrap's alert dismissal feature
     */ 
    $("div.messages.status")
        .removeClass("messages status")
        .addClass("alert alert-success");

    $("div.messages.warning")
        .removeClass("messages warning")
        .addClass("alert");

    $("div.messages.error")
        .removeClass("messages error")
        .addClass("alert alert-error");

    /* bootstrap input/textarea/select errors */
    $(".form-item input.error, "    +
      ".form-item textarea.error, " +
      ".form-item select.error")
        .removeClass("error")
        .parent()
        .addClass("control-group error");

    $("ul.tabs.primary, " +
      "ul.tabs.secondary")
        .removeClass("tabs primary secondary")
        .addClass("nav nav-pills");
});
