$(function() {
    /* add bootstrap table formatting to each table element
     * but exclude uc's delivery and billing address panes
     */
    $("table")
        .not($("div.address-pane-table")
        .children("table"))
        .addClass("table");

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
});
