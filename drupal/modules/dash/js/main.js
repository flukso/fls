tpl.loadTemplates(['header'],
    function () {
        var template = _.template(tpl.get('header'));
        $('.header').html(template());
    });
