{application, flukso, [
    {description, "flukso"},
    {vsn, "0.1"},
    {modules, [
        flukso,
        flukso_app,
        flukso_sup,
        flukso_deps,
        api_sensor,
        api_device,
        sql,
        rrd,
        check,
        event,
        event_hdlr,
        threshold,
        alarm,
        alarm_hdlr,
        flog
    ]},
    {registered, []},
    {mod, {flukso_app, []}},
    {env, [
        {cookie_domain, ".flukso.net"}
    ]},
    {applications, [
        kernel,
        stdlib,
        crypto,
        public_key,
        ssl,
        erlrrd,
        mysql,
        esmtp,
        webmachine
    ]}
]}.
