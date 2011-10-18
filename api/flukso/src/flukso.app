{application, flukso,
 [{description, "flukso"},
  {vsn, "0.1"},
  {modules, [
    flukso,
    flukso_app,
    flukso_sup,
    flukso_deps,
    api_sensor,
    api_device
  ]},
  {registered, []},
  {mod, {flukso_app, []}},
  {env, []},
  {applications, [kernel, stdlib, crypto, erlrrd, mysql, webmachine]}]}.
