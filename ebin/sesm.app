{application, sesm,
 [{description, "simple erlang service monitor"},
  {vsn, "0.0.1"},
  {modules, [
	sesm,
	sesm_sup,
	sesm_service,
	sesm_app,
	sesm_util
	]},
  {registered, []},
  {applications, [kernel, stdlib, sasl]},
  {mod, {sesm,[]}}
 ]}.
