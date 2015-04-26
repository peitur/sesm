{application, sesm,
 [{description, "simple erlang service monitor"},
  {vsn, "0.0.1"},
  {modules, [
	sesm,
	sesm_actions,
	sesm_sup,
	sesm_service,
	sesm_app,
	sesm_util,
	sesm_net,
	sesm_monitor,
	sesm_monitor_sup,
	sesm_service_net,
	sesm_service_net_sup
	]},
  {registered, []},
  {applications, [kernel, stdlib]},
  {mod, {sesm_app,[]}}
 ]}.
