{application, least,
	[{description, "Least Last.Fm power api"},
	{vsn, "1.0"},
	{modules, [least, least_web, least_supervisor, least_resolver]},
	{registered, [least_web, least_supervisor]},
	{applications, [kernel, stdlib]},
	{env, []},
	{mod, {least, [8080, 9090]}}]}.