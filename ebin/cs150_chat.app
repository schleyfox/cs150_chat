{application, cs150_chat, [
	{description,  "Nitrogen Website"},
	{mod, {cs150_chat_app, []}},
	{env, [
		{platform, inets}, %% {inets|yaws|mochiweb}
		{port, 8000},
		{session_timeout, 20},
		{sign_key, "SIGN_KEY"},
		{www_root, "./wwwroot"}
	]}
]}.