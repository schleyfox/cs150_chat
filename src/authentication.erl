-module(authentication).
-compile(export_all).

is_logged_in() ->
  case wf:session(current_user) of
    undefined -> false;
    _ -> true
  end.

current_user() ->
  wf:session(current_user).

log_user_in(Username, Password) ->
  case user:authenticate(Username, Password) of
    {ok, User} -> wf:session(current_user, User);
    _ -> false
  end.

requires_login(LoggedInBody) ->
  case is_logged_in() of
    true -> LoggedInBody();
    false -> wf:redirect("/web/users/login")
  end.

requires_no_login(LoggedInRedirect, LoggedOutBody) ->
  case is_logged_in() of
    true -> wf:redirect(LoggedInRedirect);
    false -> LoggedOutBody()
  end.

