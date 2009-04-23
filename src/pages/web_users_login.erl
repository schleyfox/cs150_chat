-module (web_users_login).
-include_lib ("nitrogen/include/wf.inc").
-compile(export_all).

main() -> 
	#template { file="./wwwroot/template.html"}.

title() ->
	"Login".

body() -> 
  authentication:requires_no_login("/",
    fun() -> 
      Body = [
      #label{ text="Username" },
      #textbox{ id=username, next=password },
      #p{},
      #label{ text="Password" },
      #password{ id=password, next=login },
      #p{},
      #button { id=login, text="Login", postback=login }
      ],
    
      wf:wire(login, username, #validate{ validators = [
        #is_required{ text="Required." }]}),
      wf:wire(login, password, #validate{ validators = [
        #is_required{ text="Required." }]}),
    
      wf:render(Body)
    end).

event(login) ->
  [Username] = wf:q(username),
  [Password] = wf:q(password),
  case authentication:log_user_in(Username, Password) of
    false -> wf:flash("Invalid username or password");
    _ -> wf:redirect("/")
  end;
event(_) -> ok.
