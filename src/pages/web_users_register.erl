-module (web_users_register).
-include_lib ("nitrogen/include/wf.inc").
-compile(export_all).

main() -> 
	#template { file="./wwwroot/template.html"}.

title() ->
	"Register".

body() ->
  authentication:requires_no_login("/",
    fun() ->
      Body = [
      #label{ text="Real Name" },
      #textbox{ id=realname, next=username },
      #p{},
      #label{ text="Username" },
      #textbox{ id=username, next=password },
      #p{},
      #label{ text="Password" },
      #password{ id=password, next=login },
      #p{},
      #label{ text="Confirm Password" },
      #password{ id=confirmPassword, next=register_user },
      #p{},
      #button { id=register_user, text="Register", postback=register_user }
      ],
    
      wf:wire(register_user, username, #validate{ validators = [
        #is_required{ text="Required." }]}),
      wf:wire(register_user, password, #validate{ validators = [
        #is_required{ text="Required." }]}),
      wf:wire(register_user, confirmPassword, #validate { validators=[
        #is_required { text="Required." },
        #confirm_password { password=password, text="Passwords must match." }
        ]}),  
    
      wf:render(Body)
    end).

event(register_user) ->
  [Username] = wf:q(username),
  [Realname] = wf:q(realname),
  [Password] = wf:q(password),
  case user:create(Realname, Username, Password) of
    {atomic, ok} -> wf:redirect("/");
    {error, user_exists} -> wf:flash("Sorry, that user name is taken")
  end;
	
event(_) -> ok.
