-module (web_index).
-include_lib ("nitrogen/include/wf.inc").
-include("src/models.hrl").
-compile(export_all).

main() -> 
	#template { file="./wwwroot/template.html"}.

title() ->
	"CS150".

body() ->
	[#label{text="whats up?"},
        case authentication:is_logged_in() of
          true -> Username = (authentication:current_user())#user.username, 
            #label{text=Username};
          false -> #label{text="Anonymous"}
        end].
	
event(_) -> ok.
