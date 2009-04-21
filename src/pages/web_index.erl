-module (web_index).
-include_lib ("nitrogen/include/wf.inc").
-compile(export_all).

main() -> 
	#template { file="./wwwroot/template.html"}.

title() ->
	"CS150".

body() ->
	#label{text="whats up?"}.
	
event(_) -> ok.
