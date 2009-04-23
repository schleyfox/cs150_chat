-module (web_index).
-include_lib ("nitrogen/include/wf.inc").
-include("src/models.hrl").
-compile(export_all).

main() -> 
	#template { file="./wwwroot/template.html"}.

title() ->
	"CS150".

body() ->
  authentication:requires_login(fun() ->
      Body = [
        #panel {id=buddyList},
        #panel {id=chatHistory, class=chat_history },
        #textbox {id=messageTextBox, next=sendButton},
        #button {id=sendButton, text="Talk", postback=chat}
      ],
      Pid = wf:comet(fun() -> listen_for_events() end),
      chats_server:add_user(authentication:current_user(), Pid),
      wf:render(Body)
    end).

event(chat) ->
  [Message] = wf:q(messageTextBox),
  chats_server:send_message(authentication:current_user(), Message),
  wf:wire("obj('messageTextBox').focus(); obj('messageTextBox').select();");

event(_) -> ok.

listen_for_events() ->
  receive
    {message, UserName, Message} ->
      Text =
        [ #panel{body=[
           #span {text=UserName, class=username}, ": ",
           #span {text=Message, class=message}]}],
      wf:insert_bottom(chatHistory, Text),
      wf:wire("obj('chatHistory').scrollTop = obj('chatHistory').scrollHeight;"),
      wf:comet_flush();
    {user_list, UserNames} ->
      Text = lists:flatten(lists:map(fun(X) -> [
                #p{},
                #span{text=X}]
              end,
              UserNames)),
      wf:update(buddyList, Text),
      wf:comet_flush()
  end,
  listen_for_events().

