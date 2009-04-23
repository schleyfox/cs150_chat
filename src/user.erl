-module(user).
-export([authenticate/2, create/3, destroy/1, setup/0]).

-include_lib("stdlib/include/qlc.hrl").
-include("src/models.hrl").


% Run once
setup() ->
  mnesia:create_schema([node()]),
  mnesia:start(),
  mnesia:create_table(user, [{attributes, record_info(fields, user)}, {disc_copies, [node()]}]),
  mnesia:stop().

authenticate(Username, Password) ->
  case do(qlc:q([X#user.salt || X <- mnesia:table(user),
                                  X#user.username =:= Username])) of
    [Salt] -> PasswordHash = erlang:md5(Password ++ Salt),
      case do(qlc:q([X || X <- mnesia:table(user),
                          X#user.username =:= Username,
                          X#user.password_hash =:= PasswordHash])) of
        [User] -> {ok, User};
        [] -> not_found
      end;
    [] -> not_found
  end.

create(Realname, Username, Password) ->
  {PasswordHash, Salt} = hash_password(Password),
  case do(qlc:q([X || X <- mnesia:table(user),
                      X#user.username =:= Username])) of
    [] ->
      User = #user{realname=Realname, username=Username, password_hash=PasswordHash,
                    salt=Salt},
      mnesia:transaction(fun() ->
          mnesia:write(User)
      end);
    _ -> {error, user_exists}
  end.

destroy(Username) ->
  Id = {user, Username},
  mnesia:transaction(fun() ->
      mnesia:delete(Id)
    end).

hash_password(Password) ->
  Salt = create_salt(),
  {erlang:md5(Password ++ Salt), Salt}.

create_salt() ->
  lists:map(fun(_Elem) ->
      random:uniform(127-33)+33
    end,
  lists:seq(0,4)).

do(Q) ->
  F = fun() -> qlc:e(Q) end,
  {atomic, Val} = mnesia:transaction(F),
  Val.
