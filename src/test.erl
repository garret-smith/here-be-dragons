-module(test).

-compile([{parse_transform, my_transform}]).

-export([hello/1]).

-record(user, {
          id :: integer(),
          name :: string(),
          group = luser :: admin | user | luser
         }).

mapfold_xs(F, Terms) ->
    lists:foldl(fun({process_me, X}, A) ->
                        [F(X) | A];
                   (_, A) ->
                        A
                end,
                [],
                Terms).

hello(Who) when is_atom(Who) ->
    io:fwrite("hello ~p", [Who]).

change_username(UserId, UserName) when UserId =:= 0 ->
  error("Can't change admin user name");
change_username(UserId, UserName) ->
  Users = ets:lookup(contentious_table, UserId),
  [User] = Users,
  case User#user.group of
    luser -> error("Insufficient privileges");
    _ -> ets:insert(contentious_table, User#user{name = UserName})
  end.

