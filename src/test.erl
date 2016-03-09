-module(test).

-compile([
          {parse_transform, print_ast},
          {parse_transform, ets_transform},
          {parse_transform, print_ast}
         ]).

-export([hello/1]).

-type point() :: {float(), float()}.

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
    luser ->
      error("Insufficient privileges");
    G when G =:= user; G =:= admin ->
      ets:insert(contentious_table, User#user{name = UserName})
  end.

