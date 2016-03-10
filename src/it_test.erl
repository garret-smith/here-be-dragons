-module(it_test).

-compile([
          {parse_transform, print_ast},
          {parse_transform, inline_transform},
          {parse_transform, print_ast}
         ]).

-export([change_username/2]).

-record(user, {
          id :: integer(),
          name :: string(),
          group = luser :: admin | user | luser
         }).

inline_transform(Forms, Options) ->
    [erl_syntax_lib:map(
       fun(Node) ->
               case erl_syntax:revert(Node) of
                   {call, Line,
                    {remote, _, {atom, _, ets}, {atom, _, insert}},
                    [{atom, _, contentious_table}, _Objects]} = Form ->
                       {block, Line,
                        [{op,Line,'!',
                          {atom,Line,ets_collector},
                          {tuple,Line,[{atom,Line,insert},
                                       {call,Line,{atom,Line,self},[]}]}},
                                      Form]};
                   Form -> Form
               end
       end,
       F)
     || F <- Forms].

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


