-module(fancyflow_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("fancyflow.hrl").
-compile(export_all).

all() ->
    [parallel
    ,pipe_trans, maybe_trans, parallel_trans
    ,mixed_trans
    ,maybe2_trans
    ,scoping
    ].

id(X) -> X.
ok_id(X) -> {ok,X}.

parallel(_) ->
    ?assertMatch([{ok, 1},
                  {ok, 2},
                  {error, {badarith, _}},
                  {ok, 4}],
                 fancyflow:parallel([
                    fun() -> timer:sleep(150), 1 end,
                    fun() -> 1+1 end,
                    fun() -> 3/(2-2) end,
                    fun() -> erlang:'+'(2,2) end
                 ])).

pipe_trans(_) ->
    ?assertEqual(42, [pipe](42)),
    ?assertEqual(10,
                 [pipe](2,
                        3 * _,
                        id(_),
                        _ + 4
                 )).

maybe_trans(_) ->
    ?assertEqual({ok, 4},
                 [maybe](0,
                         {ok, _+1},
                         ok_id(_),
                         {ok, _+1},
                         {ok, _+1},
                         ok_id(_),
                         {ok, _+1}
                 )),
    ?assertEqual({error, third_clause},
                 [maybe](0,
                         {ok, _+0},
                         {ok, _+0},
                         ok_id(_),
                         {error, third_clause},
                         {ok, _+0}
                  )).

parallel_trans(_) ->
    ?assertMatch([{ok, 1},
                  {ok, 2},
                  {error, {badarith, _}},
                  {ok, 4}],
                 [parallel](
                    begin timer:sleep(150), 1 end,
                    1+1,
                    3/(2-2),
                    erlang:'+'(2,2)
                 )).

mixed_trans(_) ->
    ?assertMatch({error, {badarith, _}},
                 [maybe](0,
                         hd([parallel](_+1, _+2)),
                         hd(tl([parallel](_+1, _+2/(1-1)))),
                         hd([parallel](_+1, _+2))
                        )),
    ?assertMatch(10,
                 [pipe](0,
                        _+1,  % 1
                        _+2,  % 3
                        [maybe](
                          _  % <- 3
                         ,{ok, _+3}   % 6
                         ,{error, _+1} % 7
                         ),    % 7
                        element(2,_)+3 % 10
                       )
                ).

-define(MYPIPE, {folder, ?MODULE, maybe2}).

maybe2(Init, Funs) ->
    Switch = fun (F, State) ->
                     case F(State) of
                         {ok, NewState} -> NewState;
                         {error, A, B} -> throw({'$return', A, B})
                     end
             end,
    try {ok, lists:foldl(Switch, Init, Funs)}
    catch {'$return', A, B} -> {error, A, B}
    end.

maybe2_trans(_) ->
    ?assertEqual({ok, 1}, [{folder,?MODULE,maybe2}](0, {ok, _+1})),
    ?assertEqual({error, third_clause, 0}
                ,[?MYPIPE](0
                          ,{ok, _ + 1}
                          ,{ok, _ - 1}
                          ,ok_id(_)
                          ,{error, third_clause, _}
                          ,{ok, _/42}
                          )
                ).

scoping(_) ->
    ?assertEqual(42, [pipe](1, _*6, _*7)),
    ?assertEqual(42, [pipe](1
                           ,begin A = 6, _*A end
                           ,begin A = 7, _*A end
                           )
                ),

    A = 5,
    Val = 7,

    ?assertEqual(
       (A - (A+Val)*2) * 3
      ,[pipe](Val, (fun (_1) -> begin X = A + _1, X*2 end end)(_),
                   begin X = A - _, X*3 end)
      ).
