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
    ,setters_trans
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

-record(my_state, {a, b, c, d, e}).
my_new_state() -> #my_state{}.
set_a(S=#my_state{}, A) when is_list(A) -> S#my_state{a = A}.
set_b(S=#my_state{}, B) when is_binary(B) -> S#my_state{b = B}.
set_c(S=#my_state{}, C) when is_atom(C) -> S#my_state{c = C}.
set_d(S=#my_state{}, D) when is_float(D) -> S#my_state{d = D}.
setters_trans(_) ->
    ?assertError(function_clause
                ,[pipe](my_new_state()
                       ,set_d(_, 42)
                       )
                ),
    Expected = #my_state{a = [],
                         b = <<"bla">>,
                         c = something,
                         d = 42.0,
                         e = undefined
                        },
    ?assertEqual(Expected
                ,[pipe](my_new_state()
                       ,set_b(_, <<"bla">>)
                       ,set_a(_, [])
                       ,set_c(_, something)
                       ,set_d(_, 42.0)
                       )
                ),
    Setters = [fun (S) -> set_b(S, <<"bla">>) end
              ,fun (S) -> set_a(S, []) end
              ,fun (S) -> set_c(S, something) end
              ,fun (S) -> set_d(S, 42.0) end
              ],
    ?assertEqual(Expected, [pipe](my_new_state(), Setters)).
