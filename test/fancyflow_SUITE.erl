-module(fancyflow_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-compile({parse_transform, fancyflow_trans}).
-compile(export_all).

all() ->
    [pipe, maybe,
     pipe_trans, maybe_trans].

pipe(_) ->
    ?assertEqual(3,
                 fancyflow:pipe(0, [
                     fun(N) -> N + 5 end,
                     fun(N) -> N - 2 end
                 ])).

maybe(_) ->
    ?assertEqual({ok, 3},
                 fancyflow:maybe(0, [
                     fun(N) -> {ok, N+1} end,
                     fun(N) -> {ok, N+1} end,
                     fun(N) -> {ok, N+1} end
                 ])),
    ?assertEqual({error, third_clause},
                 fancyflow:maybe(0, [
                     fun(N) -> {ok, N+0} end,
                     fun(N) -> {ok, N+0} end,
                     fun(_) -> {error, third_clause} end,
                     fun(N) -> {ok, N+0} end
                  ])).

pipe_trans(_) ->
    _ = fun(X) -> id(X) end,
    ?assertEqual(3,
                 [pipe](0,
                        _ + 5,
                        id(_),
                        _ - 2
                 )).

maybe_trans(_) ->
    ?assertEqual({ok, 3},
                 [maybe](0,
                         {ok, _+1},
                         ok_id(_),
                         {ok, _+1},
                         ok_id(_),
                         {ok, _+1}
                 )),
    ?assertEqual({error, third_clause},
                 [maybe](0,
                         {ok, _+0},
                         ok_id(_),
                         {error, third_clause},
                         {ok, _+0}
                  )).

id(X) -> X.
ok_id(X) -> {ok,X}.
