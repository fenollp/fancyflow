-module(fancyflow).

%% API exports
-export([pipe/2, maybe/2]).

%%====================================================================
%% API functions
%%====================================================================
-spec pipe(State, [fun((State) -> State)]) -> State when State :: any().
pipe(Init, Funs) ->
    lists:foldl(fun(F, State) -> F(State) end, Init, Funs).

-spec maybe(State, [fun((State) -> Return)]) -> Return when
    State :: any(),
    Return :: {ok, State} | {error, State}.
maybe(Init, Funs) ->
    SwitchFun = fun(F, State) ->
        case F(State) of
            {ok, NewState} -> NewState;
            {error, Reason} -> throw({'$return', Reason})
        end
    end,
    try
        {ok, lists:foldl(SwitchFun, Init, Funs)}
    catch
        {'$return', Term} -> {error, Term}
    end.

