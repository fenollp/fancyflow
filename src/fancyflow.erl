-module(fancyflow).

%% API exports
-export([pipe/2, maybe/2, parallel/1]).

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

-spec parallel([fun(() -> _)]) -> [{ok, _} | {error, _}].
parallel(Funs) ->
    Ref = make_ref(),
    ReplyTo = self(),
    Pids = [spawn(futurize(F, Ref, ReplyTo)) || F <- Funs],
    [gather(Ref, Pid) || Pid <- Pids].


%%====================================================================
%% Private
%%====================================================================
-spec futurize(fun(() -> _), reference(), pid()) -> fun(() -> _).
futurize(F, Ref, ReplyTo) ->
    fun() -> ReplyTo ! {self(), Ref,
        try
            {ok, F()}
        catch
            throw:Val -> {ok, Val};
            error:Reason -> {error, {Reason, erlang:get_stacktrace()}};
            exit:Reason -> {error, Reason}
        end}
    end.

-spec gather(reference(), pid()) -> {ok, term()} | {error, term()}.
gather(Ref, Pid) ->
    receive
        {Pid, Ref, Res} -> Res
    end.
