-module(fancyflow_demo).
-export([sans_pipe/0, pipe/0,
         sans_maybe/0, maybe/0,
         sans_parallel/0, parallel/0,
         nested/0]).

-compile({parse_transform, fancyflow}).

-spec sans_pipe() -> string().
sans_pipe() ->
    String = "a b c d e f",
    string:join(
      lists:map(fun string:to_upper/1, string:tokens(String, " ")),
      ","
    ).

-spec pipe() -> string().
pipe() ->
    [pipe]("a b c d e f",
           string:tokens(_, " "),
           lists:map(fun string:to_upper/1, _),
           string:join(_, ",")).

-spec sans_maybe() -> {ok, non_neg_integer()} | {error, term()}.
sans_maybe() ->
    case file:get_cwd() of
        {ok, Dir} ->
            case file:read_file(filename:join([Dir, "demo", "data.txt"])) of
                {ok, Bin} ->
                    {ok, {byte_size(Bin), Bin}};
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

-spec maybe() -> {ok, non_neg_integer()} | {error, term()}.
maybe() ->
    [maybe](undefined,
            file:get_cwd(),
            file:read_file(filename:join([_, "demo", "data.txt"])),
            {ok, {byte_size(_), _}}).

-spec sans_parallel() -> [term() | {badrpc, term()}].
sans_parallel() ->
    R1 = rpc:async_call(node(), lists, seq, [1,10]),
    R2 = rpc:async_call(node(), filelib, wildcard, ["*"]),
    R3 = rpc:async_call(node(), erlang, apply,
                        [fun() -> timer:sleep(10), slept end, []]),
    R4 = rpc:async_call(node(), ets, all, []),
    [rpc:yield(R1), rpc:yield(R2), rpc:yield(R3), rpc:yield(R4)].

-spec parallel() -> [{ok, term()} | {error, term()}].
parallel() ->
    [parallel](lists:seq(1,10),
               filelib:wildcard("*"),
               begin timer:sleep(10), slept end,
               ets:all()).

-spec nested() -> [{ok, _} | {error, _}].
nested() ->
    [parallel](
        %% first operation reads ./demo/data.txt
        [maybe](undefined,
                file:get_cwd(),
                file:read_file(filename:join([_, "demo", "data.txt"]))),
        %% second parallel op makes a filename and reads its size if any
        [pipe]("a b c d e f",
               string:tokens(_, " "),
               lists:map(fun string:to_upper/1, _),
               string:join(_, ","),
               %% Maybe the file doesn't exist
               [maybe](_, % the string from [pipe] is a filename here
                       file:read_file(_),
                       {ok, {byte_size(_), _}})
              )
    ).
