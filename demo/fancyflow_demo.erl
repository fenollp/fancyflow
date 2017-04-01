-module(fancyflow_demo).
-export([sans_pipe/0, pipe/0,
         sans_maybe/0, maybe/0]).

-compile({parse_transform, fancyflow_trans}).

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
