FancyFlow
=====

FancyFlow is an experimental Erlang library to bring convenience of things like the elixir pipe operator into Erlang, without needing to introduce new syntax (although we do play with existing stuff to avoid semantic confusion). It also allows some more flexibility by allowing the user to choose the placement of the state being weaved through.

It's a toy, but I'm open to feedback.

Usage
-----

Add the `fancyflow_trans` to your modules or applications, and use any of the control flow functions:

```erlang
[pipe](InitialState, Exp1, Exp2, ..., ExpN)
[maybe](InitialState, Exp1, Exp2, ..., ExpN)
```

Where each expression can be a valid Erlang expression, with the state substituted by the `_` variable.

For example:

```erlang
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
```

The `pipe()` function reworks the `sans_pipe()` function to be equivalent. The `maybe()` one gives the same result as the `sans_maybe()` one, although it ignores the initial state altogether by using `undefined`.

The expressions must be literal, and without nesting.


How it works
------------

Each form of

```erlang
[pipe](InitialState, Exp1, Exp2, ..., ExpN)
[maybe](InitialState, Exp1, Exp2, ..., ExpN)
```

is translated to:

```erlang
fancyflow:pipe(InitialState, [fun(Var) -> Exp1 end,
                              fun(Var) -> Exp2 end,
                              ...,
                              fun(Var) -> ExpN end])
fancyflow:maybe(InitialState, [fun(Var) -> Exp1 end,
                               fun(Var) -> Exp2 end,
                               ...,
                               fun(Var) -> ExpN end])
```

Which internally, runs a fold over the functions based on the state. The variable used as a function head is generated dynamically (looks like `_#Ref<0.0.3.1555>`) such that they never conflict with the surrounding scope, and never complain if they are not used.

the `[Function](...)` syntax has been chosen to not look like a regular function call and so that nobody mistakes its unorthodox (and otherwise illegal) usage of free variables with normal Erlang code.
