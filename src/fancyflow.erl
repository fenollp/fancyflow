-module(fancyflow).

-include_lib("syntax_tools/include/merl.hrl").

%% API exports
-export([parse_transform/2]).
-export([pipe/2, maybe/2, parallel/1]).

-type op_kind() :: mapper | folder.
-record(op, {kind :: op_kind()
            ,mname :: module()
            ,fname :: atom()
            }).

%%====================================================================
%% Parse Transform
%%====================================================================

parse_transform(ASTs, _Options) ->
    try [erl_syntax_lib:map(fun revert_then_transform/1, AST) || AST <- ASTs]
    catch _E:_R -> ASTs
    end.

revert_then_transform(T) ->
    transform(erl_syntax:revert(T)).

%% [pipe](Var, F1, F2)
%% [parallel](A,B,C,D)
%% [{folder,knm_numbers,pipe}](T,F1,F2,F3)
transform({call, Line,
           {cons, CLine, Operator={_,CLine,_}, {nil,CLine}},
           InitAndFunsOrJustFuns}) when is_list(InitAndFunsOrJustFuns) ->
    Op = op_new(Operator),
    Operation = op_mixin(Op, Line, CLine),
    case {Op#op.kind, InitAndFunsOrJustFuns} of
        {folder, [Init|Funs=[_|_]]} ->
            NewFuns = folder_funs(Funs, CLine, make_var()),
            {call, Line, Operation, [Init,NewFuns]};
        {mapper, Funs=[_|_]} ->
            NewFuns = mapper_funs(Funs, CLine),
            {call, Line, Operation, [NewFuns]}
    end;
transform(Term) ->
    Term.

op_new({atom,_,pipe}) -> #op{kind=folder, mname=?MODULE, fname=pipe};
op_new({atom,_,maybe}) -> #op{kind=folder, mname=?MODULE, fname=maybe};
op_new({atom,_,parallel}) -> #op{kind=mapper, mname=?MODULE, fname=parallel};
op_new({tuple, _, [{atom,_,K}, {atom,_,M}, {atom,_,F}]}) ->
    #op{kind=K, mname=M, fname=F}.

op_mixin(#op{mname=?MODULE, fname=F}, Line, CLine) ->
    %%TODO: inline at least pipe's lists:fold call here
    {remote, Line, {atom,CLine,?MODULE}, {atom,CLine,F}};
op_mixin(#op{mname=M, fname=F}, Line, CLine) ->
    {remote, Line, {atom,CLine,M}, {atom,CLine,F}}.

make_var() ->
    Int = erlang:unique_integer([monotonic, positive]),
    list_to_atom(lists:flatten(io_lib:format("~s~p", [?MODULE,Int]))).

mapper_funs([], Line) ->
    {nil, Line};
mapper_funs([F|Funs], Line) ->
    {cons, Line,
     {'fun', Line, {clauses,
        [{clause, Line, [], [], [erl_syntax:revert(F)]}]
     }},
     mapper_funs(Funs, Line)}.

folder_funs(Funs, Line, VarName) ->
    Replacer = fun(V) -> replace_var(V, VarName) end,
    folder_funs(Funs, Line, VarName, Replacer).

folder_funs([], Line, _, _) ->
    {nil, Line};
folder_funs([F|Funs], Line, VarName, Replacer) ->
    NewF = make_fun(F, Line, VarName, Replacer),
    NewFuns = folder_funs(Funs, Line, VarName, Replacer),
    {cons, Line, NewF, NewFuns}.

make_fun(F, Line, VarName, Replacer) ->
    case erl_syntax_lib:map(Replacer, F) of
        F ->
            {'fun', Line, {clauses,
               [{clause, Line, [], [], [erl_syntax:revert(F)]}]
            }};
        NewF ->
            {'fun', Line, {clauses,
               [{clause, Line, [{var,Line,VarName}], [],
                [erl_syntax:revert(NewF)]}]
            }}
    end.

replace_var({var, Line, '_'}, VarName) ->
    {var, Line, VarName};
replace_var(Exp, _) ->
    Exp.


%%====================================================================
%% Generic & supported pipe-like operators
%%====================================================================

-spec pipe(State, [fun((State) -> State)]) -> State when
      State :: any().
pipe(Init, []) -> Init;
pipe(Init, [_|_]=Funs) ->
    Apply = fun(F, State) -> F(State) end,
    lists:foldl(Apply, Init, Funs).


-spec maybe(State, [fun((State) -> Return)]) -> Return when
    State :: any(),
    Return :: {ok, State} | {error, State}.
maybe(Init, Funs) ->
    try {ok, lists:foldl(fun switch/2, Init, Funs)}
    catch {'$return', Term} -> {error, Term}
    end.

switch(F, State) ->
    case F(State) of
        {ok, NewState} -> NewState;
        {error, Reason} -> throw({'$return', Reason})
    end.

-spec parallel([fun(() -> _)]) -> [{ok, _} | {error, _}].
parallel(Funs) ->
    Ref = make_ref(),
    ReplyTo = self(),
    Pids = [spawn(futurize(F, Ref, ReplyTo)) || F <- Funs],
    [gather(Ref, Pid) || Pid <- Pids].

-spec futurize(fun(() -> _), reference(), pid()) -> fun(() -> _).
futurize(F, Ref, ReplyTo) ->
    fun() -> ReplyTo ! {self(), Ref,
        try {ok, F()}
        catch
            throw:Val -> {ok, Val};
            error:Reason -> {error, {Reason, erlang:get_stacktrace()}};
            exit:Reason -> {error, Reason}
        end}
    end.

-spec gather(reference(), pid()) -> {ok, term()} | {error, term()}.
gather(Ref, Pid) ->
    receive {Pid, Ref, Res} -> Res
    end.
