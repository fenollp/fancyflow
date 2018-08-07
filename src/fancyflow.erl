-module(fancyflow).

-include_lib("syntax_tools/include/merl.hrl").

%% API exports
-export([parse_transform/2]).
-export([parallel/1]).

-type op_kind() :: mapper | folder.
-record(op, {kind :: op_kind()
            ,mname :: module()
            ,fname :: atom()
            }).

-ifdef(OTP_RELEASE). %% Implies 21 or higher
-define(EXCEPTION(Class, Reason, Stacktrace), Class:Reason:Stacktrace).
-define(STACKTRACE(Stacktrace), Stacktrace).
-else.
-define(EXCEPTION(Class, Reason, _), Class:Reason).
-define(STACKTRACE(_), erlang:get_stacktrace()).
-endif.

%%====================================================================
%% Parse Transform
%%====================================================================

parse_transform(ASTs, _Options) ->
    try [erl_syntax_lib:map(fun revert_then_transform/1, AST) || AST <- ASTs]
    catch
        ?EXCEPTION(_E, _R, _ST) ->
            io:format(user, "\n~p:~p ~p\n", [_E,_R,?STACKTRACE(_ST)]),
            ASTs
    end.

revert_then_transform(T) ->
    transform(erl_syntax:revert(T)).

%% [pipe](Var, F1, F2)
%% [parallel](A,B,C,D)
%% [{folder,knm_numbers,pipe}](T,F1,F2,F3)
%% [pipe](T,[F1,F2,F3])
transform({call, Line,
           {cons, CLine, Operator={_,CLine,_}, {nil,CLine}}, Args}=Call)
  when is_list(Args) ->
    case {op_new(Operator), op_args(Args)} of
        {error,_} -> Call;
        {_,error} -> Call;
        {Op,flat_list} ->
            op_mixin(Op, Args, Line, CLine);
        {Op=#op{kind=folder},FlattenedArgs=[_|_]} ->
            op_mixin(Op, FlattenedArgs, Line, CLine)
    end;
transform(Term) ->
    Term.

op_new({atom,_,pipe}) -> #op{kind=folder, mname=?MODULE, fname=pipe};
op_new({atom,_,maybe}) -> #op{kind=folder, mname=?MODULE, fname=maybe};
op_new({atom,_,parallel}) -> #op{kind=mapper, mname=?MODULE, fname=parallel};
op_new({tuple, _, [{atom,_,K}, {atom,_,M}, {atom,_,F}]}) ->
    #op{kind=K, mname=M, fname=F};
op_new(_) -> error.

op_args([T,C={cons,_,_,_}]) when is_tuple(T) ->
    try to_flat_list(C) of
        L -> [T|L]
    catch error:function_flause ->
            error
    end;
op_args(L=[_|_]) ->
    case lists:all(fun is_tuple/1, L) of
        true -> flat_list;
        false -> error
    end;
op_args(_) ->
    error.

to_flat_list({cons,_,T,{nil,_}}) -> [T];
to_flat_list({cons,_,T,Rest}) -> [T | to_flat_list(Rest)].

op_mixin(#op{mname=?MODULE, fname=pipe}, InitAndFuns, Line, _) ->
    mixin_pipe(InitAndFuns, Line);
op_mixin(#op{mname=?MODULE, fname=maybe}, InitAndFuns, Line, _) ->
    mixin_maybe(InitAndFuns, Line);
%%TODO: inline [parallel]
op_mixin(#op{kind=folder, mname=M, fname=F}, [Init|Funs=[_|_]], Line, CLine) ->
    NewFuns = folder_funs(Funs, CLine),
    Operation = {remote, Line, {atom,CLine,M}, {atom,CLine,F}},
    {call, Line, Operation, [Init,NewFuns]};
op_mixin(#op{kind=mapper, mname=M, fname=F}, Funs=[_|_], Line, CLine) ->
    NewFuns = mapper_funs(Funs, CLine),
    Operation = {remote, Line, {atom,CLine,M}, {atom,CLine,F}},
    {call, Line, Operation, [NewFuns]}.

%% @doc
%% Inlines fancyflows' "pipe" which is defined as:
%% -spec pipe(State, [fun((State) -> State)]) -> State when
%%       State :: any().
%% pipe(Init, []) -> Init;
%% pipe(Init, [_|_]=Funs) ->
%%     Apply = fun(F, State) -> F(State) end,
%%     lists:foldl(Apply, Init, Funs).
%% @end
mixin_pipe([Init], _) -> Init;
mixin_pipe([Init|Funs=[_|_]], Line) ->
    Var0 = {var, Line, make_var_name()},
    Expr0 = {match, Line, Var0, Init},
    Acc0 = {Var0, [Expr0]},
    {LastVar,Exprs} = lists:foldl(fun mixin_pipe_fold/2, Acc0, Funs),
    Block = lists:reverse(Exprs, [LastVar]),
    {block, Line, Block}.

mixin_pipe_fold(Piped, {{var,_,LastVarName},Block}) ->
    L = element(2, Piped),
    Replacer = fun (T) -> replace_var(T, LastVarName) end,
    Filled = erl_syntax:revert(
               erl_syntax_lib:map(Replacer, Piped)),
    Var = {var, L, make_var_name()},
    Match = {match, L, Var, Filled},
    {Var, [Match|Block]}.

%% -spec maybe(State, [fun((State) -> Return)]) -> Return when
%%     State :: any(),
%%     Return :: {ok, State} | {error, State}.
%% maybe(Init, Funs) ->
%%     Switch = fun (F, State) ->
%%                      case F(State) of
%%                          {ok, NewState} -> NewState;
%%                          {error, Reason} -> throw({'$return', Reason})
%%                      end
%%              end,
%%     try {ok, lists:foldl(Switch, Init, Funs)}
%%     catch {'$return', Term} -> {error, Term}
%%     end.
mixin_maybe([Init], _) -> Init;
mixin_maybe([Init|Funs=[_|_]], Line) ->
    Var0 = {var, Line, make_var_name()},
    Expr0 = {match, Line, Var0, Init},
    Block = mixin_maybe_fold(Funs, Var0),
    {block, Line, [Expr0,Block]}.

mixin_maybe_fold([Piped|Rest], {var,_,LastVarName}) ->
    L = element(2, Piped),
    Replacer = fun (T) -> replace_var(T, LastVarName) end,
    Filled = erl_syntax:revert(
               erl_syntax_lib:map(Replacer, Piped)),

    ErrorVar = {var, L, make_var_name()},
    ErrorTuple = {tuple, L, [{atom,L,error},{var,L,make_var_name()}]},
    ErrorMatch = {match, L, ErrorTuple, ErrorVar},
    ErrorClause = {clause, L, [ErrorMatch], [], [ErrorVar]},

    OkVar = {var, L, make_var_name()},
    OkVarMatched = {var, L, make_var_name()},
    OkTuple = {tuple, L, [{atom,L,ok},OkVarMatched]},
    OkMatch = {match, L, OkTuple, OkVar},
    Next = case [] =:= Rest of
               false -> mixin_maybe_fold(Rest, OkVarMatched);
               true -> OkVar
           end,
    OkClause = {clause, L, [OkMatch], [], [Next]},

    {'case', L, Filled, [ErrorClause,OkClause]}.


mapper_funs([], LastLine) ->
    {nil, LastLine};
mapper_funs([Piped|Rest], LastLine) ->
    Line = element(2, Piped),
    {cons, Line,
     {'fun', Line, {clauses,
        [{clause, Line, [], [], [erl_syntax:revert(Piped)]}]
     }},
     mapper_funs(Rest, LastLine)}.

folder_funs(Pipeds, LastLine) ->
    VarName = make_var_name(),
    Replacer = fun(V) -> replace_var(V, VarName) end,
    folder_funs(Pipeds, LastLine, VarName, Replacer).

folder_funs([], LastLine, _, _) ->
    {nil, LastLine};
folder_funs([Piped|Rest], LastLine, VarName, Replacer) ->
    Line = element(2, Piped),
    Filled = make_fun(Piped, Line, VarName, Replacer),
    io:format(user, "\nFilled ~p\n", [Filled]),
    NewRest = folder_funs(Rest, LastLine, VarName, Replacer),
    {cons, Line, Filled, NewRest}.

make_var_name() ->
    Int = erlang:unique_integer([monotonic, positive]),
    list_to_atom(lists:flatten(io_lib:format("_~s~p", [?MODULE,Int]))).

make_fun(Piped, Line, VarName, Replacer) ->
    case erl_syntax_lib:map(Replacer, Piped) of
        Piped ->
            {'fun', Line, {clauses,
               [{clause, Line, [], [], [erl_syntax:revert(Piped)]}]
            }};
        Filled ->
            {'fun', Line, {clauses,
               [{clause, Line, [{var,Line,VarName}], [],
                [erl_syntax:revert(Filled)]}]
            }}
    end.

replace_var({var, Line, '_'}, VarName) ->
    {var, Line, VarName};
replace_var(Exp, _) ->
    Exp.


%%====================================================================
%% Generic & supported pipe-like operators
%%====================================================================

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
            ?EXCEPTION(error, Reason, ST) -> {error, {Reason, ?STACKTRACE(ST)}};
            exit:Reason -> {error, Reason}
        end}
    end.

-spec gather(reference(), pid()) -> {ok, term()} | {error, term()}.
gather(Ref, Pid) ->
    receive {Pid, Ref, Res} -> Res
    end.
