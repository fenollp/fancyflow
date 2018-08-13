-module(fancyflow).

-include_lib("syntax_tools/include/merl.hrl").

%% :generated -- https://erldocs.com/current/stdlib/erl_anno.html?i=0&search=erl_anno#undefined
%% https://bugs.erlang.org/browse/ERL-639

%% API exports
-export([parse_transform/2]).
-export([parallel/1]).

-type op_kind() :: mapper | folder.
-record(op, {kind :: op_kind()
            ,mname :: module()
            ,fname :: atom()
            }).

-define(PREFIX, '_fancyflow').
-define(PREFIX_MAX, '_fancyflow999999999').
-define(PREFIX_STR, "_" ?MODULE_STRING).

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
transform({call, Anno,
           {cons, _, Operator={_,_,_}, {nil,_}},
           InitAndFunsOrJustFuns}=Call)
  when is_list(InitAndFunsOrJustFuns) ->
    case op_new(Operator) of
        error -> Call;
        Op ->
            op_mixin(Op, InitAndFunsOrJustFuns, Anno)
    end;
transform(AST) ->
    AST.

op_new({atom,_,pipe}) -> #op{kind=folder, mname=?MODULE, fname=pipe};
op_new({atom,_,maybe}) -> #op{kind=folder, mname=?MODULE, fname=maybe};
op_new({atom,_,parallel}) -> #op{kind=mapper, mname=?MODULE, fname=parallel};
op_new({tuple, _, [{atom,_,K}, {atom,_,M}, {atom,_,F}]}) ->
    #op{kind=K, mname=M, fname=F};
op_new(_) -> error.

op_mixin(#op{mname=?MODULE, fname=pipe}, InitAndFuns, Anno) ->
    mixin_pipe(InitAndFuns, Anno);
op_mixin(#op{mname=?MODULE, fname=maybe}, InitAndFuns, Anno) ->
    mixin_maybe(InitAndFuns, Anno);
%%TODO: inline [parallel]
op_mixin(#op{kind=folder, mname=M, fname=F}, [Init|Funs=[_|_]], Anno) ->
    NewFuns = folder_funs(Funs, Anno),
    Operation = {remote, Anno, {atom,Anno,M}, {atom,Anno,F}},
    {call, Anno, Operation, [Init,NewFuns]};
op_mixin(#op{kind=mapper, mname=M, fname=F}, Funs=[_|_], Anno) ->
    NewFuns = mapper_funs(Funs, Anno),
    Operation = {remote, Anno, {atom,Anno,M}, {atom,Anno,F}},
    {call, Anno, Operation, [NewFuns]}.

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
mixin_pipe([Init|Funs=[_|_]], Anno) ->
    Var0 = {var, Anno, make_var_name()},
    Expr0 = {match, Anno, Var0, Init},
    Acc0 = {Var0, [Expr0]},
    {LastVar,Exprs} = lists:foldl(fun mixin_pipe_fold/2, Acc0, Funs),
    Block = lists:reverse(Exprs, [LastVar]),
    {block, Anno, Block}.

mixin_pipe_fold(Piped, {{var,_,LastVarName},Block}) ->
    Anno = element(2, Piped),
    Filled = mixin_or_make_fun_then_call(Piped, Anno, LastVarName),
    Var = {var, Anno, make_var_name()},
    Match = {match, Anno, Var, Filled},
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
mixin_maybe([Init|Funs=[_|_]], Anno) ->
    Var0 = {var, Anno, make_var_name()},
    Expr0 = {match, Anno, Var0, Init},
    Block = mixin_maybe_fold(Funs, Var0),
    {block, Anno, [Expr0,Block]}.

mixin_maybe_fold([Piped|Rest], {var,_,LastVarName}) ->
    Anno = erl_anno:set_generated(true, element(2, Piped)),
    Filled = mixin_or_make_fun_then_call(Piped, Anno, LastVarName),

    ErrorVar = {var, Anno, make_var_name()},
    ErrorTuple = {tuple, Anno, [{atom,Anno,error},{var,Anno,make_var_name()}]},
    ErrorMatch = {match, Anno, ErrorTuple, ErrorVar},
    ErrorClause = {clause, Anno, [ErrorMatch], [], [ErrorVar]},

    OkVar = {var, Anno, make_var_name()},
    OkVarMatched = {var, Anno, make_var_name()},
    OkTuple = {tuple, Anno, [{atom,Anno,ok},OkVarMatched]},
    OkMatch = {match, Anno, OkTuple, OkVar},
    Next = case [] =:= Rest of
               false -> mixin_maybe_fold(Rest, OkVarMatched);
               true -> OkVar
           end,
    OkClause = {clause, Anno, [OkMatch], [], [Next]},

    {'case', Anno, Filled, [ErrorClause,OkClause]}.


mapper_funs([], LastAnno) ->
    {nil, LastAnno};
mapper_funs([Piped|Rest], LastAnno) ->
    Anno = element(2, Piped),
    {cons, Anno,
     {'fun', Anno, {clauses,
        [{clause, Anno, [], [], [erl_syntax:revert(Piped)]}]
     }},
     mapper_funs(Rest, LastAnno)}.

folder_funs(Pipeds, LastAnno) ->
    VarName = make_var_name(),
    Replacer = fun(V) -> replace_var(V, VarName) end,
    folder_funs(Pipeds, LastAnno, VarName, Replacer).

folder_funs([], LastAnno, _, _) ->
    {nil, LastAnno};
folder_funs([Piped|Rest], LastAnno, VarName, Replacer) ->
    Anno = element(2, Piped),
    Filled = make_fun(Piped, Anno, VarName, Replacer),
    NewRest = folder_funs(Rest, LastAnno, VarName, Replacer),
    {cons, Anno, Filled, NewRest}.

%% Hack to know when we may be rebinding a variable
is_matching({match,_,{var,_,Name},_}) when ?PREFIX < Name, Name < ?PREFIX_MAX -> false;
is_matching({match,_,{var,_,_},_}) -> true;
is_matching({_,Ta}) -> is_matching(Ta);
is_matching({_,_,Ta}) -> is_matching(Ta);
is_matching({_,_,Ta,Tb}) -> is_matching(Ta) orelse is_matching(Tb);
is_matching({_,_,Ta,Tb,Tc}) -> is_matching(Ta) orelse is_matching(Tb) orelse is_matching(Tc);
is_matching(Ts) when is_list(Ts) -> lists:any(fun is_matching/1, Ts);
is_matching(T) when is_atom(T); is_number(T); is_binary(T) -> false.

mixin_or_make_fun_then_call(Piped, Anno, VarName) ->
    Replacer = fun (T) -> replace_var(T, VarName) end,
    Filled = erl_syntax:revert(erl_syntax_lib:map(Replacer, Piped)),
    case is_matching(Piped) of
        false -> Filled;
        true ->
            Clauses = [{clause, Anno, [], [], [Filled]}],
            {call, Anno, {'fun', Anno, {clauses, Clauses}}, []}
    end.

make_fun(Piped, Anno, VarName, Replacer) ->
    case erl_syntax_lib:map(Replacer, Piped) of
        Piped ->
            Clauses = [{clause, Anno, [], [], [erl_syntax:revert(Piped)]}],
            {'fun', Anno, {clauses,Clauses}};
        Filled ->
            Clauses = [{clause, Anno, [{var,Anno,VarName}], [],
                        [erl_syntax:revert(Filled)]}],
            {'fun', Anno, {clauses,Clauses}}
    end.

make_var_name() ->
    Int = erlang:unique_integer([monotonic, positive]),
    list_to_atom(?PREFIX_STR ++ integer_to_list(Int)).

replace_var({var, Anno, '_'}, VarName) ->
    {var, Anno, VarName};
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
