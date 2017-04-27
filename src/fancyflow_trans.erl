-module(fancyflow_trans).
-export([parse_transform/2]).
-include_lib("syntax_tools/include/merl.hrl").

parse_transform(ASTs, _Options) ->
    try
        [erl_syntax_lib:map(fun(T) ->
                                transform(erl_syntax:revert(T))
                            end, AST) || AST <- ASTs]
    catch
        _E:_R ->
            ASTs
    end.

transform({call, Line,
           {cons, CLine, {atom, CLine, pipe}, {nil, CLine}},
           [Init | Funs]}) ->
    {call, Line,
     {remote, Line, {atom, CLine, fancyflow}, {atom, CLine, pipe}},
     [Init, rework_funs(Funs, CLine, create_var())]};
transform({call, Line,
           {cons, CLine, {atom, CLine, maybe}, {nil, CLine}},
           [Init | Funs]}) ->
    {call, Line,
     {remote, Line, {atom, CLine, fancyflow}, {atom, CLine, maybe}},
     [Init, rework_funs(Funs, CLine, create_var())]};
transform({call, Line,
           {cons, CLine, {atom, CLine, parallel}, {nil, CLine}},
           Funs}) ->
    {call, Line,
     {remote, Line, {atom, CLine, fancyflow}, {atom, CLine, parallel}},
     [rework_funs(Funs, CLine)]};
transform(Term) ->
    Term.

create_var() ->
    binary_to_atom(iolist_to_binary(io_lib:format("_~p", [make_ref()])), utf8).

rework_funs([], Line) ->
    {nil, Line};
rework_funs([F|Funs], Line) ->
    {cons, Line,
     {'fun', Line, {clauses,
        [{clause, Line, [], [], [erl_syntax:revert(F)]}]
     }},
     rework_funs(Funs, Line)}.

rework_funs([], Line, _) ->
    {nil, Line};
rework_funs([F|Funs], Line, VarName) ->
    {cons, Line,
     {'fun', Line, {clauses,
        [{clause, Line,
          [{var,Line,VarName}],
          [],
          [erl_syntax:revert(erl_syntax_lib:map(
            fun(V) -> replace_var(V, VarName) end,
            F
          ))]}]
     }},
     rework_funs(Funs, Line, VarName)}.

replace_var({var, Line, '_'}, VarName) ->
    {var, Line, VarName};
replace_var(Exp, _) ->
    Exp.
