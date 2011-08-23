-module(metamodule).

-export([start_link/1]).
-export([init/1, terminate/2, handle_call/3, handle_info/2]).
-export([new_module/1, new_fun/1]).

-record(state, {
    ets,
    counter,
    name,
    head
    }).

%% Exported Client Functions
%% Operation & Maintenance API
start_link(Name) ->
    Arguments = [Name],
    Opts = [],
    gen_server:start_link(?MODULE, Arguments, Opts).

init([Name]) 
    when is_atom(Name) ->

    {ok, MTs, _} = erl_scan:string("-module(" ++ atom_to_list(Name) ++ ")."),

    % tokens to erl_parse trees
    {ok,MF} = erl_parse:parse_form(MTs),


    ModHead = [MF],
    E = ets:new('mm_functions', []),
    S = #state{
        ets=E,
        counter=1,
        name=Name,
        head=ModHead
        },
    {ok, S}.

terminate(_Reason, _LoopData) ->
    ok.


handle_call('get_new_name', _From,
    #state{counter=N} = LoopData) ->
    FunName = "fun" ++ integer_to_list(N),

    ServerPid = self(),
    RegFun = fun(Body) ->
                add_fun(ServerPid, FunName, Body)
                end,

    Reply = {ok, FunName, RegFun},
    NewLoopData = LoopData#state{counter=N+1},

    {reply, Reply, NewLoopData};



handle_call({'add_fun', ClientPid, FunName, FunBody}, _From,
    #state{ets=E, name=Name, head=ModHead} = LoopData) ->


    Reply = try
        {ok, FTs, _} = erl_scan:string(lists:flatten(FunBody)),
        {ok,FF} = erl_parse:parse_form(FTs),

        FunNameAtom = erlang:list_to_existing_atom(FunName),
        {function,1,FunNameAtom,Arity,_} = FF,
    

        erlang:monitor('process', ClientPid),
        true = ets:insert(E, {FunName, ClientPid, FF}),

        ModBody = lists:map(fun([X]) -> X end,
            ets:match(E,{'_','_','$1'})),

        ModForms = ModHead ++ ModBody,

        % compile forms to binary
        {ok, Name, Bin} = compile:forms(ModForms, [export_all]),

        % load module from binary
        {module, Name} = code:load_binary(Name, "nofile", Bin),


        Fun = function(Name, FunNameAtom, Arity),
        {ok, Fun}
    catch
    error:Type ->
        {error, Type}
    end,

    {reply, Reply, LoopData}.



handle_info({'DOWN', _Ref, process, FromPid, _Reason}, 
    #state{ets=E} = LoopData) ->

    true = ets:match_delete(E, {'_',FromPid,'_'}),
    
    {noreply, LoopData}.


add_fun(ServerPid, FunName, Body) ->
    ClientPid = self(),
    gen_server:call(ServerPid, 
        {'add_fun', ClientPid, FunName, Body}).
    


%%
%% API
%%

new_fun(ServerPid) ->
    gen_server:call(ServerPid, 'get_new_name').

new_module(Name) 
    when is_atom(Name) ->
    start_link(Name).



%%
%% Helpers
%%

atom_to_binary(X) ->
    list_to_binary(atom_to_list(X)).

function(Mod, Name, Arity) 
    when is_atom(Mod), is_atom(Name) ->
    BMod = atom_to_binary(Mod),
    BName = atom_to_binary(Name),
    function(BMod, BName, Arity);
function(Mod, Name, Arity) 
    when is_binary(Mod), is_binary(Name), 
        is_integer(Arity), Arity<256, Arity>=0 ->
    MS = size(Mod),
    NS = size(Name),
    true = MS > 0 andalso NS > 0,
    binary_to_term(<<131,113,100,0,MS:8,Mod/binary,100,0,NS:8,Name/binary,97,Arity:8>>).





%%
%% Tests
%%

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

common_test() ->
    % Start a new server. Get the server pid:
    {ok, M} = metamodule:new_module(test_meta),

    % Reserve a name for a new function.
    {ok, N, F} = metamodule:new_fun(M),

    {error, _} = F("bad_name() -> true."),
    {error, _} = F("bad function!"),

    {ok, FF} = F(N++"() -> true."),
    ?assert(FF()).   

-endif.


