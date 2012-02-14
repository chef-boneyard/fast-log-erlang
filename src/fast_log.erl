%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Kevin Smith <kevin@opscode.com>
%% @author Seth Falcon <seth@opscode.com>
%% @copyright Copyright 2011-2012 Opscode Inc.

-module(fast_log).

-include("fast_log.hrl").

-behaviour(gen_event).

%% API
-export([start_link/1,
         debug/2,
         debug/3,
         debug/4,
         info/2,
         info/3,
         info/4,
         warn/2,
         warn/3,
         warn/4,
         err/2,
         err/3,
         err/4]).

%% gen_event callbacks
-export([init/1,
         handle_event/2,
         handle_call/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {name,
                log_handle}).

%% example LogConfig:
%% [{name, erchef},
%%  {file, "log/erchef.log"},
%%  {files, 5},
%%  {file_size, 50}]

start_link(LogConfig) ->
    Name = proplists:get_value(name, LogConfig),
    {ok, Pid} = gen_event:start_link({local, Name}),
    ok = add_handler(LogConfig),
    {ok, Pid}.

add_handler(LogConfig) ->
    Name = proplists:get_value(name, LogConfig),
    gen_event:add_handler(Name, ?MODULE, [LogConfig]).

debug(Name, PList) ->
    send_event(Name, {debug, plist2iolist(PList)}).

debug(Name, Token, Args) ->
    Msg = format_msg("~s ~s", [Token, Args]),
    send_event(Name, {debug, Msg}).

debug(Name, Token, Format0, Args) ->
    Format = "~s " ++ Format0,
    Msg = format_msg(Format, [Token|Args]),
    send_event(Name, {debug, Msg}).

%% @doc Log a proplist as an INFO message.
%%
%% All of the arity two logging functions taking a `PList' argument expect a list of
%% key/value pairs that will be formated as `K1=V1; K2=V2'. An attempt is made to transform
%% Keys and values into iolists. In particular, atoms, integers, and floats are
%% supported. You can also specify a `{Fmt, Args}' tuple which will be passed to
%% `io_lib:format/2'.
%%
info(Name, PList) ->
    send_event(Name, {info, plist2iolist(PList)}).

info(Name, Token, Args) ->
    Msg = format_msg("~s ~s", [Token, Args]),
    send_event(Name, {info, Msg}).

info(Name, Token, Format0, Args) ->
    Format = "~s " ++ Format0,
    Msg = format_msg(Format, [Token|Args]),
    send_event(Name, {info, Msg}).

warn(Name, PList) ->
    send_event(Name, {info, plist2iolist(PList)}).

warn(Name, Token, Args) ->
    Msg = format_msg("~s ~s", [Token, Args]),
    send_event(Name, {warn, Msg}).

warn(Name, Token, Format0, Args) ->
    Format = "~s " ++ Format0,
    Msg = format_msg(Format, [Token|Args]),
    send_event(Name, {warn, Msg}).

err(Name, PList) ->
    send_event(Name, {err, plist2iolist(PList)}).

err(Name, Token, Args) ->
    Msg = format_msg("~s ~s", [Token, Args]),
    send_event(Name, {err, Msg}).

err(Name, Token, Format0, Args) ->
    Format = "~s " ++ Format0,
    Msg = format_msg(Format, [Token|Args]),
    send_event(Name, {err, Msg}).

init([LogConfig]) ->
    Name = proplists:get_value(name, LogConfig),
    FileName = proplists:get_value(file, LogConfig),
    FileSize = proplists:get_value(file_size, LogConfig, 100),
    FileCount = proplists:get_value(files, LogConfig, 3),
    MinLogLevel = proplists:get_value(log_level, LogConfig, ?LOG_DEBUG),
    _Tid = ets:new(Name, [public, named_table]),
    fast_log_util:put_config(Name, #config{min_log=MinLogLevel}),
    {ok, LogHandle} = fast_log_writer:open(atom_to_list(Name), FileName, FileCount,
                                           FileSize),
    {ok, #state{name=Name, log_handle=LogHandle}}.

handle_event({LogLevel, Msg}, #state{log_handle=LogHandle}=State) ->
    ok = fast_log_writer:write(LogHandle, LogLevel, Msg),
    {ok, State};
handle_event(_Event, State) ->
    {ok, State}.

handle_call(_Request, State) ->
    Reply = ok,
    {ok, Reply, State}.

handle_info(_Info, State) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal functions
format_msg(Format, Args) ->
    io_lib:format(Format, Args).

send_event(Name, {LogLevel, _}=Msg) ->
    case fast_log_util:should_log(Name, LogLevel) of
        true ->
            case is_logger_overloaded(Name) of
                true ->
                    ok;
                false ->
                    gen_event:notify(Name, Msg)
            end;
        false ->
            ok
    end.

is_logger_overloaded(Name) ->
    #config{overloaded=Overloaded} = Config = fast_log_util:get_config(Name),
    CheckProb = case Overloaded of
                    true ->
                        1.0;
                    false ->
                        random:uniform()
                end,
    maybe_check(Name, CheckProb, Config, Overloaded).

maybe_check(Name, CheckProb, Config, Overloaded) when CheckProb >= 0.7 ->
    Current = case erlang:process_info(whereis(Name), [message_queue_len]) of
                  [{message_queue_len, Len}] when Len >= 5000 ->
                      true;
                  [{message_queue_len, _Len}] ->
                      false
              end,
    if
        Current =:= Overloaded ->
            Current;
        true ->
            Config1 = Config#config{overloaded=Current},
            fast_log_util:put_config(Name, Config1),
            Current
    end;
maybe_check(_Name, _CheckProb, _Config, _Overloaded) ->
    false.

%% Take a proplist of key/value pairs and turn it into an iolist using a `K1=V1; K2;V2'
%% format. An attempt is made to transform Keys and values into iolists. Atoms, binaries,
%% strings, integers and floats will all be converted. You can also specify either as `{Fmt,
%% Args}' and an appropriate call to `io_lib:format(Fmt, Args)' will be made on your behalf
%% (a bill will be sent to your process' mailbox). The order of the input proplist is
%% preserved. You can also request pretty-print style formatting (no newlines) by passing
%% `{raw, Term}'. Finally, keys and values that are empty are replaced with
%% "fast_log_empty_string". Not handled are keys/values that contain the log format
%% delimiters `;' and `='.
plist2iolist([{_Key, _Val} | _Rest]=PList) ->
    plist2iolist(PList, []).

plist2iolist([{Key, Val} | Rest], Acc) ->
    plist2iolist(Rest, [<<"; ">>, [as_io(Key), <<"=">>, as_io(Val)]|Acc]);
plist2iolist([], [<<"; ">>|Acc]) ->
    lists:reverse(Acc).

%% Convert input to iolist. Handles atoms, lists, binaries, integers, floats, and tuples of
%% the form `{Fmt, Args}' suitable for passing to `io_lib:format/2'. Tuples marked as `{raw,
%% term()}' will be formated using `"~256P"'.
as_io(X) when is_atom(X) ->
    erlang:atom_to_binary(X, utf8);
as_io(X) when X =:= ""; X =:= <<"">> ->
    <<"fast_log_empty_string">>;
as_io(X) when is_binary(X); is_list(X) ->
    X;
as_io(X) when is_integer(X) ->
    integer_to_list(X);
as_io(X) when is_float(X) ->
    io_lib:format("~f", [X]);
as_io(X) when is_pid(X) orelse is_reference(X) ->
    io_lib:format("~p", [X]);
as_io({raw, X}) ->
    %% this is last-ditch effort, but may give acceptable results.
    io_lib:format("~256P", [X, 100]);
as_io({Fmt, Args}) ->
    io_lib:format(Fmt, Args).
