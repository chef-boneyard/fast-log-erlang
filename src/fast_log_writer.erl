%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Kevin Smith <kevin@opscode.com>
%% @copyright Copyright 2011 Opscode Inc.

-module(fast_log_writer).

-include("fast_log.hrl").

-export([open/4,
         write/3]).

-spec(open(string(), string(), pos_integer(), pos_integer()) ->
             {ok, #continuation{}} | {error, any()}).
open(Name, FileName, MaxFiles, MaxFileSize) ->
    disk_log:open([{name, Name},
                   {file, FileName},
                   {size, {MaxFileSize * 1024 * 1024, MaxFiles}},
                   {type, wrap},
                   {format, external}]).

-spec(write(#continuation{}, atom(), string()) -> ok).
write(Log, LogLevel0, Output) ->
    Timestamp = fast_log_util:time_iso8601(),
    Node = atom_to_list(node()),
    LogLevel = fast_log_util:log_level(LogLevel0),
    Prefix = io_lib:format("~s ~s ~s ", [Timestamp, Node, LogLevel]),
    Msg = iolist_to_binary([Prefix, Output, $\n]),
    disk_log:blog(Log, Msg).
