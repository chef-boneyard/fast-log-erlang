%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Kevin Smith <kevin@opscode.com>
%% @copyright Copyright 2011 Opscode Inc.

-include_lib("kernel/src/disk_log.hrl").

-type calendar_time() :: { non_neg_integer(),  non_neg_integer(),  non_neg_integer() }.
-type calendar_date() :: { integer(),  1..12, 1..31 }.
-type erlang_time() :: {calendar_date(), calendar_time()}.

-define(LOG_DEBUG, debug).
-define(LOG_INFO, info).
-define(LOG_WARN, warn).
-define(LOG_ERR, err).

-record(config, {min_log=?LOG_DEBUG,
                 overloaded=false}).
