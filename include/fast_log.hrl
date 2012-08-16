%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Kevin Smith <kevin@opscode.com>
%% Copyright 2011-2012 Opscode, Inc. All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%


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
