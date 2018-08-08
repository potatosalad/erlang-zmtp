%% -*- mode: erlang; tab-width: 4; indent-tabs-mode: 1; st-rulers: [70] -*-
%% vim: ts=4 sw=4 ft=erlang noet
%%%-------------------------------------------------------------------
%%% @author Andrew Bennett <potatosaladx@gmail.com>
%%% @copyright 2014-2018, Andrew Bennett
%%% @doc ZMTP/3.0
%%%      https://rfc.zeromq.org/spec:23/ZMTP
%%%      ZMTP/3.1
%%%      https://rfc.zeromq.org/spec:37/ZMTP
%%% @end
%%% Created :  25 Sep 2014 by Andrew Bennett <potatosaladx@gmail.com>
%%%-------------------------------------------------------------------

-ifndef(ZMTP3X_HRL).

-record(zmtp3x_greeting, {
	mechanism     = undefined :: undefined | binary(),
	as_server     = undefined :: undefined | boolean(),
	version_minor = undefined :: undefined | integer()
}).

-type zmtp3x_greeting() :: #zmtp3x_greeting{}.

-record(zmtp3x_command, {
	name = undefined :: undefined | binary(),
	data = undefined :: undefined | binary()
}).

-type zmtp3x_command() :: #zmtp3x_command{}.

-type zmtp3x_message_more() :: final | more.

-record(zmtp3x_message, {
	more = undefined :: undefined | zmtp3x_message_more(),
	body = undefined :: undefined | binary()
}).

-type zmtp3x_message() :: #zmtp3x_message{}.

-record(zmtp3x_ping, {
	ttl     = undefined :: undefined | timeout(),
	context = undefined :: undefined | binary()
}).

-type zmtp3x_ping() :: #zmtp3x_ping{}.

-record(zmtp3x_pong, {
	context = undefined :: undefined | binary()
}).

-type zmtp3x_pong() :: #zmtp3x_pong{}.

-define(ZMTP3X_HRL, 1).

-endif.
