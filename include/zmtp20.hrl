%% -*- mode: erlang; tab-width: 4; indent-tabs-mode: 1; st-rulers: [70] -*-
%% vim: ts=4 sw=4 ft=erlang noet
%%%-------------------------------------------------------------------
%%% @author Andrew Bennett <potatosaladx@gmail.com>
%%% @copyright 2014-2018, Andrew Bennett
%%% @doc ZMTP/2.0
%%%      https://rfc.zeromq.org/spec:15/ZMTP
%%% @end
%%% Created :  15 Sep 2014 by Andrew Bennett <potatosaladx@gmail.com>
%%%-------------------------------------------------------------------

-ifndef(ZMTP20_HRL).

% -type zmtp20_greeting_socket_type() :: 'PAIR' | 'PUB' | 'SUB' | 'REQ'
% 	| 'REP' | 'DEALER' | 'ROUTER' | 'PULL' | 'PUSH'.

-type zmtp20_greeting_socket_type() :: binary().

-record(zmtp20_greeting, {
	socket_type = undefined :: undefined | zmtp20_greeting_socket_type(),
	identity    = undefined :: undefined | binary()
}).

-type zmtp20_greeting() :: #zmtp20_greeting{}.

-type zmtp20_message_more() :: final | more.

-record(zmtp20_message, {
	more = undefined :: undefined | zmtp20_message_more(),
	body = undefined :: undefined | binary()
}).

-type zmtp20_message() :: #zmtp20_message{}.

-define(ZMTP20_HRL, 1).

-endif.
