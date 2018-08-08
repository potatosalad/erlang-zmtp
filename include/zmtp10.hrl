%% -*- mode: erlang; tab-width: 4; indent-tabs-mode: 1; st-rulers: [70] -*-
%% vim: ts=4 sw=4 ft=erlang noet
%%%-------------------------------------------------------------------
%%% @author Andrew Bennett <potatosaladx@gmail.com>
%%% @copyright 2014-2018, Andrew Bennett
%%% @doc ZMTP/1.0
%%%      https://rfc.zeromq.org/spec:13/ZMTP
%%% @end
%%% Created :  15 Sep 2014 by Andrew Bennett <potatosaladx@gmail.com>
%%%-------------------------------------------------------------------

-ifndef(ZMTP10_HRL).

-type zmtp10_greeting_type() :: anonymous | {identity, binary()}.

-record(zmtp10_greeting, {
	type = undefined :: undefined | zmtp10_greeting_type()
}).

-type zmtp10_greeting() :: #zmtp10_greeting{}.

-type zmtp10_content_more() :: final | more.

-record(zmtp10_content, {
	more = undefined :: undefined | zmtp10_content_more(),
	body = undefined :: undefined | binary()
}).

-type zmtp10_content() :: #zmtp10_content{}.

-define(ZMTP10_HRL, 1).

-endif.
