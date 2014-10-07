%% -*- mode: erlang; tab-width: 4; indent-tabs-mode: 1; st-rulers: [70] -*-
%% vim: ts=4 sw=4 ft=erlang noet
%%%-------------------------------------------------------------------
%%% @author Andrew Bennett <potatosaladx@gmail.com>
%%% @copyright 2014, Andrew Bennett
%%% @doc ZMTP-PLAIN/3.0 + ZMTP/3.1
%%%      http://rfc.zeromq.org/spec:24/ZMTP-PLAIN
%%%      http://rfc.zeromq.org/spec:37/ZMTP
%%% @end
%%% Created :  25 Sep 2014 by Andrew Bennett <potatosaladx@gmail.com>
%%%-------------------------------------------------------------------

-ifndef(ZMTP3X_PLAIN_HRL).

-record(zmtp3x_plain_hello, {
	username = undefined :: undefined | binary(),
	password = undefined :: undefined | binary()
}).

-type zmtp3x_plain_hello() :: #zmtp3x_plain_hello{}.

-record(zmtp3x_plain_welcome, {}).

-type zmtp3x_plain_welcome() :: #zmtp3x_plain_welcome{}.

-record(zmtp3x_plain_error, {
	error = undefined :: undefined | zmtp3x_error()
}).

-type zmtp3x_plain_error() :: #zmtp3x_plain_error{}.

-record(zmtp3x_plain_initiate, {
	metadata = undefined :: undefined | zmtp3x_metadata()
}).

-type zmtp3x_plain_initiate() :: #zmtp3x_plain_initiate{}.

-record(zmtp3x_plain_ready, {
	metadata = undefined :: undefined | zmtp3x_metadata()
}).

-type zmtp3x_plain_ready() :: #zmtp3x_plain_ready{}.

-define(ZMTP3X_PLAIN_HRL, 1).

-endif.
