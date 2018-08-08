%% -*- mode: erlang; tab-width: 4; indent-tabs-mode: 1; st-rulers: [70] -*-
%% vim: ts=4 sw=4 ft=erlang noet
%%%-------------------------------------------------------------------
%%% @author Andrew Bennett <potatosaladx@gmail.com>
%%% @copyright 2014-2018, Andrew Bennett
%%% @doc ZMTP-NULL/3.1
%%%      https://rfc.zeromq.org/spec:37/ZMTP/
%%% @end
%%% Created :  25 Sep 2014 by Andrew Bennett <potatosaladx@gmail.com>
%%%-------------------------------------------------------------------

-ifndef(ZMTP3X_NULL_HRL).

-record(zmtp3x_null_ready, {
	metadata = undefined :: undefined | zmtp3x_metadata()
}).

-type zmtp3x_null_ready() :: #zmtp3x_null_ready{}.

-record(zmtp3x_null_error, {
	error = undefined :: undefined | zmtp3x_error()
}).

-type zmtp3x_null_error() :: #zmtp3x_null_error{}.

-define(ZMTP3X_NULL_HRL, 1).

-endif.
