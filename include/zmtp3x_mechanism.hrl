%% -*- mode: erlang; tab-width: 4; indent-tabs-mode: 1; st-rulers: [70] -*-
%% vim: ts=4 sw=4 ft=erlang noet
%%%-------------------------------------------------------------------
%%% @author Andrew Bennett <potatosaladx@gmail.com>
%%% @copyright 2014-2018, Andrew Bennett
%%% @doc ZMTP/3.1
%%%      http://rfc.zeromq.org/spec:37/ZMTP
%%% @end
%%% Created :  25 Sep 2014 by Andrew Bennett <potatosaladx@gmail.com>
%%%-------------------------------------------------------------------

-ifndef(ZMTP3X_MECHANISM_HRL).

% -type zmtp3x_socket_type() :: 'REQ' | 'REP' | 'DEALER' | 'ROUTER'
% 	| 'PUB' | 'XPUB' | 'SUB' | 'XSUB' | 'PUSH' | 'PULL' | 'PAIR'.

-type zmtp3x_socket_type() :: binary().

-record(zmtp3x_metadata, {
	socket_type = undefined :: undefined | zmtp3x_socket_type(),
	identity    = undefined :: undefined | binary(),
	resource    = undefined :: undefined | [binary()],
	metadata    = undefined :: undefined | [{binary(), binary()}]
}).

-type zmtp3x_metadata() :: #zmtp3x_metadata{}.

-record(zmtp3x_error, {
	reason = undefined :: undefined | binary()
}).

-type zmtp3x_error() :: #zmtp3x_error{}.

-define(ZMTP3X_MECHANISM_HRL, 1).

-endif.
