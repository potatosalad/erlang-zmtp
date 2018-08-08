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
-module(zmtp20).

-include("zmtp.hrl").
-include("zmtp20.hrl").

%% API
-export([decode_greeting/1]).
-export([decode_message/1]).
-export([encode_greeting/1]).
-export([encode_message/1]).

-define(GREETING, #zmtp20_greeting). % {}
-define(MESSAGE,  #zmtp20_message).  % {}

%%%===================================================================
%%% API functions
%%%===================================================================

-spec decode_greeting(Data::binary())
	-> {Greeting::zmtp20_greeting(), Rest::binary()} | {incomplete, Data::binary()}.
decode_greeting(<< 16#FF, 0:64/big-unsigned-integer, 16#7F, 16#01, SocketType:8, Length:8, Identity:Length/binary, Rest/binary >>) ->
	{?GREETING{socket_type=socket_type_to_binary(SocketType), identity=Identity}, Rest};
decode_greeting(<< 16#FF, 0:64/big-unsigned-integer, 16#7F, 16#02, SocketType:8, Length:8, Identity:Length/binary, Rest/binary >>) ->
	{?GREETING{socket_type=socket_type_to_binary(SocketType), identity=Identity}, Rest};
decode_greeting(Data) when is_binary(Data) ->
	{incomplete, Data}.

-spec decode_message(Data::binary())
	-> {Message::zmtp20_message(), Rest::binary()} | {error, incomplete}.
decode_message(<< _Reserved:6, 0:1, More:1, Length:8/integer, Body:Length/binary, Rest/binary >>) ->
	{?MESSAGE{more=(integer_to_more_flag(More)), body=Body}, Rest};
decode_message(<< _Reserved:6, 1:1, More:1, Length:64/big-unsigned-integer, Body:Length/binary, Rest/binary >>) ->
	{?MESSAGE{more=(integer_to_more_flag(More)), body=Body}, Rest};
decode_message(_) ->
	{error, incomplete}.

-spec encode_greeting(Greeting::zmtp20_greeting())
	-> Data::binary().
encode_greeting(?GREETING{socket_type=SocketType, identity=Identity}) ->
	case byte_size(Identity) of
		Length when Length > 16#FF ->
			erlang:error(badarg);
		Length ->
			<< 16#FF, 0:64/big-unsigned-integer, 16#7F, 16#01, (socket_type_to_integer(SocketType)):8, Length:8/integer, Identity:Length/binary >>
	end.

-spec encode_message(Message::zmtp20_message())
	-> Data::binary().
encode_message(?MESSAGE{more=More0, body=Body}) when More0 == final orelse More0 == more ->
	More = more_flag_to_integer(More0),
	case byte_size(Body) of
		Length when Length > 16#FF ->
			<< 0:6, 1:1, More:1, Length:64/big-unsigned-integer, Body:Length/binary >>;
		Length ->
			<< 0:6, 0:1, More:1, Length:8/integer, Body:Length/binary >>
	end.

%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------

%% @private
socket_type_to_integer(<<"PAIR">>)   -> 16#00;
socket_type_to_integer(<<"PUB">>)    -> 16#01;
socket_type_to_integer(<<"SUB">>)    -> 16#02;
socket_type_to_integer(<<"REQ">>)    -> 16#03;
socket_type_to_integer(<<"REP">>)    -> 16#04;
socket_type_to_integer(<<"DEALER">>) -> 16#05;
socket_type_to_integer(<<"ROUTER">>) -> 16#06;
socket_type_to_integer(<<"PULL">>)   -> 16#07;
socket_type_to_integer(<<"PUSH">>)   -> 16#08.

%% @private
integer_to_more_flag(0) -> final;
integer_to_more_flag(1) -> more.

%% @private
more_flag_to_integer(final) -> 0;
more_flag_to_integer(more)  -> 1.

%% @private
socket_type_to_binary(16#00) -> <<"PAIR">>;
socket_type_to_binary(16#01) -> <<"PUB">>;
socket_type_to_binary(16#02) -> <<"SUB">>;
socket_type_to_binary(16#03) -> <<"REQ">>;
socket_type_to_binary(16#04) -> <<"REP">>;
socket_type_to_binary(16#05) -> <<"DEALER">>;
socket_type_to_binary(16#06) -> <<"ROUTER">>;
socket_type_to_binary(16#07) -> <<"PULL">>;
socket_type_to_binary(16#08) -> <<"PUSH">>.
