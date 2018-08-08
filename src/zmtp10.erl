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
-module(zmtp10).

-include("zmtp.hrl").
-include("zmtp10.hrl").

%% API
-export([decode_greeting/1]).
-export([decode_content/1]).
-export([encode_greeting/1]).
-export([encode_content/1]).

-define(CONTENT,  #zmtp10_content).  % {}
-define(GREETING, #zmtp10_greeting). % {}

%%%===================================================================
%%% API functions
%%%===================================================================

-spec decode_greeting(Data::binary())
	-> {Greeting::zmtp10_greeting(), Rest::binary()} | {incomplete, Data::binary()}.
decode_greeting(<< 16#01, _IDFlags:8, Rest/binary >>) ->
	{?GREETING{type=anonymous}, Rest};
decode_greeting(<< 16#FF, 16#01:64/big-unsigned-integer, _IDFlags:8, Rest/binary >>) ->
	{?GREETING{type=anonymous}, Rest};
decode_greeting(<< 16#FF, Length:64/big-unsigned-integer, IDFlagsIdentity:Length/binary, Rest/binary >>) ->
	decode_greeting_frame(IDFlagsIdentity, Rest);
decode_greeting(<< Length:8/integer, IDFlagsIdentity:Length/binary, Rest/binary >>) ->
	decode_greeting_frame(IDFlagsIdentity, Rest);
decode_greeting(Data) when is_binary(Data) ->
	{incomplete, Data}.

-spec decode_content(Data::binary())
	-> {Content::zmtp10_content(), Rest::binary()} | {error, incomplete}.
decode_content(<< 16#FF, Length:64/big-unsigned-integer, FlagsBody:Length/binary, Rest/binary >>) ->
	decode_content_frame(FlagsBody, Rest);
decode_content(<< Length:8/integer, FlagsBody:Length/binary, Rest/binary >>) ->
	decode_content_frame(FlagsBody, Rest);
decode_content(_) ->
	{error, incomplete}.

-spec encode_greeting(Greeting::zmtp10_greeting())
	-> Data::binary().
encode_greeting(?GREETING{type=anonymous}) ->
	<< 16#01, 16#00 >>;
encode_greeting(?GREETING{type={identity, Identity}}) ->
	case byte_size(Identity) of
		Length when Length < 253 ->
			<< (Length + 1):8/integer, 0:8, Identity:Length/binary >>;
		Length ->
			<< 16#FF, (Length + 1):64/big-unsigned-integer, 0:8, Identity:Length/binary >>
	end.

-spec encode_content(Content::zmtp10_content())
	-> Data::binary().
encode_content(?CONTENT{more=More0, body=Body}) when More0 == final orelse More0 == more ->
	More = more_flag_to_integer(More0),
	case byte_size(Body) of
		Length when Length < 253 ->
			<< (Length + 1):8/integer, 0:7, More:1, Body:Length/binary >>;
		Length ->
			<< 16#FF, (Length + 1):64/big-unsigned-integer, 0:7, More:1, Body:Length/binary >>
	end.

%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------

%% @private
decode_greeting_frame(<< _IDFlags:8, Identity/binary >>, Rest) ->
	{?GREETING{type={identity, Identity}}, Rest}.

%% @private
decode_content_frame(<< _Reserved:7, More:1/integer, Body/binary >>, Rest) ->
	{?CONTENT{more=(integer_to_more_flag(More)), body=Body}, Rest}.

%% @private
integer_to_more_flag(0) -> final;
integer_to_more_flag(1) -> more.

%% @private
more_flag_to_integer(final) -> 0;
more_flag_to_integer(more)  -> 1.
