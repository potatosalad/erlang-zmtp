%% -*- mode: erlang; tab-width: 4; indent-tabs-mode: 1; st-rulers: [70] -*-
%% vim: ts=4 sw=4 ft=erlang noet
%%%-------------------------------------------------------------------
%%% @author Andrew Bennett <potatosaladx@gmail.com>
%%% @copyright 2014, Andrew Bennett
%%% @doc ZMTP/3.0
%%%      http://rfc.zeromq.org/spec:23/ZMTP
%%%      ZMTP/3.1
%%%      http://rfc.zeromq.org/spec:37/ZMTP
%%% @end
%%% Created :  25 Sep 2014 by Andrew Bennett <potatosaladx@gmail.com>
%%%-------------------------------------------------------------------
-module(zmtp3x).

-include("zmtp.hrl").
-include("zmtp3x.hrl").

%% API
-export([build_command/1]).
-export([build_command/3]).
-export([build_greeting/1]).
-export([build_message/1]).
-export([build_message/2]).
-export([build_ping/1]).
-export([build_pong/1]).
-export([decode_greeting/1]).
-export([decode_handshake/1]).
-export([decode_handshake/2]).
-export([decode_handshake/3]).
-export([decode_traffic/1]).
-export([decode_traffic/2]).
-export([decode_traffic/3]).
-export([encode_greeting/1]).
-export([encode_handshake/1]).
-export([encode_handshake/2]).
-export([encode_handshake/3]).
-export([encode_traffic/1]).
-export([encode_traffic/2]).
-export([encode_traffic/3]).

%% Internal API
-export([opt/2]).
-export([opt/3]).
-export([req/2]).

-define(COMMAND,  #zmtp3x_command).  % {}
-define(GREETING, #zmtp3x_greeting). % {}
-define(MESSAGE,  #zmtp3x_message).  % {}
-define(PING,     #zmtp3x_ping).     % {}
-define(PONG,     #zmtp3x_pong).     % {}

%%%===================================================================
%%% API functions
%%%===================================================================

-spec build_command(Options::[{atom(), any()}])
	-> Command::zmtp3x_command().
build_command(Options) when is_list(Options) ->
	Name = req(name, Options),
	Data = req(data, Options),
	?COMMAND{name=Name, data=Data}.

-spec build_command(Name::binary(), Options::[{any(), any()}], Mechanism::module() | {module(), any()})
	-> Command::zmtp3x_command() | any().
build_command(Name, Options, Mechanism) ->
	Mechanism:build_command(Name, Options).

-spec build_greeting(Options::[{atom(), any()}])
	-> Greeting::zmtp3x_greeting().
build_greeting(Options) when is_list(Options) ->
	AsServer = opt(as_server, Options, false),
	Mechanism = opt(mechanism, Options, <<"NULL">>),
	VersionMinor = opt(minor, Options, 1),
	?GREETING{mechanism=Mechanism, as_server=AsServer, version_minor=VersionMinor}.

-spec build_message(Options::[{atom(), any()}])
	-> Message::zmtp3x_message().
build_message(Options) when is_list(Options) ->
	More = opt(more, Options, final),
	Body = opt(body, Options, <<>>),
	?MESSAGE{more=More, body=Body}.

-spec build_message(Options::[{atom(), any()}], Mechanism::module() | {module(), any()})
	-> Message::zmtp3x_message() | any().
build_message(Options, Mechanism) when is_list(Options) ->
	Mechanism:build_message(Options).

-spec build_ping(Options::[{atom(), any()}])
	-> Ping::zmtp3x_ping().
build_ping(Options) when is_list(Options) ->
	TTL = req(ttl, Options),
	Context = opt(context, Options, <<>>),
	?PING{ttl=TTL, context=Context}.

-spec build_pong(Options::[{atom(), any()}])
	-> Pong::zmtp3x_pong().
build_pong(Options) when is_list(Options) ->
	Context = opt(context, Options, <<>>),
	?PONG{context=Context}.

-spec decode_greeting(Data::binary())
	-> {Greeting::zmtp3x_greeting(), Rest::binary()} | {incomplete, Data::binary()}.
decode_greeting(<< 16#FF, _Padding:64, 16#7F, 16#03, VersionMinor:8/integer, Mechanism:20/binary, AsServer:8, _Filler:31/binary, Rest/binary >>) ->
	{?GREETING{mechanism=(decode_greeting_mechanism(Mechanism, <<>>)), as_server=(integer_to_boolean(AsServer)), version_minor=VersionMinor}, Rest};
decode_greeting(Data) when is_binary(Data) ->
	{incomplete, Data}.

-spec decode_handshake(Data::binary())
	-> {Command::zmtp3x_command() | zmtp3x_ping() | zmtp3x_pong(), Rest::binary()} | {incomplete, Data::binary()}.
decode_handshake(<< _Reserved:5, 1:1, 0:1, 0:1, CommandSize:8/integer, CommandBody:CommandSize/binary, Rest/binary >>) ->
	decode_command_body(CommandBody, Rest);
decode_handshake(<< _Reserved:5, 1:1, 1:1, 0:1, CommandSize:64/big-unsigned-integer, CommandBody:CommandSize/binary, Rest/binary >>) ->
	decode_command_body(CommandBody, Rest);
decode_handshake(Data) when is_binary(Data) ->
	{incomplete, Data}.

-spec decode_handshake(Data::binary(), Mechanism::module())
	-> {Command::zmtp3x_command() | zmtp3x_ping() | zmtp3x_pong() | any(), Rest::binary()} | {incomplete, Data::binary()}.
decode_handshake(Data, Mechanism) when is_binary(Data) ->
	case decode_handshake(Data) of
		{Command=?COMMAND{}, Rest} ->
			{Mechanism:decode_command(Command), Rest};
		NonCommand ->
			NonCommand
	end.

-spec decode_handshake(Data::binary(), Mechanism::module(), MechanismState::any())
	-> {Command::zmtp3x_command() | zmtp3x_ping() | zmtp3x_pong() | any(), Rest::binary()} | {incomplete, Data::binary()}.
decode_handshake(Data, Mechanism, MechanismState) when is_binary(Data) ->
	case decode_handshake(Data) of
		{Command=?COMMAND{}, Rest} ->
			{Mechanism:decode_command(Command, MechanismState), Rest};
		NonCommand ->
			NonCommand
	end.

-spec decode_traffic(Data::binary())
	-> {Traffic::zmtp3x_command() | zmtp3x_ping() | zmtp3x_pong() | zmtp3x_message(), Rest::binary()} | {incomplete, Data::binary()}.
decode_traffic(<< _Reserved:5, 0:1, 0:1, More:1, Length:8/integer, MessageBody:Length/binary, Rest/binary >>) ->
	{?MESSAGE{more=(integer_to_more_flag(More)), body=MessageBody}, Rest};
decode_traffic(<< _Reserved:5, 0:1, 1:1, More:1, Length:64/big-unsigned-integer, MessageBody:Length/binary, Rest/binary >>) ->
	{?MESSAGE{more=(integer_to_more_flag(More)), body=MessageBody}, Rest};
decode_traffic(<< _Reserved:5, 1:1, 0:1, 0:1, CommandSize:8/integer, CommandBody:CommandSize/binary, Rest/binary >>) ->
	decode_command_body(CommandBody, Rest);
decode_traffic(<< _Reserved:5, 1:1, 1:1, 0:1, CommandSize:64/big-unsigned-integer, CommandBody:CommandSize/binary, Rest/binary >>) ->
	decode_command_body(CommandBody, Rest);
decode_traffic(Data) when is_binary(Data) ->
	{incomplete, Data}.

-spec decode_traffic(Data::binary(), Mechanism::module())
	-> {Traffic::zmtp3x_command() | zmtp3x_ping() | zmtp3x_pong() | zmtp3x_message() | any(), Rest::binary()} | {incomplete, Data::binary()} | {error, failed}.
decode_traffic(Data, Mechanism) when is_binary(Data) ->
	case decode_traffic(Data) of
		{Command=?COMMAND{}, Rest} ->
			{Mechanism:decode_command(Command), Rest};
		NonCommand ->
			NonCommand
	end.

-spec decode_traffic(Data::binary(), Mechanism::module(), MechanismState::any())
	-> {Traffic::zmtp3x_command() | zmtp3x_ping() | zmtp3x_pong() | zmtp3x_message() | any(), Rest::binary()} | {incomplete, Data::binary()} | {error, failed}.
decode_traffic(Data, Mechanism, MechanismState) when is_binary(Data) ->
	case decode_traffic(Data) of
		{Command=?COMMAND{}, Rest} ->
			{Mechanism:decode_command(Command, MechanismState), Rest};
		NonCommand ->
			NonCommand
	end.

-spec encode_greeting(Greeting::zmtp3x_greeting())
	-> Data::binary().
encode_greeting(?GREETING{mechanism=Mechanism, as_server=AsServer, version_minor=VersionMinor}) ->
	<< 16#FF, 0:64, 16#7F, 16#03, VersionMinor:8/integer, (encode_greeting_mechanism(Mechanism)):20/binary, (boolean_to_integer(AsServer)):8/integer, 0:(31 * 8) >>.

-spec encode_handshake(Command::zmtp3x_command() | zmtp3x_ping() | zmtp3x_pong())
	-> Data::binary().
encode_handshake(?PING{ttl=TTL, context=Context}) ->
	encode_handshake(?COMMAND{name = <<"PING">>, data = << TTL:16/big-unsigned-integer, Context/binary >>});
encode_handshake(?PONG{context=Context}) ->
	encode_handshake(?COMMAND{name = <<"PONG">>, data = << Context/binary >>});
encode_handshake(?COMMAND{name=CommandName, data=CommandData}) ->
	case byte_size(CommandName) of
		CommandNameSize when CommandNameSize > 16#FF ->
			erlang:error(badarg);
		CommandNameSize ->
			CommandBody = << CommandNameSize:8/integer, CommandName:CommandNameSize/binary, CommandData/binary >>,
			case byte_size(CommandBody) of
				CommandSize when CommandSize > 16#FF ->
					<< 0:5, 1:1, 1:1, 0:1, CommandSize:64/big-unsigned-integer, CommandBody:CommandSize/binary >>;
				CommandSize ->
					<< 0:5, 1:1, 0:1, 0:1, CommandSize:8/integer, CommandBody:CommandSize/binary >>
			end
	end.

-spec encode_handshake(MechanismCommand::any(), Mechanism::module())
	-> DataOrError::binary() | any().
encode_handshake(MechanismCommand, Mechanism) when is_atom(Mechanism) ->
	case Mechanism:encode_command(MechanismCommand) of
		Command=?COMMAND{} ->
			encode_handshake(Command);
		Error ->
			Error
	end.

-spec encode_handshake(MechanismCommand::any(), Mechanism::module(), MechanismState::any())
	-> DataOrError::binary() | any().
encode_handshake(MechanismCommand, Mechanism, MechanismState) ->
	case Mechanism:encode_command(MechanismCommand, MechanismState) of
		Command=?COMMAND{} ->
			encode_handshake(Command);
		Error ->
			Error
	end.

-spec encode_traffic(Traffic::zmtp3x_command() | zmtp3x_ping() | zmtp3x_pong() | zmtp3x_message())
	-> Data::binary().
encode_traffic(?MESSAGE{more=More0, body=MessageBody}) when More0 == final orelse More0 == more ->
	More = more_flag_to_integer(More0),
	case byte_size(MessageBody) of
		Length when Length > 16#FF ->
			<< 0:5, 0:1, 1:1, More:1, Length:64/big-unsigned-integer, MessageBody:Length/binary >>;
		Length ->
			<< 0:5, 0:1, 0:1, More:1, Length:8/integer, MessageBody:Length/binary >>
	end;
encode_traffic(?PING{ttl=TTL, context=Context}) ->
	encode_traffic(?COMMAND{name = <<"PING">>, data = << TTL:16/big-unsigned-integer, Context/binary >>});
encode_traffic(?PONG{context=Context}) ->
	encode_traffic(?COMMAND{name = <<"PONG">>, data = << Context/binary >>});
encode_traffic(?COMMAND{name=CommandName, data=CommandData}) ->
	case byte_size(CommandName) of
		CommandNameSize when CommandNameSize > 16#FF ->
			erlang:error(badarg);
		CommandNameSize ->
			CommandBody = << CommandNameSize:8/integer, CommandName:CommandNameSize/binary, CommandData/binary >>,
			case byte_size(CommandBody) of
				CommandSize when CommandSize > 16#FF ->
					<< 0:5, 1:1, 1:1, 0:1, CommandSize:64/big-unsigned-integer, CommandBody:CommandSize/binary >>;
				CommandSize ->
					<< 0:5, 1:1, 0:1, 0:1, CommandSize:8/integer, CommandBody:CommandSize/binary >>
			end
	end.

-spec encode_traffic(MechanismCommand::any(), Mechanism::module())
	-> DataOrError::binary() | any().
encode_traffic(MechanismCommand, Mechanism) ->
	case Mechanism:encode_command(MechanismCommand) of
		Message=?MESSAGE{} ->
			encode_traffic(Message);
		Command=?COMMAND{} ->
			encode_traffic(Command);
		Error ->
			Error
	end.

-spec encode_traffic(MechanismCommand::any(), Mechanism::module(), MechanismState::any())
	-> DataOrError::binary() | any().
encode_traffic(MechanismCommand, Mechanism, MechanismState) ->
	case Mechanism:encode_command(MechanismCommand, MechanismState) of
		Message=?MESSAGE{} ->
			encode_traffic(Message);
		Command=?COMMAND{} ->
			encode_traffic(Command);
		Error ->
			Error
	end.

%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------

%% @private
boolean_to_integer(false) -> 0;
boolean_to_integer(true)  -> 1.

%% @private
decode_greeting_mechanism(<< 0, _/binary>>, Mechanism) ->
	Mechanism;
decode_greeting_mechanism(<< C, Rest/binary >>, Mechanism) ->
	decode_greeting_mechanism(Rest, << Mechanism/binary, C >>).

%% @private
decode_command_body(<< CommandNameSize:8/integer, CommandName:CommandNameSize/binary, CommandData/binary >>, Rest) ->
	case CommandName of
		<<"PING">> ->
			<< TTL:16/big-unsigned-integer, Context/binary >> = CommandData,
			{?PING{ttl=TTL, context=Context}, Rest};
		<<"PONG">> ->
			Context = CommandData,
			{?PONG{context=Context}, Rest};
		_ ->
			{?COMMAND{name=CommandName, data=CommandData}, Rest}
	end.

%% @private
encode_greeting_mechanism(Mechanism) when byte_size(Mechanism) == 20 ->
	Mechanism;
encode_greeting_mechanism(Mechanism) when byte_size(Mechanism) < 20 ->
	Length = byte_size(Mechanism),
	Padding = 20 - Length,
	<< Mechanism/binary, 0:(Padding * 8) >>;
encode_greeting_mechanism(Mechanism) when is_atom(Mechanism) ->
	encode_greeting_mechanism(Mechanism:key()).

%% @private
integer_to_boolean(0) -> false;
integer_to_boolean(1) -> true.

%% @private
integer_to_more_flag(0) -> final;
integer_to_more_flag(1) -> more.

%% @private
more_flag_to_integer(final) -> 0;
more_flag_to_integer(more)  -> 1.

%% @private
opt(Key, P) ->
	opt(Key, P, undefined).

%% @private
opt(Key, P, Default) ->
	case lists:keyfind(Key, 1, P) of
		false ->
			Default;
		{Key, Value} ->
			Value
	end.

%% @doc Return `Value' for `Key' in proplist `P' or crashes with an
%% informative message if no value is found.
%% @private
req(Key, P) ->
	case lists:keyfind(Key, 1, P) of
		false ->
			erlang:error({missing_required_config, Key, P});
		{Key, Value} ->
			Value
	end.
