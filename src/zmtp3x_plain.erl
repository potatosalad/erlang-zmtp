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
-module(zmtp3x_plain).
-behaviour(zmtp3x_mechanism).

-include("zmtp.hrl").
-include("zmtp3x.hrl").
-include("zmtp3x_mechanism.hrl").
-include("zmtp3x_plain.hrl").

%% API
-export([build_error/1]).
-export([build_hello/1]).
-export([build_initiate/1]).
-export([build_ready/1]).
-export([build_welcome/1]).

%% zmtp3x_mechanism callbacks
-export([build_command/2]).
-export([build_message/1]).
-export([decode_command/1]).
-export([encode_command/1]).

-define(COMMAND, #zmtp3x_command). % {}
-define(MESSAGE, #zmtp3x_message). % {}

-define(PLAIN_ERROR,    #zmtp3x_plain_error).    % {}
-define(PLAIN_HELLO,    #zmtp3x_plain_hello).    % {}
-define(PLAIN_INITIATE, #zmtp3x_plain_initiate). % {}
-define(PLAIN_READY,    #zmtp3x_plain_ready).    % {}
-define(PLAIN_WELCOME,  #zmtp3x_plain_welcome).  % {}

%%%===================================================================
%%% API functions
%%%===================================================================

-spec build_error(Options::[{atom(), any()}])
	-> Error::zmtp3x_plain_error().
build_error(Options) when is_list(Options) ->
	Error = zmtp3x_mechanism:build_error(zmtp3x:req(error, Options)),
	?PLAIN_ERROR{error=Error}.

-spec build_hello(Options::[{atom(), any()}])
	-> Hello::zmtp3x_plain_hello().
build_hello(Options) when is_list(Options) ->
	Username = zmtp3x:req(username, Options),
	Password = zmtp3x:req(password, Options),
	?PLAIN_HELLO{username=Username, password=Password}.

-spec build_initiate(Options::[{atom(), any()}])
	-> Initiate::zmtp3x_plain_initiate().
build_initiate(Options) when is_list(Options) ->
	Metadata = zmtp3x_mechanism:build_metadata(zmtp3x:req(metadata, Options)),
	?PLAIN_INITIATE{metadata=Metadata}.

-spec build_ready(Options::[{atom(), any()}])
	-> Ready::zmtp3x_plain_ready().
build_ready(Options) when is_list(Options) ->
	Metadata = zmtp3x_mechanism:build_metadata(zmtp3x:req(metadata, Options)),
	?PLAIN_READY{metadata=Metadata}.

-spec build_welcome(Options::[{atom(), any()}])
	-> Welcome::zmtp3x_plain_welcome().
build_welcome(_Options) ->
	?PLAIN_WELCOME{}.

%%%===================================================================
%%% zmtp3x_mechanism callbacks
%%%===================================================================

-spec build_command(Name::binary(), Options::[{atom(), any()}])
	-> Command::zmtp3x_plain_hello() | zmtp3x_plain_welcome() | zmtp3x_plain_initiate() | zmtp3x_plain_ready() | zmtp3x_plain_error() | zmtp3x_command().
build_command(<<"ERROR">>, Options) ->
	build_error(Options);
build_command(<<"HELLO">>, Options) ->
	build_hello(Options);
build_command(<<"INITIATE">>, Options) ->
	build_initiate(Options);
build_command(<<"READY">>, Options) ->
	build_ready(Options);
build_command(<<"WELCOME">>, Options) ->
	build_welcome(Options);
build_command(Name, Options) ->
	zmtp3x:build_command([{name, Name} | Options]).

-spec build_message(Options::[{atom(), any()}])
	-> Message::zmtp3x_message().
build_message(Options) ->
	zmtp3x:build_message(Options).

-spec decode_command(CommandOrMessage::zmtp3x_command() | zmtp3x_message())
	-> PlainCommand::zmtp3x_plain_hello() | zmtp3x_plain_welcome() | zmtp3x_plain_initiate() | zmtp3x_plain_ready() | zmtp3x_plain_error() | zmtp3x_command() | zmtp3x_message().
decode_command(Message=?MESSAGE{}) ->
	Message;
decode_command(?COMMAND{name = <<"HELLO">>, data = << UsernameSize:8/integer, Username:UsernameSize/binary, PasswordSize:8/integer, Password:PasswordSize/binary >>}) ->
	?PLAIN_HELLO{username=Username, password=Password};
decode_command(?COMMAND{name = <<"WELCOME">>, data = <<>>}) ->
	?PLAIN_WELCOME{};
decode_command(?COMMAND{name = <<"INITIATE">>, data = CommandData}) ->
	?PLAIN_INITIATE{metadata=(zmtp3x_mechanism:decode_metadata(CommandData))};
decode_command(?COMMAND{name = <<"READY">>, data = CommandData}) ->
	?PLAIN_READY{metadata=(zmtp3x_mechanism:decode_metadata(CommandData))};
decode_command(?COMMAND{name = <<"ERROR">>, data = CommandData}) ->
	?PLAIN_ERROR{error=(zmtp3x_mechanism:decode_error(CommandData))};
decode_command(Command=?COMMAND{}) ->
	Command.

-spec encode_command(PlainCommand::zmtp3x_plain_hello() | zmtp3x_plain_welcome() | zmtp3x_plain_initiate() | zmtp3x_plain_ready() | zmtp3x_plain_error() | zmtp3x_message())
	-> CommandOrMessage::zmtp3x_command() | zmtp3x_message().
encode_command(Message=?MESSAGE{}) ->
	Message;
encode_command(Command=?COMMAND{}) ->
	Command;
encode_command(?PLAIN_HELLO{username=Username, password=Password}) ->
	UsernameSize = byte_size(Username),
	PasswordSize = byte_size(Password),
	CommandData = << UsernameSize:8/integer, Username:UsernameSize/binary, PasswordSize:8/integer, Password:PasswordSize/binary >>,
	?COMMAND{name = <<"HELLO">>, data = CommandData};
encode_command(?PLAIN_WELCOME{}) ->
	?COMMAND{name = <<"WELCOME">>, data = <<>>};
encode_command(?PLAIN_INITIATE{metadata=Metadata}) ->
	?COMMAND{name = <<"INITIATE">>, data = (zmtp3x_mechanism:encode_metadata(Metadata))};
encode_command(?PLAIN_READY{metadata=Metadata}) ->
	?COMMAND{name = <<"READY">>, data = (zmtp3x_mechanism:encode_metadata(Metadata))};
encode_command(?PLAIN_ERROR{error=Error}) ->
	?COMMAND{name = <<"ERROR">>, data = (zmtp3x_mechanism:encode_error(Error))}.

%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------
