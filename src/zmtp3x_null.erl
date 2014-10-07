%% -*- mode: erlang; tab-width: 4; indent-tabs-mode: 1; st-rulers: [70] -*-
%% vim: ts=4 sw=4 ft=erlang noet
%%%-------------------------------------------------------------------
%%% @author Andrew Bennett <potatosaladx@gmail.com>
%%% @copyright 2014, Andrew Bennett
%%% @doc ZMTP-NULL/3.1
%%%      http://rfc.zeromq.org/spec:37/ZMTP
%%% @end
%%% Created :  25 Sep 2014 by Andrew Bennett <potatosaladx@gmail.com>
%%%-------------------------------------------------------------------
-module(zmtp3x_null).
-behaviour(zmtp3x_mechanism).

-include("zmtp.hrl").
-include("zmtp3x.hrl").
-include("zmtp3x_mechanism.hrl").
-include("zmtp3x_null.hrl").

%% API
-export([build_error/1]).
-export([build_ready/1]).

%% zmtp3x_mechanism callbacks
-export([build_command/2]).
-export([build_message/1]).
-export([decode_command/1]).
-export([encode_command/1]).

-define(COMMAND, #zmtp3x_command). % {}
-define(MESSAGE, #zmtp3x_message). % {}

-define(NULL_ERROR, #zmtp3x_null_error). % {}
-define(NULL_READY, #zmtp3x_null_ready). % {}

%%%===================================================================
%%% API functions
%%%===================================================================

-spec build_error(Options::[{atom(), any()}])
	-> Error::zmtp3x_null_error().
build_error(Options) when is_list(Options) ->
	Error = zmtp3x_mechanism:build_error(zmtp3x:req(error, Options)),
	?NULL_ERROR{error=Error}.

-spec build_ready(Options::[{atom(), any()}])
	-> Ready::zmtp3x_null_ready().
build_ready(Options) when is_list(Options) ->
	Metadata = zmtp3x_mechanism:build_metadata(zmtp3x:req(metadata, Options)),
	?NULL_READY{metadata=Metadata}.

%%%===================================================================
%%% zmtp3x_mechanism callbacks
%%%===================================================================

-spec build_command(Name::binary(), Options::[{atom(), any()}])
	-> Command::zmtp3x_null_error() | zmtp3x_null_ready() | zmtp3x_command().
build_command(<<"ERROR">>, Options) ->
	build_error(Options);
build_command(<<"READY">>, Options) ->
	build_ready(Options);
build_command(Name, Options) ->
	zmtp3x:build_command([{name, Name} | Options]).

-spec build_message(Options::[{atom(), any()}])
	-> Message::zmtp3x_message().
build_message(Options) ->
	zmtp3x:build_message(Options).

-spec decode_command(CommandOrMessage::zmtp3x_command() | zmtp3x_message())
	-> NullCommand::zmtp3x_null_ready() | zmtp3x_null_error() | zmtp3x_message().
decode_command(Message=?MESSAGE{}) ->
	Message;
decode_command(?COMMAND{name = <<"READY">>, data = CommandData}) ->
	?NULL_READY{metadata=(zmtp3x_mechanism:decode_metadata(CommandData))};
decode_command(?COMMAND{name = <<"ERROR">>, data = CommandData}) ->
	?NULL_ERROR{error=(zmtp3x_mechanism:decode_error(CommandData))};
decode_command(Command=?COMMAND{}) ->
	Command.

-spec encode_command(NullCommand::zmtp3x_null_ready() | zmtp3x_null_error() | zmtp3x_message())
	-> CommandOrMessage::zmtp3x_command() | zmtp3x_message().
encode_command(Message=?MESSAGE{}) ->
	Message;
encode_command(Command=?COMMAND{}) ->
	Command;
encode_command(?NULL_READY{metadata=Metadata}) ->
	?COMMAND{name = <<"READY">>, data = (zmtp3x_mechanism:encode_metadata(Metadata))};
encode_command(?NULL_ERROR{error=Error}) ->
	?COMMAND{name = <<"ERROR">>, data = (zmtp3x_mechanism:encode_error(Error))}.

%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------
