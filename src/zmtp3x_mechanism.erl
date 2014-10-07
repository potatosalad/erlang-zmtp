%% -*- mode: erlang; tab-width: 4; indent-tabs-mode: 1; st-rulers: [70] -*-
%% vim: ts=4 sw=4 ft=erlang noet
%%%-------------------------------------------------------------------
%%% @author Andrew Bennett <potatosaladx@gmail.com>
%%% @copyright 2014, Andrew Bennett
%%% @doc ZMTP/3.1
%%%      http://rfc.zeromq.org/spec:37/ZMTP
%%% @end
%%% Created :  25 Sep 2014 by Andrew Bennett <potatosaladx@gmail.com>
%%%-------------------------------------------------------------------
-module(zmtp3x_mechanism).

-include("zmtp.hrl").
-include("zmtp3x.hrl").
-include("zmtp3x_mechanism.hrl").

%% API
-export([build_error/1]).
-export([build_metadata/1]).
-export([decode_error/1]).
-export([decode_metadata/1]).
-export([encode_error/1]).
-export([encode_metadata/1]).

-define(ERROR,    #zmtp3x_error).    % {}
-define(METADATA, #zmtp3x_metadata). % {}

-callback build_command(Name::binary(), Options::[{any(), any()}])
	-> Command::any().
-callback build_message(Options::[{any(), any()}])
	-> Message::any().
-callback decode_command(CommandOrMessage::zmtp3x_command() | zmtp3x_message())
	-> MechanismCommand::any().
-callback encode_command(MechanismCommand::any())
	-> CommandOrMessage::zmtp3x_command() | zmtp3x_message().

%%%===================================================================
%%% API functions
%%%===================================================================

-spec build_error(Options::[{atom(), any()}])
	-> Error::zmtp3x_error().
build_error(Options) when is_list(Options) ->
	Reason = zmtp3x:req(reason, Options),
	?ERROR{reason=Reason}.

-spec build_metadata(Options::[{atom() | binary(), any()}])
	-> Metadata::zmtp3x_metadata().
build_metadata(Options) when is_list(Options) ->
	lists:foldl(fun
		({Name, Val}, M=?METADATA{socket_type=S, identity=I, resource=R, metadata=D}) when S =:= undefined orelse I =:= undefined orelse R =:= undefined ->
			Key = case Name of
				_ when is_binary(Name) ->
					case ?INLINE_LOWERCASE_BC(Name) of
						K = <<"socket-type">> ->
							K;
						K = <<"identity">> ->
							K;
						K = <<"resource">> ->
							K;
						_ ->
							Name
					end;
				socket_type ->
					<<"socket-type">>;
				identity ->
					<<"identity">>;
				resource ->
					<<"resource">>;
				_ when is_atom(Name) ->
					atom_to_binary(Name, utf8)
			end,
			case {Key, S, I, R} of
				{<<"socket-type">>, undefined, _, _} ->
					M?METADATA{socket_type=Val};
				{<<"socket-type">>, _, _, _} ->
					M;
				{<<"identity">>, _, undefined, _} ->
					M?METADATA{identity=Val};
				{<<"identity">>, _, _, _} ->
					M;
				{<<"resource">>, _, _, undefined} ->
					M?METADATA{resource=Val};
				{<<"resource">>, _, _, _} ->
					M;
				{Key, _, _, _} ->
					M?METADATA{metadata=[{Key, Val} | D]}
			end;
		({Name, Val}, M=?METADATA{metadata=D}) ->
			Key = case Name of
				_ when is_binary(Name) ->
					Name;
				_ when is_atom(Name) ->
					atom_to_binary(Name, utf8)
			end,
			M?METADATA{metadata=[{Key, Val} | D]}
	end, ?METADATA{metadata=[]}, Options).

-spec decode_error(Data::binary())
	-> Error::zmtp3x_error().
decode_error(<< ErrorReasonSize:8/integer, ErrorReason:ErrorReasonSize/binary >>) ->
	?ERROR{reason=ErrorReason}.

-spec decode_metadata(Data::binary())
	-> Metadata::zmtp3x_metadata().
decode_metadata(Data) when is_binary(Data) ->
	decode_metadata(Data, ?METADATA{}, []).

-spec encode_error(Error::zmtp3x_error())
	-> Data::binary().
encode_error(?ERROR{reason=ErrorReason}) ->
	ErrorReasonSize = byte_size(ErrorReason),
	<< ErrorReasonSize:8/integer, ErrorReason:ErrorReasonSize/binary >>.

-spec encode_metadata(Metadata::zmtp3x_metadata())
	-> Data::binary().
encode_metadata(?METADATA{socket_type=SocketType, identity=Identity, resource=Resource, metadata=M0}) ->
	M1 = case Resource of
		undefined ->
			M0;
		_ ->
			[{<<"Resource">>, encode_resource(Resource, <<>>)} | M0]
	end,
	M2 = case Identity of
		undefined ->
			M1;
		_ ->
			[{<<"Identity">>, Identity} | M1]
	end,
	M3 = case SocketType of
		undefined ->
			M2;
		_ ->
			[{<<"Socket-Type">>, SocketType} | M2]
	end,
	<<
		<< (byte_size(Name)):8/integer, Name/binary, (byte_size(Value)):32/big-unsigned-integer, Value/binary >>
		|| {Name, Value} <- M3
	>>.

%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------

%% @private
decode_metadata(<< NameSize:8/integer, Name:NameSize/binary, ValueSize:32/big-unsigned-integer, Value:ValueSize/binary, Rest/binary >>, Metadata, Acc) ->
	case ?INLINE_LOWERCASE_BC(Name) of
		<<"socket-type">> ->
			decode_metadata(Rest, Metadata?METADATA{socket_type=Value}, Acc);
		<<"identity">> ->
			decode_metadata(Rest, Metadata?METADATA{identity=Value}, Acc);
		<<"resource">> ->
			Resource = decode_resource(Value, <<>>, []),
			decode_metadata(Rest, Metadata?METADATA{resource=Resource}, Acc);
		_ ->
			decode_metadata(Rest, Metadata, [{Name, Value} | Acc])
	end;
decode_metadata(<<>>, Metadata, Acc) ->
	Metadata?METADATA{metadata=lists:reverse(Acc)}.

%% @private
decode_resource(<< $/, Rest/binary >>, Current, Acc) ->
	decode_resource(Rest, <<>>, [Current | Acc]);
decode_resource(<< C, Rest/binary >>, Current, Acc) ->
	decode_resource(Rest, << Current/binary, C >>, Acc);
decode_resource(<<>>, Current, Acc) ->
	lists:reverse([Current | Acc]).

%% @private
encode_resource([Part | []], Acc) ->
	<< Acc/binary, Part/binary >>;
encode_resource([Part | Rest], Acc) ->
	encode_resource(Rest, << Acc/binary, $/, Part/binary >>);
encode_resource([], Acc) ->
	Acc.
