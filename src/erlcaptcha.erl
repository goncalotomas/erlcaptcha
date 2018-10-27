%% Server validation module for Google's reCAPTCHA API (compatible with v2)
-module(erlcaptcha).

-behaviour(gen_server).

%% API
-export([start_link/0, stop/0, verify/1, verify/2]).
%% gen_server exports
-export([init/1, handle_cast/2, handle_call/3]).

-define(SERVER, ?MODULE).
-define(API_ENDPOINT, <<"https://www.google.com/recaptcha/api/siteverify">>).
-define(APP, erlcaptcha).

-record(state, {
    secret :: binary() | undefined
}).

-type success_resp() :: list(success_params()).
-type success_params() :: {success, boolean()} |
                          {timestamp, binary()} |
                          {hostname, binary()}.
-type error_resp() :: {errors, list(error_atoms())}.
-type error_atoms() :: missing_input_secret |
                       invalid_input_secret |
                       missing_input_response |
                       invalid_input_response |
                       bad_request.

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

stop() ->
    gen_server:stop(?SERVER).

init([]) ->
    Secret = case application:get_env(?APP, secret) of
        undefined -> undefined;
        {ok, Value} -> Value
    end,
    {ok, #state{secret = Secret}}.

-spec verify(binary()) -> success_resp() | error_resp().
verify(Response) ->
    gen_server:call(?SERVER, {verify, Response}).

-spec verify(binary(), binary()) -> success_resp() | error_resp().
verify(Response, RemoteIP) ->
    gen_server:call(?SERVER, {verify, Response, RemoteIP}).

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_call({verify, Response}, _From, State) ->
    Secret = State#state.secret,
    Payload = {form, [{secret, Secret}, {response, Response}]},
    {ok, _StatusCode, _RespHeaders, ClientRef} = hackney:post(?API_ENDPOINT, [], Payload, []),
    {ok, Body} = hackney:body(ClientRef),
    Json = jsx:decode(Body),
    Reply = resp_from_json(Json),
    {reply, Reply, State};

handle_call({verify, Response, RemoteIP}, _From, State) ->
    Secret = State#state.secret,
    Payload = {form, [{secret, Secret}, {response, Response}, {remoteip, RemoteIP}]},
    {ok, _StatusCode, _RespHeaders, ClientRef} = hackney:post(?API_ENDPOINT, [], Payload, []),
    {ok, Body} = hackney:body(ClientRef),
    Json = jsx:decode(Body),
    Reply = resp_from_json(Json),
    {reply, Reply, State}.

resp_from_json(Json) ->
    case proplists:get_value(<<"error-codes">>, Json) of
        undefined ->
            parse_response(Json);
        List ->
            parse_errors(List)
    end.

parse_response(Json) ->
    Success = proplists:get_value(<<"success">>, Json),
    Timestamp = proplists:get_value(<<"challenge_ts">>, Json),
    Hostname = proplists:get_value(<<"hostname">>, Json),
    %% these will only be defined for v3 reCAPTCHA keys
    Action = proplists:get_value(<<"action">>, Json),
    Score = proplists:get_value(<<"score">>, Json),
    [{success, Success}, {challenge_ts, Timestamp}, {hostname, Hostname}, {score, Score}, {action, Action}].

parse_errors(List) ->
    {errors, lists:map(fun(Error) -> parse_error(Error) end, List)}.

parse_error(<<"missing-input-secret">>) ->      missing_input_secret;
parse_error(<<"invalid-input-secret">>) ->      invalid_input_secret;
parse_error(<<"missing-input-response">>) ->    missing_input_response;
parse_error(<<"invalid-input-response">>) ->    invalid_input_response;
parse_error(<<"bad-request">>) ->               bad_request.


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
%% this is a publicly listed test secret key from Google, so don't panic. We won't post secret keys on GitHub.
-define(TEST_SECRET, <<"6LeIxAcTAAAAAGG-vFI1TnRWxMZNFuojJ4WifJWe">>).

invalid_response_test() ->
    application:ensure_all_started(jsx),
    application:ensure_all_started(hackney),
    %% notice how we are NOT setting a valid secret value
    %% this is required because the test key considers all requests to be successful
    %% application:set_env(?APP, secret, ?TEST_SECRET),
    ?MODULE:start_link(),
    ?assertEqual({errors, [invalid_input_response, invalid_input_secret]}, ?MODULE:verify(<<"someResponse">>)),
    ?MODULE:stop(),
    ok.

missing_response_test() ->
    application:ensure_all_started(jsx),
    application:ensure_all_started(hackney),
    %% notice how we are NOT setting a valid secret value
    %% this is required because the test key considers all requests to be successful
    %% application:set_env(?APP, secret, ?TEST_SECRET),
    ?MODULE:start_link(),
    ?assertEqual({errors, [missing_input_response, invalid_input_secret]}, ?MODULE:verify(<<>>)),
    ?MODULE:stop(),
    ok.

invalid_secret_test() ->
    application:ensure_all_started(jsx),
    application:ensure_all_started(hackney),
    application:set_env(?APP, secret, <<"someSecretThatIsVeryUnlikelyToBeValid">>),
    ?MODULE:start_link(),
    ?assertEqual({errors, [missing_input_response, invalid_input_secret]}, ?MODULE:verify(<<>>)),
    ?MODULE:stop(),
    ok.

successful_response_test() ->
    application:ensure_all_started(jsx),
    application:ensure_all_started(hackney),
    application:set_env(?APP, secret, ?TEST_SECRET),
    ?MODULE:start_link(),
    ?assert(is_successful(?MODULE:verify(<<"someResponse">>, <<"127.0.0.1">>))),
    ?assert(is_successful(?MODULE:verify(<<"someOtherResponse">>))),
    ?MODULE:stop(),
    ok.

is_successful(Proplist) ->
    case proplists:get_value(success, Proplist) of
        true -> true;
        _ -> false
    end.

-endif.
