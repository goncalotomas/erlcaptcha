%% Server validation module for Google's reCAPTCHA API (compatible with v2)
-module(erlcaptcha).

%% API
-export([verify/1, verify/2]).

-define(SERVER, ?MODULE).
-define(API_ENDPOINT, "https://www.google.com/recaptcha/api/siteverify").
-define(APP, erlcaptcha).

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

-spec verify(binary()) -> success_resp() | error_resp().
verify(Response) ->
    {ok, Secret} = application:get_env(?APP, secret),
    api_req(uri_string:compose_query([{"secret", Secret}, {"response", Response}])).

-spec verify(binary(), binary()) -> success_resp() | error_resp().
verify(Response, RemoteIP) ->
    {ok, Secret} = application:get_env(?APP, secret),
    api_req(uri_string:compose_query([{"secret", Secret}, {"response", Response}, {"remoteip", RemoteIP}])).

api_req(ReqBody) ->
    ReqOpts = [{sync, true}, {body_format, binary}],
    ContentType = "application/x-www-form-urlencoded",
    {ok, {_RespCode, _RespHeaders, Body}} = httpc:request(post, {?API_ENDPOINT, [], ContentType, ReqBody}, [], ReqOpts),
    io:format("decoding ~p...~n", [binary_to_list(Body)]),
    resp_from_json(jsx:decode(Body)).

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
-define(TEST_SECRET, "6LeIxAcTAAAAAGG-vFI1TnRWxMZNFuojJ4WifJWe").

invalid_response_test() ->
    application:ensure_all_started(jsx),
    %% notice how we are NOT setting a valid secret value
    %% this is required because the test key considers all requests to be successful
    application:set_env(?APP, secret, ""),
    ?assertEqual({errors, [invalid_input_response, invalid_input_secret]}, ?MODULE:verify("someResponse")),
    ok.

missing_response_test() ->
    application:ensure_all_started(jsx),
    %% notice how we are NOT setting a valid secret value
    %% this is required because the test key considers all requests to be successful
    application:set_env(?APP, secret, ""),
    ?assertEqual({errors, [missing_input_response, invalid_input_secret]}, ?MODULE:verify("")),
    ok.

invalid_secret_test() ->
    application:ensure_all_started(jsx),
    application:set_env(?APP, secret, "someSecretThatIsVeryUnlikelyToBeValid"),
    ?assertEqual({errors, [missing_input_response, invalid_input_secret]}, ?MODULE:verify("")),
    ok.

successful_response_test() ->
    application:ensure_all_started(jsx),
    application:set_env(?APP, secret, ?TEST_SECRET),
    ?assert(is_successful(?MODULE:verify("someResponse", "127.0.0.1"))),
    ?assert(is_successful(?MODULE:verify("someOtherResponse"))),
    ok.

is_successful(Proplist) ->
    case proplists:get_value(success, Proplist) of
        true -> true;
        _ -> false
    end.

-endif.
