%% @doc A client for the Network of Terms (Termennetwerk) GraphQL API as provided by the Dutch Heritage Network (NDE).
-module(network_of_terms_client).
-author("David de Boer <david@ddeboer.nl>").

-export([
    get_sources/1,
    find_terms/3,
    lookup/2
]).

-include("zotonic.hrl").

-define(URL, <<"https://termennetwerk-api.netwerkdigitaalerfgoed.nl/graphql">>).

% Exported functions:

%% @doc ACL-protected version of 'get_sources/0'
-spec get_sources(z:context()) -> list(map()).
get_sources(Context) ->
    case network_of_terms_acl:is_allowed(Context) of
        false -> [];
        true -> get_sources()
    end.

%% @doc ACL-protected version of 'find_terms/2'
-spec find_terms(list(binary()), binary(), z:context()) -> list(map()).
find_terms(Sources, Query, Context) ->
    case network_of_terms_acl:is_allowed(Context) of
        false -> [];
        true -> find_terms(Sources, Query)
    end.

%% @doc ACL-protected version of 'lookup/1'
-spec lookup(list(binary()), z:context()) -> list(map()).
lookup(Uris, Context) ->
    case network_of_terms_acl:is_allowed(Context) of
        false -> [];
        true -> lookup(Uris)
    end.

% Internal functions:

%% @doc Get list of term sources that are available in the Network of Terms.
-spec get_sources() -> list(map()).
get_sources() ->
    request(<<"sources">>, sources_query()).

%% @doc Find terms in the Network of Terms.
-spec find_terms(list(binary()), binary()) -> list(map()).
find_terms([], _) ->
    [];
find_terms(_, <<>>) ->
    [];
find_terms(Sources, Query) when is_list(Sources) and is_binary(Query) ->
    request(<<"terms">>, terms_query(Sources, Query)).

-spec lookup(list(binary())) -> list(map()).
lookup(Uris) ->
    request(<<"lookup">>, lookup_query(Uris)).

request(DataField, Query) ->
    case httpc:request(post, {binary_to_list(?URL), [], "application/json", jsx:encode(Query)}, httpc_options(), []) of
        {ok, {{_, StatusCode, _}, _Headers, Body}} when StatusCode < 400 ->
            case jsx:decode(list_to_binary(Body)) of
                #{<<"data">> := #{DataField := Results}} ->
                    Results;
                ActualData ->
                    lager:error("Received unexpected data from query ~p: ~p", [Query, ActualData]),
                    []
            end;
        {ok, Result} ->
            lager:error("Unexpected response from termennetwerk-api for query ~p: ~p", [Query, Result]),
            [];
        {error, Reason} ->
            lager:error("Termennetwerk-api error for query ~p: ~p", [Query, Reason]),
            []
    end.

sources_query() ->
    #{
        <<"query">> => <<"
            {
                sources {
                    uri
                    name
                    alternateName
                    creators {
                        uri
                        name
                        alternateName
                    }
                }
            }
        ">>
    }.

terms_query(Sources, Query) when is_list(Sources) and is_binary(Query) ->
    #{
        <<"query">> => <<"
            query ($sources: [ID]!, $query: String!) {
                terms (sources: $sources query: $query queryMode: OPTIMIZED) {
                    source {
                        name
                        uri
                        alternateName
                        creators {
                            name
                            alternateName
                        }
                    }
                    result {
                        ... on Terms {
                            terms {
                                uri
                                prefLabel
                                altLabel
                                hiddenLabel
                                scopeNote
                                seeAlso
                                broader {
                                    uri
                                    prefLabel
                                }
                                narrower {
                                    uri
                                    prefLabel
                                }
                                related {
                                    uri
                                    prefLabel
                                }
                            }
                        }
                        ... on Error {
                            __typename
                            message
                        }
                    }
                }
          }">>,
        <<"variables">> => #{
            <<"sources">> => Sources,
            <<"query">> => Query
        }
    }.

lookup_query(Uris) when is_list(Uris) ->
    #{
        <<"query">> => <<"
            query ($uris: [ID]!) {
                lookup (uris: $uris) {
                    uri
                    source {
                        ... on Source {
                            name
                            uri
                            alternateName
                            creators {
                                uri
                                name
                                alternateName
                            }
                        }
                        ... on Error {
                            __typename
                            message
                        }
                    }
                    result {
                        __typename
                        ... on Term {
                            uri
                            prefLabel
                            altLabel
                            hiddenLabel
                            scopeNote
                            seeAlso
                            broader {
                                uri
                                prefLabel
                            }
                            narrower {
                                uri
                                prefLabel
                            }
                            related {
                                uri
                                prefLabel
                            }
                        }
                        ... on Error {
                            message
                        }
                    }
                }
            }">>,
        <<"variables">> => #{
            <<"uris">> => Uris
        }
    }.

httpc_options() ->
    [
        {timeout, 15000},
        {connect_timeout, 10000}
    ].

