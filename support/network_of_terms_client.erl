%% @doc A client for the Network of Terms (Termennetwerk) GraphQL API as provided by the Dutch Heritage Network (NDE).
-module(network_of_terms_client).
-author("David de Boer <david@ddeboer.nl>").

-export([
    get_sources/0,
    find_terms/2,
    lookup/1
]).

-include("zotonic.hrl").

-define(URL, <<"https://termennetwerk-api.netwerkdigitaalerfgoed.nl/graphql">>).

%% @doc Get list of term sources that are available in the Network of Terms.
-spec get_sources() -> list(map()).
get_sources() ->
    #{<<"data">> := #{<<"sources">> := Sources}} = request(sources_query()),
    Sources.

%% @doc Find terms in the Network of Terms.
-spec find_terms(list(binary()), binary()) -> list(map()).
find_terms([], _) ->
    [];
find_terms(_, <<>>) ->
    [];
find_terms(Sources, Query) when is_list(Sources) and is_binary(Query) ->
    #{<<"data">> := #{<<"terms">> := Terms}} = request(terms_query(Sources, Query)),
    Terms.

-spec lookup(list(binary())) -> list(map()).
lookup(Uris) ->
    #{<<"data">> := #{<<"lookup">> := Results}} = request(lookup_query(Uris)),
    Results.

request(Query) ->
    case httpc:request(post, {binary_to_list(?URL), [], "application/json", jsx:encode(Query)}, httpc_options(), []) of
        {ok, {{_, StatusCode, _}, _Headers, Body}} when StatusCode < 400 ->
            jsx:decode(list_to_binary(Body))
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

