%% @doc A module to query the Network of Terms (Termennetwerk) provided by the Dutch Digital Heritage Network
%% (Netwerk Digitaal Erfgoed).
-module(mod_network_of_terms).
-author("David de Boer <david@ddeboer.nl>").

-mod_title("Network of Terms").
-mod_description("Search the Network of Terms (Termennetwerk)").
-mod_prio(500).
-mod_depends([
    mod_ginger_rdf
]).

-include_lib("zotonic.hrl").

-export([
    observe_search_query/2,
    event/2
]).

-spec observe_search_query(#search_query{}, z:context()) -> #search_result{}.
observe_search_query(#search_query{search = {terms, Args}} = _Query, _Context) ->
    #search_result{
        result = network_of_terms_client:find_terms(
            [z_convert:to_binary(S) || S <- proplists:get_value(sources, Args)],
            z_convert:to_binary(proplists:get_value(text, Args))
        )
    };
observe_search_query(#search_query{}, _Context) ->
    undefined.

-spec event(#postback_notify{}, z:context()) -> z:context().
event(#postback_notify{message = "feedback", target = TargetId, data = _Data}, Context) ->
    Sources = case z_context:get_q(sources, Context) of
        [] ->
            [];
        List -> z_string:split(List, ",")
    end,
    Vars = [
        {text, z_context:get_q(find_text, Context)},
        {sources, Sources},
        {template, z_context:get_q("template", Context)},
        {target, TargetId},
        {subject_id, z_convert:to_integer(z_context:get_q(subject_id, Context))},
        {predicate, z_context:get_q(predicate, Context, "")}
    ],
    z_render:wire(
        [
            {remove_class, [{target, TargetId}, {class, "loading"}]},
            {update, Vars}
        ],
        Context
    ).
