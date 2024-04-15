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
-mod_schema(1).

-include_lib("zotonic.hrl").
-include_lib("mod_ginger_rdf/include/rdf.hrl").

-export([
    manage_schema/2,
    observe_search_query/2,
    event/2
]).

-spec manage_schema(pos_integer(), z:context()) -> ok.
manage_schema(_Version, Context) ->
    Datamodel = #datamodel{
        categories = [
            {term, rdf, [
                {title, {trans, [
                    {nl, "Term"},
                    {en, "Term"}
                ]}},
                {summary, {trans, [
                    {nl, "Een gedeelde term gevonden via het Termennetwerk."},
                    {en, "A shared term found via the Network of Terms."}
                ]}}
            ]}
        ]
    },
    z_datamodel:manage(?MODULE, Datamodel, Context).

-spec observe_search_query(#search_query{}, z:context()) -> #search_result{}.
observe_search_query(#search_query{search = {terms, Args}} = _Query, Context) ->
    #search_result{
        result = network_of_terms_client:find_terms(
            [z_convert:to_binary(S) || S <- proplists:get_value(sources, Args)],
            z_convert:to_binary(proplists:get_value(text, Args)),
            Context
        )
    };
observe_search_query(#search_query{}, _Context) ->
    undefined.

-spec event(#postback_notify{}, z:context()) -> z:context().
event(#postback_notify{message = "feedback", target = TargetId, data = _Data}, Context) ->
    case network_of_terms_acl:is_allowed(Context) of
        false -> Context;
        true ->
            Sources = case z_context:get_q(sources, Context) of
                [] ->
                    [];
                List -> z_string:split(List, ",")
            end,
            z_context:set_session(term_source_selection, Sources, Context),
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
            )
    end.
