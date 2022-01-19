-module(m_network_of_terms).
-author("David de Boer <david@ddeboer.nl>").

-export([
    m_find_value/3,
    m_to_list/2,
    m_value/2
]).

-include_lib("zotonic.hrl").

-behaviour(gen_model).

m_find_value(sources, #m{}, _Context) ->
    network_of_terms_client:get_sources().

m_to_list(_Arg0, _Arg1) ->
    erlang:error(not_implemented).

m_value(_Arg0, _Arg1) ->
    erlang:error(not_implemented).
