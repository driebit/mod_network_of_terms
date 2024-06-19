-module(m_network_of_terms).
-author("David de Boer <david@ddeboer.nl>").

-export([
    m_find_value/3,
    m_to_list/2,
    m_value/2,
    describe/2
]).

-include_lib("zotonic.hrl").

-behaviour(gen_model).

m_find_value(is_allowed, #m{}, Context) ->
    network_of_terms_acl:is_allowed(Context);
m_find_value(sources, #m{}, Context) ->
    network_of_terms_client:get_sources(Context);
m_find_value(Uri, #m{}, Context) when not is_binary(Uri) ->
    m_find_value(z_convert:to_binary(Uri), #m{}, Context);
m_find_value(Uri, #m{}, Context) when is_binary(Uri) ->
    describe(Uri, Context).

m_to_list(_Arg0, _Arg1) ->
    erlang:error(not_implemented).

m_value(_Arg0, _Arg1) ->
    erlang:error(not_implemented).

describe(undefined, _Context) ->
    undefined;
describe(Id, Context) when is_integer(Id) ->
    Uri = m_rsc:p_no_acl(Id, uri, Context),
    describe(Uri, Context);
describe(Uri, Context) ->
    z_depcache:memo(
      fun() ->
              [Result] = network_of_terms_client:lookup([Uri], Context),
              lookup_result(Result)
      end,
      {term, Uri},
      ?WEEK,
      Context
     ).

lookup_result(#{<<"result">> := #{<<"__typename">> := <<"NotFoundError">>}}) ->
    undefined;
lookup_result(Result) ->
    Result.
