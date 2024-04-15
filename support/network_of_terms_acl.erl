%% @doc ACL checks for usage of this mod.
-module(network_of_terms_acl).
-author("Driebit <info@driebit.nl>").

-export([
    is_allowed/1
]).

-include("zotonic.hrl").


%% @doc Check whether the user is allowed to use this mod or not.
%% Note that by default all users are allowed, but one can forbid the 'use'
%% of 'mod_network_of_terms' to restrict access to it (except for the admin).
%%
%% For debugging purposes, a debug-level message is logged when access is non-default.
is_allowed(Context) ->
    case z_acl:is_admin(Context) of
        true -> true;
        _ -> case z_acl:is_allowed(use, mod_network_of_terms, Context) of
            undefined -> true;
            Result ->
                z:debug(
                    "Non-default access to mod_network_of_terms for user ~p. Allowed: ~s",
                    [ Context#context.user_id , Result ],
                    [],
                    Context
                ),
                Result
        end
    end.
