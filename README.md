mod_network_of_terms
====================

A Zotonic module for linking resources to terms from the
[Network of Terms](https://termennetwerk.netwerkdigitaalerfgoed.nl), a search engine for finding shared terms in a set
of term sources. Collection managers use shared terms to describe their data.

You can find technical usage documentation at
the [Network of Terms code repository](https://github.com/netwerk-digitaal-erfgoed/network-of-terms-api).

Features:

* Search the Network of Terms using [its GraphQL API](https://termennetwerk-api.netwerkdigitaalerfgoed.nl/graphiql).
* Link Zotonic resources to terms from the Network of Terms.

Usage
-----

Enable this module to show a ‘Network of Terms’ tab in the admin when linking resources.

## Models

### m_network_of_terms

A template model for retrieving the list of term sources.

Issues
------

Please report any issues with the Network of Terms API
at https://github.com/netwerk-digitaal-erfgoed/network-of-terms-api.
