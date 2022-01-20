{% with
    term.uri
as
    id
%}
    <div class="col-md-12">
        <a href="{{ id }}" target="_blank" class="thumbnail{% if m.rdf_triple.id[subject_id][predicate][id] %} thumbnail-connected{% endif %}" data-id="{{ id }}" data-title="{{ term.prefLabel|join:" • " }}">
            <div class="z-thumbnail-text">
                <h5>{{ term.prefLabel|join:" • " }}</h5>
                <h6>{{ term.altLabel|join:" • " }}</h6>
                <p>{{ term.scopeNote|truncate:350 }}</p>
            </div>
        </a>
    </div>
{% endwith %}
