<div class="row">
    <div class="col-md-12">
        <div href="{{ term.uri }}" class="thumbnail{% if m.rdf_triple.id[subject_id][predicate][term.uri] %} thumbnail-connected{% endif %}" data-id="{{ term.uri }}" data-title="{{ term.prefLabel|join:" • " }}">
            <h5>
                {{ term.prefLabel|join:" • " }}
                <a href="{{ term.seeAlso|first|default:term.uri }}" target="_blank" class="btn-default pull-right btn btn-xs"><span class="glyphicon glyphicon-new-window"> </span> {_ View at source _}</a>
            </h5>
            <p>{{ term.scopeNote|join:" • "|truncate:5000 }}</p>
            {% if term.altLabel|length %}
                <p>
                    {% if term.altLabel|length == 1 %}
                        {_ Alternative label _}:
                    {% else %}
                        {_ Alternative labels _}:
                    {% endif %}
                    {{ term.altLabel|join:" • " }}
                </p>
            {% endif %}
        </div>
    </div>
</div>
