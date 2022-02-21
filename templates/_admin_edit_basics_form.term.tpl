{% with m.network_of_terms[id.uri] as lookup %}
<div>
    {% if lookup == undefined %}
        <p>
            {_ A term with this URI was not found in the <a href="https://termennetwerk.netwerkdigitaalerfgoed.nl" target="_blank">Network of Terms</a>. _}
            <a href="https://termennetwerk.netwerkdigitaalerfgoed.nl/faq" target="_blank" class="z-btn-help"></a>
        </p>
    {% else %}
        <p>
            {_ This is a term from the <a href="https://termennetwerk.netwerkdigitaalerfgoed.nl" target="_blank">Network of Terms</a>. _}
            <a href="https://termennetwerk.netwerkdigitaalerfgoed.nl/faq" target="_blank" class="z-btn-help"></a>
        </p>
        {% with lookup.result, lookup.source as term, source %}
            <dl id="term" class="dl-horizontal">
                <dt>{_ URI _}</dt>
                <dd><a href="{{term.uri}}">{{ term.uri }}</a> <a href="{{ term.seeAlso|first|default:term.uri }}" target="_blank" class="btn-default pull-right btn btn-xs"><span class="glyphicon glyphicon-new-window"> </span> {_ View at source _}</a></dd>

                <dt>{_ Terminology source _}</dt>
                <dd>
                    {{ source.name }}
                    {% if source.alternateName != "null" %} ({{ source_result.source.alternateName }}){% endif %}
                    ({% for creator in source.creators %}<a href="{{ creator.uri }}">{{ creator.name }}</a>{% if not forloop.last %}, {% endif %}{% endfor %})
                </dd>

                <dt>{_ Preferred label _}</dt>
                <dd>{{ term.prefLabel|join:" • " }}</dd>

                <dt>{_ Alternative labels _}</dt>
                <dd>{{ term.altLabel|join:" • " }}</dd>

                <dt>{_ Scope note _}</dt>
                <dd>{{ term.scopeNote|join:" • " }}</dd>
            </dl>
        {% endwith %}
    {% endif %}
</div>
{% endwith %}
