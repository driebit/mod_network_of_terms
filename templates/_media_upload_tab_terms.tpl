{% if m.network_of_terms.is_allowed %}
<li {% if tab == "find_rdf" %}class="active"{% endif %}>
    <a data-toggle="tab" href="#{{ tab }}-terms">{_ Network of Terms _}</a>
</li>
{% endif %}
