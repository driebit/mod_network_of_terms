{% for source_result in result %}
    <div class="row">
        <div class="col-md-12">
            <h4 class="text-center">{{ source_result.source.name }} {% if source_result.source.alternateName != "null" %} ({{ source_result.source.alternateName }}){% endif %}
                {% if source_result.result.terms|is_defined %}
                <span class="badge badge-pill badge-light">
                    {% if source_result.result.terms|length == 0 %}
                        {_ No terms found _}
                    {% elseif source_result.result.terms|length == 1 %}
                        {_ One term found _}
                    {% else %}
                        {{ source_result.result.terms|length }} {_ terms found _}
                    {% endif %}
                </span>
                {% endif %}
            </h4>
            <h5 class="text-center text-muted">
                {% with source_result.source.creators[1] as creator %}
                    {{ creator.name }}
                    {% if creator.alternateName != creator.name %}
                        ({{ creator.alternateName }})
                    {% endif %}
                {% endwith %}
            </h5>
            {% if source_result.result.terms|is_undefined %}
                <div
                    class="text-center alert alert-danger"
                    role="alert"
                >
                    {% if source_result.result.__typename == "ServerError" %}
                        {_ The term source could not process the  request. _}
                    {% elseif sourcee_result.result.__typename == "TmeoutError" %}
                        {_ The term source is temporarily unavailable. _}
                    {% else %}
                        {_ An error occurred. _}
                    {% endif %}
                </div>
            {% endif %}
        </div>
    </div>
    {% if source_result.result.terms %}
    <div class="row">
        {% for term in source_result.result.terms %}
            {% include "_action_dialog_connect_tab_find_terms_results_item.tpl" term=term %}
        {% endfor %}
    </div>
    {% endif %}
{% endfor %}
