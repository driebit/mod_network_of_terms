{% with m.search[{terms pagelen=10 sources=sources text=text}] as result %}
{% if result|length %}
<div class="connect-results">
    <div id="dialog_connect_loop_terms_results" class="thumbnails">
        {% include "_action_dialog_connect_tab_find_terms_results_loop.tpl"
            id
            result=result
            show_no_results
        %}
    </div>
</div>
{% endif %}
{% endwith %}
