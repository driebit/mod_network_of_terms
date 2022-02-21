<div class="tab-pane form-horizontal active" id="{{ #main }}">
    {% catinclude "_admin_edit_basics.tpl" id in_dialog show_header %}
</div>

{% block modal_footer %}
    <div class="modal-footer">
        {% button class="btn btn-default" action={dialog_close} text=_"Cancel" tag="a" %}
        {% if m.acl.use.mod_admin or m.acl.use.mod_admin_frontend %}
            <a href="{% url admin_edit_rsc id=id %}" class="btn btn-default">{_ Visit full edit page _}</a>
        {% endif %}
    </div>
{% endblock %}
