<div class="tab-pane" id="{{ tab }}-terms">
    <p>{_ Use the <a href="https://termennetwerk.netwerkdigitaalerfgoed.nl" target="_blank">Network of Terms</a> to find shared terms and link your pages to them. _}
        <a href="https://termennetwerk.netwerkdigitaalerfgoed.nl/faq" target="_blank" class="z-btn-help"></a>
    </p>
    <form id="dialog-connect-find-terms" class="form form-horizontal">

        <input type="hidden" name="subject_id" value="{{ subject_id }}" />
        <input type="hidden" name="predicate" value="{{ predicate|default:'' }}" />
        <input type="hidden" name="sources" id="sources" />

        <div class="form-group row">
            <label class="control-label col-md-3" for="search">
                {_ Search words _}
            </label>
            <div class="col-md-9">
                <input required name="find_text" type="text" id="search" value="{{ text|default:'' }}" placeholder="{_ For example: Rembrandt, Amsterdam or painting _}" class="do_autofocus form-control" />
            </div>
        </div>

        <div class="form-group row">
            <label class="control-label col-md-3" for="select-sources">
                {_ Terminology sources _}
            </label>
            <div class="col-md-9">
                <select multiple required class="selectpicker form-control" title="{_ Select one or more terminology sources _}" id="select-sources">
                    {% with m.network_of_terms.sources as sources %}
                    {% for source in sources %}
                        <option name="ding" value="{{ source.uri }}"
                            data-subtext="
                                {% with source.creators[1] as creator %}
                                    {{ creator.name }}
                                    {% if creator.alternateName != creator.name %}
                                    ({{ creator.alternateName }})
                                    {% endif %}
                                {% endwith %}
                            "
                        >
                            {{ source.name }} {% if source.alternateName != "null" %} ({{ source.alternateName }}){% endif %}
                        </option>
                    {% endfor %}
                    {% endwith %}
                </select>
                <small class="help-block text-muted">
                    {_ Choose from terminology sources, such as thesauri, reference lists and classification systems, that you want to consult. _}
                </small>
            </div>
        </div>

        <div id="dialog-connect-found-terms" class="do_feedback"
            data-feedback="trigger: 'dialog-connect-find-terms', delegate: 'mod_network_of_terms', template: '_action_dialog_connect_tab_find_terms_results.tpl'">
        </div>

        <div class="modal-footer">
            <a class="btn btn-default" id="{{ #close }}">
                {% if autoclose %}{_ Cancel _}{% else %}{_ Ok _}{% endif %}
            </a>
            {% wire id=#close action={dialog_close} %}
        </div>

    </form>

</div>

{% wire name="dialog_connect_find_terms"
    action={postback
        delegate="mod_ginger_rdf"
        postback={admin_connect_select
            id=id
            subject_id=subject_id
            predicate=predicate
            callback=callback
            language=language
            action=action
            actions=actions
            category=`term`
        }
    }
%}
{% javascript %}
    $('#select-sources').on('change', function(e) {
        const values = $(this).selectpicker('val');
        $('#sources').val(values);
    });

    $('#select-sources').selectpicker();
    $('#select-sources').selectpicker('val', {{ m.session.term_source_selection|to_json }});
    $('#select-sources').trigger('change');

    $("#dialog-connect-found-terms").on('click', '.thumbnail', function(e) {
        e.preventDefault();
        z_event('dialog_connect_find_terms', {
            object_props: $(this).data(),
            object: $(this).data('id'),
            object_title: $(this).data('title')
        });
        $(this).effect("highlight").toggleClass("thumbnail-connected");
        $('#{{ #close }}').removeClass("btn-default").addClass("btn-primary");
    });
{% endjavascript %}

{% lib
    "css/network-of-terms-admin.css"
%}
