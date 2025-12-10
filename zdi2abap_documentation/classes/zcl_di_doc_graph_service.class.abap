"@Service
CLASS zcl_di_doc_graph_service DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_di_doc_graph_service .

    METHODS constructor
      IMPORTING
        io_repository TYPE REF TO object.
  PROTECTED SECTION.

    "@Inject( io_repository )
    DATA mo_repository TYPE REF TO zcl_di_doc_repository.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_DI_DOC_GRAPH_SERVICE IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_DI_DOC_GRAPH_SERVICE->CONSTRUCTOR
* +-------------------------------------------------------------------------------------------------+
* | [--->] IO_REPOSITORY                  TYPE REF TO OBJECT
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD constructor.
    mo_repository ?= io_repository.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_DI_DOC_GRAPH_SERVICE->ZIF_DI_DOC_GRAPH_SERVICE~BUILD_GRAPH
* +-------------------------------------------------------------------------------------------------+
* | [--->] IT_PACKAGES                    TYPE        LXE_TT_DC
* | [<---] ES_DATA                        TYPE        MTY_S_GRAPH
* | [<---] EV_JSON                        TYPE        STRING
* | [!CX!] ZCX_DI_ERROR
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD zif_di_doc_graph_service~build_graph.

    DATA:
      lt_rel_dep TYPE SORTED TABLE OF seoclass-clsname WITH UNIQUE DEFAULT KEY.

    TRY.
        "Список пакетов
        DATA(lt_package_list) = mo_repository->read_packages( it_packages ).

        "Сканируем и решаем зависимости, ничего не инстанцируем, только регистрация
        DATA(lo_scanner) = NEW zcl_di_scanner( ).
        DATA(lo_resolver) = NEW zcl_di_dependensy_resolver( ).

        lo_scanner->set_scan_packages( lt_package_list ).

        DATA(lt_class_info) = lo_scanner->scan_package( iv_component_only = abap_false ).
        DATA(lt_ordered) = lo_resolver->resolve_dependencies( CHANGING ct_class = lt_class_info ).

        lo_scanner->dispose( ).
        lo_resolver->dispose( ).

        FREE lo_scanner.
        FREE lo_resolver.
      CATCH zcx_di_error INTO DATA(lx_error).
        RAISE EXCEPTION lx_error.
    ENDTRY.

    "Докинем используемые классы, те, которые просто болтаются как атрибуты, без аннотаций
    LOOP AT lt_class_info ASSIGNING FIELD-SYMBOL(<ls_class>).
      LOOP AT <ls_class>-related_dependencies ASSIGNING FIELD-SYMBOL(<ls_rel_dep>).
        READ TABLE lt_ordered TRANSPORTING NO FIELDS
          WITH KEY table_line = <ls_rel_dep>-class_name.
        CHECK sy-subrc <> 0.
        INSERT <ls_rel_dep>-class_name INTO TABLE lt_rel_dep.
      ENDLOOP.
    ENDLOOP.

    APPEND LINES OF lt_rel_dep TO lt_ordered.

    SORT lt_class_info BY class_name.

    "Заполняем граф
    LOOP AT lt_ordered ASSIGNING FIELD-SYMBOL(<lv_class_name>).
      DATA(lv_index) = sy-tabix.

      "Узлы
      APPEND INITIAL LINE TO es_data-nodes ASSIGNING FIELD-SYMBOL(<ls_node>).
      <ls_node>-id    = lv_index.
      <ls_node>-label = <ls_node>-title = <lv_class_name>.
      <ls_node>-group = 'component'.

      READ TABLE lt_class_info ASSIGNING <ls_class>
        WITH KEY class_name = <lv_class_name>
        BINARY SEARCH.
      CHECK sy-subrc = 0.

      <ls_node>-group = to_lower( <ls_class>-component_type ).
      <ls_node>-title = <ls_class>-absolute_type.

      LOOP AT <ls_class>-annotations ASSIGNING FIELD-SYMBOL(<ls_annotation>).
        IF <ls_annotation>-value IS NOT INITIAL.
          APPEND |@{ <ls_annotation>-name }( { <ls_annotation>-value } )| TO <ls_node>-annotations.
        ELSE.
          APPEND |@{ <ls_annotation>-name }| TO <ls_node>-annotations.
        ENDIF.
      ENDLOOP.

      <ls_node>-methods = 0.
      <ls_node>-dependencies = lines( <ls_class>-injected_dependencies ).
    ENDLOOP.

    "И связи
    LOOP AT lt_class_info ASSIGNING <ls_class>.

      LOOP AT <ls_class>-injected_dependencies ASSIGNING FIELD-SYMBOL(<ls_dep>).
        APPEND INITIAL LINE TO es_data-edges ASSIGNING FIELD-SYMBOL(<ls_edge>).

        <ls_edge>-from   = line_index( lt_ordered[ table_line = <ls_class>-class_name ] ).
        <ls_edge>-to     = line_index( lt_ordered[ table_line = <ls_dep>-class_name ] ).

        <ls_edge>-label  = 'depends on'.
        <ls_edge>-dashes = abap_true.
        <ls_edge>-color  = '#f56565'.

        <ls_edge>-arrows = 'to'.
      ENDLOOP.

      LOOP AT <ls_class>-related_dependencies ASSIGNING <ls_rel_dep>.
        APPEND INITIAL LINE TO es_data-edges ASSIGNING <ls_edge>.

        <ls_edge>-from   = line_index( lt_ordered[ table_line = <ls_class>-class_name ] ).
        <ls_edge>-to     = line_index( lt_ordered[ table_line = <ls_rel_dep>-class_name ] ).

        <ls_edge>-label  = 'uses'.
        <ls_edge>-color  = '#48bb78'.

        <ls_edge>-arrows = 'to'.
      ENDLOOP.

    ENDLOOP.

    "Сериализуем в JSON
    ev_json = /ui2/cl_json=>serialize(
                pretty_name = 'L' "lower case
                data        = es_data
                compress    = abap_true
              ).

  ENDMETHOD.
ENDCLASS.