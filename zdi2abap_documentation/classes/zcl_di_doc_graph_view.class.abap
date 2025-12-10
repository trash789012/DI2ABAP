"@Component
CLASS zcl_di_doc_graph_view DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_di_doc_graph_view .
  PROTECTED SECTION.

    METHODS:
      generate_html
        IMPORTING
          iv_json        TYPE string
        RETURNING
          VALUE(rv_html) TYPE string.

  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_DI_DOC_GRAPH_VIEW IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Protected Method ZCL_DI_DOC_GRAPH_VIEW->GENERATE_HTML
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_JSON                        TYPE        STRING
* | [<-()] RV_HTML                        TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD generate_html.

    DATA:
      lt_template    TYPE TABLE OF w3mime,
      lt_template_js TYPE TABLE OF w3mime,
      lv_template    TYPE string,
      lv_template_js TYPE string.

    "Основное тело
    DATA(ls_key) = VALUE wwwdatatab( relid = 'MI'
                                     objid = 'ZDI_DOC_TEMPLATE' ).

    CALL FUNCTION 'WWWDATA_IMPORT'
      EXPORTING
        key    = ls_key
      TABLES
        mime   = lt_template
      EXCEPTIONS
        OTHERS = 4.
    IF sy-subrc = 0.
      DATA(lv_size) = VALUE wwwparams-value( ).
      CALL FUNCTION 'WWWPARAMS_READ'
        EXPORTING
          relid  = ls_key-relid
          objid  = ls_key-objid
          name   = 'filesize'
        IMPORTING
          value  = lv_size
        EXCEPTIONS
          OTHERS = 0.

      DATA(lv_length) = VALUE i( ).
      lv_length = lv_size.
    ENDIF.

    DATA(lv_output_length) = VALUE i( ).
    CALL FUNCTION 'SCMS_BINARY_TO_STRING'
      EXPORTING
        input_length  = lv_length
      IMPORTING
        text_buffer   = lv_template
        output_length = lv_output_length
      TABLES
        binary_tab    = lt_template
      EXCEPTIONS
        failed        = 1
        OTHERS        = 2.

    "JS Для графа
    ls_key = VALUE wwwdatatab( relid = 'MI'
                               objid = 'ZDI_DOC_TEMPLATE_JS' ).

    CALL FUNCTION 'WWWDATA_IMPORT'
      EXPORTING
        key    = ls_key
      TABLES
        mime   = lt_template_js
      EXCEPTIONS
        OTHERS = 4.
    IF sy-subrc = 0.
      lv_size = VALUE wwwparams-value( ).
      CALL FUNCTION 'WWWPARAMS_READ'
        EXPORTING
          relid  = ls_key-relid
          objid  = ls_key-objid
          name   = 'filesize'
        IMPORTING
          value  = lv_size
        EXCEPTIONS
          OTHERS = 0.

      lv_length = lv_size.
    ENDIF.

    CALL FUNCTION 'SCMS_BINARY_TO_STRING'
      EXPORTING
        input_length  = lv_length
      IMPORTING
        text_buffer   = lv_template_js
        output_length = lv_output_length
      TABLES
        binary_tab    = lt_template_js
      EXCEPTIONS
        failed        = 1
        OTHERS        = 2.

    REPLACE ALL OCCURRENCES OF '%%VIS_SCRIPT%%'      IN lv_template WITH lv_template_js.
    REPLACE ALL OCCURRENCES OF '%%ABAP_GRAPH_DATA%%' IN lv_template WITH iv_json.

    rv_html = lv_template.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_DI_DOC_GRAPH_VIEW->ZIF_DI_DOC_GRAPH_VIEW~SHOW
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_JSON                        TYPE        STRING
* | [--->] IV_IN_CONTAINER                TYPE        FLAG (default =SPACE)
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD zif_di_doc_graph_view~show.

    DATA:
      lv_xstring       TYPE xstring,
      lt_binary_tab    TYPE solix_tab,
      lt_string_tab    TYPE soli_tab,
      lv_output_length TYPE i,
      lv_url           TYPE char200.

    DATA(lv_html) = generate_html( iv_json ).

    CALL FUNCTION 'SCMS_STRING_TO_XSTRING'
      EXPORTING
        text   = lv_html
      IMPORTING
        buffer = lv_xstring
      EXCEPTIONS
        failed = 1
        OTHERS = 2.

    CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
      EXPORTING
        buffer        = lv_xstring
      IMPORTING
        output_length = lv_output_length
      TABLES
        binary_tab    = lt_binary_tab.

    DATA(lv_filename) = VALUE string( ).
    DATA(lv_path) = VALUE string( ).
    DATA(lv_fullpath) = VALUE string( ).
    DATA(lv_action) = VALUE i( ).
    cl_gui_frontend_services=>file_save_dialog(
      EXPORTING
        window_title              = 'Save'
        file_filter               = cl_gui_frontend_services=>filetype_html
      CHANGING
        user_action               = lv_action
        filename                  = lv_filename
        path                      = lv_path
        fullpath                  = lv_fullpath
      EXCEPTIONS
        cntl_error                = 1
        error_no_gui              = 2
        not_supported_by_gui      = 3
        invalid_default_file_name = 4
        OTHERS                    = 5
    ).
    IF lv_action <> 0 OR sy-subrc <> 0.
      RETURN.
    ENDIF.

    cl_gui_frontend_services=>gui_download(
      EXPORTING
        bin_filesize              = lv_output_length
        filename                  = lv_fullpath
        filetype                  = 'BIN'
      CHANGING
        data_tab                  = lt_binary_tab
      EXCEPTIONS
        file_write_error          = 1
        no_batch                  = 2
        gui_refuse_filetransfer   = 3
        invalid_type              = 4
        no_authority              = 5
        unknown_error             = 6
        header_not_allowed        = 7
        separator_not_allowed     = 8
        filesize_not_allowed      = 9
        header_too_long           = 10
        dp_error_create           = 11
        dp_error_send             = 12
        dp_error_write            = 13
        unknown_dp_error          = 14
        access_denied             = 15
        dp_out_of_memory          = 16
        disk_full                 = 17
        dp_timeout                = 18
        file_not_found            = 19
        dataprovider_exception    = 20
        control_flush_error       = 21
        not_supported_by_gui      = 22
        error_no_gui              = 23
        OTHERS                    = 24
    ).

  ENDMETHOD.
ENDCLASS.