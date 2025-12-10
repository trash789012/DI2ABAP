"@Component
CLASS zcl_di_doc_application DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor
      IMPORTING
        !io_graph_service TYPE REF TO object
        !io_graph_view    TYPE REF TO object.
    METHODS run
      IMPORTING
        !it_packages TYPE lxe_tt_dc
      RAISING
        zcx_di_error.
  PROTECTED SECTION.
  PRIVATE SECTION.

    "@Inject( io_graph_service )
    DATA mo_graph_service TYPE REF TO zif_di_doc_graph_service.

    "@Inject( io_graph_view )
    DATA mo_graph_view TYPE REF TO zif_di_doc_graph_view.
ENDCLASS.



CLASS ZCL_DI_DOC_APPLICATION IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_DI_DOC_APPLICATION->CONSTRUCTOR
* +-------------------------------------------------------------------------------------------------+
* | [--->] IO_GRAPH_SERVICE               TYPE REF TO OBJECT
* | [--->] IO_GRAPH_VIEW                  TYPE REF TO OBJECT
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD constructor.
    mo_graph_service ?= io_graph_service.
    mo_graph_view    ?= io_graph_view.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_DI_DOC_APPLICATION->RUN
* +-------------------------------------------------------------------------------------------------+
* | [--->] IT_PACKAGES                    TYPE        LXE_TT_DC
* | [!CX!] ZCX_DI_ERROR
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD run.
    mo_graph_service->build_graph( EXPORTING it_packages = it_packages
                                   IMPORTING ev_json     = DATA(lv_json) ).
    mo_graph_view->show( lv_json ).
  ENDMETHOD.
ENDCLASS.