"@Component
CLASS zcl_di_component_stvarv DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS read_type_line_name
      RETURNING
        VALUE(rv_result) TYPE string .
    METHODS read_container_name
      RETURNING
        VALUE(rv_result) TYPE string .
    METHODS get_run_mode
      RETURNING
        VALUE(rv_mode) TYPE string .
    METHODS set_run_mode
      IMPORTING
        !iv_mode TYPE string .
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA mv_report_run_mode TYPE string VALUE 'HTTP' ##NO_TEXT.
ENDCLASS.



CLASS ZCL_DI_COMPONENT_STVARV IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_DI_COMPONENT_STVARV->GET_RUN_MODE
* +-------------------------------------------------------------------------------------------------+
* | [<-()] RV_MODE                        TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_run_mode.
    rv_mode = mv_report_run_mode.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_DI_COMPONENT_STVARV->READ_CONTAINER_NAME
* +-------------------------------------------------------------------------------------------------+
* | [<-()] RV_RESULT                      TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD read_container_name.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_DI_COMPONENT_STVARV->READ_TYPE_LINE_NAME
* +-------------------------------------------------------------------------------------------------+
* | [<-()] RV_RESULT                      TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD read_type_line_name.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_DI_COMPONENT_STVARV->SET_RUN_MODE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_MODE                        TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD set_run_mode.
    mv_report_run_mode = iv_mode.
  ENDMETHOD.
ENDCLASS.