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
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_DI_COMPONENT_STVARV IMPLEMENTATION.


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
ENDCLASS.