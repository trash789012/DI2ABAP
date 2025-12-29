"@Component
"@Proxy( true )
CLASS zcl_di_model_demo DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_di_model_demo .

    METHODS constructor
      IMPORTING
        io_stvarv TYPE REF TO zif_di_stvarv.
  PROTECTED SECTION.
  PRIVATE SECTION.

    "@Inject( io_stvarv )
    DATA mo_stvarv TYPE REF TO zif_di_stvarv.

ENDCLASS.



CLASS ZCL_DI_MODEL_DEMO IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_DI_MODEL_DEMO->CONSTRUCTOR
* +-------------------------------------------------------------------------------------------------+
* | [--->] IO_STVARV                      TYPE REF TO ZIF_DI_STVARV
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD constructor.
    mo_stvarv ?= io_stvarv.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_DI_MODEL_DEMO->ZIF_DI_MODEL_DEMO~DO
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD zif_di_model_demo~do.
    WRITE : / 'in do'.

    mo_stvarv->read( ).
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_DI_MODEL_DEMO->ZIF_DI_MODEL_DEMO~DO_EXCEPT
* +-------------------------------------------------------------------------------------------------+
* | [!CX!] ZCX_DI_ERROR
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD zif_di_model_demo~do_except.
    RAISE EXCEPTION TYPE zcx_di_error.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_DI_MODEL_DEMO->ZIF_DI_MODEL_DEMO~SELECT
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD zif_di_model_demo~select.
    WRITE : / 'in delect'.
  ENDMETHOD.
ENDCLASS.