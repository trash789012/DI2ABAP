"@Component
"@Proxy( true )
CLASS zcl_di_demo_stvarv DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_di_stvarv .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_DI_DEMO_STVARV IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_DI_DEMO_STVARV->ZIF_DI_STVARV~READ
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD zif_di_stvarv~read.
    WRITE : / 'read stvarv'.
  ENDMETHOD.
ENDCLASS.