"@Service
CLASS zcl_di_component_service DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS:
      get_data_reference
        RETURNING
          VALUE(rr_data) TYPE REF TO data.
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA:
      mt_data TYPE STANDARD TABLE OF mara.
ENDCLASS.



CLASS ZCL_DI_COMPONENT_SERVICE IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_DI_COMPONENT_SERVICE->GET_DATA_REFERENCE
* +-------------------------------------------------------------------------------------------------+
* | [<-()] RR_DATA                        TYPE REF TO DATA
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_data_reference.
    rr_data = REF #( mt_data ).
  ENDMETHOD.
ENDCLASS.