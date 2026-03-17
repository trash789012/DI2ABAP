"@Service
CLASS zcl_di_component_service DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor
      IMPORTING
        io_util TYPE REF TO zcl_di_component_http_util.
    METHODS get_data_reference
      RETURNING
        VALUE(rr_data) TYPE REF TO data .
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA mt_data TYPE STANDARD TABLE OF mara.

    "@Inject( io_util )
    DATA mo_http TYPE REF TO zif_di_component_util.
ENDCLASS.



CLASS ZCL_DI_COMPONENT_SERVICE IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_DI_COMPONENT_SERVICE->CONSTRUCTOR
* +-------------------------------------------------------------------------------------------------+
* | [--->] IO_UTIL                        TYPE REF TO ZCL_DI_COMPONENT_HTTP_UTIL
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD constructor.
    mo_http = io_util.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_DI_COMPONENT_SERVICE->GET_DATA_REFERENCE
* +-------------------------------------------------------------------------------------------------+
* | [<-()] RR_DATA                        TYPE REF TO DATA
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_data_reference.
    rr_data = REF #( mt_data ).
  ENDMETHOD.
ENDCLASS.