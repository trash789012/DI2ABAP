"@Component
CLASS zcl_di_complex_application DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor
      IMPORTING
        io_service TYPE REF TO zif_di_compl_service
        io_logger  TYPE REF TO zif_di_compl_logger.
    METHODS start .
  PROTECTED SECTION.
  PRIVATE SECTION.

    "@Inject( io_service )
    DATA mo_service TYPE REF TO zif_di_compl_service .

    "@Inject( io_logger )
    DATA mo_logger TYPE REF TO zif_di_compl_logger.
ENDCLASS.



CLASS ZCL_DI_COMPLEX_APPLICATION IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_DI_COMPLEX_APPLICATION->CONSTRUCTOR
* +-------------------------------------------------------------------------------------------------+
* | [--->] IO_SERVICE                     TYPE REF TO ZIF_DI_COMPL_SERVICE
* | [--->] IO_LOGGER                      TYPE REF TO ZIF_DI_COMPL_LOGGER
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD constructor.
    mo_service ?= io_service.
    mo_logger  ?= io_logger.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_DI_COMPLEX_APPLICATION->START
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD start.

    mo_service->calculate( ).

    mo_logger->log( 'finish' ).

  ENDMETHOD.
ENDCLASS.