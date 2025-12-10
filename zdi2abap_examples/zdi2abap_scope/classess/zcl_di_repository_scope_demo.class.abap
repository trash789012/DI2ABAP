"@Repository
CLASS zcl_di_repository_scope_demo DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor
      IMPORTING
        io_logger     TYPE REF TO object
        io_logger_slg TYPE REF TO object.
    METHODS select .

    "@Inject( io_logger )
    "@RefQualifier( XmlLog )
    DATA mo_logger TYPE REF TO zif_di_logger_demo.

    "@Inject( io_logger_slg )
    DATA mo_logger_slg TYPE REF TO zif_di_logger_demo .
  PROTECTED SECTION.
  PRIVATE SECTION.

ENDCLASS.



CLASS ZCL_DI_REPOSITORY_SCOPE_DEMO IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_DI_REPOSITORY_SCOPE_DEMO->CONSTRUCTOR
* +-------------------------------------------------------------------------------------------------+
* | [--->] IO_LOGGER                      TYPE REF TO OBJECT
* | [--->] IO_LOGGER_SLG                  TYPE REF TO OBJECT
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD constructor.
    mo_logger ?= io_logger.
    mo_logger_slg ?= io_logger_slg.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_DI_REPOSITORY_SCOPE_DEMO->SELECT
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD select.
    mo_logger->log( 'repository begin of select data' ).

    WRITE : / 'select data'.

    mo_logger->log( 'repository end of select data' ).
  ENDMETHOD.
ENDCLASS.