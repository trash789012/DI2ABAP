"@Component
CLASS zcl_di_application_scope DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor
      IMPORTING
        !io_repository TYPE REF TO object
        !io_logger     TYPE REF TO object
        !io_logger_slg TYPE REF TO object .
    METHODS run .

    "@Inject( io_repository )
    DATA mo_repository TYPE REF TO zcl_di_repository_scope_demo .
    "@Inject( io_logger )
    "@RefQualifier( XmlLog )
    DATA mo_logger TYPE REF TO zif_di_logger_demo .
    "@Inject( io_logger_slg )
    DATA mo_logger_slg TYPE REF TO zif_di_logger_demo .
  PROTECTED SECTION.
  PRIVATE SECTION.

ENDCLASS.



CLASS ZCL_DI_APPLICATION_SCOPE IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_DI_APPLICATION_SCOPE->CONSTRUCTOR
* +-------------------------------------------------------------------------------------------------+
* | [--->] IO_REPOSITORY                  TYPE REF TO OBJECT
* | [--->] IO_LOGGER                      TYPE REF TO OBJECT
* | [--->] IO_LOGGER_SLG                  TYPE REF TO OBJECT
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD constructor.
    mo_repository ?= io_repository.
    mo_logger     ?= io_logger.
    mo_logger_slg ?= io_logger_slg.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_DI_APPLICATION_SCOPE->RUN
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD run.

    mo_logger->log( 'app start select' ).
    mo_logger_slg->log( 'app start select' ).

    mo_repository->select( ).

    mo_logger->log( 'app end select' ).
    mo_logger_slg->log( 'app end select' ).

  ENDMETHOD.
ENDCLASS.