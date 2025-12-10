"@Component
CLASS zcl_di_application_simple DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor
      IMPORTING
        !io_repository TYPE REF TO object .
    METHODS run .
  PROTECTED SECTION.
  PRIVATE SECTION.
    "@Inject( io_repository )
    DATA mo_repository TYPE REF TO zif_di_repository_simple.
ENDCLASS.



CLASS ZCL_DI_APPLICATION_SIMPLE IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_DI_APPLICATION_SIMPLE->CONSTRUCTOR
* +-------------------------------------------------------------------------------------------------+
* | [--->] IO_REPOSITORY                  TYPE REF TO OBJECT
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD constructor.
    mo_repository ?= io_repository.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_DI_APPLICATION_SIMPLE->RUN
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD run.
    mo_repository->select( ).
  ENDMETHOD.
ENDCLASS.