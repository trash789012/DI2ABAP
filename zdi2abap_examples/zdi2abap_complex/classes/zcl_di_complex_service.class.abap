"@Service
CLASS zcl_di_complex_service DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_di_compl_service .

    METHODS constructor
      IMPORTING
        io_repository     TYPE REF TO zif_di_compl_repository
        io_repository_two TYPE REF TO zif_di_compl_repository.
  PROTECTED SECTION.
  PRIVATE SECTION.

    "@Inject( io_repository )
    DATA mo_repository TYPE REF TO zif_di_compl_repository.

    DATA mo_repository_two TYPE REF TO zif_di_compl_repository.
ENDCLASS.



CLASS ZCL_DI_COMPLEX_SERVICE IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_DI_COMPLEX_SERVICE->CONSTRUCTOR
* +-------------------------------------------------------------------------------------------------+
* | [--->] IO_REPOSITORY                  TYPE REF TO ZIF_DI_COMPL_REPOSITORY
* | [--->] IO_REPOSITORY_TWO              TYPE REF TO ZIF_DI_COMPL_REPOSITORY
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD constructor.
    mo_repository     ?= io_repository.
    mo_repository_two ?= io_repository_two.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_DI_COMPLEX_SERVICE->ZIF_DI_COMPL_SERVICE~CALCULATE
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD zif_di_compl_service~calculate.

    mo_repository->read( ).
    mo_repository_two->read( ).

    mo_repository->save( ).
    mo_repository_two->save( ).

  ENDMETHOD.
ENDCLASS.