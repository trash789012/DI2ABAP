"@Repository
CLASS zcl_di_complex_repository DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_di_compl_repository .

    METHODS constructor
      IMPORTING
        io_logger TYPE REF TO object.
  PROTECTED SECTION.
  PRIVATE SECTION.

    "@Inject( io_logger )
    DATA mo_logger TYPE REF TO zif_di_compl_logger.
ENDCLASS.



CLASS ZCL_DI_COMPLEX_REPOSITORY IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_DI_COMPLEX_REPOSITORY->CONSTRUCTOR
* +-------------------------------------------------------------------------------------------------+
* | [--->] IO_LOGGER                      TYPE REF TO OBJECT
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD constructor.
    mo_logger ?= io_logger.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_DI_COMPLEX_REPOSITORY->ZIF_DI_COMPL_REPOSITORY~READ
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD zif_di_compl_repository~read.
    mo_logger->log( 'on read' ).
    WRITE : / 'read'.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_DI_COMPLEX_REPOSITORY->ZIF_DI_COMPL_REPOSITORY~SAVE
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD zif_di_compl_repository~save.
    mo_logger->log( 'on save' ).
    WRITE : / 'save'.
  ENDMETHOD.
ENDCLASS.