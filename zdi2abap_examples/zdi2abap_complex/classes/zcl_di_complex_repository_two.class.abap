"@Repository
CLASS zcl_di_complex_repository_two DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_di_compl_repository .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_DI_COMPLEX_REPOSITORY_TWO IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_DI_COMPLEX_REPOSITORY_TWO->ZIF_DI_COMPL_REPOSITORY~READ
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD zif_di_compl_repository~read.
    WRITE : / 'read two repo'.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_DI_COMPLEX_REPOSITORY_TWO->ZIF_DI_COMPL_REPOSITORY~SAVE
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD zif_di_compl_repository~save.
    WRITE : / 'save two repo'.
  ENDMETHOD.
ENDCLASS.