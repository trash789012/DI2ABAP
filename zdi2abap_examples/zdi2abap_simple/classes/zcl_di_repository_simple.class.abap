"@Repository
CLASS zcl_di_repository_simple DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_di_repository_simple .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_DI_REPOSITORY_SIMPLE IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_DI_REPOSITORY_SIMPLE->ZIF_DI_REPOSITORY_SIMPLE~SELECT
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD zif_di_repository_simple~select.
    WRITE : / 'Select data...'.
  ENDMETHOD.
ENDCLASS.