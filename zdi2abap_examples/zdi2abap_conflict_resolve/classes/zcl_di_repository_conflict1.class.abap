"@Repository
CLASS zcl_di_repository_conflict1 DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_di_repository_conflict .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_DI_REPOSITORY_CONFLICT1 IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_DI_REPOSITORY_CONFLICT1->ZIF_DI_REPOSITORY_CONFLICT~SELECT
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD zif_di_repository_conflict~select.
    WRITE : / 'select from users'.
  ENDMETHOD.
ENDCLASS.