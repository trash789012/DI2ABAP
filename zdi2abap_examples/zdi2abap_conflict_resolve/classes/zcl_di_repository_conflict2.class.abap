"@Repository
"@Qualifier( BookRepo )
CLASS zcl_di_repository_conflict2 DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_di_repository_conflict .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_DI_REPOSITORY_CONFLICT2 IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_DI_REPOSITORY_CONFLICT2->ZIF_DI_REPOSITORY_CONFLICT~SELECT
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD zif_di_repository_conflict~select.
    WRITE : / 'select from book'.
  ENDMETHOD.
ENDCLASS.