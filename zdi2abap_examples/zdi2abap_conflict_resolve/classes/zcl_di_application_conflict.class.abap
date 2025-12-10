"@Component
CLASS zcl_di_application_conflict DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor
      IMPORTING
        !io_repository_user TYPE REF TO object
        !io_repository_book TYPE REF TO object.
    METHODS run .
  PROTECTED SECTION.
  PRIVATE SECTION.
    "@Inject( io_repository_user )
    DATA mo_repository_user TYPE REF TO zif_di_repository_conflict.

    "@Inject( io_repository_book )
    "@RefQualifier( BookRepo )
    DATA mo_repository_book TYPE REF TO zif_di_repository_conflict.
ENDCLASS.



CLASS ZCL_DI_APPLICATION_CONFLICT IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_DI_APPLICATION_CONFLICT->CONSTRUCTOR
* +-------------------------------------------------------------------------------------------------+
* | [--->] IO_REPOSITORY_USER             TYPE REF TO OBJECT
* | [--->] IO_REPOSITORY_BOOK             TYPE REF TO OBJECT
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD constructor.
    mo_repository_user ?= io_repository_user.
    mo_repository_book ?= io_repository_book.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_DI_APPLICATION_CONFLICT->RUN
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD run.
    mo_repository_user->select( ).
    mo_repository_book->select( ).
  ENDMETHOD.
ENDCLASS.