"@Repository
CLASS zcl_di_doc_repository DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS read_packages
      IMPORTING
        !it_devcls      TYPE lxe_tt_dc
      RETURNING
        VALUE(rt_class) TYPE string_table
      RAISING
        zcx_di_error .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_DI_DOC_REPOSITORY IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_DI_DOC_REPOSITORY->READ_PACKAGES
* +-------------------------------------------------------------------------------------------------+
* | [--->] IT_DEVCLS                      TYPE        LXE_TT_DC
* | [<-()] RT_CLASS                       TYPE        STRING_TABLE
* | [!CX!] ZCX_DI_ERROR
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD read_packages.

    DATA:
      lt_return TYPE bapiret2_tt.

    IF it_devcls IS INITIAL.
      MESSAGE e006 INTO DATA(lv_dummy).
      APPEND zcx_di_error=>sy2bapiret( ) TO lt_return.
    ENDIF.

    SELECT DISTINCT devclass
      INTO TABLE @rt_class
      FROM tdevc
      WHERE devclass IN @it_devcls.
    IF sy-subrc <> 0.
      MESSAGE e006 INTO lv_dummy.
      APPEND zcx_di_error=>sy2bapiret( ) TO lt_return.
    ENDIF.

    IF lt_return IS NOT INITIAL.
      RAISE EXCEPTION TYPE zcx_di_error EXPORTING mt_errors = lt_return.
    ENDIF.

  ENDMETHOD.
ENDCLASS.