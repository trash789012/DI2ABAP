INTERFACE zif_di_container
  PUBLIC .


  CONSTANTS mc_default_scope TYPE string VALUE zif_annotations=>mc_default_scope ##NO_TEXT.
  CONSTANTS mc_absolute_name TYPE seoclsname VALUE 'ZIF_DI_CONTAINER' ##NO_TEXT.

  METHODS resolve
    IMPORTING
      !iv_classname           TYPE string OPTIONAL
      !iv_qualifier           TYPE string OPTIONAL
      !iv_prepare_object_tree TYPE abap_bool DEFAULT 'X'
    EXPORTING
      VALUE(eo_instance)      TYPE any
    RAISING
      zcx_di_error .
  METHODS create_container
    IMPORTING
      !it_package         TYPE string_table
    RETURNING
      VALUE(ro_container) TYPE REF TO zif_di_container
    RAISING
      zcx_di_error .
ENDINTERFACE.