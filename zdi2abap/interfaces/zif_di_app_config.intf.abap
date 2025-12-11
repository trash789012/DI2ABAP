INTERFACE zif_di_app_config
  PUBLIC .


  METHODS is_active
    RETURNING
      VALUE(rv_active) TYPE abap_bool .
  METHODS set_proxy_control
    IMPORTING
      !iv_enable TYPE abap_bool .
  METHODS set_active
    IMPORTING
      !iv_active TYPE abap_bool .
  METHODS get_class
    IMPORTING
      !iv_class        TYPE seoclsname
    RETURNING
      VALUE(ro_config) TYPE REF TO zif_di_app_config
    RAISING
      zcx_di_error .
  METHODS get_attribute
    IMPORTING
      !iv_attribute    TYPE vseoattrib-cmpname
    RETURNING
      VALUE(ro_config) TYPE REF TO zif_di_app_config
    RAISING
      zcx_di_error .
  METHODS set_component_type
    IMPORTING
      !iv_component_type TYPE string
    RETURNING
      VALUE(ro_config)   TYPE REF TO zif_di_app_config
    RAISING
      zcx_di_error .
  METHODS set_scope
    IMPORTING
      !iv_scope        TYPE string
    RETURNING
      VALUE(ro_config) TYPE REF TO zif_di_app_config
    RAISING
      zcx_di_error .
  METHODS set_qualifier
    IMPORTING
      !iv_qualifier    TYPE string
    RETURNING
      VALUE(ro_config) TYPE REF TO zif_di_app_config
    RAISING
      zcx_di_error .
  METHODS set_proxy
    IMPORTING
      !iv_enable       TYPE abap_bool
    RETURNING
      VALUE(ro_config) TYPE REF TO zif_di_app_config
    RAISING
      zcx_di_error .
  METHODS set_attr_inject
    IMPORTING
      !iv_constructor_parname TYPE string
    RETURNING
      VALUE(ro_config)        TYPE REF TO zif_di_app_config
    RAISING
      zcx_di_error .
  METHODS set_attr_qualifier
    IMPORTING
      !iv_constructor_parname TYPE string
      !iv_qualifier           TYPE string
    RETURNING
      VALUE(ro_config)        TYPE REF TO zif_di_app_config
    RAISING
      zcx_di_error .
ENDINTERFACE.