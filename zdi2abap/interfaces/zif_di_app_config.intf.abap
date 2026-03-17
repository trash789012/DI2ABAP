INTERFACE zif_di_app_config
  PUBLIC .


  TYPES:
    BEGIN OF mty_s_class_configuration,
      o_meta TYPE REF TO cl_oo_object, "cl_oo_class,
      info   TYPE zcl_di_scanner=>mty_s_class_info,
    END OF mty_s_class_configuration .
  TYPES:
    BEGIN OF mty_s_proxy_configuration,
      enable TYPE abap_bool,
    END OF mty_s_proxy_configuration .
  TYPES:
    BEGIN OF mty_s_component_configuration,
      enable TYPE abap_bool,
    END OF mty_s_component_configuration .
  TYPES:
    BEGIN OF mty_s_configurations,
      is_active           TYPE abap_bool,
      s_proxy             TYPE mty_s_proxy_configuration,
      s_composite_objects TYPE mty_s_component_configuration,
      t_class             TYPE SORTED TABLE OF mty_s_class_configuration
                         WITH UNIQUE KEY info-class_name,
    END OF mty_s_configurations .

  METHODS is_active
    RETURNING
      VALUE(rv_active) TYPE abap_bool .
  METHODS get_config
    RETURNING
      VALUE(rs_config) TYPE zif_di_app_config=>mty_s_configurations .
  METHODS set_proxy_control
    IMPORTING
      !iv_enable TYPE abap_bool .
  METHODS set_composite_objects_enable
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
  METHODS add_attribute
    IMPORTING
      !iv_attribute    TYPE vseoattrib-cmpname
      !iv_type         TYPE vseoattrib-type
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
  METHODS set_composite_object
    IMPORTING
      !iv_composite    TYPE abap_bool DEFAULT 'X'
    RETURNING
      VALUE(ro_config) TYPE REF TO zif_di_app_config
    RAISING
      zcx_di_error .
  METHODS set_composite_params
    IMPORTING
      !iv_method       TYPE string
      !iv_class        TYPE string
      !iv_return_pname TYPE string
    RETURNING
      VALUE(ro_config) TYPE REF TO zif_di_app_config
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
      !iv_type_in_constructor TYPE string OPTIONAL
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