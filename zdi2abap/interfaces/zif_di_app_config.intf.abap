interface ZIF_DI_APP_CONFIG
  public .


  types:
    BEGIN OF mty_s_class_configuration,
      o_meta TYPE REF TO cl_oo_class,
      info   TYPE zcl_di_scanner=>mty_s_class_info,
    END OF mty_s_class_configuration .
  types:
    BEGIN OF mty_s_proxy_configuration,
      enable TYPE abap_bool,
    END OF mty_s_proxy_configuration .
  types:
    BEGIN OF mty_s_component_configuration,
      enable TYPE abap_bool,
    END OF mty_s_component_configuration .
  types:
    BEGIN OF mty_s_configurations,
      is_active           TYPE abap_bool,
      s_proxy             TYPE mty_s_proxy_configuration,
      s_composite_objects TYPE mty_s_component_configuration,
      t_class             TYPE SORTED TABLE OF mty_s_class_configuration
                         WITH UNIQUE KEY info-class_name,
    END OF mty_s_configurations .

  methods IS_ACTIVE
    returning
      value(RV_ACTIVE) type ABAP_BOOL .
  methods GET_CONFIG
    returning
      value(RS_CONFIG) type ZIF_DI_APP_CONFIG=>MTY_S_CONFIGURATIONS .
  methods SET_PROXY_CONTROL
    importing
      !IV_ENABLE type ABAP_BOOL .
  methods SET_COMPOSITE_OBJECTS_ENABLE
    importing
      !IV_ENABLE type ABAP_BOOL .
  methods SET_ACTIVE
    importing
      !IV_ACTIVE type ABAP_BOOL .
  methods GET_CLASS
    importing
      !IV_CLASS type SEOCLSNAME
    returning
      value(RO_CONFIG) type ref to ZIF_DI_APP_CONFIG
    raising
      ZCX_DI_ERROR .
  methods GET_ATTRIBUTE
    importing
      !IV_ATTRIBUTE type VSEOATTRIB-CMPNAME
    returning
      value(RO_CONFIG) type ref to ZIF_DI_APP_CONFIG
    raising
      ZCX_DI_ERROR .
  methods ADD_ATTRIBUTE
    importing
      !IV_ATTRIBUTE type VSEOATTRIB-CMPNAME
      !IV_TYPE type VSEOATTRIB-TYPE
    returning
      value(RO_CONFIG) type ref to ZIF_DI_APP_CONFIG
    raising
      ZCX_DI_ERROR .
  methods SET_COMPONENT_TYPE
    importing
      !IV_COMPONENT_TYPE type STRING
    returning
      value(RO_CONFIG) type ref to ZIF_DI_APP_CONFIG
    raising
      ZCX_DI_ERROR .
  methods SET_COMPOSITE_OBJECT
    importing
      !IV_COMPOSITE type ABAP_BOOL default 'X'
    returning
      value(RO_CONFIG) type ref to ZIF_DI_APP_CONFIG
    raising
      ZCX_DI_ERROR .
  methods SET_COMPOSITE_PARAMS
    importing
      !IV_METHOD type STRING
      !IV_CLASS type STRING
      !IV_RETURN_PNAME type STRING
    returning
      value(RO_CONFIG) type ref to ZIF_DI_APP_CONFIG
    raising
      ZCX_DI_ERROR .
  methods SET_SCOPE
    importing
      !IV_SCOPE type STRING
    returning
      value(RO_CONFIG) type ref to ZIF_DI_APP_CONFIG
    raising
      ZCX_DI_ERROR .
  methods SET_QUALIFIER
    importing
      !IV_QUALIFIER type STRING
    returning
      value(RO_CONFIG) type ref to ZIF_DI_APP_CONFIG
    raising
      ZCX_DI_ERROR .
  methods SET_PROXY
    importing
      !IV_ENABLE type ABAP_BOOL
    returning
      value(RO_CONFIG) type ref to ZIF_DI_APP_CONFIG
    raising
      ZCX_DI_ERROR .
  methods SET_ATTR_INJECT
    importing
      !IV_CONSTRUCTOR_PARNAME type STRING
      !IV_TYPE_IN_CONSTRUCTOR type STRING optional
    returning
      value(RO_CONFIG) type ref to ZIF_DI_APP_CONFIG
    raising
      ZCX_DI_ERROR .
  methods SET_ATTR_QUALIFIER
    importing
      !IV_CONSTRUCTOR_PARNAME type STRING
      !IV_QUALIFIER type STRING
    returning
      value(RO_CONFIG) type ref to ZIF_DI_APP_CONFIG
    raising
      ZCX_DI_ERROR .
endinterface.