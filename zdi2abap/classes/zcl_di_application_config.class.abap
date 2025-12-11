CLASS zcl_di_application_config DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_di_app_config .

    TYPES:
      BEGIN OF mty_s_class_configuration,
        o_meta TYPE REF TO cl_oo_class,
        info   TYPE zcl_di_scanner=>mty_s_class_info,
      END OF mty_s_class_configuration .
    TYPES:
      BEGIN OF mty_s_proxy_configuration,
        enable TYPE abap_bool,
      END OF mty_s_proxy_configuration .
    TYPES:
      BEGIN OF mty_s_configurations,
        is_active TYPE abap_bool,
        s_proxy   TYPE mty_s_proxy_configuration,
        t_class   TYPE SORTED TABLE OF mty_s_class_configuration
                             WITH UNIQUE KEY info-class_name,
      END OF mty_s_configurations .

    METHODS constructor .
    METHODS dispose .
    METHODS get_config
      RETURNING
        VALUE(rs_config) TYPE mty_s_configurations .
    METHODS replace_annotation
      IMPORTING
        !iv_name        TYPE string
        !iv_value       TYPE string OPTIONAL
      CHANGING
        !ct_annotations TYPE zcl_di_annotation_processor=>mty_t_annotations .
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA ms_app_config TYPE mty_s_configurations .
    DATA mv_last_class_name TYPE seoclsname .
    DATA ms_last_attribute TYPE vseoattrib .

    METHODS lookup_cls
      RETURNING
        VALUE(rr_class) TYPE REF TO mty_s_class_configuration
      RAISING
        zcx_di_error .
    METHODS lookup_attr
      RETURNING
        VALUE(rr_class) TYPE REF TO zcl_di_scanner=>mty_s_class_info
      RAISING
        zcx_di_error .
    METHODS lookup_dependensy
      IMPORTING
        !iv_parameter_name   TYPE string
        !iv_parameter_type   TYPE vseoattrib-type
      RETURNING
        VALUE(rr_dependensy) TYPE REF TO zcl_di_scanner=>mty_s_parameter_info
      RAISING
        zcx_di_error .
ENDCLASS.



CLASS ZCL_DI_APPLICATION_CONFIG IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_DI_APPLICATION_CONFIG->CONSTRUCTOR
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD constructor.
    ms_app_config-is_active = abap_true.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_DI_APPLICATION_CONFIG->DISPOSE
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD dispose.
    FREE ms_app_config.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_DI_APPLICATION_CONFIG->GET_CONFIG
* +-------------------------------------------------------------------------------------------------+
* | [<-()] RS_CONFIG                      TYPE        MTY_S_CONFIGURATIONS
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_config.
    rs_config = ms_app_config.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_DI_APPLICATION_CONFIG->LOOKUP_ATTR
* +-------------------------------------------------------------------------------------------------+
* | [<-()] RR_CLASS                       TYPE REF TO ZCL_DI_SCANNER=>MTY_S_CLASS_INFO
* | [!CX!] ZCX_DI_ERROR
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD lookup_attr.
    DATA:
      lt_return TYPE bapiret2_tt.

    DATA(lr_class) = lookup_cls( ).

    IF ms_last_attribute IS INITIAL.
      MESSAGE e012 INTO DATA(lv_dummy).
      APPEND zcx_di_error=>sy2bapiret( ) TO lt_return.

      RAISE EXCEPTION TYPE zcx_di_error EXPORTING mt_errors = lt_return.
    ENDIF.

    rr_class = REF #( lr_class->info ).

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_DI_APPLICATION_CONFIG->LOOKUP_CLS
* +-------------------------------------------------------------------------------------------------+
* | [<-()] RR_CLASS                       TYPE REF TO MTY_S_CLASS_CONFIGURATION
* | [!CX!] ZCX_DI_ERROR
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD lookup_cls.
    DATA:
      lt_return TYPE bapiret2_tt.

    READ TABLE ms_app_config-t_class REFERENCE INTO rr_class
      WITH TABLE KEY info-class_name = mv_last_class_name.
    IF sy-subrc <> 0.
      MESSAGE e011 INTO DATA(lv_dummy).
      APPEND zcx_di_error=>sy2bapiret( ) TO lt_return.

      RAISE EXCEPTION TYPE zcx_di_error EXPORTING mt_errors = lt_return.
    ENDIF.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_DI_APPLICATION_CONFIG->LOOKUP_DEPENDENSY
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_PARAMETER_NAME              TYPE        STRING
* | [--->] IV_PARAMETER_TYPE              TYPE        VSEOATTRIB-TYPE
* | [<-()] RR_DEPENDENSY                  TYPE REF TO ZCL_DI_SCANNER=>MTY_S_PARAMETER_INFO
* | [!CX!] ZCX_DI_ERROR
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD lookup_dependensy.

    DATA(lr_info) = lookup_attr( ).

    READ TABLE lr_info->injected_dependencies REFERENCE INTO rr_dependensy
      WITH KEY k1 COMPONENTS attribute_name = ms_last_attribute-cmpname
                             parameter_name = iv_parameter_name
                             parameter_type = iv_parameter_type.
    IF sy-subrc <> 0.
      APPEND INITIAL LINE TO lr_info->injected_dependencies REFERENCE INTO rr_dependensy.
      rr_dependensy->attribute_name = ms_last_attribute-cmpname.
      rr_dependensy->parameter_name = iv_parameter_name.
      rr_dependensy->parameter_type = iv_parameter_type.
    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_DI_APPLICATION_CONFIG->REPLACE_ANNOTATION
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_NAME                        TYPE        STRING
* | [--->] IV_VALUE                       TYPE        STRING(optional)
* | [<-->] CT_ANNOTATIONS                 TYPE        ZCL_DI_ANNOTATION_PROCESSOR=>MTY_T_ANNOTATIONS
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD replace_annotation.

    ASSIGN ct_annotations[ name = iv_name ] TO FIELD-SYMBOL(<ls_annotation>).
    IF sy-subrc = 0.
      <ls_annotation>-value = iv_value.
      RETURN.
    ENDIF.

    APPEND VALUE #( name  = iv_name
                    value = iv_value ) TO ct_annotations.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_DI_APPLICATION_CONFIG->ZIF_DI_APP_CONFIG~GET_ATTRIBUTE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_ATTRIBUTE                   TYPE        VSEOATTRIB-CMPNAME
* | [<-()] RO_CONFIG                      TYPE REF TO ZIF_DI_APP_CONFIG
* | [!CX!] ZCX_DI_ERROR
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD zif_di_app_config~get_attribute.

    DATA:
      lt_return TYPE bapiret2_tt.

    CLEAR:
      ms_last_attribute.

    DATA(lr_info) = lookup_cls( ).

    DATA(lt_attr) = lr_info->o_meta->get_attributes( reference_attributes_only = abap_true ).

    ASSIGN lt_attr[ cmpname = iv_attribute ] TO FIELD-SYMBOL(<ls_attr>).
    IF sy-subrc <> 0.
      MESSAGE e012 INTO DATA(lv_dummy).
      APPEND zcx_di_error=>sy2bapiret( ) TO lt_return.

      RAISE EXCEPTION TYPE zcx_di_error EXPORTING mt_errors = lt_return.
    ENDIF.

    ms_last_attribute = <ls_attr>.

    ro_config ?= me.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_DI_APPLICATION_CONFIG->ZIF_DI_APP_CONFIG~GET_CLASS
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_CLASS                       TYPE        SEOCLSNAME
* | [<-()] RO_CONFIG                      TYPE REF TO ZIF_DI_APP_CONFIG
* | [!CX!] ZCX_DI_ERROR
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD zif_di_app_config~get_class.

    CLEAR:
      mv_last_class_name,
      ms_last_attribute.

    READ TABLE ms_app_config-t_class ASSIGNING FIELD-SYMBOL(<ls_class>)
      WITH TABLE KEY info-class_name = iv_class.
    IF sy-subrc = 0.
      ro_config                    ?= me.
      mv_last_class_name = iv_class.
      RETURN.
    ENDIF.

    TRY.
        mv_last_class_name = iv_class.

        DATA(lo_meta) = CAST cl_oo_class( cl_oo_object=>get_instance( iv_class ) ).

        INSERT VALUE #( info       = VALUE #( class_name = iv_class )
                        o_meta     = lo_meta )
         INTO TABLE ms_app_config-t_class.
      CATCH cx_class_not_existent INTO DATA(lx_error).
        RAISE EXCEPTION TYPE zcx_di_error EXPORTING previous = lx_error.
    ENDTRY.

    ro_config ?= me.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_DI_APPLICATION_CONFIG->ZIF_DI_APP_CONFIG~IS_ACTIVE
* +-------------------------------------------------------------------------------------------------+
* | [<-()] RV_ACTIVE                      TYPE        ABAP_BOOL
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD zif_di_app_config~is_active.
    ms_app_config-is_active = rv_active.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_DI_APPLICATION_CONFIG->ZIF_DI_APP_CONFIG~SET_ACTIVE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_ACTIVE                      TYPE        ABAP_BOOL
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD zif_di_app_config~set_active.
    ms_app_config-is_active = iv_active.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_DI_APPLICATION_CONFIG->ZIF_DI_APP_CONFIG~SET_ATTR_INJECT
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_CONSTRUCTOR_PARNAME         TYPE        STRING
* | [<-()] RO_CONFIG                      TYPE REF TO ZIF_DI_APP_CONFIG
* | [!CX!] ZCX_DI_ERROR
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD zif_di_app_config~set_attr_inject.

    DATA(lr_dependensy) = lookup_dependensy(
                            iv_parameter_name = iv_constructor_parname
                            iv_parameter_type = ms_last_attribute-type
                          ).

    lr_dependensy->has_inject = abap_true.

    replace_annotation( EXPORTING iv_name        = zif_annotations=>mc_anotations-inject
                                  iv_value       = iv_constructor_parname
                        CHANGING  ct_annotations = lr_dependensy->annotations ).

    lr_dependensy->override_controls-set_attr_inject = abap_true.

    ro_config ?= me.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_DI_APPLICATION_CONFIG->ZIF_DI_APP_CONFIG~SET_ATTR_QUALIFIER
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_CONSTRUCTOR_PARNAME         TYPE        STRING
* | [--->] IV_QUALIFIER                   TYPE        STRING
* | [<-()] RO_CONFIG                      TYPE REF TO ZIF_DI_APP_CONFIG
* | [!CX!] ZCX_DI_ERROR
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD zif_di_app_config~set_attr_qualifier.

    DATA(lr_dependensy) = lookup_dependensy(
                            iv_parameter_name = iv_constructor_parname
                            iv_parameter_type = ms_last_attribute-type
                          ).

    lr_dependensy->qualifier = iv_qualifier.

    replace_annotation( EXPORTING iv_name        = zif_annotations=>mc_anotations-reference_qualifier
                                  iv_value       = iv_qualifier
                        CHANGING  ct_annotations = lr_dependensy->annotations ).

    lr_dependensy->override_controls-set_attr_qualifier = abap_true.

    ro_config ?= me.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_DI_APPLICATION_CONFIG->ZIF_DI_APP_CONFIG~SET_COMPONENT_TYPE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_COMPONENT_TYPE              TYPE        STRING
* | [<-()] RO_CONFIG                      TYPE REF TO ZIF_DI_APP_CONFIG
* | [!CX!] ZCX_DI_ERROR
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD zif_di_app_config~set_component_type.

    DATA:
      lt_return TYPE bapiret2_tt.

    DATA(lr_class_conf) = lookup_cls( ).

*--------------------------------------------------------------------*
*   Check
*--------------------------------------------------------------------*
    DATA(lt_allowed_types) = zcl_di_annotation_processor=>get_all_component_types( ).

    IF NOT line_exists( lt_allowed_types[ table_line = iv_component_type ] ).
      MESSAGE e010
        WITH iv_component_type
        INTO DATA(lv_dummy).

      APPEND zcx_di_error=>sy2bapiret( ) TO lt_return.
    ENDIF.

    IF lt_return IS NOT INITIAL.
      RAISE EXCEPTION TYPE zcx_di_error EXPORTING mt_errors = lt_return.
    ENDIF.

*--------------------------------------------------------------------*
*   Set
*--------------------------------------------------------------------*
    lr_class_conf->info-has_component  = abap_true.
    lr_class_conf->info-component_type = iv_component_type.

    replace_annotation( EXPORTING iv_name        = iv_component_type
                        CHANGING  ct_annotations = lr_class_conf->info-annotations ).

    lr_class_conf->info-override_controls-set_component_type = abap_true.

    ro_config ?= me.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_DI_APPLICATION_CONFIG->ZIF_DI_APP_CONFIG~SET_PROXY
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_ENABLE                      TYPE        ABAP_BOOL
* | [<-()] RO_CONFIG                      TYPE REF TO ZIF_DI_APP_CONFIG
* | [!CX!] ZCX_DI_ERROR
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD zif_di_app_config~set_proxy.
    DATA(lr_info) = lookup_cls( ).

    lr_info->info-proxy_enable = iv_enable.

    DATA(lv_value) = COND #( WHEN iv_enable = abap_true
                              THEN zif_annotations=>mc_annotation_values-true
                              ELSE zif_annotations=>mc_annotation_values-false ).

    replace_annotation( EXPORTING iv_name        = zif_annotations=>mc_anotations-proxy
                                  iv_value       = lv_value
                        CHANGING  ct_annotations = lr_info->info-annotations ).

    lr_info->info-override_controls-set_proxy = abap_true.

    ro_config ?= me.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_DI_APPLICATION_CONFIG->ZIF_DI_APP_CONFIG~SET_PROXY_CONTROL
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_ENABLE                      TYPE        ABAP_BOOL
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD zif_di_app_config~set_proxy_control.
    ms_app_config-s_proxy-enable = iv_enable.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_DI_APPLICATION_CONFIG->ZIF_DI_APP_CONFIG~SET_QUALIFIER
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_QUALIFIER                   TYPE        STRING
* | [<-()] RO_CONFIG                      TYPE REF TO ZIF_DI_APP_CONFIG
* | [!CX!] ZCX_DI_ERROR
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD zif_di_app_config~set_qualifier.

    DATA(lr_info) = lookup_cls( ).

    lr_info->info-qualifier = iv_qualifier.

    replace_annotation( EXPORTING iv_name        = zif_annotations=>mc_anotations-qualifier
                                  iv_value       = iv_qualifier
                        CHANGING  ct_annotations = lr_info->info-annotations ).

    lr_info->info-override_controls-set_qualifier = abap_true.

    ro_config ?= me.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_DI_APPLICATION_CONFIG->ZIF_DI_APP_CONFIG~SET_SCOPE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_SCOPE                       TYPE        STRING
* | [<-()] RO_CONFIG                      TYPE REF TO ZIF_DI_APP_CONFIG
* | [!CX!] ZCX_DI_ERROR
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD zif_di_app_config~set_scope.

    DATA:
      lt_return TYPE bapiret2_tt.

    DATA(lr_info) = lookup_cls( ).

*--------------------------------------------------------------------*
*   Check
*--------------------------------------------------------------------*
    DATA(lt_scope) = zcl_di_annotation_processor=>get_all_scopes( ).

    READ TABLE lt_scope TRANSPORTING NO FIELDS
      WITH KEY table_line = iv_scope.
    IF sy-subrc <> 0.
      MESSAGE e003
        WITH lr_info->info-class_name
             iv_scope
        INTO DATA(lv_dummy).

      APPEND zcx_di_error=>sy2bapiret( ) TO lt_return.
      RAISE EXCEPTION TYPE zcx_di_error EXPORTING mt_errors = lt_return.
    ENDIF.

*--------------------------------------------------------------------*
*   Set
*--------------------------------------------------------------------*
    lr_info->info-scope = iv_scope.

    replace_annotation( EXPORTING iv_name        = zif_annotations=>mc_anotations-scope
                                  iv_value       = iv_scope
                        CHANGING  ct_annotations = lr_info->info-annotations ).

    lr_info->info-override_controls-set_scope = abap_true.

    ro_config ?= me.
  ENDMETHOD.
ENDCLASS.