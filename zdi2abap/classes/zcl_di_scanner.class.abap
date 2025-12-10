CLASS zcl_di_scanner DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      mty_t_configs TYPE STANDARD TABLE OF REF TO zcl_di_application_config WITH EMPTY KEY.

    TYPES:
      BEGIN OF mty_s_dep_controls,
        set_attr_inject    TYPE abap_bool,
        set_attr_qualifier TYPE abap_bool,
      END OF mty_s_dep_controls.
    TYPES:
      BEGIN OF mty_s_parameter_info,
        attribute_name    TYPE string,
        parameter_name    TYPE string,
        parameter_type    TYPE string,
        class_name        TYPE seoclname,
        annotations       TYPE zcl_di_annotation_processor=>mty_t_annotations,
        has_inject        TYPE string,
        qualifier         TYPE string,
        override_controls TYPE mty_s_dep_controls,
      END OF mty_s_parameter_info .
    TYPES:
      mty_t_parameter_info TYPE STANDARD TABLE OF mty_s_parameter_info
                             WITH KEY parameter_name
                             WITH NON-UNIQUE SORTED KEY k1 COMPONENTS attribute_name parameter_type
                             WITH NON-UNIQUE SORTED KEY k2 COMPONENTS has_inject.
    TYPES:
      BEGIN OF mty_s_controls,
        set_component_type TYPE abap_bool,
        set_scope          TYPE abap_bool,
        set_qualifier      TYPE abap_bool,
        set_proxy          TYPE abap_bool,
      END OF mty_s_controls.
    TYPES:
      BEGIN OF mty_s_class_info,
        class_name            TYPE seoclass-clsname,
        absolute_type         TYPE string,
        annotations           TYPE zcl_di_annotation_processor=>mty_t_annotations,
        has_component         TYPE abap_bool,
        component_type        TYPE string,
        scope                 TYPE string,
        qualifier             TYPE string,
        proxy_enable          TYPE abap_bool,
        injected_dependencies TYPE mty_t_parameter_info,
        related_dependencies  TYPE mty_t_parameter_info,
        override_controls     TYPE mty_s_controls,
      END OF mty_s_class_info .
    TYPES:
      mty_t_class_info TYPE STANDARD TABLE OF mty_s_class_info
                         WITH KEY class_name
                         WITH NON-UNIQUE SORTED KEY k1 COMPONENTS absolute_type.

    METHODS dispose.
    METHODS set_scan_packages
      IMPORTING
        !it_package TYPE string_table .
    METHODS scan_package
      IMPORTING
        !iv_component_only TYPE abap_bool DEFAULT 'X'
      RETURNING
        VALUE(rt_classes)  TYPE mty_t_class_info
      RAISING
        zcx_di_error .
    METHODS get_scan_result
      RETURNING
        VALUE(rt_scan_result) TYPE mty_t_class_info .
    METHODS set_scan_result
      IMPORTING
        !it_scan_result TYPE mty_t_class_info .
  PROTECTED SECTION.

    TYPES:
      BEGIN OF mty_s_config,
        scan_packages   TYPE string_table,
        t_class_configs TYPE mty_t_configs,
      END OF mty_s_config .
  PRIVATE SECTION.

    DATA ms_configuration TYPE mty_s_config .
    DATA mt_scan_result TYPE mty_t_class_info .

    METHODS override_settings_by_config
      CHANGING
        VALUE(cs_info) TYPE mty_s_class_info .
    METHODS search_and_call_config_classes
      IMPORTING
        !it_classes       TYPE poc_class_tab
      RETURNING
        VALUE(rt_configs) TYPE mty_t_configs
      RAISING
        zcx_di_error .
    METHODS checks_after_scan
      CHANGING
        !ct_class TYPE mty_t_class_info
      RAISING
        zcx_di_error .
    METHODS scan_single_class
      IMPORTING
        !iv_class_name       TYPE seoclsname
      RETURNING
        VALUE(rs_class_info) TYPE mty_s_class_info
      RAISING
        zcx_di_error .
    METHODS get_class_scr_code
      IMPORTING
        !iv_class_name     TYPE seoclsname
      RETURNING
        VALUE(rt_scr_code) TYPE string_table
      RAISING
        zcx_di_error .
    METHODS find_dependencies
      IMPORTING
        !it_scr_code           TYPE string_table
        !io_class              TYPE REF TO cl_oo_object OPTIONAL
      RETURNING
        VALUE(rt_dependencies) TYPE mty_t_parameter_info .
    METHODS check_doubles
      IMPORTING
        !it_class TYPE mty_t_class_info
      RAISING
        zcx_di_error .
    METHODS check_scope_value
      IMPORTING
        !iv_component TYPE string
        !iv_class     TYPE seoclass-clsname
        !iv_scope     TYPE string
      RAISING
        zcx_di_error .
    METHODS check_constructor_params
      IMPORTING
        !io_class      TYPE REF TO cl_oo_class
        !is_class_info TYPE mty_s_class_info
      RAISING
        zcx_di_error .
ENDCLASS.



CLASS ZCL_DI_SCANNER IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_DI_SCANNER->CHECKS_AFTER_SCAN
* +-------------------------------------------------------------------------------------------------+
* | [<-->] CT_CLASS                       TYPE        MTY_T_CLASS_INFO
* | [!CX!] ZCX_DI_ERROR
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD checks_after_scan.
    TYPES:
      BEGIN OF lty_s_qualifier_key,
        absolute_name TYPE string,
        qualifier     TYPE string,
        count         TYPE i,
      END OF lty_s_qualifier_key.

    DATA:
      lt_all_types     TYPE SORTED TABLE OF string WITH UNIQUE KEY table_line,
      lt_implements    TYPE STANDARD TABLE OF string WITH EMPTY KEY,
      lt_qualifier_key TYPE SORTED TABLE OF lty_s_qualifier_key
                         WITH UNIQUE KEY absolute_name qualifier,
      lt_return        TYPE bapiret2_tt.

    "Собираем список абсолютных типов
    LOOP AT ct_class ASSIGNING FIELD-SYMBOL(<ls_class>).

      LOOP AT <ls_class>-injected_dependencies ASSIGNING FIELD-SYMBOL(<ls_dep>)
        WHERE has_inject IS NOT INITIAL.
        INSERT <ls_dep>-parameter_type INTO TABLE lt_all_types.
      ENDLOOP.

    ENDLOOP.

    "Проверки связанные с дублированием
    TRY.
        check_doubles( ct_class ).
      CATCH zcx_di_error INTO DATA(lx_error).
        APPEND LINES OF lx_error->get_messages( ) TO lt_return.
    ENDTRY.

    "Идем по зависимостям и выполняем проверки
    LOOP AT ct_class ASSIGNING <ls_class>.
      TRY.
          DATA(lo_class) = CAST cl_oo_class( cl_oo_object=>get_instance( <ls_class>-class_name ) ).
          DATA(lt_interfaces) = lo_class->get_implemented_interfaces( ).
        CATCH cx_class_not_existent.
          CONTINUE.
      ENDTRY.

      "Класс интерцептор не может быть проксирован
      IF <ls_class>-component_type = zif_annotations=>mc_component-interceptor AND
         <ls_class>-proxy_enable IS NOT INITIAL.
        MESSAGE e009
          WITH <ls_class>-class_name
               zif_annotations=>mc_component-interceptor
          INTO DATA(lv_dummy).

        APPEND zcx_di_error=>sy2bapiret( ) TO lt_return.
      ENDIF.

      "Проверим скоуп, он должен быть одним из существующих в константах
      TRY.
          check_scope_value( iv_component = <ls_class>-component_type
                             iv_class     = <ls_class>-class_name
                             iv_scope     = <ls_class>-scope ).
        CATCH zcx_di_error INTO lx_error.
          APPEND LINES OF lx_error->get_messages( ) TO lt_return.
      ENDTRY.

      "Проверим конструктор класса на наличие параметров, которые планируется внедрять
      TRY.
          check_constructor_params( io_class      = lo_class
                                    is_class_info = <ls_class> ).
        CATCH zcx_di_error INTO lx_error.
          APPEND LINES OF lx_error->get_messages( ) TO lt_return.
      ENDTRY.

      "Класс не должен имплементировать два запрашиваемых типа (например ZIF_REPO + ZIF_LOGGER)
      lt_implements = VALUE #( ).

      READ TABLE lt_all_types TRANSPORTING NO FIELDS
        WITH TABLE KEY table_line = <ls_class>-class_name.
      IF sy-subrc = 0.
        APPEND <ls_class>-class_name TO lt_implements.

        <ls_class>-absolute_type = <ls_class>-class_name.
      ENDIF.

      LOOP AT lt_interfaces ASSIGNING FIELD-SYMBOL(<ls_iterface>).
        READ TABLE lt_all_types TRANSPORTING NO FIELDS
          WITH TABLE KEY table_line = <ls_iterface>-refclsname.
        IF sy-subrc = 0.
          APPEND <ls_iterface>-refclsname TO lt_implements.

          <ls_class>-absolute_type = <ls_iterface>-refclsname.
        ENDIF.
      ENDLOOP.

      IF <ls_class>-absolute_type IS INITIAL.
        "Не реализует ни один интерфейс, значит класс
        <ls_class>-absolute_type = <ls_class>-class_name.
      ENDIF.

      IF lines( lt_implements ) > 1.
        CONCATENATE LINES OF lt_implements INTO DATA(lv_impl_all) SEPARATED BY ','.
        MESSAGE e000
          WITH <ls_class>-class_name
               lv_impl_all
          INTO lv_dummy.

        APPEND zcx_di_error=>sy2bapiret( ) TO lt_return.
        CONTINUE.
      ELSE.
        COLLECT VALUE lty_s_qualifier_key( absolute_name = <ls_class>-absolute_type
                                           qualifier     = <ls_class>-qualifier
                                           count         = 1 ) INTO lt_qualifier_key.
      ENDIF.
    ENDLOOP.

    "Не должно быть двух однотипных компонентов с одинаковым qualifier
    DELETE lt_qualifier_key WHERE count < 2.

    LOOP AT lt_qualifier_key ASSIGNING FIELD-SYMBOL(<ls_double>).
      MESSAGE e001
          WITH <ls_double>-absolute_name
               <ls_double>-qualifier
          INTO lv_dummy.

      APPEND zcx_di_error=>sy2bapiret( ) TO lt_return.
    ENDLOOP.

    IF lt_return IS NOT INITIAL.
      RAISE EXCEPTION TYPE zcx_di_error EXPORTING mt_errors = lt_return.
    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_DI_SCANNER->CHECK_CONSTRUCTOR_PARAMS
* +-------------------------------------------------------------------------------------------------+
* | [--->] IO_CLASS                       TYPE REF TO CL_OO_CLASS
* | [--->] IS_CLASS_INFO                  TYPE        MTY_S_CLASS_INFO
* | [!CX!] ZCX_DI_ERROR
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD check_constructor_params.
    DATA:
     lt_return TYPE bapiret2_tt.

    CONSTANTS:
      lc_method TYPE seocpdname VALUE 'CONSTRUCTOR'.

    IF is_class_info-injected_dependencies IS INITIAL.
      RETURN.
    ENDIF.

    IF io_class IS NOT BOUND.
      RETURN.
    ENDIF.

    TRY.
        DATA(ls_signature) = io_class->get_component_signature( cpdname = lc_method ).
      CATCH cx_component_not_existing.
        "конструктор класса не содержит параметров при наличии зависимостей
        MESSAGE e004
          WITH is_class_info-class_name
          INTO DATA(lv_dummy).
        APPEND zcx_di_error=>sy2bapiret( ) TO lt_return.
    ENDTRY.

    LOOP AT is_class_info-injected_dependencies ASSIGNING FIELD-SYMBOL(<ls_dep>)
      WHERE has_inject = abap_true.

      READ TABLE ls_signature-params TRANSPORTING NO FIELDS
        WITH KEY sconame = <ls_dep>-parameter_name.
      CHECK sy-subrc <> 0.

      MESSAGE e005
        WITH <ls_dep>-parameter_name
             is_class_info-class_name
        INTO lv_dummy.
      APPEND zcx_di_error=>sy2bapiret( ) TO lt_return.
    ENDLOOP.

    IF lt_return IS NOT INITIAL.
      RAISE EXCEPTION TYPE zcx_di_error EXPORTING mt_errors = lt_return.
    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_DI_SCANNER->CHECK_DOUBLES
* +-------------------------------------------------------------------------------------------------+
* | [--->] IT_CLASS                       TYPE        MTY_T_CLASS_INFO
* | [!CX!] ZCX_DI_ERROR
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD check_doubles.
    DATA:
     lt_return TYPE bapiret2_tt.

    DATA(lv_interceptor_count) = REDUCE i(
      INIT x = 0
      FOR <ls_class> IN it_class WHERE ( component_type = zif_annotations=>mc_component-interceptor )
      NEXT x = x + 1 ).

    IF lv_interceptor_count > 1.
      MESSAGE e008
        WITH zif_annotations=>mc_component-interceptor
        INTO DATA(lv_dummy).

      APPEND zcx_di_error=>sy2bapiret( ) TO lt_return.
    ENDIF.

    IF lt_return IS NOT INITIAL.
      RAISE EXCEPTION TYPE zcx_di_error EXPORTING mt_errors = lt_return.
    ENDIF.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_DI_SCANNER->CHECK_SCOPE_VALUE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_COMPONENT                   TYPE        STRING
* | [--->] IV_CLASS                       TYPE        SEOCLASS-CLSNAME
* | [--->] IV_SCOPE                       TYPE        STRING
* | [!CX!] ZCX_DI_ERROR
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD check_scope_value.
    DATA:
      lt_return TYPE bapiret2_tt.

    DATA(lt_scope) = zcl_di_annotation_processor=>get_all_scopes( ).

    READ TABLE lt_scope TRANSPORTING NO FIELDS
      WITH KEY table_line = iv_scope.
    IF sy-subrc <> 0.
      MESSAGE e003
        WITH iv_class
             iv_scope
        INTO DATA(lv_dummy).

      APPEND zcx_di_error=>sy2bapiret( ) TO lt_return.
      RAISE EXCEPTION TYPE zcx_di_error EXPORTING mt_errors = lt_return.
    ENDIF.

    IF iv_component = zif_annotations=>mc_component-interceptor AND
       iv_scope <> zif_annotations=>mc_scope-singleton.

      MESSAGE e007
        WITH iv_class
             to_lower( zif_annotations=>mc_component-interceptor )
             zif_annotations=>mc_scope-singleton
        INTO lv_dummy.

      APPEND zcx_di_error=>sy2bapiret( ) TO lt_return.
      RAISE EXCEPTION TYPE zcx_di_error EXPORTING mt_errors = lt_return.
    ENDIF.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_DI_SCANNER->DISPOSE
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD dispose.
    FREE:
      ms_configuration,
      mt_scan_result.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_DI_SCANNER->FIND_DEPENDENCIES
* +-------------------------------------------------------------------------------------------------+
* | [--->] IT_SCR_CODE                    TYPE        STRING_TABLE
* | [--->] IO_CLASS                       TYPE REF TO CL_OO_OBJECT(optional)
* | [<-()] RT_DEPENDENCIES                TYPE        MTY_T_PARAMETER_INFO
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD find_dependencies.

    DATA(lt_attr) = io_class->get_attributes( reference_attributes_only = abap_true ).

    LOOP AT lt_attr ASSIGNING FIELD-SYMBOL(<ls_attr>).

      "Находим атрибут
      LOOP AT it_scr_code TRANSPORTING NO FIELDS
        WHERE table_line CP |*DATA*{ <ls_attr>-cmpname }*|.

        DATA(lv_form) = sy-tabix - 1.
        EXIT.
      ENDLOOP.
      CHECK sy-subrc = 0.

      "Двигаемся вверх и находим аннотации
      DATA(lt_annotations) = VALUE zcl_di_annotation_processor=>mty_t_annotations( ).

      DO.
        READ TABLE it_scr_code INDEX lv_form ASSIGNING FIELD-SYMBOL(<lv_line>).
        IF sy-subrc <> 0.
          EXIT.
        ENDIF.

        "парсим аннотации
        DATA(lt_annotations_line) = zcl_di_annotation_processor=>parse_annotations(
                                      it_source_code = VALUE #( ( <lv_line> ) )
                                    ).
        IF lt_annotations_line IS INITIAL.
          EXIT.
        ENDIF.

        APPEND LINES OF lt_annotations_line TO lt_annotations.

        "Двигаемся вверх
        lv_form = lv_form - 1.
      ENDDO.

      APPEND INITIAL LINE TO rt_dependencies ASSIGNING FIELD-SYMBOL(<ls_dep>).

      "имя атрибута
      <ls_dep>-attribute_name = <ls_attr>-cmpname.

      "имя входящего параметра для внедрения через конструктор
      <ls_dep>-parameter_name = zcl_di_annotation_processor=>get_inject_argument( lt_annotations ).
      IF <ls_dep>-parameter_name IS INITIAL.
        "если не указано, то название = имя атрибута
        <ls_dep>-parameter_name = <ls_attr>-cmpname.
      ENDIF.

      <ls_dep>-parameter_type = <ls_attr>-type.
      <ls_dep>-annotations    = lt_annotations.
      <ls_dep>-has_inject     = zcl_di_annotation_processor=>has_inject( lt_annotations ).
      <ls_dep>-qualifier      = zcl_di_annotation_processor=>get_ref_qualifier( lt_annotations ).
    ENDLOOP.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_DI_SCANNER->GET_CLASS_SCR_CODE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_CLASS_NAME                  TYPE        SEOCLSNAME
* | [<-()] RT_SCR_CODE                    TYPE        STRING_TABLE
* | [!CX!] ZCX_DI_ERROR
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_class_scr_code.

    cl_reca_rs_services=>get_source(
      EXPORTING
        id_objtype = 'CLAS'
        id_objname = iv_class_name
      IMPORTING
        et_source  = rt_scr_code
      EXCEPTIONS
        not_found  = 1
        OTHERS     = 2
    ).
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_di_error.
    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_DI_SCANNER->GET_SCAN_RESULT
* +-------------------------------------------------------------------------------------------------+
* | [<-()] RT_SCAN_RESULT                 TYPE        MTY_T_CLASS_INFO
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_scan_result.
    rt_scan_result = mt_scan_result.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_DI_SCANNER->OVERRIDE_SETTINGS_BY_CONFIG
* +-------------------------------------------------------------------------------------------------+
* | [<-->] CS_INFO                        TYPE        MTY_S_CLASS_INFO
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD override_settings_by_config.

    LOOP AT ms_configuration-t_class_configs ASSIGNING FIELD-SYMBOL(<lo_config>).

      DATA(ls_config) = <lo_config>->get_config( ).

      CHECK ls_config-is_active IS NOT INITIAL.

*--------------------------------------------------------------------*
*     Общее
*--------------------------------------------------------------------*
      "Прокси может отключаться не на уровне класса, а на уровне всей системы
      IF ls_config-s_proxy-enable IS INITIAL.
        CLEAR cs_info-proxy_enable.
        DELETE cs_info-annotations WHERE name = zif_annotations=>mc_anotations-proxy.
      ENDIF.

      READ TABLE ls_config-t_class ASSIGNING FIELD-SYMBOL(<ls_cls_config>)
        WITH TABLE KEY info-class_name = cs_info-class_name.
      CHECK sy-subrc = 0.

      "меняем в соответствии с настройкой
*--------------------------------------------------------------------*
*     Основной класс
*--------------------------------------------------------------------*
      IF <ls_cls_config>-info-override_controls-set_component_type IS NOT INITIAL.
        cs_info-component_type = <ls_cls_config>-info-component_type.
        cs_info-has_component  = <ls_cls_config>-info-has_component.

        LOOP AT zcl_di_annotation_processor=>get_all_component_types( )
          ASSIGNING FIELD-SYMBOL(<lv_type>).

          DELETE cs_info-annotations WHERE name = <lv_type>.
        ENDLOOP.

        <lo_config>->replace_annotation(
           EXPORTING
             iv_name        = cs_info-component_type
           CHANGING
             ct_annotations = cs_info-annotations ).
      ENDIF.

      IF <ls_cls_config>-info-override_controls-set_scope IS NOT INITIAL.
        cs_info-scope = <ls_cls_config>-info-scope.

        <lo_config>->replace_annotation(
           EXPORTING
             iv_name        = zif_annotations=>mc_anotations-scope
             iv_value       = cs_info-scope
           CHANGING
             ct_annotations = cs_info-annotations ).
      ENDIF.

      IF <ls_cls_config>-info-override_controls-set_qualifier IS NOT INITIAL.
        cs_info-qualifier = <ls_cls_config>-info-qualifier.

        <lo_config>->replace_annotation(
           EXPORTING
             iv_name        = zif_annotations=>mc_anotations-qualifier
             iv_value       = cs_info-qualifier
           CHANGING
             ct_annotations = cs_info-annotations ).
      ENDIF.

      IF ls_config-s_proxy-enable IS INITIAL.
        "Отключено на уровне настройки
        cs_info-proxy_enable = abap_false.
        DELETE cs_info-annotations WHERE name = zif_annotations=>mc_anotations-proxy.
      ELSEIF <ls_cls_config>-info-override_controls-set_proxy IS NOT INITIAL.
        cs_info-proxy_enable = <ls_cls_config>-info-proxy_enable.

        DATA(lv_value) = COND #( WHEN cs_info-proxy_enable = abap_true
                                   THEN zif_annotations=>mc_annotation_values-true
                                   ELSE zif_annotations=>mc_annotation_values-false ).

        <lo_config>->replace_annotation(
          EXPORTING
            iv_name        = zif_annotations=>mc_anotations-proxy
            iv_value       = lv_value
          CHANGING
            ct_annotations = cs_info-annotations
        ).

      ENDIF.

*--------------------------------------------------------------------*
*     Зависимости
*--------------------------------------------------------------------*
      LOOP AT <ls_cls_config>-info-injected_dependencies ASSIGNING FIELD-SYMBOL(<ls_conf_dep>).

        READ TABLE cs_info-injected_dependencies ASSIGNING FIELD-SYMBOL(<ls_dep>)
          WITH KEY k1
          COMPONENTS attribute_name = <ls_conf_dep>-attribute_name
                     parameter_type = <ls_conf_dep>-parameter_type.
        IF sy-subrc <> 0.
          APPEND INITIAL LINE TO cs_info-injected_dependencies ASSIGNING <ls_dep>.
          <ls_dep>-parameter_name = <ls_conf_dep>-parameter_name.
          <ls_dep>-parameter_type = <ls_conf_dep>-parameter_type.
        ENDIF.

        IF <ls_conf_dep>-override_controls-set_attr_inject IS NOT INITIAL.
          <ls_dep>-has_inject     = <ls_conf_dep>-has_inject.
          <ls_dep>-parameter_name = <ls_conf_dep>-parameter_name.

          <lo_config>->replace_annotation(
            EXPORTING
              iv_name        = zif_annotations=>mc_anotations-inject
              iv_value       = <ls_conf_dep>-parameter_name
            CHANGING
              ct_annotations = <ls_dep>-annotations
          ).
        ENDIF.

        IF <ls_conf_dep>-override_controls-set_attr_qualifier IS NOT INITIAL.
          <ls_dep>-qualifier = <ls_conf_dep>-qualifier.

          <lo_config>->replace_annotation(
             EXPORTING
               iv_name        = zif_annotations=>mc_anotations-reference_qualifier
               iv_value       = <ls_dep>-qualifier
             CHANGING
               ct_annotations = <ls_dep>-annotations
            ).
        ENDIF.

      ENDLOOP.

    ENDLOOP.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_DI_SCANNER->SCAN_PACKAGE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_COMPONENT_ONLY              TYPE        ABAP_BOOL (default ='X')
* | [<-()] RT_CLASSES                     TYPE        MTY_T_CLASS_INFO
* | [!CX!] ZCX_DI_ERROR
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD scan_package.

    DATA:
      lt_packages_key    TYPE STANDARD TABLE OF devclass WITH EMPTY KEY,
      lt_packages        TYPE STANDARD TABLE OF tdevc WITH EMPTY KEY,
      lt_packages_parent LIKE lt_packages.

    CLEAR:
      mt_scan_result.

    IF ms_configuration-scan_packages IS INITIAL.
      RETURN.
    ENDIF.

    LOOP AT ms_configuration-scan_packages ASSIGNING FIELD-SYMBOL(<lv_scan_pack>).
      APPEND <lv_scan_pack> TO lt_packages_key.
    ENDLOOP.

    "Список пакетов
    SELECT devclass
      FROM tdevc
      FOR ALL ENTRIES IN @lt_packages_key
      WHERE devclass = @lt_packages_key-table_line
      INTO TABLE @lt_packages.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    "Список классов
    DATA(lt_class) = VALUE poc_class_tab( ).
    SELECT seoclass~clsname
      FROM seoclass
      JOIN tadir
        ON tadir~obj_name = seoclass~clsname
      FOR ALL ENTRIES IN @lt_packages
      WHERE tadir~devclass = @lt_packages-devclass
      INTO TABLE @lt_class.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    "Поиск классов конфигураторов
    ms_configuration-t_class_configs = search_and_call_config_classes( lt_class ).

    LOOP AT lt_class ASSIGNING FIELD-SYMBOL(<lv_class>).
      TRY.
          DATA(ls_class_info) = scan_single_class( <lv_class> ).
          IF ls_class_info IS NOT INITIAL.
            APPEND ls_class_info TO rt_classes.
          ENDIF.
        CATCH zcx_di_error.
      ENDTRY.
    ENDLOOP.

    IF iv_component_only IS NOT INITIAL.
      "Оставляем только компоненты
      DELETE rt_classes WHERE has_component IS INITIAL.
    ENDIF.

    "Проверки
    checks_after_scan( CHANGING ct_class = rt_classes ).

    "Отдаем результат
    mt_scan_result = rt_classes.

    "Конфиг больше не нужен, прощаемся с ним
    FREE ms_configuration-t_class_configs.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_DI_SCANNER->SCAN_SINGLE_CLASS
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_CLASS_NAME                  TYPE        SEOCLSNAME
* | [<-()] RS_CLASS_INFO                  TYPE        MTY_S_CLASS_INFO
* | [!CX!] ZCX_DI_ERROR
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD scan_single_class.

    TRY.
        DATA(lo_class) = cl_oo_class=>get_instance( iv_class_name ).
      CATCH cx_class_not_existent.
    ENDTRY.

    rs_class_info-class_name = iv_class_name.

    "code
    DATA(lt_code) = get_class_scr_code( iv_class_name ).

    LOOP AT lt_code TRANSPORTING NO FIELDS
      WHERE table_line CP to_upper( 'endclass*' ).
      DATA(lv_from) = sy-tabix.
      EXIT.
    ENDLOOP.
    IF sy-subrc = 0.
      DELETE lt_code FROM lv_from + 1.
    ENDIF.

    "annotations
    rs_class_info-annotations = zcl_di_annotation_processor=>parse_annotations( lt_code ).

    "dependencies
    rs_class_info-injected_dependencies = find_dependencies( it_scr_code = lt_code
                                                             io_class    = lo_class ).

    DATA(lt_all_comp_type) = zcl_di_annotation_processor=>get_all_component_types( ).

    LOOP AT rs_class_info-annotations ASSIGNING FIELD-SYMBOL(<ls_annotation>).

      IF line_exists( lt_all_comp_type[ table_line = <ls_annotation>-name ] ).
        rs_class_info-has_component  = abap_true.
        rs_class_info-component_type = <ls_annotation>-name.
      ENDIF.

      IF rs_class_info-component_type = zif_annotations=>mc_component-configuration.
        "Мы такие не инстанцируем, они отработали и больше не нужны
        CLEAR rs_class_info-has_component.
      ENDIF.

      CASE <ls_annotation>-name.
        WHEN zif_annotations=>mc_anotations-scope.

          rs_class_info-scope = <ls_annotation>-value.
        WHEN zif_annotations=>mc_anotations-qualifier.

          rs_class_info-qualifier = <ls_annotation>-value.
        WHEN zif_annotations=>mc_anotations-proxy.

          rs_class_info-proxy_enable = xsdbool( <ls_annotation>-value =
                                                  zif_annotations=>mc_annotation_values-true ).
      ENDCASE.

    ENDLOOP.

    IF rs_class_info-scope IS INITIAL.
      rs_class_info-scope = zif_di_container=>mc_default_scope.
    ENDIF.

    "Если есть настройка, то она переопределяет
    override_settings_by_config( CHANGING cs_info = rs_class_info ).

    "остались зависимости без @Inject ? Сохраним их
    LOOP AT rs_class_info-injected_dependencies ASSIGNING FIELD-SYMBOL(<ls_dep>)
      USING KEY k2
      WHERE has_inject IS INITIAL.

      APPEND VALUE #( class_name = <ls_dep>-parameter_type )
        TO rs_class_info-related_dependencies.
    ENDLOOP.

    "Отдаем только @Inject
    DELETE rs_class_info-injected_dependencies
      USING KEY k2
      WHERE has_inject IS INITIAL.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_DI_SCANNER->SEARCH_AND_CALL_CONFIG_CLASSES
* +-------------------------------------------------------------------------------------------------+
* | [--->] IT_CLASSES                     TYPE        POC_CLASS_TAB
* | [<-()] RT_CONFIGS                     TYPE        MTY_T_CONFIGS
* | [!CX!] ZCX_DI_ERROR
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD search_and_call_config_classes.

    CONSTANTS:
      lc_intf_enhance      TYPE string VALUE `ZIF_DI_APP_CONFIG_ENHANCER`,
      lc_intf_enhance_meth TYPE string VALUE `CONFIGURATE`,
      lc_base_conf_class   TYPE string VALUE `ZCL_DI_APPLICATION_CONFIG`.

    DATA:
      lo_config TYPE REF TO zif_di_app_config.

    LOOP AT it_classes ASSIGNING FIELD-SYMBOL(<lv_class>).
      TRY.
          DATA(lo_class) = CAST cl_oo_class( cl_oo_object=>get_instance( <lv_class> ) ).
          DATA(lt_interfaces) = lo_class->get_implemented_interfaces( ).
        CATCH cx_root.
          CONTINUE.
      ENDTRY.

      DELETE lt_interfaces WHERE refclsname <> lc_intf_enhance.
      CHECK lt_interfaces IS NOT INITIAL.

      TRY.
          CREATE OBJECT lo_config TYPE (lc_base_conf_class).
        CATCH cx_root.
          CONTINUE.
      ENDTRY.

      DATA(lv_meth) = |{ lc_intf_enhance }~{ lc_intf_enhance_meth }|.
      CALL METHOD (<lv_class>)=>(lv_meth)
        EXPORTING
          io_app_config = lo_config.

      APPEND INITIAL LINE TO rt_configs ASSIGNING FIELD-SYMBOL(<lo_config>).
      <lo_config> ?= lo_config.
    ENDLOOP.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_DI_SCANNER->SET_SCAN_PACKAGES
* +-------------------------------------------------------------------------------------------------+
* | [--->] IT_PACKAGE                     TYPE        STRING_TABLE
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD set_scan_packages.
    ms_configuration-scan_packages = it_package.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_DI_SCANNER->SET_SCAN_RESULT
* +-------------------------------------------------------------------------------------------------+
* | [--->] IT_SCAN_RESULT                 TYPE        MTY_T_CLASS_INFO
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD set_scan_result.
    FREE:
      mt_scan_result.

    mt_scan_result = it_scan_result.
  ENDMETHOD.
ENDCLASS.