"@Core
CLASS zcl_di2abap DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.

    INTERFACES zif_di_container .

    ALIASES create_container
      FOR zif_di_container~create_container .
    ALIASES resolve
      FOR zif_di_container~resolve .

    CLASS-METHODS get_instance
      RETURNING
        VALUE(ro_instance) TYPE REF TO zcl_di2abap .
    METHODS dispose .
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA mo_scanner TYPE REF TO zcl_di_scanner .
    DATA mo_dependency_resolver TYPE REF TO zcl_di_dependensy_resolver .
    DATA mo_di_container TYPE REF TO zcl_di_container .
    CLASS-DATA mo_instance TYPE REF TO zcl_di2abap .

    METHODS prepare_depends_for_create
      IMPORTING
        !iv_force_resolve  TYPE abap_bool OPTIONAL
        !iv_target_class   TYPE seoclass-clsname OPTIONAL
        !iv_qualifier      TYPE string OPTIONAL
      RETURNING
        VALUE(ro_instance) TYPE REF TO object
      RAISING
        zcx_di_error .
ENDCLASS.



CLASS ZCL_DI2ABAP IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_DI2ABAP->DISPOSE
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD dispose.

    IF mo_scanner IS BOUND.
      mo_scanner->dispose( ).
    ENDIF.

    IF mo_dependency_resolver IS BOUND.
      mo_dependency_resolver->dispose( ).
    ENDIF.

    IF mo_di_container IS BOUND.
      mo_di_container->dispose( ).
    ENDIF.

    FREE:
      mo_scanner,
      mo_dependency_resolver,
      mo_di_container.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_DI2ABAP=>GET_INSTANCE
* +-------------------------------------------------------------------------------------------------+
* | [<-()] RO_INSTANCE                    TYPE REF TO ZCL_DI2ABAP
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_instance.

    IF mo_instance IS NOT BOUND.
      mo_instance = NEW #( ).
    ENDIF.

    ro_instance = mo_instance.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_DI2ABAP->PREPARE_DEPENDS_FOR_CREATE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_FORCE_RESOLVE               TYPE        ABAP_BOOL(optional)
* | [--->] IV_TARGET_CLASS                TYPE        SEOCLASS-CLSNAME(optional)
* | [--->] IV_QUALIFIER                   TYPE        STRING(optional)
* | [<-()] RO_INSTANCE                    TYPE REF TO OBJECT
* | [!CX!] ZCX_DI_ERROR
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD prepare_depends_for_create.

    DATA:
      lt_objlist TYPE TABLE OF REF TO object.

    DATA(lt_class_info) = mo_scanner->get_scan_result( ).
    DATA(lt_class_ordered) = mo_dependency_resolver->get_resolve_dependencies( ).

    SORT lt_class_info BY class_name.

    LOOP AT lt_class_ordered ASSIGNING FIELD-SYMBOL(<lv_class>).

      READ TABLE lt_class_info ASSIGNING FIELD-SYMBOL(<ls_info>)
        WITH KEY class_name = <lv_class>
        BINARY SEARCH.
      CHECK sy-subrc = 0.

      "Если есть зависимости, мы их должны прорезолвить и передать
      DATA(lt_parameters) = VALUE abap_parmbind_tab( ).

      IF iv_force_resolve = abap_true.
        LOOP AT <ls_info>-injected_dependencies ASSIGNING FIELD-SYMBOL(<ls_dep>)
          WHERE has_inject IS NOT INITIAL.

          TRY.
              DATA(lo_depend_object) = mo_di_container->resolve(
                                         iv_abstract  = CONV #( <ls_dep>-parameter_type )
                                         iv_qualifier = <ls_dep>-qualifier
                                       ).

              INSERT lo_depend_object INTO lt_objlist INDEX 1.

              INSERT VALUE #( name  = <ls_dep>-parameter_name
                              kind  = cl_abap_objectdescr=>exporting
                              value = REF #( lt_objlist[ 1 ] ) ) INTO TABLE lt_parameters.
            CATCH zcx_di_error.
          ENDTRY.
        ENDLOOP.
      ENDIF.

      "Регистрируем текущий класс
      DATA(lv_abstract) = COND seoclsname( WHEN <ls_info>-absolute_type IS NOT INITIAL
                                             THEN <ls_info>-absolute_type
                                             ELSE <ls_info>-class_name ).
      mo_di_container->register(
        iv_qualifier      = <ls_info>-qualifier
        iv_abstract       = lv_abstract
        iv_concrete       = <ls_info>-class_name
        it_parameters     = lt_parameters
        iv_is_proxy       = <ls_info>-proxy_enable
        iv_component_type = <ls_info>-component_type
        iv_singleton      = xsdbool( <ls_info>-scope = zif_annotations=>mc_scope-singleton )
      ).

      IF iv_force_resolve = abap_true.

        DATA(lv_qmatch) = xsdbool( iv_qualifier IS SUPPLIED AND <ls_info>-qualifier = iv_qualifier
                                   OR iv_qualifier IS NOT SUPPLIED ).

        IF <ls_info>-scope = zif_annotations=>mc_scope-singleton OR
           ( ( <lv_class> = iv_target_class OR <ls_info>-absolute_type = iv_target_class ) AND
               lv_qmatch = abap_true
            ).

          "И резолвим его, чтобы был в памяти
          TRY.
              ro_instance = mo_di_container->resolve( iv_abstract  = lv_abstract
                                                      iv_qualifier = <ls_info>-qualifier ).
            CATCH zcx_di_error.
          ENDTRY.
        ENDIF.
      ENDIF.

      IF iv_target_class IS NOT INITIAL AND
         iv_target_class = <lv_class>.
        EXIT.
      ENDIF.
    ENDLOOP.

    FREE:
      lt_objlist.

    "Очищаем параметры для создания
    mo_di_container->refresh_create_parameters( ).

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_DI2ABAP->ZIF_DI_CONTAINER~CREATE_CONTAINER
* +-------------------------------------------------------------------------------------------------+
* | [--->] IT_PACKAGE                     TYPE        STRING_TABLE
* | [<-()] RO_CONTAINER                   TYPE REF TO ZIF_DI_CONTAINER
* | [!CX!] ZCX_DI_ERROR
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD zif_di_container~create_container.
    "Зачищаем всё
    dispose( ).

    "Сканируем пакет
    IF mo_scanner IS NOT BOUND.
      mo_scanner = NEW #( ).
    ENDIF.

    mo_scanner->set_scan_packages( it_package ).

    DATA(lt_class_info) = mo_scanner->scan_package( ).

    "Решаем конфликт зависимостей
    IF mo_dependency_resolver IS NOT BOUND.
      mo_dependency_resolver = NEW #( ).
    ENDIF.

    mo_dependency_resolver->resolve_dependencies( CHANGING ct_class = lt_class_info ).

    "После решения конфликта проставляется тип компонента, подкинем данные
    mo_scanner->set_scan_result( lt_class_info ).

    "Готовим контейнер для resolv'a
    mo_di_container = NEW #( ).

    "Регистрируем классы для создания
    prepare_depends_for_create( ).

    "Регистрируем ядро
    mo_di_container->register_instance(
      EXPORTING
        iv_abstract = zif_di_container=>mc_absolute_name
        io_instance = me
    ).

    "Создаем прокси
    mo_di_container->prepare_proxy_objects( ).

    ro_container ?= me.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_DI2ABAP->ZIF_DI_CONTAINER~RESOLVE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_CLASSNAME                   TYPE        STRING(optional)
* | [--->] IV_QUALIFIER                   TYPE        STRING(optional)
* | [<---] EO_INSTANCE                    TYPE        ANY
* | [!CX!] ZCX_DI_ERROR
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD zif_di_container~resolve.
    FREE:
      eo_instance.

    IF iv_classname IS NOT SUPPLIED AND iv_classname IS INITIAL.
      DATA(lv_classname) = CAST cl_abap_refdescr(
                             cl_abap_typedescr=>describe_by_data( eo_instance )
                           )->get_referenced_type( )->absolute_name.

      SPLIT lv_classname AT '=' INTO DATA(lv_pref) DATA(lv_name).
      CONDENSE lv_name.
    ELSE.
      lv_name = iv_classname.
    ENDIF.

    "Готовим контекст для resolve, по сути проходим по всем зависимостям
    "и создаем их, и в конце создаем целевой класс
    prepare_depends_for_create(
      iv_force_resolve = abap_true "насильно создаем, именно он нам нужен
      iv_target_class  = CONV #( lv_name )
    ).

    "А теперь его резолвим
    DATA(lo_instance) = mo_di_container->resolve( iv_abstract  = CONV #( lv_name )
                                                  iv_qualifier = iv_qualifier
                                                  iv_no_create = abap_true ).
    IF iv_qualifier IS INITIAL AND lo_instance IS NOT BOUND.
      lo_instance = mo_di_container->resolve( iv_abstract  = CONV #( lv_name )
                                              iv_no_create = abap_true ).
    ENDIF.

    IF lo_instance IS NOT BOUND.
      RAISE EXCEPTION TYPE zcx_di_error.
    ENDIF.

    IF eo_instance IS REQUESTED.
      eo_instance ?= lo_instance.
    ENDIF.
  ENDMETHOD.
ENDCLASS.