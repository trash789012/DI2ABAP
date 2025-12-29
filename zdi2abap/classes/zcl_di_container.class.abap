CLASS zcl_di_container DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS dispose .
    " Методы регистрации зависимостей
    METHODS register
      IMPORTING
        !iv_abstract       TYPE seoclsname
        !iv_concrete       TYPE seoclsname
        !iv_qualifier      TYPE string OPTIONAL
        !iv_component_type TYPE string OPTIONAL
        !iv_singleton      TYPE abap_bool DEFAULT abap_true
        !iv_is_proxy       TYPE abap_bool OPTIONAL
        !it_parameters     TYPE abap_parmbind_tab OPTIONAL .
    METHODS register_instance
      IMPORTING
        !iv_abstract TYPE seoclsname
        !io_instance TYPE REF TO object .
    " Методы получения зависимостей
    METHODS resolve
      IMPORTING
        !iv_abstract       TYPE seoclsname
        !iv_qualifier      TYPE string OPTIONAL
        !iv_component_type TYPE string OPTIONAL
        !iv_no_create      TYPE abap_bool OPTIONAL
      RETURNING
        VALUE(ro_instance) TYPE REF TO object
      RAISING
        zcx_di_error .
    " Проверка зарегистрированных зависимостей
    METHODS is_registered
      IMPORTING
        !iv_abstract         TYPE seoclsname
      RETURNING
        VALUE(rv_registered) TYPE abap_bool .
    METHODS refresh_create_parameters .
    METHODS prepare_proxy_objects
      RAISING
        zcx_di_error.
private section.

  types:
    BEGIN OF mty_s_binding,
        abstract        TYPE seoclsname,
        concrete        TYPE seoclsname,
        qualifier       TYPE string,
        singleton       TYPE abap_bool,
        is_proxy        TYPE abap_bool,
        component_type  TYPE string,
        instance        TYPE REF TO object,
        proxy           TYPE REF TO object,
        t_create_params TYPE abap_parmbind_tab,
      END OF mty_s_binding .
  types:
    mty_t_bindings TYPE HASHED TABLE OF mty_s_binding WITH UNIQUE KEY abstract concrete qualifier .

  data MT_BINDINGS type MTY_T_BINDINGS .

  methods CREATE_INSTANCE
    importing
      !IV_CLASSNAME type SEOCLSNAME
      !IT_PARAMETERS type ABAP_PARMBIND_TAB optional
    returning
      value(RO_INSTANCE) type ref to OBJECT
    raising
      ZCX_DI_ERROR .
ENDCLASS.



CLASS ZCL_DI_CONTAINER IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_DI_CONTAINER->CREATE_INSTANCE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_CLASSNAME                   TYPE        SEOCLSNAME
* | [--->] IT_PARAMETERS                  TYPE        ABAP_PARMBIND_TAB(optional)
* | [<-()] RO_INSTANCE                    TYPE REF TO OBJECT
* | [!CX!] ZCX_DI_ERROR
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD create_instance.
    DATA:
      lt_parameters TYPE abap_parmbind_tab.

    " Если переданы параметры - используем их, иначе пустой список
    lt_parameters = COND #( WHEN it_parameters IS SUPPLIED THEN it_parameters ELSE VALUE #( ) ).

    TRY.
        CREATE OBJECT ro_instance TYPE (iv_classname)
          PARAMETER-TABLE lt_parameters.
      CATCH cx_root INTO DATA(lx_exc).
        RAISE EXCEPTION TYPE zcx_di_error EXPORTING previous = lx_exc.
    ENDTRY.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_DI_CONTAINER->DISPOSE
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD dispose.

    LOOP AT mt_bindings ASSIGNING FIELD-SYMBOL(<ls_bind>).
      FREE <ls_bind>-proxy.
      FREE <ls_bind>-instance.
    ENDLOOP.

    FREE mt_bindings.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_DI_CONTAINER->IS_REGISTERED
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_ABSTRACT                    TYPE        SEOCLSNAME
* | [<-()] RV_REGISTERED                  TYPE        ABAP_BOOL
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD is_registered.
    rv_registered = xsdbool( line_exists( mt_bindings[ abstract = iv_abstract ] ) ).
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_DI_CONTAINER->PREPARE_PROXY_OBJECTS
* +-------------------------------------------------------------------------------------------------+
* | [!CX!] ZCX_DI_ERROR
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD prepare_proxy_objects.

    DATA(lt_proxify) = VALUE zcl_aop_base=>mty_t_proxy( ).

    LOOP AT mt_bindings ASSIGNING FIELD-SYMBOL(<ls_bind>)
      WHERE is_proxy IS NOT INITIAL.

      CHECK <ls_bind>-concrete IS NOT INITIAL.

      APPEND VALUE zcl_aop_base=>mty_s_proxy(
        class_name = <ls_bind>-concrete
      ) TO lt_proxify.
    ENDLOOP.

    IF lt_proxify IS INITIAL.
      RETURN.
    ENDIF.

    DELETE ADJACENT DUPLICATES FROM lt_proxify USING KEY k1.

    zcl_aop_base=>generate_proxy( lt_proxify ).

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_DI_CONTAINER->REFRESH_CREATE_PARAMETERS
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD refresh_create_parameters.

    LOOP AT mt_bindings ASSIGNING FIELD-SYMBOL(<ls_bind>).
      FREE <ls_bind>-t_create_params.
    ENDLOOP.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_DI_CONTAINER->REGISTER
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_ABSTRACT                    TYPE        SEOCLSNAME
* | [--->] IV_CONCRETE                    TYPE        SEOCLSNAME
* | [--->] IV_QUALIFIER                   TYPE        STRING(optional)
* | [--->] IV_COMPONENT_TYPE              TYPE        STRING(optional)
* | [--->] IV_SINGLETON                   TYPE        ABAP_BOOL (default =ABAP_TRUE)
* | [--->] IV_IS_PROXY                    TYPE        ABAP_BOOL(optional)
* | [--->] IT_PARAMETERS                  TYPE        ABAP_PARMBIND_TAB(optional)
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD register.
    READ TABLE mt_bindings ASSIGNING FIELD-SYMBOL(<ls_bind>)
      WITH KEY abstract        = iv_abstract
               concrete        = iv_concrete
               qualifier       = iv_qualifier
               singleton       = iv_singleton.
    IF sy-subrc = 0.
      <ls_bind>-t_create_params = it_parameters.
    ELSE.
      INSERT VALUE #( abstract        = iv_abstract
                      concrete        = iv_concrete
                      qualifier       = iv_qualifier
                      singleton       = iv_singleton
                      is_proxy        = iv_is_proxy
                      component_type  = iv_component_type
                      t_create_params = it_parameters ) INTO TABLE mt_bindings.
    ENDIF.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_DI_CONTAINER->REGISTER_INSTANCE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_ABSTRACT                    TYPE        SEOCLSNAME
* | [--->] IO_INSTANCE                    TYPE REF TO OBJECT
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD register_instance.
    INSERT VALUE #( abstract  = iv_abstract
                    instance  = io_instance
                    singleton = abap_true ) INTO TABLE mt_bindings.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_DI_CONTAINER->RESOLVE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_ABSTRACT                    TYPE        SEOCLSNAME
* | [--->] IV_QUALIFIER                   TYPE        STRING(optional)
* | [--->] IV_COMPONENT_TYPE              TYPE        STRING(optional)
* | [--->] IV_NO_CREATE                   TYPE        ABAP_BOOL(optional)
* | [<-()] RO_INSTANCE                    TYPE REF TO OBJECT
* | [!CX!] ZCX_DI_ERROR
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD resolve.

*--------------------------------------------------------------------*
*   READ
*--------------------------------------------------------------------*
    "by qualifier only
    IF iv_qualifier IS SUPPLIED.
      DATA(ls_binding) = VALUE #( mt_bindings[ abstract  = iv_abstract
                                               qualifier = iv_qualifier ] OPTIONAL ).
      IF ls_binding IS INITIAL AND iv_qualifier IS NOT INITIAL.
        ls_binding = VALUE #( mt_bindings[ qualifier  = iv_qualifier ] OPTIONAL ).
      ENDIF.
    ENDIF.

    "by component type + qualifier
    IF ls_binding IS INITIAL AND iv_component_type IS NOT INITIAL.
      IF iv_qualifier IS SUPPLIED.
        ls_binding = VALUE #( mt_bindings[ abstract       = iv_abstract
                                           qualifier      = iv_qualifier
                                           component_type = iv_component_type ] OPTIONAL ).
      ELSEIF iv_abstract IS NOT INITIAL.
        ls_binding = VALUE #( mt_bindings[ abstract       = iv_abstract
                                           component_type = iv_component_type ] OPTIONAL ).
      ELSE.
        ls_binding = VALUE #( mt_bindings[ component_type = iv_component_type ] OPTIONAL ).
      ENDIF.
    ENDIF.

    "by abstract if empty
    IF ls_binding IS INITIAL.
      ls_binding = VALUE #( mt_bindings[ abstract  = iv_abstract ] OPTIONAL ).
    ENDIF.

*--------------------------------------------------------------------*
*   Try return by cache
*--------------------------------------------------------------------*
    IF ( ls_binding-instance IS BOUND AND iv_no_create = abap_true ) OR
       ( ls_binding-singleton = abap_true AND ls_binding-instance IS BOUND ).

      IF ls_binding-is_proxy IS NOT INITIAL AND ls_binding-proxy IS BOUND.
        ro_instance = ls_binding-proxy.
      ELSE.
        ro_instance = ls_binding-instance.
      ENDIF.

      RETURN.
    ENDIF.

    IF iv_no_create = abap_true.
      RETURN.
    ENDIF.

*--------------------------------------------------------------------*
*   Intance
*--------------------------------------------------------------------*
    " Создание нового экземпляра
    ro_instance = create_instance( iv_classname  = ls_binding-concrete
                                   it_parameters = ls_binding-t_create_params ).

    ls_binding-instance = ro_instance.

*--------------------------------------------------------------------*
*   Proxy(optional)
*--------------------------------------------------------------------*
    IF ls_binding-is_proxy IS NOT INITIAL.
      TRY.
          TRY.
              DATA(lo_interceptor) = resolve(
                                       iv_abstract       = space
                                       iv_component_type = zif_annotations=>mc_component-interceptor
                                     ).
            CATCH zcx_di_error.
              "не важно, можно без него
          ENDTRY.

          ls_binding-proxy = zcl_aop_base=>set_original(
            iv_classname   = CONV #( ls_binding-concrete )
            io_interceptor = lo_interceptor
            iv_clone       = xsdbool( ls_binding-singleton IS INITIAL )
            io_original    = ro_instance
          ).

          IF ls_binding-proxy IS BOUND.
            ro_instance = ls_binding-proxy.
          ENDIF.
        CATCH zcx_di_error.
          "Вообще без разницы
      ENDTRY.
    ENDIF.

*--------------------------------------------------------------------*
*   Cache
*--------------------------------------------------------------------*
    "Кэширование (если не singletone, то просто последний инстанс лежит)
    MODIFY TABLE mt_bindings FROM ls_binding.
  ENDMETHOD.
ENDCLASS.