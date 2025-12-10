CLASS zcl_aop_base DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      BEGIN OF mty_s_proxy,
        class_name TYPE string,
        original   TYPE REF TO object,
        proxy      TYPE REF TO object,
      END OF mty_s_proxy .
    TYPES:
      mty_t_proxy TYPE STANDARD TABLE OF mty_s_proxy WITH KEY class_name
                          WITH NON-UNIQUE SORTED KEY k1 COMPONENTS class_name .

    CLASS-METHODS generate_proxy
      IMPORTING
        !it_class       TYPE mty_t_proxy
      RETURNING
        VALUE(rt_proxy) TYPE mty_t_proxy
      RAISING
        zcx_di_error .
    CLASS-METHODS lookup
      IMPORTING
        !iv_name        TYPE string
      RETURNING
        VALUE(rs_proxy) TYPE mty_s_proxy
      RAISING
        zcx_di_error .
    CLASS-METHODS set_original
      IMPORTING
        !iv_classname   TYPE string
        !iv_clone       TYPE abap_bool OPTIONAL
        !io_interceptor TYPE REF TO object OPTIONAL
        !io_original    TYPE REF TO object
      RETURNING
        VALUE(ro_proxy) TYPE REF TO object
      RAISING
        zcx_di_error .
  PROTECTED SECTION.
  PRIVATE SECTION.

    CLASS-DATA mt_proxy TYPE mty_t_proxy .
ENDCLASS.



CLASS ZCL_AOP_BASE IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_AOP_BASE=>GENERATE_PROXY
* +-------------------------------------------------------------------------------------------------+
* | [--->] IT_CLASS                       TYPE        MTY_T_PROXY
* | [<-()] RT_PROXY                       TYPE        MTY_T_PROXY
* | [!CX!] ZCX_DI_ERROR
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD generate_proxy.

    TYPES:
      BEGIN OF lty_s_map,
        classname_orig TYPE string,
        classname      TYPE string,
        orig           TYPE REF TO object,
      END OF lty_s_map.

    DATA:
      lt_src       TYPE STANDARD TABLE OF string,
      lo_obj       TYPE REF TO object,
      lt_objcl_map TYPE STANDARD TABLE OF lty_s_map WITH KEY classname,
      lo_proxy     TYPE REF TO object.

    FREE:
      mt_proxy.

    APPEND |PROGRAM subpool.                       | TO lt_src.

    LOOP AT it_class ASSIGNING FIELD-SYMBOL(<ls_class>).
*--------------------------------------------------------------------*
*     Get info
*--------------------------------------------------------------------*
      TRY.
          DATA(lo_class) = CAST cl_oo_class( cl_oo_object=>get_instance( CONV #( <ls_class>-class_name ) ) ).
          DATA(lt_interfaces) = lo_class->get_implemented_interfaces( ).
        CATCH cx_class_not_existent.
      ENDTRY.

      DATA(lv_classname) = |lcl_{ to_lower( <ls_class>-class_name ) }|.
      IF strlen( lv_classname ) >= 30.
        lv_classname = to_lower( <ls_class>-class_name ).
      ENDIF.
*--------------------------------------------------------------------*
*     Map
*--------------------------------------------------------------------*
      APPEND VALUE #( classname_orig = <ls_class>-class_name
                      classname      = to_upper( lv_classname )
                      orig           = <ls_class>-original ) TO lt_objcl_map.

*--------------------------------------------------------------------*
*     Generate
*--------------------------------------------------------------------*
      APPEND |CLASS { lv_classname } DEFINITION INHERITING FROM cl_os_state. | TO lt_src.
      APPEND |                                                               | TO lt_src.
      APPEND | PUBLIC SECTION.                                               | TO lt_src.
      APPEND |   ALIASES: clone FOR if_os_clone~clone.                       | TO lt_src.
      APPEND |                                             | TO lt_src.
      LOOP AT lt_interfaces ASSIGNING FIELD-SYMBOL(<lv_if>).
        APPEND |INTERFACES { <lv_if>-refclsname }.| TO lt_src.
      ENDLOOP.
      APPEND |                                             | TO lt_src.
      APPEND |INTERFACES zif_di_proxy.| TO lt_src.
      APPEND |                                             | TO lt_src.
      APPEND | PRIVATE SECTION.                            | TO lt_src.
      APPEND |   DATA mo_original    TYPE REF TO object.   | TO lt_src.
      APPEND |   DATA mo_interceptor TYPE REF TO object.   | TO lt_src.
      APPEND |ENDCLASS.                                    | TO lt_src.
      APPEND |                                             | TO lt_src.
      APPEND |CLASS { lv_classname } IMPLEMENTATION.       | TO lt_src.
      APPEND |                                             | TO lt_src.

      LOOP AT lt_interfaces ASSIGNING <lv_if>.
        TRY.
            DATA(lo_intf) = CAST cl_oo_interface( cl_oo_object=>get_instance( <lv_if>-refclsname ) ).
          CATCH cx_class_not_existent.
            CONTINUE.
        ENDTRY.

        LOOP AT lo_intf->get_methods( ) ASSIGNING FIELD-SYMBOL(<ls_mehod>).
          SELECT SINGLE mtdnewexc
            INTO @DATA(lv_classexceptions)
            FROM vseomethod
            WHERE clsname = @<ls_mehod>-clsname
              AND cmpname = @<ls_mehod>-cmpname.
          TRY.
              DATA(ls_sign) = lo_intf->get_component_signature( CONV #( <ls_mehod>-cmpname ) ).
            CATCH cx_component_not_existing.
              CONTINUE.
          ENDTRY.

          DATA(lv_method) = |{ <lv_if>-refclsname }~{ <ls_mehod>-cmpname }|.

          APPEND | METHOD { lv_method }.| TO lt_src.
          APPEND |   DATA(lt_parameters) = VALUE abap_parmbind_tab( ).  | TO lt_src.
          APPEND |   DATA(lt_exeptions)  = VALUE abap_excpbind_tab( ).  | TO lt_src.

          LOOP AT ls_sign-params ASSIGNING FIELD-SYMBOL(<ls_params>).
            CASE <ls_params>-pardecltyp.
              WHEN 0. "imp
                DATA(lv_kind) = cl_abap_objectdescr=>exporting.
              WHEN 1. "exp
                lv_kind = cl_abap_objectdescr=>importing.
              WHEN 2. "chg
                lv_kind = cl_abap_objectdescr=>changing.
              WHEN 3. "ret
                lv_kind = cl_abap_objectdescr=>receiving.
              WHEN OTHERS.
                CONTINUE.
            ENDCASE.

            APPEND |  INSERT VALUE #( name  = '{ <ls_params>-sconame }'          | TO lt_src.
            APPEND |                  kind  = '{ lv_kind }'                      | TO lt_src.
            APPEND |                  value = REF #( { <ls_params>-sconame } ) ) | TO lt_src.
            APPEND |    INTO TABLE lt_parameters. | TO lt_src.
          ENDLOOP.
          IF lv_classexceptions IS INITIAL.
            LOOP AT ls_sign-exceps ASSIGNING FIELD-SYMBOL(<ls_exception>).
              DATA(lv_index) = sy-tabix.
              APPEND |  INSERT VALUE #( name  = '{ <ls_exception>-sconame }'          | TO lt_src.
              APPEND |                  value = { lv_index } ) | TO lt_src.
              APPEND |    INTO TABLE lt_exeptions. | TO lt_src.
            ENDLOOP.
          ENDIF.

          APPEND |                                                      | TO lt_src.
          APPEND | IF mo_interceptor IS BOUND.                          | TO lt_src.
          APPEND |   CALL METHOD mo_interceptor->('CALL_BEFORE')        | TO lt_src.
          APPEND |     EXPORTING                                        | TO lt_src.
          APPEND |       IV_CLASS      = '{ <ls_class>-class_name }'    | TO lt_src.
          APPEND |       IV_METHOD     = '{ lv_method }'                | TO lt_src.
          APPEND |       IO_OBJECT     =  mo_original                   | TO lt_src.
          APPEND |       IT_PARAMETERS =  lt_parameters.                | TO lt_src.
          APPEND | ENDIF.                                               | TO lt_src.
          APPEND |                                                      | TO lt_src.

          IF lv_classexceptions IS NOT INITIAL.
            APPEND | TRY.                                                 | TO lt_src.
          ENDIF.

          APPEND |   CALL METHOD mo_original->('{ lv_method }')| TO lt_src.
          IF lv_classexceptions IS NOT INITIAL.
            APPEND |     PARAMETER-TABLE lt_parameters. | TO lt_src.
          ELSE.
            APPEND |     PARAMETER-TABLE lt_parameters  | TO lt_src.
            APPEND |     EXCEPTION-TABLE lt_exeptions.  | TO lt_src.
            APPEND |                                    | TO lt_src.

            IF ls_sign-exceps IS NOT INITIAL.
              APPEND |                                                      | TO lt_src.
              APPEND | IF sy-subrc <> 0 AND mo_interceptor IS BOUND.        | TO lt_src.
              APPEND |   CALL METHOD mo_interceptor->('CALL_IN_EXCEPTION')  | TO lt_src.
              APPEND |     EXPORTING                                        | TO lt_src.
              APPEND |       IV_CLASS      = '{ <ls_class>-class_name }'    | TO lt_src.
              APPEND |       IV_METHOD     = '{ lv_method }'                | TO lt_src.
              APPEND |       IO_OBJECT     =  mo_original                   | TO lt_src.
              APPEND |       IV_EXCEPTION  =  sy-subrc.                     | TO lt_src.
              APPEND | ENDIF.                                               | TO lt_src.
              APPEND |                                                      | TO lt_src.
            ENDIF.

            LOOP AT ls_sign-exceps ASSIGNING <ls_exception>.
              lv_index = sy-tabix.

              APPEND | CASE sy-subrc.                   | TO lt_src.
              APPEND |   WHEN { lv_index }.                          | TO lt_src.
              APPEND |     RAISE { <ls_exception>-sconame }. | TO lt_src.
              APPEND | ENDCASE.                         | TO lt_src.
            ENDLOOP.
          ENDIF.

          IF lv_classexceptions IS NOT INITIAL.
            LOOP AT ls_sign-exceps ASSIGNING <ls_exception>.
              lv_index = sy-tabix.

              DATA(lv_ex_name) = |lx_error_{ lv_index }|.
              APPEND | CATCH { <ls_exception>-sconame } INTO DATA({ lv_ex_name }). | TO lt_src.
              APPEND |                                                      | TO lt_src.
              APPEND | IF mo_interceptor IS BOUND.                          | TO lt_src.
              APPEND |   CALL METHOD mo_interceptor->('CALL_IN_EXCEPTION')  | TO lt_src.
              APPEND |     EXPORTING                                        | TO lt_src.
              APPEND |       IV_CLASS      = '{ <ls_class>-class_name }'    | TO lt_src.
              APPEND |       IV_METHOD     = '{ lv_method }'                | TO lt_src.
              APPEND |       IO_OBJECT     =  mo_original                   | TO lt_src.
              APPEND |       IO_EXCEPTION  =  { lv_ex_name }.               | TO lt_src.
              APPEND | ENDIF.                                               | TO lt_src.
              APPEND |                                                      | TO lt_src.
              APPEND |   RAISE EXCEPTION { lv_ex_name }.                    | TO lt_src.
              APPEND | ENDTRY.                                              | TO lt_src.
            ENDLOOP.
          ENDIF.


          APPEND |                                                      | TO lt_src.
          APPEND | IF mo_interceptor IS BOUND.                          | TO lt_src.
          APPEND |   CALL METHOD mo_interceptor->('CALL_AFTER')         | TO lt_src.
          APPEND |     EXPORTING                                        | TO lt_src.
          APPEND |       IV_CLASS      = '{ <ls_class>-class_name }'    | TO lt_src.
          APPEND |       IV_METHOD     = '{ lv_method }'                | TO lt_src.
          APPEND |       IO_OBJECT     =  mo_original                   | TO lt_src.
          APPEND |       IT_PARAMETERS =  lt_parameters.                | TO lt_src.
          APPEND | ENDIF.                                               | TO lt_src.
          APPEND |                                                      | TO lt_src.

          APPEND | ENDMETHOD.                                           | TO lt_src.
          APPEND |                                                      | TO lt_src.
        ENDLOOP.
      ENDLOOP.

      APPEND | METHOD zif_di_proxy~set_original.       | TO lt_src.
      APPEND |   mo_original ?= io_instance.           | TO lt_src.
      APPEND | ENDMETHOD.                              | TO lt_src.
      APPEND |                                         | TO lt_src.
      APPEND | METHOD zif_di_proxy~set_interceptor.    | TO lt_src.
      APPEND |   mo_interceptor ?= io_interceptor.     | TO lt_src.
      APPEND | ENDMETHOD.                              | TO lt_src.
      APPEND |ENDCLASS.                                | TO lt_src.
      APPEND |                                         | TO lt_src.
    ENDLOOP.

*--------------------------------------------------------------------*
*   Create transient pool
*--------------------------------------------------------------------*
    GENERATE SUBROUTINE POOL lt_src
      NAME         DATA(lv_prog)
      MESSAGE      DATA(lv_mess)
      SHORTDUMP-ID DATA(lv_sid).
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_di_error.
    ENDIF.
*--------------------------------------------------------------------*
*   Return
*--------------------------------------------------------------------*
    LOOP AT lt_objcl_map ASSIGNING FIELD-SYMBOL(<ls_map>).
      FREE lo_proxy.

      DATA(lv_class) = |\\PROGRAM={ lv_prog }\\CLASS={ <ls_map>-classname }|.
      TRY.
          CREATE OBJECT lo_proxy TYPE (lv_class).

          CALL METHOD lo_proxy->('ZIF_DI_PROXY~SET_ORIGINAL')
            EXPORTING
              io_instance = <ls_map>-orig.
        CATCH cx_root.
      ENDTRY.

      APPEND VALUE #( class_name = <ls_map>-classname_orig
                      original   = <ls_map>-orig
                      proxy      = lo_proxy ) TO rt_proxy.
    ENDLOOP.

    mt_proxy = rt_proxy.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_AOP_BASE=>LOOKUP
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_NAME                        TYPE        STRING
* | [<-()] RS_PROXY                       TYPE        MTY_S_PROXY
* | [!CX!] ZCX_DI_ERROR
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD lookup.

    READ TABLE mt_proxy ASSIGNING FIELD-SYMBOL(<ls_proxy>)
      WITH KEY k1 COMPONENTS class_name = iv_name.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_di_error.
    ENDIF.

    rs_proxy = <ls_proxy>.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_AOP_BASE=>SET_ORIGINAL
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_CLASSNAME                   TYPE        STRING
* | [--->] IV_CLONE                       TYPE        ABAP_BOOL(optional)
* | [--->] IO_INTERCEPTOR                 TYPE REF TO OBJECT(optional)
* | [--->] IO_ORIGINAL                    TYPE REF TO OBJECT
* | [<-()] RO_PROXY                       TYPE REF TO OBJECT
* | [!CX!] ZCX_DI_ERROR
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD set_original.

    DATA:
      lo_proxy TYPE REF TO zif_di_proxy,
      lo_clone TYPE REF TO object.

    DATA(ls_proxy) = lookup( iv_classname ).
    CHECK ls_proxy-class_name IS NOT INITIAL.

    IF ls_proxy-original IS BOUND.
      FREE ls_proxy-original.
    ENDIF.

    IF iv_clone IS NOT INITIAL.
      CALL METHOD ls_proxy-proxy->('CLONE')
        RECEIVING
          result = lo_clone.

      lo_proxy ?= lo_clone.
    ELSE.
      lo_proxy ?= ls_proxy-proxy.
    ENDIF.

    IF lo_proxy IS BOUND.
      lo_proxy->set_original( io_original ).

      IF io_interceptor IS BOUND.
        lo_proxy->set_interceptor( CAST #( io_interceptor ) ).
      ENDIF.
      ro_proxy ?= lo_proxy.
    ENDIF.

  ENDMETHOD.
ENDCLASS.