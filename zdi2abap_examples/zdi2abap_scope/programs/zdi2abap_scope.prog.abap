*&---------------------------------------------------------------------*
*& Report ZDI2ABAP_SCOPE
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zdi2abap_scope.

START-OF-SELECTION.
  DATA:
    lo_app  TYPE REF TO zcl_di_application_scope,
    lo_repo TYPE REF TO zcl_di_repository_scope_demo.

  TRY.
      DATA(lo_container) = zcl_di2abap=>get_instance( )->create_container(
                                                          it_package = VALUE #( ( `ZDI2ABAP_SCOPE` ) )
                                                         ).
      lo_container->resolve( IMPORTING eo_instance = lo_app ).
      lo_container->resolve( IMPORTING eo_instance = lo_repo ).

      ASSERT lo_app->mo_logger_slg = lo_repo->mo_logger_slg.

      "Класс zcl_di_logger_to_xml имеет @Scope( prototype ),
      "должен инстанцироваться каждый раз заново
      ASSERT lo_app->mo_logger     <> lo_repo->mo_logger.
    CATCH zcx_di_error INTO DATA(lx_error).
      lx_error->display_exceptions( ).
  ENDTRY.