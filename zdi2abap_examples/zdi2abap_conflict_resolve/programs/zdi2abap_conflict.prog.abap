*&---------------------------------------------------------------------*
*& Report ZDI2ABAP_CONFLICT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zdi2abap_conflict.

"Решение конфликта зависимостей через указание квалификатора RefQualifier

START-OF-SELECTION.
  DATA:
    lo_app TYPE REF TO zcl_di_application_conflict.

  TRY.
      DATA(lo_container) = zcl_di2abap=>get_instance( )->create_container(
                                                          it_package = VALUE #( ( `ZDI2ABAP_CONFLICT_RESOLVE` ) )
                                                         ).
      lo_container->resolve( IMPORTING eo_instance  = lo_app ).

      lo_app->run( ).
    CATCH zcx_di_error INTO DATA(lx_error).
      lx_error->display_exceptions( ).
  ENDTRY.