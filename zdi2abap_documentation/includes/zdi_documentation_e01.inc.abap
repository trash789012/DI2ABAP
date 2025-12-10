*&---------------------------------------------------------------------*
*&  Include           ZDI_DOCUMENTATION_E01
*&---------------------------------------------------------------------*

START-OF-SELECTION.
  TRY.
      DATA(lo_container) = zcl_di2abap=>get_instance( )->create_container(
                                                           VALUE #( ( lc_devclass ) )
                                                         ).
      lo_container->resolve( IMPORTING eo_instance = go_application ).

      go_application->run( s_pkg[] ).
    CATCH zcx_di_error INTO DATA(lx_error).
      lx_error->display_exceptions( ).
  ENDTRY.