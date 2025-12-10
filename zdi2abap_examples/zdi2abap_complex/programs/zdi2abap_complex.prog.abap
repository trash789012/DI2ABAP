*&---------------------------------------------------------------------*
*& Report ZDI2ABAP_COMPLEX
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zdi2abap_complex.


"Комплексный пример
"- Класс логгер zcl_di_complex_logger не декларирован как @Component, но настраивается в zcl_di_app_compl_configuration
"  императивно. Он затребован в нескольких компонентах как интерфейс ZIF_DI_COMPL_LOGGER
"  Мы задаем ему Component и ставим скоуп Prototype

"- Класс zcl_di_complex_repository_two не имеет квалификатора, что грозит пересечением по Qualifier
"  В сервисе zcl_di_complex_service есть атрибут mo_repository_two, и есть параметр в конструкторе,
"  но нет декларативного описания. См. снова zcl_di_app_compl_configuration.

"- Включаем проксирование для класса ZCL_DI_COMPLEX_REPOSITORY_TWO в zcl_di_app_compl_configuration
" ВАЖНО ВСЕГДА УКАЗЫВАТЬ ALIASES У INTERCEPTOR!!!

START-OF-SELECTION.
  DATA:
    lo_app TYPE REF TO zcl_di_complex_application.

  TRY.
      DATA(lo_container) = zcl_di2abap=>get_instance( )->create_container(
                                                          it_package = VALUE #( ( `ZDI2ABAP_COMPLEX` ) )
                                                         ).
      lo_container->resolve( IMPORTING eo_instance  = lo_app ).

      lo_app->start( ).
    CATCH zcx_di_error INTO DATA(lx_error).
      lx_error->display_exceptions( ).
  ENDTRY.