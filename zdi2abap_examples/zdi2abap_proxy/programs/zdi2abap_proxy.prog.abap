*&---------------------------------------------------------------------*
*& Report ZDI2ABAP_PROXY
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zdi2abap_proxy.

"Демонстраци работы с прокси объектами
"Классы ZCL_DI_MODEL_DEMO и zcl_di_demo_stvarv аннотированы @Proxy( true ),
"Управление передается классу-interceptor'у, исключения так же ловит он

"ВАЖНО ВСЕГДА УКАЗЫВАТЬ ALIASES У INTERCEPTOR!!!

START-OF-SELECTION.

  DATA:
    lo_model  TYPE REF TO zif_di_model_demo,
    lo_stvarv TYPE REF TO zif_di_stvarv.

  TRY.
      DATA(lo_container) = zcl_di2abap=>get_instance( )->create_container(
                                                          it_package = VALUE #( ( `ZDI2ABAP_PROXY` ) )
                                                         ).
      "Класс zcl_di_demo_stvarv внедряется как зависимость через интерфейс zif_di_stvarv
      "в класс zif_di_model_demo,
      "Поэтому при резолве указывать точный тип не нужно
      lo_container->resolve( IMPORTING eo_instance  = lo_stvarv ).

      lo_stvarv->read( ).

      SKIP.

      "Касс кастуется до интерфейса, это нужно явно указать, т.к. класс нигде не затребован,
      "как зависимость через интерфейс
      lo_container->resolve( EXPORTING iv_classname = 'ZCL_DI_MODEL_DEMO'
                             IMPORTING eo_instance  = lo_model ).

      lo_model->do( ).
      SKIP.
      lo_model->select( ).
      SKIP.
      lo_model->do_except( ).
    CATCH zcx_di_error INTO DATA(lx_error).
      SKIP.
      WRITE : / lx_error->get_text( ).
  ENDTRY.