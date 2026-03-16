*&---------------------------------------------------------------------*
*& Report ZDI2ABAP_COMPOSITE_OBJECTS
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zdi2abap_composite_objects.

"Отчет демонстрирует, как вынести инстанцирование объекта на уровень ABAP.
"Основные метаданные задаются в классе конфигураторе,
"Все статические методы класса конфигуратора, которые возвращают объект,
"будут использоваться для получения компонента.

"Текущий пример демонстрирует конфигурирование алв
"В целом подход применим для любых сложных объектов.


START-OF-SELECTION.
  DATA:
    lo_composite_component TYPE REF TO zcl_mdg_view_base.

  TRY.
      "Помимо основного пакета еще докинем пакет, из которого подключим класс zcl_mdg_view_base
      DATA(lo_container) = zcl_di2abap=>get_instance( )->create_container(
                                                          it_package = VALUE #( ( `ZDI2ABAP_COMPONENT_CONFIG` )
                                                                                ( `ZMDG_46` ) )
                                                         ).

      "Объект ALV будет создаваться через статический метод zcl_di_app_component_config=>build_alv.
      "Если на вход будут затребованы параметры с типом Объект, фрэймворк пройдется по зависимостям
      "и предоставит на вход необходимые компоненты (и их зависимости)
      lo_container->resolve( IMPORTING eo_instance  = lo_composite_component ).

      lo_composite_component->dispose( ).
    CATCH zcx_di_error INTO DATA(lx_error).
      lx_error->display_exceptions( ).
  ENDTRY.