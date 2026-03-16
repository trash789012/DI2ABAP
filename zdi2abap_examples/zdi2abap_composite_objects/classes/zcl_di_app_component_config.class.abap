"@Configuration
CLASS zcl_di_app_component_config DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_di_app_config_enhancer .

    CLASS-METHODS build_alv
      IMPORTING
        !io_stvarv    TYPE REF TO zcl_di_component_stvarv
        !io_service   TYPE REF TO zcl_di_component_service
      RETURNING
        VALUE(ro_alv) TYPE REF TO zcl_mdg_view_base .
    CLASS-METHODS build_http_util
      IMPORTING
        !io_container  TYPE REF TO zif_di_container
      RETURNING
        VALUE(ro_util) TYPE REF TO zcl_di_component_http_util .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_DI_APP_COMPONENT_CONFIG IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_DI_APP_COMPONENT_CONFIG=>BUILD_ALV
* +-------------------------------------------------------------------------------------------------+
* | [--->] IO_STVARV                      TYPE REF TO ZCL_DI_COMPONENT_STVARV
* | [--->] IO_SERVICE                     TYPE REF TO ZCL_DI_COMPONENT_SERVICE
* | [<-()] RO_ALV                         TYPE REF TO ZCL_MDG_VIEW_BASE
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD build_alv.

    "!!!!! --- Метод не должен содержать бизнес логику, только инициализацию и конфигурацию !!!!!

    "Здесь мы сконфигурируем ALV и отдадим фрэймворку.
    "Все входящие параметры - это компоненты, которые фрэймворк должен будет создать и передать в
    "метод BUILD_ALV автоматически.
    IF io_stvarv  IS NOT BOUND OR
       io_service IS NOT BOUND.
      RETURN.
    ENDIF.

    "Конфирурируем алв на основании существующих параметров
    ro_alv = NEW #( ).

    ro_alv->set_ui_properties( VALUE #(
      type_line_name = io_stvarv->read_type_line_name( )
      container_name = io_stvarv->read_container_name( )
      force_refresh  = abap_true
    ) ).

    ro_alv->bind_data( io_service->get_data_reference( ) ).

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_DI_APP_COMPONENT_CONFIG=>BUILD_HTTP_UTIL
* +-------------------------------------------------------------------------------------------------+
* | [--->] IO_CONTAINER                   TYPE REF TO ZIF_DI_CONTAINER
* | [<-()] RO_UTIL                        TYPE REF TO ZCL_DI_COMPONENT_HTTP_UTIL
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD build_http_util.
    "Еще один из вариантов, это затребовать на вход метода zif_di_container, и тогда из него можно резолвить
    "любой существующий компонент.
    "Но нужно быть очень аккуратным, т.к. компонент может еще и не существовать,
    "поэтому рекоммендуется использовать типизированные входящие параметры вместо контейнера
    ro_util = NEW #( ).
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_DI_APP_COMPONENT_CONFIG=>ZIF_DI_APP_CONFIG_ENHANCER~CONFIGURATE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IO_APP_CONFIG                  TYPE REF TO ZIF_DI_APP_CONFIG
* | [!CX!] ZCX_DI_ERROR
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD zif_di_app_config_enhancer~configurate.
    "Даем понять фрэймворку, что класс ZCL_DI_APP_COMPONENT_CONFIG содержит методы,
    "которые порождают компоненты
    "Такие методы используются для создания сложных объектов, например, через билдеры
    io_app_config->set_composite_objects_enable( abap_true ).

    "Так как мы добавляем внешний объект (создаем через метод), нужно аннотировать класс,
    "хотя бы базово (указать тип компонента и скоуп)
    "плюс указать, что компонент создается через метод (BUILD_ALV) через set_composite_object
    io_app_config->get_class( 'ZCL_MDG_VIEW_BASE'
                )->set_component_type( zif_annotations=>mc_component-component
                )->set_scope( zif_di_container=>mc_default_scope
                )->set_composite_object( abap_true ).

	"если уже есть аннотация @Component, достаточно указать, что компонент создается через метод (build_http_util)
    io_app_config->get_class( 'ZCL_DI_COMPONENT_HTTP_UTIL'
                )->set_composite_object( abap_true ).

  ENDMETHOD.
ENDCLASS.