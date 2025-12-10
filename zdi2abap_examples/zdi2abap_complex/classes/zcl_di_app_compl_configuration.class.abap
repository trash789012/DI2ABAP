"@Configuration
CLASS zcl_di_app_compl_configuration DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_di_app_config_enhancer .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_DI_APP_COMPL_CONFIGURATION IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_DI_APP_COMPL_CONFIGURATION=>ZIF_DI_APP_CONFIG_ENHANCER~CONFIGURATE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IO_APP_CONFIG                  TYPE REF TO ZIF_DI_APP_CONFIG
* | [!CX!] ZCX_DI_ERROR
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD zif_di_app_config_enhancer~configurate.

    "accept proxy in application layer ?
    io_app_config->set_proxy_control( abap_true ).

    "set @Qualifier( prototype ) FOR ZCL_DI_COMPLEX_LOGGER
    io_app_config->get_class( 'ZCL_DI_COMPLEX_LOGGER'
                )->set_component_type( zif_annotations=>mc_component-component
                )->set_scope( iv_scope = zif_annotations=>mc_scope-prototype ).

    "set @Inject( io_repository_two )   FOR ZCL_DI_COMPLEX_SERVICE->MO_REPOSITORY_TWO
    "set @RefQualifier( TwoRepository ) FOR ZCL_DI_COMPLEX_SERVICE->MO_REPOSITORY_TWO
    io_app_config->get_class( 'ZCL_DI_COMPLEX_SERVICE'
                )->get_attribute( 'MO_REPOSITORY_TWO'
                )->set_attr_inject( 'IO_REPOSITORY_TWO'
                )->set_attr_qualifier( iv_constructor_parname = 'IO_REPOSITORY_TWO'
                                       iv_qualifier           = 'TwoRepository' ).

    "set @Qualifier( TwoRepository ) FOR ZCL_DI_COMPLEX_REPOSITORY_TWO
    "set @Proxy( true ) FOR ZCL_DI_COMPLEX_REPOSITORY_TWO
    io_app_config->get_class( 'ZCL_DI_COMPLEX_REPOSITORY_TWO'
                )->set_qualifier( iv_qualifier = 'TwoRepository'
                )->set_proxy( abap_true ).

  ENDMETHOD.
ENDCLASS.