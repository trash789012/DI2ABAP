INTERFACE zif_di_app_config_enhancer
  PUBLIC .


  CLASS-METHODS configurate
    IMPORTING
      !io_app_config TYPE REF TO zif_di_app_config
    RAISING
      zcx_di_error .
ENDINTERFACE.