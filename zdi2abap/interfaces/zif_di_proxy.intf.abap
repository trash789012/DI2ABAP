INTERFACE zif_di_proxy
  PUBLIC .


  METHODS set_original DEFAULT IGNORE
    IMPORTING
      !io_instance TYPE REF TO object .
  METHODS set_interceptor
    IMPORTING
      !io_interceptor TYPE REF TO zif_di_proxy_interceptor .
ENDINTERFACE.