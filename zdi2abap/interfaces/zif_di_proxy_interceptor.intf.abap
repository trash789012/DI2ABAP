INTERFACE zif_di_proxy_interceptor
  PUBLIC .

  METHODS call_before
    IMPORTING
      !iv_class      TYPE string
      !iv_method     TYPE string
      !io_object     TYPE REF TO object
      !it_parameters TYPE abap_parmbind_tab .
  METHODS call_after
    IMPORTING
      !iv_class      TYPE string
      !iv_method     TYPE string
      !io_object     TYPE REF TO object
      !it_parameters TYPE abap_parmbind_tab .
  METHODS call_in_exception
    IMPORTING
      !iv_class     TYPE string
      !iv_method    TYPE string
      !io_object    TYPE REF TO object
      !io_exception TYPE REF TO object OPTIONAL
      !iv_exception TYPE i OPTIONAL .
ENDINTERFACE.