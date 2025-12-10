"@Interceptor
CLASS zcl_di_interceptor DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_di_proxy_interceptor .

    ALIASES call_after
      FOR zif_di_proxy_interceptor~call_after .
    ALIASES call_before
      FOR zif_di_proxy_interceptor~call_before .
    ALIASES call_in_exception
      FOR zif_di_proxy_interceptor~call_in_exception .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_DI_INTERCEPTOR IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_DI_INTERCEPTOR->ZIF_DI_PROXY_INTERCEPTOR~CALL_AFTER
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_CLASS                       TYPE        STRING
* | [--->] IV_METHOD                      TYPE        STRING
* | [--->] IO_OBJECT                      TYPE REF TO OBJECT
* | [--->] IT_PARAMETERS                  TYPE        ABAP_PARMBIND_TAB
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD zif_di_proxy_interceptor~call_after.
    WRITE : / |Call after { iv_class }->{ iv_method }| COLOR COL_KEY.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_DI_INTERCEPTOR->ZIF_DI_PROXY_INTERCEPTOR~CALL_BEFORE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_CLASS                       TYPE        STRING
* | [--->] IV_METHOD                      TYPE        STRING
* | [--->] IO_OBJECT                      TYPE REF TO OBJECT
* | [--->] IT_PARAMETERS                  TYPE        ABAP_PARMBIND_TAB
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD zif_di_proxy_interceptor~call_before.
    WRITE : / |Call before { iv_class }->{ iv_method }| COLOR COL_KEY.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_DI_INTERCEPTOR->ZIF_DI_PROXY_INTERCEPTOR~CALL_IN_EXCEPTION
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_CLASS                       TYPE        STRING
* | [--->] IV_METHOD                      TYPE        STRING
* | [--->] IO_OBJECT                      TYPE REF TO OBJECT
* | [--->] IO_EXCEPTION                   TYPE REF TO OBJECT(optional)
* | [--->] IV_EXCEPTION                   TYPE        I(optional)
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD zif_di_proxy_interceptor~call_in_exception.
    WRITE : / |Exception in { iv_class }->{ iv_method }| COLOR COL_NEGATIVE.
  ENDMETHOD.
ENDCLASS.