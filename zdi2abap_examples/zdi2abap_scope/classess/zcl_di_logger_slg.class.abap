"@Component
CLASS zcl_di_logger_slg DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_di_logger_demo .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_DI_LOGGER_SLG IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_DI_LOGGER_SLG->ZIF_DI_LOGGER_DEMO~LOG
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_VALUE                       TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD zif_di_logger_demo~log.
    WRITE: / |Log to slg1 { iv_value }|.
  ENDMETHOD.
ENDCLASS.