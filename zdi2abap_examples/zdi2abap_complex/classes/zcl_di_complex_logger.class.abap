CLASS zcl_di_complex_logger DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_di_compl_logger .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_DI_COMPLEX_LOGGER IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_DI_COMPLEX_LOGGER->ZIF_DI_COMPL_LOGGER~LOG
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_VALUE                       TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD zif_di_compl_logger~log.
    WRITE : / |логирую { iv_value }|.
  ENDMETHOD.
ENDCLASS.