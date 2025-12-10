"@Component
"@Qualifier( XmlLog )
"@Scope( Prototype )
CLASS zcl_di_logger_to_xml DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_di_logger_demo .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_DI_LOGGER_TO_XML IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_DI_LOGGER_TO_XML->ZIF_DI_LOGGER_DEMO~LOG
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_VALUE                       TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD zif_di_logger_demo~log.
    WRITE : / |Log to XML file { iv_value }|.
  ENDMETHOD.
ENDCLASS.