"@Component
"@Qualifier( Ftp )
CLASS zcl_di_component_ftp_util DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_di_component_util .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_DI_COMPONENT_FTP_UTIL IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_DI_COMPONENT_FTP_UTIL->ZIF_DI_COMPONENT_UTIL~RUN
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD zif_di_component_util~run.
    WRITE : / 'Run By Ftp'.
  ENDMETHOD.
ENDCLASS.