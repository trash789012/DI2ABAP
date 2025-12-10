CLASS zcl_di_annotation_processor DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      BEGIN OF mty_s_annotations,
        name  TYPE string,
        value TYPE string,
      END OF mty_s_annotations .
    TYPES:
      mty_t_annotations TYPE STANDARD TABLE OF mty_s_annotations WITH KEY name .

    CLASS-METHODS extract_annotation_value
      IMPORTING
        !iv_line        TYPE string
      RETURNING
        VALUE(rv_value) TYPE string .
    CLASS-METHODS parse_annotations
      IMPORTING
        !it_source_code       TYPE string_table
      RETURNING
        VALUE(rt_annotations) TYPE mty_t_annotations .
    CLASS-METHODS get_qualifier
      IMPORTING
        !it_annotations     TYPE mty_t_annotations
      RETURNING
        VALUE(rv_qualifier) TYPE string .
    CLASS-METHODS get_ref_qualifier
      IMPORTING
        !it_annotations     TYPE mty_t_annotations
      RETURNING
        VALUE(rv_qualifier) TYPE string .
    CLASS-METHODS get_inject_argument
      IMPORTING
        !it_annotations    TYPE mty_t_annotations
      RETURNING
        VALUE(rv_argument) TYPE string .
    CLASS-METHODS has_inject
      IMPORTING
        !it_annotations TYPE mty_t_annotations
      RETURNING
        VALUE(rv_has)   TYPE abap_bool .
    CLASS-METHODS get_all_scopes
      RETURNING
        VALUE(rt_scopes) TYPE string_table .
    CLASS-METHODS get_all_component_types
      RETURNING
        VALUE(rt_scopes) TYPE string_table .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_DI_ANNOTATION_PROCESSOR IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_DI_ANNOTATION_PROCESSOR=>EXTRACT_ANNOTATION_VALUE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_LINE                        TYPE        STRING
* | [<-()] RV_VALUE                       TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD extract_annotation_value.
    DATA(lv_temp) = iv_line.

    "Удалить ковычки и пробелы
    REPLACE REGEX `^"\s*` IN lv_temp WITH ''.

    "Поиск значения в скобках
    FIND REGEX `@\w+\(([^)]+)\)` IN lv_temp SUBMATCHES rv_value.
    IF sy-subrc <> 0.
      "Нет скобок, но есть аннотация - возвращаем пустую строку
      FIND REGEX `@(\w+)` IN lv_temp SUBMATCHES rv_value.
      IF sy-subrc = 0.
        CLEAR rv_value. "для аннотаций без значения
      ENDIF.
    ENDIF.

    rv_value = condense( rv_value ).
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_DI_ANNOTATION_PROCESSOR=>GET_ALL_COMPONENT_TYPES
* +-------------------------------------------------------------------------------------------------+
* | [<-()] RT_SCOPES                      TYPE        STRING_TABLE
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_all_component_types.
    DO.
      ASSIGN COMPONENT sy-index OF STRUCTURE zif_annotations=>mc_component
        TO FIELD-SYMBOL(<lv_scope>).
      IF sy-subrc <> 0.
        EXIT.
      ENDIF.

      APPEND <lv_scope> TO rt_scopes.
    ENDDO.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_DI_ANNOTATION_PROCESSOR=>GET_ALL_SCOPES
* +-------------------------------------------------------------------------------------------------+
* | [<-()] RT_SCOPES                      TYPE        STRING_TABLE
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_all_scopes.

    DO.
      ASSIGN COMPONENT sy-index OF STRUCTURE zif_annotations=>mc_scope
        TO FIELD-SYMBOL(<lv_scope>).
      IF sy-subrc <> 0.
        EXIT.
      ENDIF.

      APPEND <lv_scope> TO rt_scopes.
    ENDDO.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_DI_ANNOTATION_PROCESSOR=>GET_INJECT_ARGUMENT
* +-------------------------------------------------------------------------------------------------+
* | [--->] IT_ANNOTATIONS                 TYPE        MTY_T_ANNOTATIONS
* | [<-()] RV_ARGUMENT                    TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_inject_argument.
    rv_argument = VALUE #( it_annotations[
                              name = zif_annotations=>mc_anotations-inject
                            ]-value OPTIONAL
                   ).
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_DI_ANNOTATION_PROCESSOR=>GET_QUALIFIER
* +-------------------------------------------------------------------------------------------------+
* | [--->] IT_ANNOTATIONS                 TYPE        MTY_T_ANNOTATIONS
* | [<-()] RV_QUALIFIER                   TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_qualifier.
    rv_qualifier = VALUE #(
      it_annotations[ name = zif_annotations=>mc_anotations-qualifier ]-value OPTIONAL
    ).
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_DI_ANNOTATION_PROCESSOR=>GET_REF_QUALIFIER
* +-------------------------------------------------------------------------------------------------+
* | [--->] IT_ANNOTATIONS                 TYPE        MTY_T_ANNOTATIONS
* | [<-()] RV_QUALIFIER                   TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_ref_qualifier.
    rv_qualifier = VALUE #( it_annotations[
                              name = zif_annotations=>mc_anotations-reference_qualifier
                            ]-value OPTIONAL
                   ).
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_DI_ANNOTATION_PROCESSOR=>HAS_INJECT
* +-------------------------------------------------------------------------------------------------+
* | [--->] IT_ANNOTATIONS                 TYPE        MTY_T_ANNOTATIONS
* | [<-()] RV_HAS                         TYPE        ABAP_BOOL
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD has_inject.
    rv_has = xsdbool(
      line_exists( it_annotations[
                     name = zif_annotations=>mc_anotations-inject
                   ]
      )
    ).
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_DI_ANNOTATION_PROCESSOR=>PARSE_ANNOTATIONS
* +-------------------------------------------------------------------------------------------------+
* | [--->] IT_SOURCE_CODE                 TYPE        STRING_TABLE
* | [<-()] RT_ANNOTATIONS                 TYPE        MTY_T_ANNOTATIONS
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD parse_annotations.
    DATA:
      lv_pattern TYPE string VALUE `"\s*@(\w+)(?:\(([^)]+)\))?`.

    LOOP AT it_source_code ASSIGNING FIELD-SYMBOL(<lv_line>).
      CHECK <lv_line> CP |*"@*|.

      FIND REGEX lv_pattern IN <lv_line>
        SUBMATCHES DATA(lv_name) DATA(lv_value).
      CHECK sy-subrc = 0.
      CHECK lv_name IS NOT INITIAL.

      APPEND VALUE #( name  = to_upper( lv_name )
                      value = condense( to_upper( lv_value ) ) ) TO rt_annotations.

    ENDLOOP.
  ENDMETHOD.
ENDCLASS.