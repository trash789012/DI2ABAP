INTERFACE zif_annotations
  PUBLIC .

  CONSTANTS:
      mc_default_scope TYPE string VALUE `SINGLETON`.

  CONSTANTS:
    BEGIN OF mc_scope,
      default   TYPE string VALUE mc_default_scope,
      singleton TYPE string VALUE `SINGLETON`,
      prototype TYPE string VALUE `PROTOTYPE`,
    END OF mc_scope.

  CONSTANTS:
    BEGIN OF mc_component,
      core          TYPE string VALUE `CORE`,
      component     TYPE string VALUE `COMPONENT`,
      repository    TYPE string VALUE `REPOSITORY`,
      service       TYPE string VALUE `SERVICE`,
      interceptor   TYPE string VALUE `INTERCEPTOR`,
      configuration TYPE string VALUE `CONFIGURATION`,
    END OF mc_component.

  CONSTANTS:
    BEGIN OF mc_anotations,
      component           TYPE string VALUE mc_component-component,
      scope               TYPE string VALUE `SCOPE`,
      qualifier           TYPE string VALUE `QUALIFIER`,
      reference_qualifier TYPE string VALUE `REFQUALIFIER`,
      inject              TYPE string VALUE `INJECT`,
      proxy               TYPE string VALUE `PROXY`,
    END OF mc_anotations,

    BEGIN OF mc_annotation_values,
      true  TYPE string VALUE `TRUE`,
      false TYPE string VALUE `FALSE`,
    END OF mc_annotation_values.

ENDINTERFACE.