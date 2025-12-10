INTERFACE zif_di_doc_graph_service
  PUBLIC .

  TYPES:
    mty_t_annotations TYPE STANDARD TABLE OF string WITH EMPTY KEY.

  TYPES:
    BEGIN OF mty_s_node,
      id           TYPE i,
      label        TYPE string,
      group        TYPE string,
      title        TYPE string,
      annotations  TYPE mty_t_annotations,
      methods      TYPE i,
      dependencies TYPE i,
    END OF mty_s_node.

  TYPES:
    BEGIN OF mty_s_edge,
      from   TYPE i,
      to     TYPE i,
      label  TYPE string,
      color  TYPE string,
      arrows TYPE string,
      dashes TYPE abap_bool,
    END OF mty_s_edge.

  TYPES:
    BEGIN OF mty_s_graph,
      nodes TYPE STANDARD TABLE OF mty_s_node WITH KEY id,
      edges TYPE STANDARD TABLE OF mty_s_edge WITH KEY from to,
    END OF mty_s_graph.

  METHODS build_graph
    IMPORTING
      !it_packages TYPE lxe_tt_dc
    EXPORTING
      !es_data     TYPE mty_s_graph
      !ev_json     TYPE string
    RAISING
      zcx_di_error .
ENDINTERFACE.