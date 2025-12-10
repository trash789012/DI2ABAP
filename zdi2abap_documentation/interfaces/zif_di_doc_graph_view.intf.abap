INTERFACE zif_di_doc_graph_view
  PUBLIC .

  METHODS:
    show
      IMPORTING
        iv_json         TYPE string
        iv_in_container TYPE flag DEFAULT space.

ENDINTERFACE.