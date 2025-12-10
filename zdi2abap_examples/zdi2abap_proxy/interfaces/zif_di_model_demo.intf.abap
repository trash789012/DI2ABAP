INTERFACE zif_di_model_demo
  PUBLIC .


  METHODS do .
  METHODS select .
  METHODS do_except
    RAISING
      zcx_di_error .
ENDINTERFACE.