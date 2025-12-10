CLASS zcx_di_error DEFINITION
  PUBLIC
  INHERITING FROM cx_static_check
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_t100_dyn_msg .
    INTERFACES if_t100_message .

    DATA mt_errors TYPE bapiret2_t .

    METHODS constructor
      IMPORTING
        !textid    LIKE if_t100_message=>t100key OPTIONAL
        !previous  LIKE previous OPTIONAL
        !mt_errors TYPE bapiret2_t OPTIONAL .
    METHODS get_messages
      RETURNING
        VALUE(rt_messages) TYPE bapiret2_t .
    CLASS-METHODS sy2bapiret
      IMPORTING
        !iv_parameter    TYPE bapiret2-parameter OPTIONAL
        !iv_row          TYPE bapiret2-row OPTIONAL
        !iv_field        TYPE bapiret2-field OPTIONAL
      RETURNING
        VALUE(rs_return) TYPE bapiret2 .
    METHODS display_exceptions
      IMPORTING
        !iv_type         TYPE sy-msgty OPTIONAL
        !iv_display_like TYPE sy-msgty OPTIONAL .
    CLASS-METHODS show_log
      IMPORTING
        !iv_type         TYPE sy-msgty OPTIONAL
        !iv_display_like TYPE sy-msgty OPTIONAL
        !it_return       TYPE bapiret2_t OPTIONAL .
    CLASS-METHODS write_to_spool
      IMPORTING
        VALUE(it_return) TYPE bapiret2_t .
    CLASS-METHODS show_question
      IMPORTING
        !iv_text      TYPE c
      RETURNING
        VALUE(rv_yes) TYPE abap_bool .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcx_di_error IMPLEMENTATION.


  METHOD constructor ##ADT_SUPPRESS_GENERATION.
    CALL METHOD super->constructor
      EXPORTING
        previous = previous.
    me->mt_errors = mt_errors .
    CLEAR me->textid.
    IF textid IS INITIAL.
      if_t100_message~t100key = if_t100_message=>default_textid.
    ELSE.
      if_t100_message~t100key = textid.
    ENDIF.
  ENDMETHOD.


  METHOD display_exceptions.
*--------------------------------------------------------------------*
*  DISPLAY_EXCEPTIONS
*--------------------------------------------------------------------*
* Показать список сообщений
*--------------------------------------------------------------------*

    "Получаем список сообщений
    DATA(lt_return) = get_messages( ).

    "Показываем лог
    show_log( iv_type         = iv_type
              iv_display_like = iv_type
              it_return       = lt_return ).

  ENDMETHOD.


  METHOD get_messages.
*--------------------------------------------------------------------*
* GET_MESSAGES
*--------------------------------------------------------------------*
* Получить список сообщений
*--------------------------------------------------------------------*


    DATA:
      lt_return TYPE STANDARD TABLE OF bapiret2 WITH DEFAULT KEY
                  WITH NON-UNIQUE SORTED KEY k1 COMPONENTS id
                                                           type
                                                           number
                                                           message_v1
                                                           message_v2
                                                           message_v3
                                                           message_v4.

    IF mt_errors IS INITIAL.
      CALL FUNCTION 'RS_EXCEPTION_TO_BAPIRET2'
        EXPORTING
          i_r_exception = me
        CHANGING
          c_t_bapiret2  = mt_errors.
    ENDIF.

    IF previous IS NOT BOUND.
      DATA(lt_prev_errors) = VALUE bapiret2_t( ).
      CALL FUNCTION 'RS_EXCEPTION_TO_BAPIRET2'
        EXPORTING
          i_r_exception = previous
        CHANGING
          c_t_bapiret2  = lt_prev_errors.

      APPEND LINES OF lt_prev_errors TO mt_errors.
    ENDIF.

    APPEND LINES OF mt_errors TO lt_return.

    DELETE ADJACENT DUPLICATES FROM lt_return USING KEY k1. "#EC CI_STDSEQ

    rt_messages = lt_return.


  ENDMETHOD.


  METHOD show_log.
*--------------------------------------------------------------------*
* SHOW_LOG
*--------------------------------------------------------------------*
*Показать список сообщений
*--------------------------------------------------------------------*

    "Для фоновой обработки просто пишем в спул
    IF sy-batch = abap_true.
      write_to_spool( it_return ).
      RETURN.
    ENDIF.

    DATA(lv_lines) = lines( it_return ).

    IF lv_lines = 1.
      DATA(ls_return) = VALUE #( it_return[ 1 ] OPTIONAL ).

      DATA(lv_type) = COND #( WHEN iv_type IS INITIAL
                                THEN ls_return-type
                                ELSE iv_type ).

      DATA(lv_like) = COND #( WHEN iv_display_like IS INITIAL
                                THEN ls_return-type
                                ELSE rs_c_error ).

      MESSAGE ID ls_return-id
              TYPE lv_type
              NUMBER ls_return-number
              WITH ls_return-message_v1
                   ls_return-message_v2
                   ls_return-message_v3
                   ls_return-message_v4 DISPLAY LIKE lv_like.
    ELSEIF lv_lines IS NOT INITIAL.
      CALL FUNCTION 'SUSR_DISPLAY_LOG'
        EXPORTING
          display_in_popup = abap_true
        TABLES
          it_log_bapiret2  = it_return
        EXCEPTIONS
          OTHERS           = 0.
    ENDIF.

  ENDMETHOD.


  METHOD show_question.

    DATA:
      lv_ans TYPE char1.

    "Вызов диалога предупреждения
    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        text_question         = iv_text
        display_cancel_button = abap_false
      IMPORTING
        answer                = lv_ans
      EXCEPTIONS
        OTHERS                = 0.

    rv_yes = xsdbool( lv_ans = 1 ).

  ENDMETHOD.


  METHOD sy2bapiret.
*--------------------------------------------------------------------*
* SY2BAPIRET
*--------------------------------------------------------------------*
* Преобразование к bapiret2
*--------------------------------------------------------------------*

    "Преобразуем
    CALL FUNCTION 'FS_BAPI_BAPIRET2_FILL'
      EXPORTING
        type      = sy-msgty
        cl        = sy-msgid
        number    = sy-msgno
        par1      = sy-msgv1
        par2      = sy-msgv2
        par3      = sy-msgv3
        par4      = sy-msgv4
        parameter = iv_parameter
        row       = iv_row
        field     = iv_field
      IMPORTING
        return    = rs_return.

  ENDMETHOD.


  METHOD write_to_spool.
*--------------------------------------------------------------------*
* WRITE_TO_SPOOL
*--------------------------------------------------------------------*
* Записать сообщение в спул
*--------------------------------------------------------------------*

    DATA:
      lo_cont TYPE REF TO cl_gui_docking_container ##NEEDED.

    TRY.
        cl_salv_table=>factory(
          EXPORTING
            r_container    = lo_cont
          IMPORTING
            r_salv_table   = DATA(lo_table)
          CHANGING
            t_table        = it_return
        ).

        lo_table->display( ).
      CATCH cx_salv_msg ##NO_HANDLER.
        "not needed
    ENDTRY.

    FREE lo_table.
  ENDMETHOD.                                             "#EC CI_VALPAR
ENDCLASS.