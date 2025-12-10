CLASS zcl_di_dependensy_resolver DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      BEGIN OF mty_s_dependency_edge,
        source_class TYPE seoclname, "класс, который зависит от кого то
        target_class TYPE seoclname, "класс, от которого зависит исходный
        qualifier    TYPE string,
      END OF mty_s_dependency_edge .
    TYPES:
      mty_t_dependency_edge TYPE STANDARD TABLE OF mty_s_dependency_edge WITH EMPTY KEY
                              WITH NON-UNIQUE SORTED KEY k1 COMPONENTS target_class
                              WITH NON-UNIQUE SORTED KEY k2 COMPONENTS source_class.
    TYPES:
      BEGIN OF mty_s_class_node,
        class_name TYPE seoclname,
        class_info TYPE zcl_di_scanner=>mty_s_class_info,
        state      TYPE char1, "see mc_state values
      END OF mty_s_class_node .
    TYPES:
      mty_t_class_node TYPE HASHED TABLE OF mty_s_class_node
                         WITH UNIQUE KEY class_name.
    TYPES:
      mty_t_class_order TYPE STANDARD TABLE OF seoclname WITH EMPTY KEY .

    CONSTANTS:
      BEGIN OF mc_state,
        solved   TYPE char1 VALUE 'S',
        pending  TYPE char1 VALUE 'P',
        unsolved TYPE char1 VALUE 'U',
      END OF mc_state .

    METHODS dispose .
    METHODS resolve_dependencies
      CHANGING
        !ct_class                 TYPE zcl_di_scanner=>mty_t_class_info
      RETURNING
        VALUE(rt_ordered_classes) TYPE mty_t_class_order
      RAISING
        zcx_di_error .
    METHODS get_resolve_dependencies
      RETURNING
        VALUE(rt_resolve_dependencies) TYPE mty_t_class_order .
  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      BEGIN OF mty_s_match_cache,
        interface TYPE string,
        qualifier TYPE string,
        classname TYPE seoclname,
      END OF mty_s_match_cache .
    TYPES:
      mty_t_match_cache TYPE SORTED TABLE OF mty_s_match_cache
                              WITH UNIQUE KEY interface qualifier .

    DATA mt_match_cache TYPE mty_t_match_cache .
    DATA mt_resolve_dependencies TYPE mty_t_class_order .

    METHODS build_dependency_graph
      CHANGING
        !ct_classes     TYPE zcl_di_scanner=>mty_t_class_info
      RETURNING
        VALUE(rt_edges) TYPE mty_t_dependency_edge
      RAISING
        zcx_di_error .
    METHODS topological_sort
      IMPORTING
        !it_edges                 TYPE mty_t_dependency_edge
        !it_nodes                 TYPE mty_t_class_node
      RETURNING
        VALUE(rt_ordered_classes) TYPE mty_t_class_order
      RAISING
        zcx_di_error .
    METHODS find_class_by_interface
      IMPORTING
        !iv_interface        TYPE string
        !iv_qualifier        TYPE string
      CHANGING
        !ct_classes          TYPE zcl_di_scanner=>mty_t_class_info
      RETURNING
        VALUE(rv_class_name) TYPE seoclname
      RAISING
        zcx_di_error .
ENDCLASS.



CLASS ZCL_DI_DEPENDENSY_RESOLVER IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_DI_DEPENDENSY_RESOLVER->BUILD_DEPENDENCY_GRAPH
* +-------------------------------------------------------------------------------------------------+
* | [<-->] CT_CLASSES                     TYPE        ZCL_DI_SCANNER=>MTY_T_CLASS_INFO
* | [<-()] RT_EDGES                       TYPE        MTY_T_DEPENDENCY_EDGE
* | [!CX!] ZCX_DI_ERROR
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD build_dependency_graph.

    DATA:
      lt_return TYPE bapiret2_tt.

    LOOP AT ct_classes ASSIGNING FIELD-SYMBOL(<ls_class>).
      "Анализируем зависимости
      LOOP AT <ls_class>-injected_dependencies ASSIGNING FIELD-SYMBOL(<ls_dep>)
        WHERE has_inject IS NOT INITIAL.

        "Определяем конкретный класс для реализации
        TRY.
            <ls_dep>-class_name = find_class_by_interface(
                                    EXPORTING
                                      iv_interface = <ls_dep>-parameter_type
                                      iv_qualifier = <ls_dep>-qualifier
                                    CHANGING
                                      ct_classes   = ct_classes
                                    ).

            APPEND VALUE #(
              source_class = <ls_class>-class_name
              target_class = <ls_dep>-class_name
              qualifier    = <ls_dep>-qualifier
            ) TO rt_edges.
          CATCH zcx_di_error.
            MESSAGE e002
              WITH <ls_class>-class_name
                   <ls_dep>-parameter_name
                   <ls_dep>-qualifier
              INTO DATA(lv_dummy).
            APPEND zcx_di_error=>sy2bapiret( ) TO lt_return.
        ENDTRY.
      ENDLOOP.
    ENDLOOP.

    IF lt_return IS NOT INITIAL.
      RAISE EXCEPTION TYPE zcx_di_error
        EXPORTING
          mt_errors = lt_return.
    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_DI_DEPENDENSY_RESOLVER->DISPOSE
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD dispose.
    FREE:
      mt_match_cache,
      mt_resolve_dependencies.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_DI_DEPENDENSY_RESOLVER->FIND_CLASS_BY_INTERFACE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_INTERFACE                   TYPE        STRING
* | [--->] IV_QUALIFIER                   TYPE        STRING
* | [<-->] CT_CLASSES                     TYPE        ZCL_DI_SCANNER=>MTY_T_CLASS_INFO
* | [<-()] RV_CLASS_NAME                  TYPE        SEOCLNAME
* | [!CX!] ZCX_DI_ERROR
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD find_class_by_interface.

    READ TABLE mt_match_cache ASSIGNING FIELD-SYMBOL(<ls_match_buffer>)
      WITH TABLE KEY interface = iv_interface
                     qualifier = iv_qualifier.
    IF sy-subrc = 0.
      rv_class_name = <ls_match_buffer>-classname.
      RETURN.
    ENDIF.

    "Ищем класс, который реализует интерфейс и имеет нужный qualifier
    READ TABLE ct_classes ASSIGNING FIELD-SYMBOL(<ls_class>)
      WITH KEY k1
      COMPONENTS absolute_type = iv_interface
                 qualifier     = iv_qualifier.
    IF sy-subrc <> 0.
      READ TABLE ct_classes ASSIGNING <ls_class>
        WITH KEY k1
        COMPONENTS absolute_type = iv_interface.
    ENDIF.
    IF sy-subrc = 0.
      "Подходит
      rv_class_name = <ls_class>-class_name.
    ENDIF.

    INSERT VALUE #( interface = iv_interface
                    qualifier = iv_qualifier
                    classname = rv_class_name ) INTO TABLE mt_match_cache.

    IF rv_class_name IS INITIAL.
      RAISE EXCEPTION TYPE zcx_di_error.
    ENDIF.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_DI_DEPENDENSY_RESOLVER->GET_RESOLVE_DEPENDENCIES
* +-------------------------------------------------------------------------------------------------+
* | [<-()] RT_RESOLVE_DEPENDENCIES        TYPE        MTY_T_CLASS_ORDER
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_resolve_dependencies.
    rt_resolve_dependencies = mt_resolve_dependencies.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_DI_DEPENDENSY_RESOLVER->RESOLVE_DEPENDENCIES
* +-------------------------------------------------------------------------------------------------+
* | [<-->] CT_CLASS                       TYPE        ZCL_DI_SCANNER=>MTY_T_CLASS_INFO
* | [<-()] RT_ORDERED_CLASSES             TYPE        MTY_T_CLASS_ORDER
* | [!CX!] ZCX_DI_ERROR
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD resolve_dependencies.

    CLEAR:
      mt_match_cache.

    "Строим граф зависимостей
    DATA(lt_edges) = build_dependency_graph( CHANGING ct_classes = ct_class ).

    "Создаем узлы графа
    DATA(lt_nodes) = VALUE mty_t_class_node( ).
    LOOP AT ct_class ASSIGNING FIELD-SYMBOL(<ls_class>).
      INSERT VALUE #(
        class_name = <ls_class>-class_name
        class_info = <ls_class>
        state      = mc_state-unsolved
      ) INTO TABLE lt_nodes.
    ENDLOOP.

    "Топологическая сортировка
    rt_ordered_classes = topological_sort(
                           it_edges = lt_edges
                           it_nodes = lt_nodes
                         ).

    mt_resolve_dependencies = rt_ordered_classes.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_DI_DEPENDENSY_RESOLVER->TOPOLOGICAL_SORT
* +-------------------------------------------------------------------------------------------------+
* | [--->] IT_EDGES                       TYPE        MTY_T_DEPENDENCY_EDGE
* | [--->] IT_NODES                       TYPE        MTY_T_CLASS_NODE
* | [<-()] RT_ORDERED_CLASSES             TYPE        MTY_T_CLASS_ORDER
* | [!CX!] ZCX_DI_ERROR
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD topological_sort.

    DATA:
      lt_no_dependencies TYPE mty_t_class_order,
      lt_sorted_classes  TYPE mty_t_class_order.

    DATA(lt_edges) = it_edges.
    DATA(lt_nodes) = it_nodes.

    "Используем Kahn's Algoritm для топологической сортировки

    "1. Находим узлы без входящих зависимостей
    LOOP AT lt_nodes ASSIGNING FIELD-SYMBOL(<ls_node>).
      DATA(lv_has_incoming) = abap_false.

      LOOP AT lt_edges ASSIGNING FIELD-SYMBOL(<ls_edge>)
        USING KEY k1
        WHERE target_class = <ls_node>-class_name.

        lv_has_incoming = abap_true.
        EXIT.
      ENDLOOP.

      "Нет зависимостей ? - solved
      IF lv_has_incoming = abap_false.
        APPEND <ls_node>-class_name TO lt_no_dependencies.
        <ls_node>-state = mc_state-solved.
      ENDIF.
    ENDLOOP.

    "Если нет классов без зависимостей, значит есть циклы, либо что то не так
    IF lt_no_dependencies IS INITIAL.
      RAISE EXCEPTION TYPE zcx_di_error.
    ENDIF.

    "2. Обрабатываем узлы без зависимостей
    WHILE lt_no_dependencies IS NOT INITIAL.
      "Берем первый узел из списка
      DATA(lv_current_class) = lt_no_dependencies[ 1 ].

      "У таких классов наивысший приоритет для инстанцирования
      INSERT lv_current_class INTO lt_sorted_classes INDEX 1.

      DELETE lt_no_dependencies INDEX 1.

      "Находим все исходящие зависимости
      DATA(lt_outgoing_edges) = VALUE mty_t_dependency_edge( ).
      LOOP AT lt_edges ASSIGNING <ls_edge>
        USING KEY k2
        WHERE source_class = lv_current_class.

        APPEND <ls_edge> TO lt_outgoing_edges.
      ENDLOOP.

      "Удаляем эти зависимости из графа
      DELETE lt_edges WHERE source_class = lv_current_class.

      "Проверяем, появились ли новые узлы без зависимостей
      LOOP AT lt_outgoing_edges ASSIGNING <ls_edge>.
        DATA(lv_has_other_incoming) = abap_false.

        LOOP AT lt_edges ASSIGNING FIELD-SYMBOL(<ls_remaining_edge>)
          USING KEY k1
          WHERE target_class = <ls_edge>-target_class.
          lv_has_other_incoming = abap_true.
          EXIT.
        ENDLOOP.

        "Если у целевого узла больше нет входящих зависимостей
        IF lv_has_other_incoming = abap_false.
          READ TABLE lt_nodes ASSIGNING FIELD-SYMBOL(<ls_target_node>)
            WITH TABLE KEY class_name = <ls_edge>-target_class.
          IF sy-subrc = 0 AND <ls_target_node>-state <> mc_state-solved.
            "докидываем его в свободные, и по нему пойдет следующая итерация while
            APPEND <ls_target_node>-class_name TO lt_no_dependencies.
            <ls_target_node>-state = mc_state-solved.
          ENDIF.
        ENDIF.
      ENDLOOP.

    ENDWHILE.

    "3. Проверка на циклы (если остались ребра - есть цикл)
    IF lt_edges IS NOT INITIAL.
      RAISE EXCEPTION TYPE zcx_di_error.
    ENDIF.

    rt_ordered_classes = lt_sorted_classes.

  ENDMETHOD.
ENDCLASS.