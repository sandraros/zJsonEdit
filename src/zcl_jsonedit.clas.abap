CLASS zcl_jsonedit DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.

    CLASS-METHODS create
      RETURNING
        VALUE(result) TYPE REF TO zcl_jsonedit.

    METHODS display
      IMPORTING
        parent TYPE REF TO cl_gui_container
      RAISING
        zcx_jsonedit.

    METHODS load_from_string
      IMPORTING
        json TYPE csequence
      RAISING
        zcx_jsonedit.

  PROTECTED SECTION.

  PRIVATE SECTION.
    DATA: json               TYPE string,
          json_xml           TYPE xstring,
          tree               TYPE REF TO lcl_tree_control,
          tree2              TYPE REF TO cl_column_tree_model,
          splitter_container TYPE REF TO cl_gui_splitter_container,
          container_left     TYPE REF TO cl_gui_container,
          dialogbox          TYPE REF TO cl_gui_dialogbox_container,
          textedit           TYPE REF TO cl_gui_textedit.

    METHODS build_tree
      IMPORTING
        tree TYPE REF TO lif_tree_control
      RAISING
        zcx_jsonedit.

    METHODS on_close
                FOR EVENT close OF cl_gui_dialogbox_container
      IMPORTING sender.

    METHODS on_node_context_menu_request
                FOR EVENT node_context_menu_request OF cl_column_tree_model
      IMPORTING node_key menu sender.

    METHODS on_node_context_menu_select
                FOR EVENT node_context_menu_select OF cl_column_tree_model
      IMPORTING node_key fcode sender.

    METHODS on_node_double_click
                FOR EVENT node_double_click OF cl_column_tree_model
      IMPORTING node_key sender.

ENDCLASS.



CLASS zcl_jsonedit IMPLEMENTATION.


  METHOD build_tree.
    TYPES: BEGIN OF ty_stack_level,
             node_key TYPE tm_nodekey,
             open     TYPE REF TO if_sxml_open_element,
           END OF ty_stack_level,
           ty_stack_levels TYPE STANDARD TABLE OF ty_stack_level WITH EMPTY KEY.
    DATA: parent_element  TYPE ty_stack_level,
          current_element TYPE ty_stack_level,
          stack_levels    TYPE ty_stack_levels,
          text            TYPE string.

    DATA(next_node_number) = 1.
    DATA(l_node_key) = VALUE tm_nodekey( ).

    TRY.
        DATA(reader) = cl_sxml_string_reader=>create( input = json_xml ).
        DO.
          DATA(node) = reader->read_next_node( ).
          IF node IS NOT BOUND.
            EXIT.
          ENDIF.

          CASE node->type.
            WHEN if_sxml_node=>co_nt_element_open.
              DATA(open) = CAST if_sxml_open_element( node ).

              parent_element = current_element.
              current_element = VALUE #( node_key = |{ next_node_number }| open = open ).
              APPEND current_element TO stack_levels.

              CASE current_element-open->qname-name.
                WHEN 'array' OR 'object'.
                  text = current_element-open->qname-name.
                  IF parent_element-open IS BOUND AND parent_element-open->qname-name = 'object'.
                    text = |"{ current_element-open->get_attribute_value( 'name' )->get_value( ) }":{ text }|.
                  ENDIF.
                  tree->add_node(
                      node_key          = |{ next_node_number }|
                      relative_node_key = COND #( WHEN parent_element IS NOT INITIAL THEN parent_element-node_key )
                      relationship      = COND #( WHEN parent_element IS NOT INITIAL THEN cl_column_tree_model=>relat_last_child )
                      isfolder          = ''
                      item_table        = VALUE #( ( item_name = 'C1'
                                                     class     = cl_column_tree_model=>item_class_text
                                                     text      = text ) )
                      image             = SWITCH #( current_element-open->qname-name
                                                WHEN 'array' THEN 'ICON_LIST'
                                                WHEN 'object' THEN 'ICON_STRUCTURE' )
                      expanded_image    = '' ).
                  next_node_number = next_node_number + 1.
              ENDCASE.

            WHEN if_sxml_node=>co_nt_value.

              DATA(value) = CAST if_sxml_value( node ).

              CASE current_element-open->qname-name.
                WHEN 'str' OR 'num' OR 'bool'.
                  text = COND #( WHEN value->type = if_sxml_value=>co_vt_text THEN value->get_value( ) ).
                  IF current_element-open->qname-name = 'str'.
                    text = |"{ escape( val = text format = cl_abap_format=>e_json_string ) }"|.
                  ENDIF.
                  IF parent_element-open IS BOUND AND parent_element-open->qname-name = 'object'.
                    text = |"{ current_element-open->get_attribute_value( 'name' )->get_value( ) }":{ text }|.
                  ENDIF.
                  tree->add_node(
                      node_key          = |{ next_node_number }|
                      relative_node_key = COND #( WHEN parent_element IS NOT INITIAL THEN parent_element-node_key )
                      relationship      = COND #( WHEN parent_element IS NOT INITIAL THEN cl_column_tree_model=>relat_last_child )
                      isfolder          = ''
                      item_table        = VALUE #( ( item_name = 'C1'
                                                     class     = cl_column_tree_model=>item_class_text
                                                     text      = text ) )
                      image             = 'ICON_ELEMENT'
                      expanded_image    = '' ).
                  next_node_number = next_node_number + 1.
              ENDCASE.

            WHEN if_sxml_node=>co_nt_element_close.

              DELETE stack_levels INDEX lines( stack_levels ).
              current_element = COND #( WHEN lines( stack_levels ) >= 1 THEN stack_levels[ lines( stack_levels ) ] ).
              parent_element = COND #( WHEN lines( stack_levels ) >= 2 THEN stack_levels[ lines( stack_levels ) - 1 ] ).

          ENDCASE.
        ENDDO.
      CATCH cx_root INTO DATA(lx).
        RAISE EXCEPTION NEW zcx_jsonedit( previous = lx ).
    ENDTRY.
  ENDMETHOD.


  METHOD create.
    result = NEW zcl_jsonedit( ).
    result->json = '{"JSON":"default"}'.
    CALL TRANSFORMATION id SOURCE XML result->json RESULT XML result->json_xml.
  ENDMETHOD.


  METHOD display.

    IF tree2 IS NOT BOUND.

      CREATE OBJECT splitter_container
        EXPORTING
          parent  = parent
          rows    = 1
          columns = 1.
      container_left = splitter_container->get_container( row = 1 column = 1 ).

      DATA(hierarchy_header) = VALUE treemhhdr(
            heading = 'Hierarchy node'                      "#EC NOTEXT
            width   = 30 ).                  " width: 30 characters
      CREATE OBJECT tree2
        EXPORTING
          node_selection_mode   = cl_column_tree_model=>node_sel_mode_single
          hierarchy_column_name = 'C1'
          hierarchy_header      = hierarchy_header.

      tree2->create_tree_control(
        EXPORTING
          parent                       = container_left
        EXCEPTIONS
          lifetime_error               = 1
          cntl_system_error            = 2
          create_error                 = 3
          failed                       = 4
          tree_control_already_created = 5
          OTHERS                       = 6 ).
      IF sy-subrc <> 0.
      ENDIF.

      tree2->set_registered_events(
        EXPORTING
          events                    = VALUE #( ( eventid = cl_column_tree_model=>eventid_node_context_menu_req )
                                               ( eventid = cl_column_tree_model=>eventid_node_double_click ) )
        EXCEPTIONS
          illegal_event_combination = 1
          unknown_event             = 2
          OTHERS                    = 3 ).
      IF sy-subrc <> 0.
      ENDIF.

      SET HANDLER on_node_double_click FOR tree2.
      SET HANDLER on_node_context_menu_request FOR tree2.
      SET HANDLER on_node_context_menu_select FOR tree2.

      tree = NEW lcl_tree_control( ).
      tree->set_gui_control( tree2 ).

      build_tree( tree ).
    ENDIF.
  ENDMETHOD.


  METHOD load_from_string.
    TRY.
        me->json = json.
        CALL TRANSFORMATION id SOURCE XML me->json RESULT XML json_xml OPTIONS xml_header = 'no'.
      CATCH cx_root INTO DATA(err).
        RAISE EXCEPTION NEW zcx_jsonedit( previous = err ).
    ENDTRY.
  ENDMETHOD.


  METHOD on_close.
    CALL METHOD sender->free.
  ENDMETHOD.


  METHOD on_node_context_menu_request.
    IF node_key = '1'.
      DATA(menu_2) = NEW cl_ctmenu( ).
      menu_2->add_function( fcode = 'TO_ARRAY' text = 'Array' ).
      menu_2->add_function( fcode = 'TO_OBJECT' text = 'Object' ).
      menu_2->add_function( fcode = 'TO_SIMPLE' text = 'Simple' ).
      menu->add_submenu( menu = menu_2 text = 'Change type' ).
    ENDIF.
  ENDMETHOD.


  METHOD on_node_context_menu_select.
    CASE fcode.
      WHEN 'CHANGE_TYPE'.
    ENDCASE.
  ENDMETHOD.


  METHOD on_node_double_click.
    CREATE OBJECT dialogbox
      EXPORTING
        width                       = 100
        height                      = 100
        top                         = 100
        left                        = 100
        repid                       = sy-cprog
        dynnr                       = sy-dynnr
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5
        event_already_registered    = 6
        error_regist_event          = 7
        OTHERS                      = 8.
*    dialogbox = NEW cl_gui_dialogbox_container( width = 100 height = 100 repid = sy-repid dynnr = sy-dynnr ).
    SET HANDLER on_close FOR dialogbox.
    CREATE OBJECT textedit
      EXPORTING
        parent                 = dialogbox
      EXCEPTIONS
        error_cntl_create      = 1
        error_cntl_init        = 2
        error_cntl_link        = 3
        error_dp_create        = 4
        gui_type_not_supported = 5
        OTHERS                 = 6.
*    textedit->set_left( 100 ).
*    textedit->set_top( 100 ).
*    textedit->set_height( 100 ).
*    textedit->set_width( 100 ).
    textedit->set_wordwrap_behavior(
      EXPORTING
        wordwrap_mode              = cl_gui_textedit=>wordwrap_at_windowborder
        wordwrap_to_linebreak_mode = cl_gui_textedit=>true
      EXCEPTIONS
        error_cntl_call_method     = 1 ).
    textedit->set_textstream(
      EXPORTING
        text                   = ``
      EXCEPTIONS
        error_cntl_call_method = 1
        not_supported_by_gui   = 2
        OTHERS                 = 3 ).
    IF sy-subrc <> 0.
    ENDIF.
    cl_gui_control=>set_focus(
      EXPORTING
        control           = dialogbox
      EXCEPTIONS
        cntl_error        = 1                " cntl_error
        cntl_system_error = 2                " cntl_system_error
        OTHERS            = 3
    ).
    IF sy-subrc <> 0.
*     MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*       WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
    cl_gui_cfw=>flush( ).
  ENDMETHOD.
ENDCLASS.
