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
          container_left     TYPE REF TO cl_gui_container.

    METHODS build_tree
      IMPORTING
        tree TYPE REF TO lif_tree_control
      RAISING
        zcx_jsonedit.

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
              current_element = VALUE #( node_key = next_node_number open = open ).
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
    CALL TRANSFORMATION id SOURCE XML result->json RESULT json = result->json_xml.
  ENDMETHOD.


  METHOD display.
    TYPES : BEGIN OF ty_ls_relationship2,
              caller TYPE string,
              callee TYPE string,
            END OF ty_ls_relationship2.
    DATA ls_relationship2 TYPE ty_ls_relationship2.
    DATA lt_relationship2 TYPE TABLE OF ty_ls_relationship2.
    DATA ls_hierarchy_header TYPE treemhhdr.
    DATA l_node_key TYPE tm_nodekey.
    DATA l_node_key_2 TYPE tm_nodekey.
    DATA lt_column TYPE treemcitab.
    DATA ls_column TYPE treemcitem.
    TYPE-POOLS icon.

    IF tree2 IS NOT BOUND.

      CREATE OBJECT splitter_container
        EXPORTING
          parent  = parent
          rows    = 1
          columns = 1.
      container_left = splitter_container->get_container( row = 1 column = 1 ).

      ls_hierarchy_header = VALUE #( heading = 'Hierarchy Header' "#EC NOTEXT
                                     width   = 30 ).                  " width: 30 characters
      CREATE OBJECT tree2
        EXPORTING
          node_selection_mode   = cl_gui_simple_tree=>node_sel_mode_single
          hierarchy_column_name = 'C1'
          hierarchy_header      = ls_hierarchy_header.

      tree2->create_tree_control(
        EXPORTING
          parent = container_left ).

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
ENDCLASS.
