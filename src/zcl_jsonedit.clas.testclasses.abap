*"* use this source file for your ABAP unit test classes
CLASS ltc_main DEFINITION DEFERRED.
CLASS zcl_jsonedit DEFINITION LOCAL FRIENDS ltc_main.

CLASS ltd_tree_control DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif_tree_control.
    TYPES: BEGIN OF ty_node,
             node_key          TYPE tm_nodekey,
             relative_node_key TYPE tm_nodekey,
             relationship      TYPE i,
             isfolder          TYPE as4flag,
             hidden            TYPE as4flag,
             disabled          TYPE as4flag,
             style             TYPE int4,
             no_branch         TYPE as4flag,
             expander          TYPE as4flag,
             image             TYPE tv_image,
             expanded_image    TYPE tv_image,
             drag_drop_id      TYPE i,
             user_object       TYPE REF TO object,
             items_incomplete  TYPE as4flag,
             item_table        TYPE treemcitab,
           END OF ty_node,
           ty_nodes TYPE STANDARD TABLE OF ty_node WITH EMPTY KEY.
    DATA: nodes TYPE ty_nodes.
ENDCLASS.
CLASS ltd_tree_control IMPLEMENTATION.
  METHOD lif_tree_control~add_node.
    nodes = VALUE #(
            BASE nodes
            ( node_key          = node_key
              relative_node_key = relative_node_key
              relationship      = relationship
              isfolder          = isfolder
              hidden            = hidden
              disabled          = disabled
              style             = style
              no_branch         = no_branch
              expander          = expander
              image             = image
              expanded_image    = expanded_image
              drag_drop_id      = drag_drop_id
              user_object       = user_object
              items_incomplete  = items_incomplete
              item_table        = item_table ) ).
  ENDMETHOD.
ENDCLASS.

CLASS ltc_main DEFINITION
      FOR TESTING
      DURATION SHORT
      RISK LEVEL HARMLESS.
  PRIVATE SECTION.
    METHODS string FOR TESTING RAISING cx_static_check.
    METHODS structure FOR TESTING RAISING cx_static_check.
    METHODS table FOR TESTING RAISING cx_static_check.
    METHODS setup.
    DATA: node_elem_base         TYPE ltd_tree_control=>ty_node,
          node_elem_child_base   TYPE ltd_tree_control=>ty_node,
          node_struct_base       TYPE ltd_tree_control=>ty_node,
          node_struct_child_base TYPE ltd_tree_control=>ty_node,
          node_table_base        TYPE ltd_tree_control=>ty_node,
          item_base              TYPE treemcitem.
ENDCLASS.
CLASS ltc_main IMPLEMENTATION.
  METHOD setup.
    node_elem_base = VALUE #( image = 'ICON_ELEMENT' ).
    node_elem_child_base = VALUE #( BASE node_elem_base relationship = cl_column_tree_model=>relat_last_child ).
    node_struct_base = VALUE #( image = 'ICON_STRUCTURE' ).
    node_struct_child_base = VALUE #( BASE node_struct_base relationship = cl_column_tree_model=>relat_last_child ).
    node_table_base = VALUE #( image = 'ICON_LIST' ).
    item_base = VALUE #( item_name = 'C1' class = cl_column_tree_model=>item_class_text ).
  ENDMETHOD.
  METHOD string.
    DATA(cut) = zcl_jsonedit=>create( ).
    DATA(tree) = NEW ltd_tree_control( ).
    cut->load_from_string( json = '"default"' ).
    cut->build_tree( tree ).
    cl_abap_unit_assert=>assert_equals(
        act = tree->nodes
        exp = VALUE ltd_tree_control=>ty_nodes(
              ( VALUE #( BASE node_elem_base node_key = '1' item_table = VALUE #( ( VALUE #( BASE item_base text = '"default"' ) ) ) ) ) ) ).
  ENDMETHOD.
  METHOD structure.
    DATA(cut) = zcl_jsonedit=>create( ).
    DATA(tree) = NEW ltd_tree_control( ).
    cut->load_from_string( json = '{"str":"text","num":1}' ).
    cut->build_tree( tree ).
    cl_abap_unit_assert=>assert_equals(
        act = tree->nodes
        exp = VALUE ltd_tree_control=>ty_nodes(
              ( VALUE #( BASE node_struct_base     node_key = '1' relative_node_key = ''  item_table = VALUE #( ( VALUE #( BASE item_base text = 'object' ) ) ) ) )
              ( VALUE #( BASE node_elem_child_base node_key = '2' relative_node_key = '1' item_table = VALUE #( ( VALUE #( BASE item_base text = '"str":"text"' ) ) ) ) )
              ( VALUE #( BASE node_elem_child_base node_key = '3' relative_node_key = '1' item_table = VALUE #( ( VALUE #( BASE item_base text = '"num":1' ) ) ) ) ) ) ).
  ENDMETHOD.
  METHOD table.
    DATA(cut) = zcl_jsonedit=>create( ).
    DATA(tree) = NEW ltd_tree_control( ).
    cut->load_from_string( json = '[1,{"num":1}]' ).
    cut->build_tree( tree ).
    cl_abap_unit_assert=>assert_equals(
        act = tree->nodes
        exp = VALUE ltd_tree_control=>ty_nodes(
              ( VALUE #( BASE node_table_base        node_key = '1' relative_node_key = ''  item_table = VALUE #( ( VALUE #( BASE item_base text = 'array' ) ) ) ) )
              ( VALUE #( BASE node_elem_child_base   node_key = '2' relative_node_key = '1' item_table = VALUE #( ( VALUE #( BASE item_base text = '1' ) ) ) ) )
              ( VALUE #( BASE node_STRUCT_child_base node_key = '3' relative_node_key = '1' item_table = VALUE #( ( VALUE #( BASE item_base text = 'object' ) ) ) ) )
              ( VALUE #( BASE node_elem_child_base   node_key = '4' relative_node_key = '3' item_table = VALUE #( ( VALUE #( BASE item_base text = '"num":1' ) ) ) ) ) ) ).
  ENDMETHOD.
ENDCLASS.
