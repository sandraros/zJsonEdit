*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations
CLASS lcl_tree_control_factory IMPLEMENTATION.
  METHOD create.
  ENDMETHOD.
ENDCLASS.
CLASS lcl_tree_control IMPLEMENTATION.
  METHOD lif_tree_control~add_node.
    gui_control->add_node(
      EXPORTING
        node_key                = node_key
        relative_node_key       = relative_node_key
        relationship            = relationship
        isfolder                = isfolder
*    hidden                  =                  " 'X': Node is Invisible
*    disabled                =                  " 'X': Node Cannot be Selected
*    style                   =                  " See Method Documentation
*    no_branch               =                  " 'X': Do Not Draw Hierarchy Lines
*    expander                =                  " See Method Documentation
*    image                   =                  " See Method Documentation
*    expanded_image          =                  " See Method Documentation
*    drag_drop_id            =                  " See Method Documentation
*    user_object             =                  " User Object
*    items_incomplete        =                  " See Method Documentation
        item_table              = item_table
      EXCEPTIONS
        node_key_exists         = 1                " Node Key Already Exists
        node_key_empty          = 2                " NODE_KEY is Initial or Contains Only Blanks
        illegal_relationship    = 3                " RELATIONSHIP Contains Invalid Value
        relative_node_not_found = 4                " Node With Key RELATIVE_NODE Does Not Exist
        error_in_item_table     = 5                " ITEM_TABLE Contains an Entry With Errors
        OTHERS                  = 6 ).
    IF sy-subrc <> 0.
      CASE sy-subrc.
        WHEN 1. RAISE node_key_exists.
        WHEN 2. RAISE node_key_empty.
        WHEN 3. RAISE illegal_relationship.
        WHEN 4. RAISE relative_node_not_found.
        WHEN 5. RAISE error_in_item_table.
        WHEN 6. RAISE unclassified.
      ENDCASE.
    ENDIF.
  ENDMETHOD.
  METHOD set_gui_control.
me->gui_control = value.
  ENDMETHOD.

ENDCLASS.
