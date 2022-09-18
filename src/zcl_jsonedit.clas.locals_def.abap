*"* use this source file for any type of declarations (class
*"* definitions, interfaces or type declarations) you need for
*"* components in the private section
INTERFACE lif_tree_control.
  methods ADD_NODE
    importing
      !NODE_KEY type TM_NODEKEY
      !RELATIVE_NODE_KEY type TM_NODEKEY optional
      value(RELATIONSHIP) type I optional
      value(ISFOLDER) type AS4FLAG
      value(HIDDEN) type AS4FLAG optional
      value(DISABLED) type AS4FLAG optional
      value(STYLE) type INT4 optional
      value(NO_BRANCH) type AS4FLAG optional
      value(EXPANDER) type AS4FLAG optional
      value(IMAGE) type TV_IMAGE optional
      value(EXPANDED_IMAGE) type TV_IMAGE optional
      value(DRAG_DROP_ID) type I optional
      value(USER_OBJECT) type ref to OBJECT optional
      value(ITEMS_INCOMPLETE) type AS4FLAG optional
      !ITEM_TABLE type TREEMCITAB
    exceptions
      NODE_KEY_EXISTS
      NODE_KEY_EMPTY
      ILLEGAL_RELATIONSHIP
      RELATIVE_NODE_NOT_FOUND
      ERROR_IN_ITEM_TABLE
      unclassified.
ENDINTERFACE.
CLASS lcl_tree_control_factory DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS create
      RETURNING
        VALUE(result) TYPE REF TO lif_tree_control.
ENDCLASS.
CLASS lcl_tree_control DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif_tree_control.
   data: gui_control type ref to cl_column_tree_model.
    methods set_gui_control
    importing
    value type ref to cl_column_tree_model.
ENDCLASS.
