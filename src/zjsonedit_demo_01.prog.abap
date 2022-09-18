*&---------------------------------------------------------------------*
*& Report zjsonedit_demo_01
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zjsonedit_demo_01.

PARAMETERS dummy.

AT SELECTION-SCREEN OUTPUT.
  DATA: jsonedit TYPE REF TO zcl_jsonedit.

  IF jsonedit IS NOT BOUND.
    jsonedit = zcl_jsonedit=>create( ).
*  jsonedit->load_from_string( '"value"' ).
    jsonedit->display( cl_gui_container=>screen0 ).
  ENDIF.
