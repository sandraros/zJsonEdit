*&---------------------------------------------------------------------*
*& Report zjsonedit_demo_01
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zjsonedit_demo_01.

PARAMETERS dummy.

AT SELECTION-SCREEN OUTPUT.
  DATA: jsonedit TYPE REF TO zcl_jsonedit.

    try.
  IF jsonedit IS NOT BOUND.
    jsonedit = zcl_jsonedit=>create( ).
    jsonedit->load_from_string( '[1,{"num":1}]' ).
    jsonedit->display( cl_gui_container=>screen0 ).
  ENDIF.
  catch cx_root into data(err).
  message err type 'I'.
  endtry.
