*&---------------------------------------------------------------------*
*& Report ZTAB_ZHU_OO_ALV_DEMO
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ztab_zhu_oo_alv_demo.

INCLUDE ztab_zhu_oo_alv_demo_top.
INCLUDE ztab_zhu_oo_alv_demo_sel.
INCLUDE ztab_zhu_oo_alv_demo_cls.
INCLUDE ztab_zhu_oo_alv_demo_f01.

INITIALIZATION.
  PERFORM frm_init.

AT SELECTION-SCREEN OUTPUT."PBO
  PERFORM frm_pbo.

AT SELECTION-SCREEN."PAI
  PERFORM frm_pai.

START-OF-SELECTION.
  PERFORM frm_start_of_selection.

*&---------------------------------------------------------------------*
*& Module STATUS_9000 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE status_9000 OUTPUT.
  SET PF-STATUS 'STATUS'.
  SET TITLEBAR 'TITLE'.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_9000 INPUT.
  CASE ok_code.
    WHEN 'EXIT' OR 'BACK'.
      LEAVE TO SCREEN 0.
    WHEN  'CANC'.
      LEAVE PROGRAM.
    WHEN OTHERS.
  ENDCASE.
  CLEAR ok_code.
ENDMODULE.
