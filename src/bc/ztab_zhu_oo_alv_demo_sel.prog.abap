*&---------------------------------------------------------------------*
*& Include          ZTAB_ZHU_OO_ALV_DEMO_SEL
*&---------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK blk WITH FRAME TITLE TEXT-s01.
  SELECT-OPTIONS:s_carrid FOR sflight-carrid,
                 s_connid FOR sflight-connid,
                 s_fldate FOR sflight-fldate.
  PARAMETERS:rb_1 RADIOBUTTON GROUP gp1 DEFAULT 'X',"Single ALV
             rb_2 RADIOBUTTON GROUP gp1."Double ALV
SELECTION-SCREEN END OF BLOCK blk.
