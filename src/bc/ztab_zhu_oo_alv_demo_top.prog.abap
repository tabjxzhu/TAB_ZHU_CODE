*&---------------------------------------------------------------------*
*& Include          ZTAB_ZHU_OO_ALV_DEMO_TOP
*&---------------------------------------------------------------------*
TABLES:sflight.

CONSTANTS:
  gc_e         TYPE char01 VALUE 'E',
  gc_s         TYPE char01 VALUE 'S',
  gc_variant_1 TYPE disvariant-handle VALUE '1',
  gc_variant_2 TYPE disvariant-handle VALUE '2'.

DATA ok_code TYPE sy-ucomm.
DATA go_alv TYPE REF TO zcl_tabzhu_oo_alv.
DATA go_splitter TYPE REF TO cl_gui_splitter_container.

* >>> AI_SLOT:TYPES DATA OUTPUT
TYPES:tt_alv1 TYPE STANDARD TABLE OF sflight.
TYPES:tt_alv2 TYPE STANDARD TABLE OF scarr.

* <<< AI_SLOT:TYPES OUTPUT

DATA:gt_alv1 TYPE tt_alv1.
DATA:gt_alv2 TYPE tt_alv2.
