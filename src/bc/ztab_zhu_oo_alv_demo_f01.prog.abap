*&---------------------------------------------------------------------*
*& Include          ZTAB_ZHU_OO_ALV_DEMO_F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form frm_init
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_init .

ENDFORM.
*&---------------------------------------------------------------------*
*& Form frm_pbo
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_pbo .

ENDFORM.
*&---------------------------------------------------------------------*
*& Form frm_pai
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_pai .

ENDFORM.
*&---------------------------------------------------------------------*
*& Form frm_stat_of_selection
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_start_of_selection .

*>>> AI_SLOT:get data logic
  SELECT * FROM sflight INTO TABLE @gt_alv1
    WHERE carrid IN @s_carrid AND connid IN @s_connid
    AND fldate IN @s_fldate.
  IF sy-subrc EQ 0.
    SORT gt_alv1 BY carrid connid fldate.

    SELECT * FROM scarr INTO TABLE @gt_alv2
      FOR ALL ENTRIES IN @gt_alv1
      WHERE carrid = @gt_alv1-carrid.
    IF sy-subrc EQ 0.
      SORT gt_alv2 BY carrid.
    ENDIF.
  ENDIF.
*<<< AI_SLOT:get data logic

  PERFORM frm_display_data.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form frm_display_data
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_display_data .
  FIELD-SYMBOLS <lfs_table> TYPE STANDARD TABLE.
  DATA ls_layout TYPE lvc_s_layo.

  ls_layout-sel_mode    = |D|."
  ls_layout-zebra = abap_on. "
  ls_layout-cwidth_opt = abap_on. "
  ls_layout-info_fname = |CLR|.

  "TO_AI:When there is only single ALV, the code for displaying multiple ALVs can be omitted.
  CASE abap_on.
    WHEN rb_1."Single ALV
      gr_alv_left = NEW zcl_tabzhu_oo_alv( ).
      ASSIGN gt_alv1 TO <lfs_table>.

      CALL METHOD zcl_tabzhu_oo_alv=>get_default_fieldcat
        CHANGING
          it_data               = <lfs_table>
        RECEIVING
          rt_fieldcat           = DATA(lt_fieldcat)
        EXCEPTIONS
          error_in_get_fieldcat = 1.
      IF sy-subrc NE 0.
        CLEAR lt_fieldcat[].
        MESSAGE s208(00) WITH |Error happened when generate the fieldcat.| DISPLAY LIKE gc_e."Error happened when generate the fieldcat.
        LEAVE LIST-PROCESSING.
      ENDIF.

      LOOP AT lt_fieldcat ASSIGNING FIELD-SYMBOL(<fs>).
        DATA(lv_tabix) = sy-tabix.
        <fs>-f4availabl = abap_false.
        CASE <fs>-fieldname.
          WHEN 'MESSAGE'.
            <fs>-coltext = <fs>-reptext = <fs>-seltext = TEXT-f01."Set column headers on ALV
          WHEN OTHERS.
        ENDCASE.
      ENDLOOP.

      CALL METHOD gr_alv_left->create_oo_alv
        EXPORTING
          is_layout         = ls_layout
          it_fieldcat       = lt_fieldcat
          iv_container_name = 'CONTAINER'
          iv_variant_handle = gc_variant_1
*         i_toolbar_form    = 'FRM_HANDLE_TOOLBAR'
*         i_user_command_form = 'FRM_HANDLE_USER_COMMAND'
*         i_hotspot_form    = 'FRM_HANDLE_HOTSPOT'
*         i_double_click_form = 'FRM_HANDLE_DOUBLE_CLICK'
        CHANGING
          it_data           = <lfs_table>.


    WHEN rb_2."Double ALVs
      CALL METHOD zcl_tabzhu_oo_alv=>split_container
        EXPORTING
          iv_container_name  = 'CONTAINER'
          i_row              = 1
          i_col              = 2
        IMPORTING
          et_container_t     = DATA(lt_container_t)
          er_split_container = go_splitter.

      CHECK lt_container_t IS NOT INITIAL.

      READ TABLE lt_container_t INTO DATA(ls_container) INDEX 1.
      IF sy-subrc EQ 0.
        gr_alv_left = NEW zcl_tabzhu_oo_alv( ).
        ASSIGN gt_alv1 TO <lfs_table>.

        CLEAR lt_fieldcat.
        CALL METHOD zcl_tabzhu_oo_alv=>get_default_fieldcat
          CHANGING
            it_data               = <lfs_table>
          RECEIVING
            rt_fieldcat           = lt_fieldcat
          EXCEPTIONS
            error_in_get_fieldcat = 1.
        IF sy-subrc NE 0.
          CLEAR lt_fieldcat[].
          MESSAGE s208(00) WITH |Error happened when generate the fieldcat.| DISPLAY LIKE gc_e."Error happened when generate the fieldcat.
          LEAVE LIST-PROCESSING.
        ENDIF.

        LOOP AT lt_fieldcat ASSIGNING <fs>.
          lv_tabix = sy-tabix.
          <fs>-f4availabl = abap_false.
          CASE <fs>-fieldname.
            WHEN 'MESSAGE'.
              <fs>-coltext = <fs>-reptext = <fs>-seltext = TEXT-f01."Set column headers on ALV
            WHEN OTHERS.
          ENDCASE.
        ENDLOOP.

        CALL METHOD gr_alv_left->create_oo_alv
          EXPORTING
            is_layout          = ls_layout
            it_fieldcat        = lt_fieldcat
            iv_split_number    = 1
            iv_split_container = ls_container-container
            iv_variant_handle  = gc_variant_1
          CHANGING
            it_data            = gt_alv1.

      ENDIF.

      READ TABLE lt_container_t INTO ls_container INDEX 2.
      IF sy-subrc EQ 0.
        gr_alv_right = NEW zcl_tabzhu_oo_alv( ).
        ASSIGN gt_alv2 TO <lfs_table>.

        CLEAR lt_fieldcat.
        CALL METHOD zcl_tabzhu_oo_alv=>get_default_fieldcat
          CHANGING
            it_data               = <lfs_table>
          RECEIVING
            rt_fieldcat           = lt_fieldcat
          EXCEPTIONS
            error_in_get_fieldcat = 1.
        IF sy-subrc NE 0.
          CLEAR lt_fieldcat[].
          MESSAGE s208(00) WITH |Error happened when generate the fieldcat.| DISPLAY LIKE gc_e."Error happened when generate the fieldcat.
          LEAVE LIST-PROCESSING.
        ENDIF.

        LOOP AT lt_fieldcat ASSIGNING <fs>.
          lv_tabix = sy-tabix.
          <fs>-f4availabl = abap_false.
          CASE <fs>-fieldname.
            WHEN 'MESSAGE'.
              <fs>-coltext = <fs>-reptext = <fs>-seltext = TEXT-f01."Set column headers on ALV
            WHEN OTHERS.
          ENDCASE.
        ENDLOOP.

        CALL METHOD gr_alv_right->create_oo_alv
          EXPORTING
            is_layout          = ls_layout
            it_fieldcat        = lt_fieldcat
            iv_split_number    = 2
            iv_split_container = ls_container-container
            iv_variant_handle  = gc_variant_2
          CHANGING
            it_data            = gt_alv2.
      ENDIF.
  ENDCASE.

  CALL SCREEN 9000.

ENDFORM.
