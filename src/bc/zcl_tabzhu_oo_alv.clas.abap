CLASS zcl_tabzhu_oo_alv DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      BEGIN OF ty_splits,
        row       TYPE i,
        col       TYPE i,
        container TYPE REF TO cl_gui_container,
      END OF ty_splits .
    TYPES:
      ty_t_splits TYPE TABLE OF ty_splits .

    CONSTANTS gc_a TYPE c VALUE 'A' ##NO_TEXT.
    CONSTANTS gc_d TYPE c VALUE 'D' ##NO_TEXT.
    DATA cr_grid TYPE REF TO cl_gui_alv_grid .
    DATA cr_container TYPE REF TO cl_gui_custom_container .
    DATA split_n TYPE i .
    DATA m_repid TYPE progname .
    DATA m_f4_form TYPE string .                                         "
    DATA m_toolbar_form TYPE string .
    DATA m_hotspot_form TYPE string .
    DATA m_user_command_form TYPE string .
    DATA m_datachanged_form TYPE string .
    DATA m_datachanged_finished_form TYPE string .
    DATA m_before_ucomm_form TYPE string .
    DATA m_double_click_form TYPE string .
    DATA m_menu_button_form TYPE string .

    METHODS create_oo_alv
      IMPORTING
        VALUE(iv_repid)              TYPE progname DEFAULT sy-cprog
        VALUE(iv_screen)             TYPE sy-dynnr DEFAULT '9000'               "默认屏幕号码
        VALUE(iv_default_ex)         TYPE char1 DEFAULT 'X'           "使用默认的按钮组
        VALUE(is_layout)             TYPE lvc_s_layo OPTIONAL
        VALUE(it_fieldcat)           TYPE lvc_t_fcat OPTIONAL
        VALUE(it_exclude)            TYPE ui_functions OPTIONAL              "需要排除的按钮
        VALUE(iv_split_number)       TYPE i OPTIONAL         "分割容器号码
        VALUE(iv_split_container)    TYPE REF TO cl_gui_container OPTIONAL            "分割容器号码有值时,请传入容器类，必填
        VALUE(iv_container_name)     TYPE char30 OPTIONAL       "当分割容器号没有值时，必填
        VALUE(iv_variant_handle)     TYPE disvariant-handle DEFAULT '1'
        !i_f4_form                   TYPE string OPTIONAL
        VALUE(it_f4)                 TYPE lvc_t_f4 OPTIONAL
        !i_toolbar_form              TYPE string OPTIONAL
        !i_user_command_form         TYPE string OPTIONAL
        !i_hotspot_form              TYPE string OPTIONAL
        !i_datachanged_form          TYPE string OPTIONAL
        !i_datachanged_finished_form TYPE string OPTIONAL
        !i_before_ucomm_form         TYPE string OPTIONAL
        !i_double_click_form         TYPE string OPTIONAL
        !i_menu_button_form          TYPE string OPTIONAL
      CHANGING
        !it_data                     TYPE STANDARD TABLE .
    "分割 容器
    CLASS-METHODS split_container
      IMPORTING
        !iv_container_name  TYPE char30 OPTIONAL
        !i_row              TYPE i
        !i_col              TYPE i
        !ir_container       TYPE REF TO cl_gui_container OPTIONAL
      EXPORTING
        !et_container_t     TYPE ty_t_splits
        !er_split_container TYPE REF TO cl_gui_splitter_container.
    "设置默认排除按钮-OO ALV
    CLASS-METHODS default_exclude_func
      EXPORTING
        VALUE(it_exclude) TYPE ui_functions .
    "得到默认字段属性-根据内表
    CLASS-METHODS get_default_fieldcat
      CHANGING
        !it_data           TYPE STANDARD TABLE
      RETURNING
        VALUE(rt_fieldcat) TYPE lvc_t_fcat
      EXCEPTIONS
        error_in_get_fieldcat .
    "得到默认的Layout
    CLASS-METHODS get_default_layout
      RETURNING
        VALUE(es_layout) TYPE lvc_s_layo .
    "设置按钮
    METHODS handle_toolbar
      FOR EVENT toolbar OF cl_gui_alv_grid
      IMPORTING
        !e_object
        !e_interactive .
    "设置F4帮助
    METHODS handle_f4
      FOR EVENT onf4 OF cl_gui_alv_grid
      IMPORTING
        !e_fieldname
        !e_fieldvalue
        !es_row_no
        !er_event_data
        !et_bad_cells
        !e_display .
    "设置热点单击
    METHODS handle_hotspot
      FOR EVENT hotspot_click OF cl_gui_alv_grid
      IMPORTING
        !e_row_id
        !e_column_id
        !es_row_no .
    METHODS handle_user_command
      FOR EVENT user_command OF cl_gui_alv_grid
      IMPORTING
        !e_ucomm .
    METHODS handle_datachanged
      FOR EVENT data_changed OF cl_gui_alv_grid
      IMPORTING
        !er_data_changed
        !e_onf4
        !e_onf4_before
        !e_onf4_after
        !e_ucomm .
    METHODS handle_datachanged_finished
      FOR EVENT data_changed_finished OF cl_gui_alv_grid
      IMPORTING
        !e_modified
        !et_good_cells .
    METHODS handle_before_ucomm
      FOR EVENT before_user_command OF cl_gui_alv_grid
      IMPORTING
        !e_ucomm .
    METHODS handle_double_click
      FOR EVENT double_click OF cl_gui_alv_grid
      IMPORTING
        !e_row
        !e_column
        !es_row_no .
    METHODS handle_menu_button
      FOR EVENT menu_button OF cl_gui_alv_grid
      IMPORTING
        !e_object
        !e_ucomm .

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_TABZHU_OO_ALV IMPLEMENTATION.


  METHOD create_oo_alv.

    DATA lt_filedcat TYPE lvc_t_fcat.
    DATA ls_layout TYPE lvc_s_layo.
    "iv_split_number
    "判断是否是分割容器
    IF iv_split_number IS INITIAL.
      CREATE OBJECT cr_container
        EXPORTING
          repid          = iv_repid
          dynnr          = iv_screen
          container_name = iv_container_name. "屏幕上用户自定义控件名
* create alv
      CREATE OBJECT cr_grid
        EXPORTING
          i_parent = cr_container.
    ELSE.
      "分割容器
      CREATE OBJECT cr_grid
        EXPORTING
          i_parent = iv_split_container.
    ENDIF.

    CHECK cr_grid IS NOT INITIAL.

    DATA lt_exclude TYPE ui_functions.
    IF iv_default_ex = 'X'.
      "使用默认排除按钮
      CALL METHOD me->default_exclude_func
        IMPORTING
          it_exclude = lt_exclude.
    ENDIF.
    "手动排除按钮
    IF it_exclude[] IS NOT INITIAL.
      APPEND LINES OF it_exclude TO lt_exclude.
      SORT lt_exclude.
      DELETE ADJACENT DUPLICATES FROM lt_exclude.
    ENDIF.
    "布局
    IF is_layout IS NOT INITIAL.
      ls_layout = is_layout.
    ELSE.
      ls_layout = me->get_default_layout( ).
    ENDIF.
    "字段目录
    IF it_fieldcat[] IS NOT INITIAL.
      lt_filedcat = it_fieldcat.
    ELSE.
      CALL METHOD me->get_default_fieldcat
        CHANGING
          it_data               = it_data
        RECEIVING
          rt_fieldcat           = lt_filedcat
        EXCEPTIONS
          error_in_get_fieldcat = 1
          OTHERS                = 2.
      IF sy-subrc NE 0.
        MESSAGE e368(00) WITH 'Failed to get fiedlcat'(t02) ''.
      ENDIF.
    ENDIF.

    "变式布局
    DATA:ls_disvariant TYPE disvariant.
    ls_disvariant-report = sy-repid.
    ls_disvariant-handle = iv_variant_handle.
    ls_disvariant-username = sy-uname.

*    CALL METHOD
    CALL METHOD cr_grid->set_table_for_first_display
      EXPORTING
        is_variant           = ls_disvariant
        i_default            = abap_on
        i_save               = gc_a
        is_layout            = ls_layout
        it_toolbar_excluding = lt_exclude
      CHANGING
        it_outtab            = it_data
        it_fieldcatalog      = lt_filedcat.
*Begin of 9000021830(9400009406)
    IF i_f4_form IS SUPPLIED AND it_f4 IS NOT INITIAL.
      CALL METHOD cr_grid->register_f4_for_fields
        EXPORTING
          it_f4 = it_f4[].
    ENDIF.
*End of 9000021830(9400009406)
*SET EVENT
    DEFINE d_set_handler.
      IF &1 IS SUPPLIED.
        &2 = &1.
        SET HANDLER me->&3 FOR cr_grid.
      ENDIF.
    END-OF-DEFINITION.

    m_repid = iv_repid.

    "绑定事件
    d_set_handler:
      i_f4_form                     m_f4_form                     handle_f4,
      i_toolbar_form                m_toolbar_form                handle_toolbar,
      i_user_command_form           m_user_command_form           handle_user_command,
      i_hotspot_form                m_hotspot_form                handle_hotspot,
      i_datachanged_form            m_datachanged_form            handle_datachanged,
      i_datachanged_finished_form   m_datachanged_finished_form   handle_datachanged_finished,
      i_before_ucomm_form           m_before_ucomm_form           handle_before_ucomm,
      i_double_click_form           m_double_click_form           handle_double_click,
      i_menu_button_form            m_menu_button_form            handle_menu_button.

*    SET HANDLER me->handle_toolbar       FOR cr_grid.
*    SET HANDLER me->handle_datachanged_finished  FOR cr_grid.
*    SET HANDLER me->handle_datachanged FOR cr_grid.
*    SET HANDLER me->handle_user_command  FOR cr_grid.
*    SET HANDLER me->handle_hotspot FOR cr_grid .

    CALL METHOD cr_grid->set_toolbar_interactive.
* register enter event
    CALL METHOD cr_grid->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_enter
      EXCEPTIONS
        error      = 1
        OTHERS     = 2.
    IF sy-subrc NE 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1
      sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    CALL METHOD cr_grid->register_edit_event     "register modify event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_modified
      EXCEPTIONS
        error      = 1
        OTHERS     = 2.
    IF sy-subrc NE 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1
      sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ENDMETHOD.


  METHOD default_exclude_func.
    DATA: ls_exclude TYPE ui_func.
    ls_exclude = cl_gui_alv_grid=>mc_fc_loc_copy .
    APPEND ls_exclude TO it_exclude.
    ls_exclude = cl_gui_alv_grid=>mc_fc_loc_copy_row .
    APPEND ls_exclude TO it_exclude.
    ls_exclude = cl_gui_alv_grid=>mc_fc_loc_append_row.
    APPEND ls_exclude TO it_exclude.
    ls_exclude = cl_gui_alv_grid=>mc_fc_loc_cut.
    APPEND ls_exclude TO it_exclude.
    ls_exclude = cl_gui_alv_grid=>mc_fc_loc_delete_row.
    APPEND ls_exclude TO it_exclude.
    ls_exclude = cl_gui_alv_grid=>mc_fc_loc_insert_row.
    APPEND ls_exclude TO it_exclude.
    ls_exclude = cl_gui_alv_grid=>mc_fc_loc_move_row.
    APPEND ls_exclude TO it_exclude.
    ls_exclude = cl_gui_alv_grid=>mc_fc_loc_paste.
    APPEND ls_exclude TO it_exclude.
    ls_exclude = cl_gui_alv_grid=>mc_fc_loc_paste_new_row.
    APPEND ls_exclude TO it_exclude.
    ls_exclude = cl_gui_alv_grid=>mc_fc_loc_undo.
    APPEND ls_exclude TO it_exclude.
    ls_exclude = cl_gui_alv_grid=>mc_fc_maximum .
    APPEND ls_exclude TO it_exclude.
    ls_exclude = cl_gui_alv_grid=>mc_fc_minimum .
    APPEND ls_exclude TO it_exclude.
*  ls_exclude = cl_gui_alv_grid=>mc_fc_view_excel.
*  append ls_exclude to it_exclude.
    ls_exclude = cl_gui_alv_grid=>mc_fc_view_lotus .
    APPEND ls_exclude TO it_exclude.
*    LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_MB_EXPORT .
*    APPEND LS_EXCLUDE TO IT_EXCLUDE.
*    ls_exclude = cl_gui_alv_grid=>mc_fc_sum .
*    APPEND ls_exclude TO it_exclude.
*    ls_exclude = cl_gui_alv_grid=>mc_fc_average .
*    APPEND ls_exclude TO it_exclude.
*  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_MB_SUBTOT .
*  APPEND LS_EXCLUDE TO it_exclude.
    ls_exclude = cl_gui_alv_grid=>mc_fc_check.
    APPEND ls_exclude TO it_exclude.
    ls_exclude = cl_gui_alv_grid=>mc_fc_refresh.
    APPEND ls_exclude TO it_exclude.

*  ls_exclude = '&LOAD'.
*  append ls_exclude to it_exclude.
*  LS_EXCLUDE = '&COL0'.
*  APPEND LS_EXCLUDE TO it_exclude.
*  LS_EXCLUDE = '&SAVE '.
*  APPEND LS_EXCLUDE TO it_exclude.
*  LS_EXCLUDE = '&MAINTAIN '.
*  APPEND LS_EXCLUDE TO IT_EXCLUDE.
    ls_exclude = cl_gui_alv_grid=>mc_fc_detail.
    APPEND ls_exclude TO it_exclude.
    ls_exclude = cl_gui_alv_grid=>mc_fc_print .
    APPEND ls_exclude TO it_exclude.
    ls_exclude = '&INFO'.
    APPEND ls_exclude TO it_exclude.
    ls_exclude = '&GRAPH'.
    APPEND ls_exclude TO it_exclude.
  ENDMETHOD.


  METHOD get_default_fieldcat.
    DATA: salv_table TYPE REF TO cl_salv_table.
    TRY.
        cl_salv_table=>factory( IMPORTING
                                  r_salv_table   = salv_table
                                CHANGING
                                  t_table        = it_data[] ).
        rt_fieldcat = cl_salv_controller_metadata=>get_lvc_fieldcatalog(
            r_columns      = salv_table->get_columns( ) " ALV Filter
            r_aggregations = salv_table->get_aggregations( ) " ALV Aggregations
    ).
      CATCH cx_root INTO DATA(lx_root).
        DATA(lv_text) = lx_root->get_text( ).
        RAISE error_in_get_fieldcat.
    ENDTRY.

    "当你的内表字段不是参考数据库结构/表创建的时候,F4搜索帮助有图标但是无效
    "有2种处理方法
    "1. F4AVAILABL = abap_false
    "2. 加上：REF_TABLE ，REF_FIELD


  ENDMETHOD.


  METHOD get_default_layout.
    es_layout-sel_mode    = gc_d ."
    es_layout-zebra = abap_on. "
    es_layout-cwidth_opt = abap_on. "
  ENDMETHOD.


  METHOD handle_before_ucomm.
    PERFORM (m_before_ucomm_form) IN PROGRAM (m_repid) IF FOUND
      USING e_ucomm.
*FORM 示例
*FORM handle_before_ucomm USING e_ucomm TYPE sy-ucomm.
*ENDFORM.                    "handle_before_ucomm
  ENDMETHOD.


  METHOD handle_datachanged.
    PERFORM (m_datachanged_form) IN PROGRAM (m_repid) IF FOUND
          USING er_data_changed e_onf4 e_onf4_before e_onf4_after e_ucomm.
*FORM 示例
*FORM handle_datachanged_cpmx USING  er_data_changed TYPE REF TO cl_alv_changed_data_protocol
*                                    e_onf4 TYPE char01
*                                    e_onf4_before TYPE char01
*                                    e_onf4_after TYPE char01
*                                    e_ucomm TYPE sy-ucomm.
*  FIELD-SYMBOLS: <ls_cell> TYPE lvc_s_modi.
*  DATA: ls_cpmx TYPE ty_cpmx,
*        lt_mod_cells TYPE lvc_t_modi WITH HEADER LINE.
*
*  CLEAR: g_error.
*
*  DEFINE add_protocol_entry.
*    g_error = 'X'.
*    call method er_data_changed->add_protocol_entry
*      exporting
*        i_msgid     = &1
*        i_msgty     = &2
*        i_msgno     = &3
*        i_msgv1     = &4
*        i_msgv2     = &5
*        i_msgv3     = &6
*        i_msgv4     = &7
*        i_fieldname = <ls_cell>-fieldname
*        i_row_id    = <ls_cell>-row_id
*        i_tabix     = <ls_cell>-tabix.
*  END-OF-DEFINITION.
*  DEFINE modify_cell.
*    call method er_data_changed->modify_cell
*      exporting
*        i_row_id    = <ls_cell>-row_id
*        i_tabix     = <ls_cell>-tabix
*        i_fieldname = &1
*        i_value     = &2.
*  END-OF-DEFINITION.
*
*  LOOP AT er_data_changed->mt_mod_cells ASSIGNING <ls_cell>.
*    CASE <ls_cell>-fieldname.
*      WHEN 'BUKRS'.  "公司代码描述
*        CLEAR gs_t001.
*        READ TABLE gt_t001 INTO gs_001 WITH KEY bukrs = <ls_cell>-value.
*        IF sy-subrc <> 0 AND <ls_cell>-value IS NOT INITIAL.
*          add_protocol_entry: '00' 'E' '001' '公司代码:' <ls_cell>-value '不存在' ''.
*        ENDIF.
*        modify_cell: 'BUTXT' gs_001-butxt.
*    ENDCASE.
*  ENDLOOP.
  ENDMETHOD.


  METHOD handle_datachanged_finished.
    PERFORM (m_datachanged_finished_form) IN PROGRAM (m_repid) IF FOUND
  USING e_modified et_good_cells.
*FORM 示例
*FORM handle_datachanged_finished USING  e_modified TYPE char01
*                                        et_good_cells TYPE lvc_t_modi.
*ENDFORM.                    "HANDLE_DATACHANGED_FINISHED
  ENDMETHOD.


  METHOD handle_double_click.
    PERFORM (m_double_click_form) IN PROGRAM (m_repid) IF FOUND
      USING e_row e_column es_row_no.
*FORM 示例
*FORM handle_double_click USING  e_row TYPE lvc_s_row
*                                e_column TYPE lvc_s_col
*                                es_row_no TYPE lvc_s_roid.
*ENDFORM.                    "handle_before_ucomm
  ENDMETHOD.


  METHOD handle_f4.
    PERFORM (m_f4_form) IN PROGRAM (m_repid) IF FOUND
    USING e_fieldname e_fieldvalue es_row_no er_event_data et_bad_cells e_display.
*FORM 示例
*FORM handle_f4  USING           e_fieldname TYPE lvc_fname
*                                e_fieldvalue TYPE lvc_value
*                                es_row_no TYPE lvc_s_roid
*                                er_event_data TYPE REF TO cl_alv_event_data
*                                et_bad_cells TYPE lvc_t_modi
*                                e_display TYPE char01.
*  DATA: ls_row TYPE lvc_s_row,
*        ls_col TYPE lvc_s_col,
*        ls_modi TYPE lvc_s_modi,
*        l_tabname TYPE tabname,
*        l_fieldtext TYPE fieldtext,
*        l_ref_table TYPE lvc_rtname,
*        l_ref_field TYPE lvc_rfname.
*  FIELD-SYMBOLS: <lt_modi> TYPE lvc_t_modi.
*
*  er_event_data->m_event_handled = 'X'.
*
*  CASE e_fieldname.
*    WHEN 'TABNAME'.
*      PERFORM f4_dd_table(rsaqddic) USING 'SAPLAQJD_CNTRL'
*                                          '0300'
*                                          'G_DYN_0300-TNAME'
*                                    CHANGING e_fieldvalue.  "搜索帮助代码，来于SQVI中“插入表”的搜索帮助
*    WHEN OTHERS.
*      EXIT.
*  ENDCASE.
*
*  ASSIGN er_event_data->m_data->* TO <lt_modi>.
*  ls_modi-row_id    = es_row_no-row_id."
*  ls_modi-fieldname = e_fieldname.
*  ls_modi-value     = e_fieldvalue.
*  APPEND ls_modi TO <lt_modi>.
*  IF e_fieldname = 'FIELDNAME'.
*    ls_modi-row_id    = es_row_no-row_id."
*    ls_modi-fieldname = 'SCRTEXT_L'.
*    ls_modi-value     = l_fieldtext.
*    APPEND ls_modi TO <lt_modi>.
*
*    ls_modi-row_id    = es_row_no-row_id."
*    ls_modi-fieldname = 'REF_TABLE'.
*    ls_modi-value     = l_ref_table.
*    APPEND ls_modi TO <lt_modi>.
*
*    ls_modi-row_id    = es_row_no-row_id."
*    ls_modi-fieldname = 'REF_FIELD'.
*    ls_modi-value     = l_ref_field.
*    APPEND ls_modi TO <lt_modi>.
*  ENDIF.
*
*ENDFORM.
  ENDMETHOD.


  METHOD handle_hotspot.
    PERFORM (m_hotspot_form) IN PROGRAM (m_repid) IF FOUND
      USING e_row_id e_column_id es_row_no.
*FORM 示例
*FORM handle_hotspot  USING    p_row_id     TYPE lvc_s_row
*                              p_column_id  TYPE lvc_s_col
*                              p_row_no     TYPE lvc_s_roid.
*  CASE p_column_id-fieldname.
*    WHEN 'COUNT'.
*  ENDCASE.
*ENDFORM.
  ENDMETHOD.                    "handle_hotspot


  METHOD handle_menu_button.
    PERFORM (m_menu_button_form) IN PROGRAM (m_repid) IF FOUND USING e_object e_ucomm.
*FORM 示例
*FORM handle_menu_button  USING  e_object TYPE REF TO cl_ctmenu
*                                e_ucomm TYPE sy-ucomm.
*  CASE e_ucomm.
*    WHEN 'MENU'.
*      CALL METHOD e_object->add_function
*        EXPORTING
*          fcode = 'MENU1'
*          text  = '菜单1'.
*  ENDCASE.
*ENDFORM.                    "handle_menu_button
  ENDMETHOD.


  METHOD handle_toolbar.
    PERFORM (m_toolbar_form) IN PROGRAM (m_repid) IF FOUND USING e_object e_interactive.
*FORM 示例
*FORM handle_toolbar USING   e_object TYPE REF TO cl_alv_event_toolbar_set
*                            e_interactive TYPE char1.
*  DATA: ls_toolbar TYPE stb_button.
*
*  ls_toolbar-function = 'IMPORT'.
*  ls_toolbar-icon = icon_import.
*  ls_toolbar-text = '导入表格字段'.
*  ls_toolbar-quickinfo = '导入表格字段'.
*  APPEND ls_toolbar TO e_object->mt_toolbar.
*  CLEAR: ls_toolbar.
*
*  ls_toolbar-function = 'ALL'.
*  ls_toolbar-text = '输出：全选'.
*  ls_toolbar-quickinfo = '全部输出'.
*  APPEND ls_toolbar TO e_object->mt_toolbar.
*  CLEAR: ls_toolbar.
*
*  ls_toolbar-function = 'NONE'.
*  ls_toolbar-text = '输出：取消全选'.
*  ls_toolbar-quickinfo = '全部不输出'.
*  APPEND ls_toolbar TO e_object->mt_toolbar.
*  CLEAR: ls_toolbar.
*ENDFORM.
  ENDMETHOD.


  METHOD handle_user_command.
    PERFORM (m_user_command_form) IN PROGRAM (m_repid) IF FOUND USING e_ucomm.
*FORM 示例
*FORM handle_user_command  USING    e_ucomm TYPE sy-ucomm.
*  DATA: ok_code TYPE sy-ucomm.
*  ok_code = e_ucomm.
*  CLEAR e_ucomm.
*
*  CASE ok_code.
*    WHEN 'IMPORT'.
*      go_alv->refresh_table_display( ).
*  ENDCASE.
*ENDFORM.
  ENDMETHOD.


  METHOD split_container.
    DATA lv_row TYPE i.
    DATA lv_col TYPE i.
    DATA ls_splits TYPE ty_splits.

    IF iv_container_name IS NOT INITIAL.
      DATA(lo_cc) = NEW cl_gui_custom_container( container_name = iv_container_name ).
      IF lo_cc IS BOUND.
        DATA(lo_split) = NEW cl_gui_splitter_container( parent = lo_cc rows = i_row columns = i_col ).
      ENDIF.
    ELSE.
      lo_split =  NEW cl_gui_splitter_container( parent = ir_container rows = i_row columns = i_col ).
    ENDIF.

    er_split_container  = lo_split.

    lv_row = 1.
    lv_col = 1.

    DO.
      DATA(lo_split_cc1) = lo_split->get_container( EXPORTING
      row       = lv_row
      column    = lv_col
      ).
      CHECK lo_split_cc1 IS BOUND.
      ls_splits-row = lv_row.
      ls_splits-col = lv_col.
      ls_splits-container = lo_split_cc1.
      APPEND ls_splits TO et_container_t.
      CLEAR ls_splits.

      IF lv_col < i_col.
        lv_col = lv_col + 1.
      ELSE.
        "lv_col = i_col
        IF lv_row < i_row.
          lv_row = lv_row + 1.
        ELSE.
          EXIT.
        ENDIF.
      ENDIF.
      CLEAR lo_split_cc1.
    ENDDO.
  ENDMETHOD.
ENDCLASS.
