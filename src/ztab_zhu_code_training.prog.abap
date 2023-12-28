*&---------------------------------------------------------------------*
*& Report ZTAB_ZHU_CODE_TRAINING
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ZTAB_ZHU_CODE_TRAINING.
TYPES: BEGIN OF ty_ship,
         tknum TYPE tknum, "Shipment Number
         name  TYPE ernam, "Name of Person who Created the Object
         city  TYPE ort01, "Starting city
         route TYPE route, "Shipment route
         age   TYPE i,
       END OF ty_ship.
TYPES: ty_ships TYPE SORTED TABLE OF ty_ship WITH UNIQUE KEY tknum.
TYPES: ty_citys TYPE STANDARD TABLE OF ort01 WITH EMPTY KEY.

CLASS zcl_tab_zhu_test_01 DEFINITION.
  PUBLIC SECTION.
    METHODS test_01
      IMPORTING
        iv_string TYPE string.

  PROTECTED SECTION.

  PRIVATE SECTION.

ENDCLASS.

CLASS zcl_tab_zhu_test_01 IMPLEMENTATION.
  METHOD test_01.

  ENDMETHOD.
ENDCLASS.

"条件语句 转化
CLASS cx_cant_be DEFINITION INHERITING FROM cx_no_check.

ENDCLASS.

CLASS cx_overflow DEFINITION INHERITING FROM cx_static_check.
*  PUBLIC SECTION.
*    METHODS write_error.
ENDCLASS.

*CLASS cx_overflow IMPLEMENTATION.
*  METHOD  write_error.
*    WRITE:/ 'ERROR'.
*  ENDMETHOD.
*ENDCLASS.

START-OF-SELECTION.

  DATA(gt_ships) = VALUE ty_ships(
( tknum = '001' name = 'John' city = 'Shanghai' route = 'R0001'  age = 18 )
( tknum = '002' name = 'Gavin' city = 'Shenzhen' route = 'R0002' age = 25 )
( tknum = '003' name = 'Lucy' city = 'Shanghai' route = 'R0001' age = 26 )
( tknum = '004' name = 'Tab' city = 'Hangzhou' route = 'R0003' age = 27 )
       ).

  "得到citys 字段值 到内表 gt_citys中  ls_ship 会隐式定义
  DATA(gt_citys) =    VALUE  ty_citys( FOR ls_ship IN gt_ships ( ls_ship-city ) ).

  "得到 当route = 'R0001' 时 citys 字段值 到内表 gt_citys中  ls_ship 会隐式定义
  DATA(gt_citys_r0001) = VALUE ty_citys( FOR ls_ship IN gt_ships WHERE ( route = 'R0001' ) ( ls_ship-city ) ).

  TYPES outref TYPE REF TO if_demo_output.
  " 将 gt_ships 实例化 参考的是 if_demo_output.
  DATA(output) = REDUCE outref( INIT out = cl_demo_output=>new( )
                                FOR wa IN gt_ships " where
                                NEXT out = out->write( wa ) ).

  "得到ROUTE=R0001 时 lv_lines 的数量有多少条
  DATA(lv_lines_r0001) = REDUCE i( INIT x = 0 FOR wa IN gt_ships WHERE ( route = 'R0001' ) NEXT x = x + 1 ).
*WRITE:/ lv_lines_r0001.
  output->write( lv_lines_r0001 ).
  "得到route = r0001 时 age 的和
  DATA(lv_sum_age_r0001) = REDUCE i( INIT x = 0 FOR wa IN gt_ships WHERE ( route = 'R0001') NEXT x = x + wa-age ).
*WRITE:/ lv_sum_age_r0001.
  output->write( lv_sum_age_r0001 ).
  "暂时注释
*output->display( ).

  DATA(time) =
    COND string(
        LET t = '120000' IN
      WHEN sy-timlo < '120000' THEN
        |{ sy-timlo TIME = ISO } AM|
      WHEN sy-timlo > '120000' THEN
        |{ CONV t( sy-timlo - 12 * 3600 ) TIME = ISO } PM |
      WHEN sy-timlo = '120000' THEN
        |High Noon|
      ELSE
        THROW cx_cant_be( ) ).

  DATA(age_level) =
    COND string(
    LET avg = 20 IN
    WHEN gt_ships[ 1 ]-age < avg THEN
     |{ '青少年' }|
    WHEN gt_ships[ 1 ]-age > avg THEN
     |{ '青年' }|
    WHEN gt_ships[ 1 ]-age = avg THEN
     |20岁的年轻人|
    ELSE
      THROW cx_cant_be( )
    ).

*WRITE:/ time,age_level."like 01:39:00 PM
  output->write( |{ time } { age_level } | ).

  output->write( '测试Switch' ).
  TRY.
      output->write( SWITCH #( sy-langu
              WHEN 'D' THEN `DE`
              WHEN 'E' THEN `EN`
              WHEN '1' THEN 'ZH'
               ELSE THROW cx_overflow( ) ) ).
    CATCH cx_overflow.
      WRITE:/ 'ERROR'.
      EXIT.
  ENDTRY.

  output->write( '测试CORRESPONDING' ).
  PERFORM frm_correspond_operator.

  PERFORM frm_string.

  PERFORM frm_loop_group_by.

  PERFORM frm_class.

  PERFORM frm_meshes.

  PERFORM frm_filter.

  PERFORM frm_sql.

  output->display( ).

  INCLUDE ztab_zhu_code_training_f01.
