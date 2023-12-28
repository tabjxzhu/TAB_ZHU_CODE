*&---------------------------------------------------------------------*
*& Include ztab_zhu_code_training_f01
*&---------------------------------------------------------------------*
CLASS my_class DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS get_name IMPORTING line      TYPE i
                           RETURNING VALUE(wa) TYPE ty_ship.
*                     CHANGING name.
ENDCLASS.
CLASS my_class IMPLEMENTATION.
  METHOD get_name .
    wa = gt_ships[ line ].
  ENDMETHOD.
ENDCLASS.
*&---------------------------------------------------------------------*
*& Form FRM_CORRESPOND_OPERATOR
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_correspond_operator .
  TYPES: BEGIN OF lty_age,
*         tknum TYPE tknum,     "Shipment Number
           name TYPE ernam,     "Name of Person who Created the Object
*         city  TYPE ort01,     "Starting city
*         route TYPE route,     "Shipment route
           age  TYPE i,
         END OF lty_age.

  TYPES:BEGIN OF lty_city,
          name  TYPE ernam,     "Name of Person who Created the Object
          route TYPE route,     "Shipment route
          age   TYPE i,
        END OF lty_city.
  DATA(ls_age_info) = CORRESPONDING lty_age( gt_ships[ tknum = '001' ] ).


  DATA(ls_ship) = VALUE #( gt_ships[ 4 ] ).

  "将LS_SHIP 给 ls_city_info  同时用 ls_age_info 来代替 相对应的值
  DATA(ls_city_info) = CORRESPONDING ty_ship( BASE ( ls_ship ) ls_age_info ).

*  WRITE:/ ls_age_info-name,ls_age_info-age , ls_ship , ls_city_info.

*  data(out_correspond) = cl_demo_output=>new( ).

  CALL METHOD output->write( ls_age_info ).
  CALL METHOD output->write( ls_ship ).
  CALL METHOD output->write( ls_city_info ).

*  CALL METHOD out_correspond->display( ).
ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_STRING
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_string .
  "直接输出
  output->write('直接输出字符串').
  output->write( |Info: { gt_ships[ tknum = '004' ]-name }|  ).
ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_LOOP_GROUP_BY
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_loop_group_by .
  DATA lv_sum_age TYPE i.
  DATA lv_ave_age TYPE i.
  DATA(lt_ship) = gt_ships[].
  output->write('loop at group by的使用').
  LOOP AT lt_ship INTO DATA(ls_ship)
       GROUP BY ( route = ls_ship-route
                  size = GROUP SIZE
                  index = GROUP INDEX )
       ASCENDING
       ASSIGNING FIELD-SYMBOL(<group>).

    output->write( | Group: { <group>-index }    Role: { <group>-route WIDTH = 15 }|

              & | Number in this route: { <group>-size }| ).
    CLEAR lv_sum_age.
    LOOP AT GROUP <group>  ASSIGNING FIELD-SYMBOL(<ls_member>).
      lv_sum_age = lv_sum_age + <ls_member>-age.
      output->write( <ls_member>-name ).
    ENDLOOP.

    output->write( | Route平均年龄为:' { lv_sum_age / <group>-size } |   ).
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_CLASS
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_class .
  DATA(lv_name) = my_class=>get_name( 1 )-name.

  output->write( | 静态方法直接输出名字： { lv_name } | ).

  "直接申明时使用
  DATA(ls_ship) = NEW my_class( )->get_name( 4 ).

  output->write( ls_ship ).
ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_MESHES
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_meshes .
  TYPES: BEGIN OF t_manager,
           name   TYPE char10,
           salary TYPE int4,
         END OF t_manager,
         tt_manager TYPE SORTED TABLE OF t_manager WITH UNIQUE KEY name.
  TYPES: BEGIN OF t_developer,
           name    TYPE char10,
           salary  TYPE int4,
           manager TYPE char10,   "name of manager
         END OF t_developer,
         tt_developer TYPE SORTED TABLE OF t_developer WITH UNIQUE KEY name.

  TYPES:BEGIN OF MESH m_team,
          managers   TYPE tt_manager ASSOCIATION my_employee TO developers
                                    ON manager = name,
          developers TYPE tt_developer ASSOCIATION my_manager TO managers
                                  ON name = manager,
        END OF MESH m_team.

  "FILL VALUE
  DATA(lt_developer) = VALUE tt_developer(
    ( name = 'Bob' salary = '2100' manager = 'Jason' )
    ( name = 'David' salary = '2000' manager = 'Thomas' )
    ( name = 'Jack' salary = '1100' manager = 'Thomas' )
    ( name = 'Jerry' salary = '3100' manager = 'Jason' )
    ( name = 'John' salary = '1100' manager = 'Thomas' )
    ( name = 'Tom' salary = '1200' manager = 'Jason' )
    ( name = 'Tab' salary = '6600' manager = 'Thomas' )
   ).

  DATA(lt_manager) = VALUE tt_manager(
  ( name = 'Jason' salary = '8800' )
  ( name = 'Thomas' salary = '8900' )
   ).


*Get details of Jerry’s manager *
  DATA ls_team TYPE m_team.
  ls_team-managers   = lt_manager.
  ls_team-developers = lt_developer.
  "get line of dev table
  ASSIGN lt_developer[ name = 'Jerry' ] TO FIELD-SYMBOL(<ls_jerry>).
  DATA(ls_jmanager) =  ls_team-developers\my_manager[ <ls_jerry> ].
  "找到 jerry 的 经理
  output->write( |Jerry‘s manager: { ls_jmanager-name } Salary: { ls_jmanager-salary }| ).

  output->write( |Thomas‘ developers: | )
  .
  ASSIGN lt_manager[ name = 'Thomas' ] TO FIELD-SYMBOL(<ls_thomas>).
  "找到 Thomas 的下属
  LOOP AT ls_team-managers\my_employee[ <ls_thomas> ]
          ASSIGNING FIELD-SYMBOL(<ls_emp>).
    output->write( |Employee name: { <ls_emp>-name }| ).
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_FILTER
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_filter .
  TYPES:BEGIN OF lty_ship,
          city  TYPE ort01,     "Starting city
          route TYPE route,     "Shipment route
        END OF lty_ship.
  TYPES:ty_filter_tab TYPE HASHED TABLE OF lty_ship WITH UNIQUE KEY city route.

  DATA(lt_filter) = VALUE ty_filter_tab(
               ( city = 'Shanghai' route = 'R0001' )
               ( city = 'Hangzhou' route = 'R0003' )
               ).

  DATA(lt_myrecs) = FILTER #( gt_ships IN lt_filter
                                  WHERE city = city
                                    AND route = route ).
*“Output filtered records

  LOOP AT lt_myrecs ASSIGNING FIELD-SYMBOL(<ls_rec>).
    output->write( | ID: { <ls_rec>-tknum } name: { <ls_rec>-name } | ).
  ENDLOOP.

ENDFORM.
FORM frm_sql.

"For all entries in
  WITH +connections AS ( SELECT FROM spfli FIELDS carrid,connid WHERE cityfrom = 'NEW YORK' )
    SELECT FROM +connections INNER JOIN sbook AS a
    ON +connections~carrid = a~carrid AND +connections~connid = a~connid
    FIELDS +connections~carrid,+connections~connid,a~fldate,a~bookid,a~customid,a~custtype
    INTO TABLE @DATA(lt_sbook_new).

"Right Join

  SELECT a~agencynum,a~name,a~city,b~loccurkey,SUM( b~loccuram ) AS paysum
      FROM stravelag AS a
      RIGHT OUTER JOIN sbook AS b ON a~agencynum = b~agencynum
      INTO TABLE @DATA(lt_agency)
      GROUP BY a~agencynum,a~name,a~city,b~loccurkey
      ORDER BY a~agencynum,b~loccurkey.
  IF sy-subrc EQ 0.

  ENDIF.

  "Union

  SELECT agencynum,name,street,city,country FROM stravelag
   UNION
  SELECT id AS agencynum,name,street,city,country FROM scustom
    INTO TABLE @DATA(lt_union).
  IF sy-subrc EQ 0.

  ENDIF.

  "Join Inner Table

  SELECT carrid,connid
  INTO TABLE @DATA(lt_connections)
  FROM spfli
  WHERE cityfrom = 'NEW YORK'.
  IF sy-subrc EQ 0.
    SELECT
     FROM sbook AS a
     INNER JOIN @lt_connections AS b ON b~carrid = a~carrid AND b~connid = a~connid
     FIELDS b~carrid,b~connid,a~fldate,a~bookid,a~customid,a~custtype
     INTO TABLE @DATA(lt_sbook_join_inner).
    IF sy-subrc EQ 0.

    ENDIF.
  ENDIF.

  SELECT a~carrid,a~carrname,b~loccurkey,t~ktext
     FROM scarr AS a LEFT OUTER JOIN (
     sbook AS b LEFT OUTER JOIN tcurt AS t ON
                                b~loccurkey = t~waers
                            AND spras = @sy-langu )
     ON a~carrid = b~carrid
     WHERE b~cancelled <> 'X'
     OR b~cancelled IS NULL
     GROUP BY a~carrid,a~carrname,b~loccurkey,t~ktext
     ORDER BY a~carrid
     INTO TABLE @DATA(lt_bookings).
  IF sy-subrc EQ 0.

  ENDIF.

ENDFORM.
