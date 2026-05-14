gv_data =
'AR1USyBFbGV2YXRvciBTYXVkaSBBcmFiaWEgTHRkLgIPMzAwMDU1NTc5NjAwM'
&& 'DAzAxMyMDI0LTA4LTE5VDA4OjM0OjU5BAcyOTMyLjUwBQYzODIuNTAGLFpOd3hPa2YrWmF'
&& 'MdE5mc056YlAyM3dpWWJ2eGpIY2d2cFlESVFWcUVBcE09B2BNRVVDSUNnU0NlN'
&& 'EFkaDZ3YUREYVhtWERQZlVJOHBtZG9ROHM5amdRelJvY2M2TVJBaUVBOTc1WWhBOTU0djV'
&& 'qYU12TEtBZURSa0dyYjFldG5lQVYyN0huL2lldDR0QT0IWDBWMBAGByqGSM49'
&& 'AgEGBSuBBAAKA0IABEBPmRnHhlqTPa4fAB8kEMRV9JghSiywBzuuWW5clou+d3DM8WeSu2t'
&& 'eYqejoV7ZqIGbm+8i6yzGGgCovIgz7TI='.

DATA:lv_len TYPE i.
DATA:lv_len_left TYPE i.
lv_len = strlen( gv_data ).
IF lv_len < 250.
  iv_sn_str1 = gv_data.
ELSE.
  lv_len_left = lv_len - 250.
  iv_sn_str1 = gv_data+0(250).
  iv_sn_str2 = gv_data+250(lv_len_left).
ENDIF.

DATA(lv_data) = |{ gv_data  }|.

DATA ls_text_line TYPE string.
DATA lt_text_lines TYPE STANDARD TABLE OF string.
ls_text_line = |{ lv_data }|.
APPEND ls_text_line TO lt_text_lines.
CALL FUNCTION 'CONVERT_STREAM_TO_ITF_TEXT'
  EXPORTING
    stream_lines = lt_text_lines
    lf           = 'X'
  TABLES
    itf_text     = gt_longtext.
READ TABLE gt_longtext ASSIGNING FIELD-SYMBOL(<fs_1>)
INDEX 1.
IF sy-subrc EQ 0.
  <fs_1>-tdformat = '*'.
ENDIF.



