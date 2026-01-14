*&---------------------------------------------------------------------*
*& Report ZA_JSON
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ztool_json.
DATA: LO_WRITER  TYPE REF TO CL_SXML_STRING_WRITER,
      LV_XSTRING TYPE XSTRING.
DATA lv_string TYPE string.
DATA lv_string1 TYPE string.
DATA lv_string2 TYPE string.
DATA EV_JSON_STRING TYPE string.
TYPES:BEGIN OF ty_itab,
        data TYPE ZTCOS0004,
      END OF ty_itab.
*DATA git_itab TYPE TABLE OF ty_itab WITH HEADER LINE.
*DATA git_itab1 TYPE TABLE OF ty_itab.
DATA:lv_param_table TYPE TABLE OF zcos0004.
DATA: BEGIN OF git_itab OCCURS 0,
        data TYPE TABLE OF zcos0004,
      END OF git_itab.
DATA: BEGIN OF git_itab1 OCCURS 0,
        data TYPE TABLE OF zcos0004,
      END OF git_itab1.
      DATA: BEGIN OF git_itab2 OCCURS 0,
        data TYPE TABLE OF zcos0004,
      END OF git_itab2.
      DATA: BEGIN OF gs_itab ,
        data TYPE TABLE OF zcos0004,
      END OF gs_itab.
SELECT a~aufnr,
 a~werks,
 b~matnr,
*         B~ZLOTID,
 a~user6,
 a~aufex,

 b~meins
FROM aufk AS a
INNER JOIN afpo AS b
ON a~aufnr = b~aufnr

WHERE a~werks = '1710'
INTO CORRESPONDING FIELDS OF TABLE @lv_param_table UP TO 2 ROWS.
git_itab-data = lv_param_table.
APPEND git_itab.
gs_itab-data = CORRESPONDING #( lv_param_table ).
/ui2/cl_json=>serialize( EXPORTING data = git_itab[]         "序列化数据
                             compress =  ''         "是否跳过空元素
                             numc_as_string   = ''   "数量转换成字符
                             pretty_name  = ''     "详细用法在下方描述 "详细用法在下方描述 L 小写 空 大写 X 驼峰
                            " name_mappings    = lt_name_mappings "详细使用在下面描述
                   RECEIVING r_json = lv_string     ). "返回JSON字符
/ui2/cl_json=>serialize( EXPORTING data = git_itab-data         "序列化数据
                             compress =  ''         "是否跳过空元素
                             numc_as_string   = ''   "数量转换成字符
                             pretty_name  = ''     "详细用法在下方描述 "详细用法在下方描述 L 小写 空 大写 X 驼峰
                            " name_mappings    = lt_name_mappings "详细使用在下面描述
                   RECEIVING r_json = lv_string1     ). "返回JSON字符
/ui2/cl_json=>deserialize( EXPORTING json        = lv_string
                                   "  pretty_name = 'X'
                                     "name_mappings = lt_mappings
                           CHANGING  data        = git_itab1[] ).
*LOOP AT git_itab1.
*cl_demo_output=>display( git_itab1-data ) .
*ENDLOOP.

WRITE lv_string1.
WRITE lv_string.
" 检查是否有表数据
*IF lV_PARAM_TABLE IS NOT INITIAL.
IF gs_itab-data IS NOT INITIAL.
  TRY.
      LO_WRITER = CL_SXML_STRING_WRITER=>CREATE( TYPE = IF_SXML=>CO_XT_JSON ).
*      CALL TRANSFORMATION ID SOURCE DATA = lV_PARAM_TABLE RESULT XML LO_WRITER.
      CALL TRANSFORMATION ID SOURCE DATA = gs_itab-data RESULT XML LO_WRITER.
      LV_XSTRING = LO_WRITER->GET_OUTPUT( ).
      EV_JSON_STRING = CL_ABAP_CODEPAGE=>CONVERT_FROM( LV_XSTRING ).
    CATCH CX_ROOT.
      EV_JSON_STRING = '{"ERROR": "表参数转换失败"}'.
  ENDTRY.
"  EV_JSON_STRING = '[' && EV_JSON_STRING && ']'.
  /ui2/cl_json=>deserialize( EXPORTING json        = EV_JSON_STRING
                                   "  pretty_name = 'X'
                                     "name_mappings = lt_mappings
                           CHANGING  data        = git_itab2[] ).
  ENDIF.
*  LOOP AT git_itab2.
*cl_demo_output=>display( git_itab2-data ) .
*ENDLOOP.
  /ui2/cl_json=>serialize( EXPORTING data = gs_itab        "序列化数据
                             compress =  ''         "是否跳过空元素
                             numc_as_string   = 'X'   "数量转换成字符
                             pretty_name  = ''     "详细用法在下方描述 "详细用法在下方描述 L 小写 空 大写 X 驼峰
                            " name_mappings    = lt_name_mappings "详细使用在下面描述
                   RECEIVING r_json = lv_string2     ). "返回JSON字符

  WRITE EV_JSON_STRING.WRITE lv_string2.
