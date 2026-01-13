*&---------------------------------------------------------------------*
*& Report ZRFC_LOG
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ZRFC_LOG.


*&---------------------------------------------------------------------*
*& 先定义事件处理类
*&---------------------------------------------------------------------*
CLASS LCL_HANDLER DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS:
      ON_DOUBLE_CLICK FOR EVENT DOUBLE_CLICK OF CL_SALV_EVENTS_TABLE
        IMPORTING ROW COLUMN,
      ON_DOUBLE_CLICK1 FOR EVENT DOUBLE_CLICK OF CL_SALV_EVENTS_TABLE
        IMPORTING ROW COLUMN,
      ON_USER_COMMAND FOR EVENT ADDED_FUNCTION OF CL_SALV_EVENTS
        IMPORTING E_SALV_FUNCTION.
ENDCLASS.

CLASS LCL_HANDLER IMPLEMENTATION.
  METHOD ON_DOUBLE_CLICK.
    PERFORM SHOW_DETAIL USING ROW COLUMN.
  ENDMETHOD.

  METHOD ON_DOUBLE_CLICK1.
    PERFORM SHOW_DETAIL2 USING ROW COLUMN.
  ENDMETHOD.

  METHOD ON_USER_COMMAND.
    PERFORM USER_COMMAND USING E_SALV_FUNCTION.
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*& DATA:
*&---------------------------------------------------------------------*
DATA: GT_RFC_LOG TYPE TABLE OF ZRFC_LOG,
      GT_DATA    TYPE TABLE OF ZRFC_LOG,
      GT_FAND    TYPE TABLE OF ZRFC_LOG,
      GS_RFC_LOG TYPE ZRFC_LOG.
*DATA: LT_ZCOS_MES_008 TYPE TABLE OF ZCOS_MES_008.
*DATA: LT_ZCOS_MES_001 TYPE ZCOS_MES_001.
DATA: GO_ALV     TYPE REF TO CL_SALV_TABLE,
      GO_EVENTS  TYPE REF TO CL_SALV_EVENTS_TABLE.

*&---------------------------------------------------------------------*
*& 选择屏幕
*&---------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS: S_FUNC FOR GS_RFC_LOG-FUNCNAME,
                  S_DATE FOR SY-DATUM,
                  S_UUID FOR GS_RFC_LOG-UUID,
                  S_UNAME FOR GS_RFC_LOG-UNAME,
                  S_TYPE FOR GS_RFC_LOG-TYPE.
SELECTION-SCREEN END OF BLOCK B1.

*&---------------------------------------------------------------------*
*& 主程序
*&---------------------------------------------------------------------*
START-OF-SELECTION.
  PERFORM FRM_GET_DATA.
  PERFORM FRM_DISPLAY_ALV.

*&---------------------------------------------------------------------*
*& FORM FRM_GET_DATA 获取数据
*&---------------------------------------------------------------------*
FORM FRM_GET_DATA.
* 获取数据
  SELECT *
    FROM ZRFC_LOG
    INTO TABLE @GT_RFC_LOG
   WHERE FUNCNAME IN @S_FUNC
     AND UNAME IN @S_UNAME
     AND UUID IN @S_UUID
     AND TYPE IN @S_TYPE
     AND DATUM IN @S_DATE.
ENDFORM.

*&---------------------------------------------------------------------*
*& FORM FRM_DISPLAY_ALV
*&---------------------------------------------------------------------*
FORM FRM_DISPLAY_ALV.
* DATA:HANDLER TYPE REF TO SET_LIST_HANDLER.

  TRY.
      CL_SALV_TABLE=>FACTORY(
        IMPORTING
          R_SALV_TABLE = GO_ALV
        CHANGING
          T_TABLE      = GT_RFC_LOG ).

      " 设置ALV属性
      PERFORM SET_ALV_PROPERTIES.

      " 注册双击事件
      GO_EVENTS = GO_ALV->GET_EVENT( ).
      SET HANDLER LCL_HANDLER=>ON_DOUBLE_CLICK FOR GO_EVENTS.
      SET HANDLER LCL_HANDLER=>ON_USER_COMMAND FOR GO_EVENTS.

      " 显示ALV
      GO_ALV->DISPLAY( ).

    CATCH CX_SALV_MSG.
      MESSAGE 'ALV显示错误' TYPE 'E'.
  ENDTRY.
ENDFORM.

*&---------------------------------------------------------------------*
*& FORM SET_ALV_PROPERTIES
*&---------------------------------------------------------------------*
FORM SET_ALV_PROPERTIES.
  DATA: LO_COLUMNS  TYPE REF TO CL_SALV_COLUMNS_TABLE,
        LO_COLUMN   TYPE REF TO CL_SALV_COLUMN_TABLE,
        LO_FUNCTIONS TYPE REF TO CL_SALV_FUNCTIONS_LIST.

  " 设置工具栏功能
  GO_ALV->SET_SCREEN_STATUS(
    PFSTATUS    = 'STANDARD1'
    REPORT      = 'ZRFC_LOG'
    SET_FUNCTIONS = GO_ALV->C_FUNCTIONS_ALL ).

* LO_FUNCTIONS = GO_ALV->GET_FUNCTIONS( ).
* LO_FUNCTIONS->SET_ALL( ABAP_TRUE ).

  " 设置列属性
  LO_COLUMNS = GO_ALV->GET_COLUMNS( ).
  LO_COLUMNS->SET_OPTIMIZE( ABAP_TRUE ).

  " 设置列标题
  TRY.
      LO_COLUMN ?= LO_COLUMNS->GET_COLUMN( 'UUID' ).
      LO_COLUMN->SET_LONG_TEXT( 'UUID' ).
      LO_COLUMN->SET_MEDIUM_TEXT( 'UUID' ).
      LO_COLUMN->SET_SHORT_TEXT( 'UUID' ).
    CATCH CX_SALV_NOT_FOUND.
  ENDTRY.

  TRY.
      LO_COLUMN ?= LO_COLUMNS->GET_COLUMN( 'FUNCNAME' ).
      LO_COLUMN->SET_LONG_TEXT( '函数名称' ).
      LO_COLUMN->SET_MEDIUM_TEXT( '函数名称' ).
      LO_COLUMN->SET_SHORT_TEXT( '函数名称' ).
    CATCH CX_SALV_NOT_FOUND.
  ENDTRY.

  TRY.
      LO_COLUMN ?= LO_COLUMNS->GET_COLUMN( 'UNAME' ).
      LO_COLUMN->SET_LONG_TEXT( '执行人' ).
      LO_COLUMN->SET_MEDIUM_TEXT( '执行人' ).
      LO_COLUMN->SET_SHORT_TEXT( '执行人' ).
    CATCH CX_SALV_NOT_FOUND.
  ENDTRY.

  TRY.
      LO_COLUMN ?= LO_COLUMNS->GET_COLUMN( 'TIMESTAMP' ).
      LO_COLUMN->SET_LONG_TEXT( '执行时间' ).
      LO_COLUMN->SET_MEDIUM_TEXT( '执行时间' ).
      LO_COLUMN->SET_SHORT_TEXT( '时间' ).
    CATCH CX_SALV_NOT_FOUND.
  ENDTRY.

  TRY.
      LO_COLUMN ?= LO_COLUMNS->GET_COLUMN( 'IMPORT01' ).
      LO_COLUMN->SET_LONG_TEXT( '传入参数(JSON)1' ).
      LO_COLUMN->SET_MEDIUM_TEXT( '传入参数1' ).
      LO_COLUMN->SET_SHORT_TEXT( '传入1' ).
    CATCH CX_SALV_NOT_FOUND.
  ENDTRY.

  TRY.
      LO_COLUMN ?= LO_COLUMNS->GET_COLUMN( 'IMPORT02' ).
      LO_COLUMN->SET_LONG_TEXT( '传入参数(JSON)2' ).
      LO_COLUMN->SET_MEDIUM_TEXT( '传入参数2' ).
      LO_COLUMN->SET_SHORT_TEXT( '传入2' ).
    CATCH CX_SALV_NOT_FOUND.
  ENDTRY.

  TRY.
      LO_COLUMN ?= LO_COLUMNS->GET_COLUMN( 'IMPORT03' ).
      LO_COLUMN->SET_LONG_TEXT( '传入参数(JSON)3' ).
      LO_COLUMN->SET_MEDIUM_TEXT( '传入参数3' ).
      LO_COLUMN->SET_SHORT_TEXT( '传入3' ).
    CATCH CX_SALV_NOT_FOUND.
  ENDTRY.

  TRY.
      LO_COLUMN ?= LO_COLUMNS->GET_COLUMN( 'EXPORT01' ).
      LO_COLUMN->SET_LONG_TEXT( '传出参数(JSON)1' ).
      LO_COLUMN->SET_MEDIUM_TEXT( '传出参数1' ).
      LO_COLUMN->SET_SHORT_TEXT( '传出1' ).
    CATCH CX_SALV_NOT_FOUND.
  ENDTRY.

  TRY.
      LO_COLUMN ?= LO_COLUMNS->GET_COLUMN( 'EXPORT02' ).
      LO_COLUMN->SET_LONG_TEXT( '传出参数(JSON)2' ).
      LO_COLUMN->SET_MEDIUM_TEXT( '传出参数2' ).
      LO_COLUMN->SET_SHORT_TEXT( '传出2' ).
    CATCH CX_SALV_NOT_FOUND.
  ENDTRY.

  TRY.
      LO_COLUMN ?= LO_COLUMNS->GET_COLUMN( 'EXPORT03' ).
      LO_COLUMN->SET_LONG_TEXT( '传出参数(JSON)3' ).
      LO_COLUMN->SET_MEDIUM_TEXT( '传出参数3' ).
      LO_COLUMN->SET_SHORT_TEXT( '传出3' ).
    CATCH CX_SALV_NOT_FOUND.
  ENDTRY.

  TRY.
      LO_COLUMN ?= LO_COLUMNS->GET_COLUMN( 'TABLE01' ).
      LO_COLUMN->SET_LONG_TEXT( '表参数(JSON)1' ).
      LO_COLUMN->SET_MEDIUM_TEXT( '表参数1' ).
      LO_COLUMN->SET_SHORT_TEXT( '表参数1' ).
    CATCH CX_SALV_NOT_FOUND.
  ENDTRY.

  TRY.
      LO_COLUMN ?= LO_COLUMNS->GET_COLUMN( 'TABLE02' ).
      LO_COLUMN->SET_LONG_TEXT( '表参数(JSON)2' ).
      LO_COLUMN->SET_MEDIUM_TEXT( '表参数2' ).
      LO_COLUMN->SET_SHORT_TEXT( '表参数2' ).
    CATCH CX_SALV_NOT_FOUND.
  ENDTRY.

  TRY.
      LO_COLUMN ?= LO_COLUMNS->GET_COLUMN( 'TABLE03' ).
      LO_COLUMN->SET_LONG_TEXT( '表参数(JSON)3' ).
      LO_COLUMN->SET_MEDIUM_TEXT( '表参数3' ).
      LO_COLUMN->SET_SHORT_TEXT( '表参数3' ).
    CATCH CX_SALV_NOT_FOUND.
  ENDTRY.
ENDFORM.

*&---------------------------------------------------------------------*
*& SHOW_DETAIL 显示详细信息
*&---------------------------------------------------------------------*
FORM SHOW_DETAIL USING P_ROW TYPE I P_COLUMN TYPE LVC_FNAME.
  DATA: LV_JSON  TYPE STRING,
        LV_TITLE TYPE STRING.

  READ TABLE GT_RFC_LOG INTO GS_RFC_LOG INDEX P_ROW.
  IF SY-SUBRC <> 0.
    RETURN.
  ENDIF.

  CASE P_COLUMN.
    WHEN 'IMPORT01'.
      LV_JSON = GS_RFC_LOG-IMPORT01.
    WHEN 'IMPORT02'.
      LV_JSON = GS_RFC_LOG-IMPORT02.
    WHEN 'IMPORT03'.
      LV_JSON = GS_RFC_LOG-IMPORT03.
    WHEN 'EXPORT01'.
      LV_JSON = GS_RFC_LOG-EXPORT01.
    WHEN 'EXPORT02'.
      LV_JSON = GS_RFC_LOG-EXPORT02.
    WHEN 'EXPORT03'.
      LV_JSON = GS_RFC_LOG-EXPORT03.
    WHEN 'TABLE01'.
      LV_JSON = GS_RFC_LOG-TABLE01.
    WHEN 'TABLE02'.
      LV_JSON = GS_RFC_LOG-TABLE02.
    WHEN 'TABLE03'.
      LV_JSON = GS_RFC_LOG-TABLE03.
    WHEN OTHERS.
      RETURN.
  ENDCASE.

  " 调用显示JSON的弹窗
  PERFORM SHOW_JSON_POPUP USING LV_JSON.
ENDFORM.
*&---------------------------------------------------------------------*
*& FORM SHOW_JSON_POPUP
*&---------------------------------------------------------------------*
FORM SHOW_JSON_POPUP USING P_JSON TYPE STRING.
  DATA: LT_STRING  TYPE TABLE OF STRING,
        LV_STRING  TYPE STRING.

  DATA: LV_JSON     TYPE STRING,
        LV_HTML     TYPE XSTRING,
        LV_CONVERT  TYPE STRING.

* 如果JSON为空，显示提示信息
  IF P_JSON IS INITIAL.
    P_JSON = '无参数数据'.
  ENDIF.

* 将JSON字符串转换为HTML格式
  TRY.
      CALL TRANSFORMATION SJSON2HTML SOURCE XML P_JSON
                                    RESULT XML LV_HTML.
    CATCH CX_XSLT_RUNTIME_ERROR INTO DATA(LO_ERR).
      DATA(LV_ERR_TEXT) = LO_ERR->GET_TEXT( ).
      WRITE: / 'ERROR DURING TRANSFORMATION: ', LV_ERR_TEXT.
      RETURN.
  ENDTRY.

  LV_CONVERT = CL_ABAP_CODEPAGE=>CONVERT_FROM( LV_HTML ).
  CL_ABAP_BROWSER=>SHOW_HTML( HTML_STRING = LV_CONVERT ).
ENDFORM.


*&---------------------------------------------------------------------*
*& FORM USER_COMMAND
*&---------------------------------------------------------------------*
FORM USER_COMMAND USING P_USER_COMMAND TYPE SALV_DE_FUNCTION.
  DATA:LV_UCOMM TYPE SY-UCOMM.
  DATA: LT_VALUE TYPE TABLE OF SVAL,
        LS_VALUE TYPE SVAL,
        LV_DATA  TYPE CHAR50.
  CLEAR: LT_VALUE,
         LS_VALUE.

  DATA: LV_XSTRING   TYPE XSTRING,
        LV_JSON_CLEAN TYPE STRING.

  LV_UCOMM = P_USER_COMMAND.
  CASE LV_UCOMM.
    WHEN '&FAND'.
      LS_VALUE-TABNAME = 'ZTACS_LOT_COST_B'.
      LS_VALUE-FIELDNAME = 'AUFEX'.
      LS_VALUE-FIELDTEXT = 'MES工单编号'.
      APPEND LS_VALUE TO LT_VALUE.

      CALL FUNCTION 'POPUP_GET_VALUES'
        EXPORTING
          POPUP_TITLE       = '输入MES工单编号'
        TABLES
          FIELDS            = LT_VALUE
        EXCEPTIONS
          ERROR_IN_FIELDS   = 1
          OTHERS            = 2.

      READ TABLE LT_VALUE INTO LS_VALUE INDEX 1.
      IF LS_VALUE-VALUE IS NOT INITIAL.
        CLEAR:GT_FAND, LV_DATA.
        LS_VALUE-VALUE = |{ LS_VALUE-VALUE ALPHA = IN }|.

        IF GT_RFC_LOG IS NOT INITIAL.
          LOOP AT GT_RFC_LOG ASSIGNING FIELD-SYMBOL(<FS_LOG>).
            IF <FS_LOG>-FUNCNAME = 'ZCO_MES_ORD_CREATE1'.
              PERFORM FRM_JSON_CLEAN1 CHANGING LV_JSON_CLEAN
                                               LV_XSTRING
                                               <FS_LOG>-TABLE01.
*              READ TABLE LT_ZCOS_MES_008 TRANSPORTING NO FIELDS
*                WITH KEY AUFEX = LS_VALUE-VALUE.
*              IF SY-SUBRC = 0.
*                APPEND <FS_LOG> TO GT_FAND.
*              ENDIF.
            ELSEIF <FS_LOG>-FUNCNAME = 'ZCO_MES_ORD_CREATE'.
              PERFORM FRM_JSON_CLEAN2 CHANGING LV_JSON_CLEAN
                                               LV_XSTRING
                                               <FS_LOG>-IMPORT01.
*              IF LT_ZCOS_MES_001-AUFEX = LS_VALUE-VALUE.
*                APPEND <FS_LOG> TO GT_FAND.
*              ENDIF.
            ENDIF.
            CLEAR:
*            LT_ZCOS_MES_008,
*            LT_ZCOS_MES_001,
            LV_JSON_CLEAN, LV_XSTRING.
          ENDLOOP.
        ENDIF.
      ENDIF.
  ENDCASE.

  IF GT_FAND IS NOT INITIAL.
    " 销毁原有ALV
    IF GO_ALV IS BOUND.
      FREE GO_ALV.
    ENDIF.

    " 重新创建ALV
    TRY.
        CL_SALV_TABLE=>FACTORY(
          IMPORTING
            R_SALV_TABLE = GO_ALV
          CHANGING
            T_TABLE      = GT_FAND ).

        " 设置工具栏和功能
        PERFORM SET_ALV_PROPERTIES.

        " 注册双击事件
        GO_EVENTS = GO_ALV->GET_EVENT( ).
        SET HANDLER LCL_HANDLER=>ON_DOUBLE_CLICK1 FOR GO_EVENTS.
        SET HANDLER LCL_HANDLER=>ON_USER_COMMAND FOR GO_EVENTS.

        " 显示ALV
        GO_ALV->DISPLAY( ).

      CATCH CX_SALV_MSG.
        MESSAGE 'ALV 刷新失败' TYPE 'E'.
    ENDTRY.
  ELSE.
    MESSAGE '未查询到数据！' TYPE 'S' DISPLAY LIKE 'E'.
  ENDIF.
  GO_ALV->REFRESH( ).
ENDFORM.


*&---------------------------------------------------------------------*
*& FORM SHOW_DETAIL2
*&---------------------------------------------------------------------*
FORM SHOW_DETAIL2 USING P_ROW P_COLUMN.
  DATA: LV_JSON  TYPE STRING,
        LV_TITLE TYPE STRING.

  READ TABLE GT_FAND INTO GS_RFC_LOG INDEX P_ROW.
  IF SY-SUBRC <> 0.
    RETURN.
  ENDIF.

  CASE P_COLUMN.
    WHEN 'IMPORT01'.
      LV_JSON = GS_RFC_LOG-IMPORT01.
    WHEN 'IMPORT02'.
      LV_JSON = GS_RFC_LOG-IMPORT02.
    WHEN 'IMPORT03'.
      LV_JSON = GS_RFC_LOG-IMPORT03.
    WHEN 'EXPORT01'.
      LV_JSON = GS_RFC_LOG-EXPORT01.
    WHEN 'EXPORT02'.
      LV_JSON = GS_RFC_LOG-EXPORT02.
    WHEN 'EXPORT03'.
      LV_JSON = GS_RFC_LOG-EXPORT03.
    WHEN 'TABLE01'.
      LV_JSON = GS_RFC_LOG-TABLE01.
    WHEN 'TABLE02'.
      LV_JSON = GS_RFC_LOG-TABLE02.
    WHEN 'TABLE03'.
      LV_JSON = GS_RFC_LOG-TABLE03.
    WHEN OTHERS.
      RETURN.
  ENDCASE.

  " 调用显示JSON的弹窗
  PERFORM SHOW_JSON_POPUP USING LV_JSON.
ENDFORM.
*&---------------------------------------------------------------------*
*& FORM FRM_JSON_CLEAN1
*&---------------------------------------------------------------------*
FORM FRM_JSON_CLEAN1 USING LV_JSON_CLEAN TYPE STRING
                          LV_XSTRING TYPE XSTRING
                          LV_DATA TYPE STRING.
  TRY.
      " 清理 JSON 字符串（移除可能的 BOM 或特殊字符）
      LV_JSON_CLEAN = LV_DATA.

      " 将 JSON 字符串转换为 XSTRING
      LV_XSTRING = CL_ABAP_CODEPAGE=>CONVERT_TO( LV_JSON_CLEAN ).

      " 创建 JSON 读取器
      DATA(LO_READER) = CL_SXML_STRING_READER=>CREATE( INPUT = LV_XSTRING ).

      " 使用转换将 JSON 转回内表
*      CALL TRANSFORMATION ID
*        SOURCE XML LO_READER
*        RESULT DATA = LT_ZCOS_MES_008.

    CATCH CX_ROOT INTO DATA(LX_ERROR).
      " 详细的错误处理
      DATA(LV_ERROR_MSG) = LX_ERROR->GET_TEXT( ).
      RAISE JSON_CONVERSION_ERROR.
  ENDTRY.
ENDFORM.
*&---------------------------------------------------------------------*
*& FORM FRM_JSON_CLEAN2
*&---------------------------------------------------------------------*
FORM FRM_JSON_CLEAN2 USING LV_JSON_CLEAN TYPE STRING
                          LV_XSTRING TYPE XSTRING
                          LV_DATA TYPE STRING.
  TRY.
      " 清理 JSON 字符串（移除可能的 BOM 或特殊字符）
      LV_JSON_CLEAN = LV_DATA.

      " 将 JSON 字符串转换为 XSTRING
      LV_XSTRING = CL_ABAP_CODEPAGE=>CONVERT_TO( LV_JSON_CLEAN ).

      " 创建 JSON 读取器
      DATA(LO_READER) = CL_SXML_STRING_READER=>CREATE( INPUT = LV_XSTRING ).

*      " 使用转换将 JSON 转回内表
*      CALL TRANSFORMATION ID
*        SOURCE XML LO_READER
*        RESULT DATA = LT_ZCOS_MES_001.

    CATCH CX_ROOT INTO DATA(LX_ERROR).
      " 详细的错误处理
      DATA(LV_ERROR_MSG) = LX_ERROR->GET_TEXT( ).
      RAISE JSON_CONVERSION_ERROR.
  ENDTRY.
ENDFORM.
