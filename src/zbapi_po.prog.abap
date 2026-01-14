*&---------------------------------------------------------------------*
*& Report ZBAPI_PO
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ZBAPI_PO.

TABLES SSCRFIELDS.
TYPE-POOLS: SLIS.
TYPE-POOLS:ICON.


*订单类型+采购组织+公司代码+采购组+工厂+供应商+合并标记相同

TYPES:BEGIN OF TY_PO,
        BSART     TYPE EKKO-BSART,   "订单类型
        EKORG     TYPE EKKO-EKORG,   "采购组织
        BUKRS     TYPE EKKO-BUKRS,   "公司代码
        EKGRP     TYPE EKKO-EKGRP,   "采购组
        WERKS     TYPE EKPO-WERKS,   "工厂
        LIFNR     TYPE EKKO-LIFNR,   "供应商
        HBBJ(3),                 "合并标记
        PSTYP     TYPE EKPO-PSTYP,   "行项目类别
        MATNR     TYPE EKPO-MATNR,   "物料号
        MENGE(13)," TYPE EKPO-MENGE,   "数量
        EINDT     TYPE EKET-EINDT,   "交货期
      END OF TY_PO.

DATA:IT_PO TYPE TABLE OF TY_PO WITH HEADER LINE,
     WA_PO TYPE TY_PO.

TYPES:BEGIN OF TY_PO_RESULT,
        BSART        TYPE EKKO-BSART,   "订单类型
        EKORG        TYPE EKKO-EKORG,   "采购组织
        BUKRS        TYPE EKKO-BUKRS,   "公司代码
        EKGRP        TYPE EKKO-EKGRP,   "采购组
        WERKS        TYPE EKPO-WERKS,   "工厂
        LIFNR        TYPE EKKO-LIFNR,   "供应商
        PSTYP        TYPE EKPO-PSTYP,   "行项目类别
        MATNR        TYPE EKPO-MATNR,   "物料号
        MENGE(13)," TYPE EKPO-MENGE,   "数量
        EINDT        TYPE EKET-EINDT,   "交货期
        HBBJ(1),                 "合并标记
        EBELN        TYPE EKPO-EBELN,
        EBELP        TYPE EKPO-EBELP,
        FLAG(1),
        MESSAGE(200),
      END OF TY_PO_RESULT.

DATA:IT_PO_RESULT  TYPE TABLE OF TY_PO_RESULT WITH HEADER LINE,
     TIT_PO_RESULT TYPE TABLE OF TY_PO_RESULT WITH HEADER LINE.

DATA:LV_MESSAGE TYPE STRING.

DATA:LV_EBELN TYPE EKPO-EBELN.


"DATA IT_FLDTAB TYPE TPIT_T_FNAME  WITH HEADER LINE.
DATA: L_IRC  TYPE I,L_LINE TYPE I.

DATA:L_TEXT(200).

DATA:IT_MESSTAB LIKE BDCMSGCOLL OCCURS 0 WITH HEADER LINE.
DATA:G_INFO TYPE STRING.

*----------------------------------------------------------------------*
* CONSTANTS                                                            *
*----------------------------------------------------------------------*
CONSTANTS:                            """上传数据准备
  C_BEGIN_ROW TYPE I VALUE 1,       "BEGINNING ROW OF EXCEL FILE
  C_BEGIN_COL TYPE I VALUE 1,       "BEGINNING COLUMN OF EXCEL FILE
  C_END_ROW   TYPE I VALUE 50000,   "ENDING ROW OF EXCEL FILE
  C_END_COL   TYPE I VALUE 100.     "ENDING COLUMN OF EXCEL FILE


*DATA:LV_DTYPE(1).
**ALV
DATA:GT_EVENT_EXIT TYPE SLIS_T_EVENT_EXIT.
DATA:GS_EVENT_EXIT TYPE SLIS_EVENT_EXIT.

DATA:GT_FIELDCAT TYPE LVC_T_FCAT WITH HEADER LINE.
DATA:GC_GLAY TYPE LVC_S_GLAY.
DATA:GS_LAYOUT TYPE LVC_S_LAYO,     "SLIS_LAYOUT_ALV,
     WK_REPID  LIKE SY-REPID.
DATA:GS_GRID TYPE LVC_S_GLAY.
DATA:TEM_GRID TYPE REF TO CL_GUI_ALV_GRID.
DATA:GT_EVENTS TYPE SLIS_T_EVENT.
DATA:GS_EVENTS LIKE LINE OF GT_EVENTS.

DATA:LV_DATUM TYPE SY-DATUM.

SELECTION-SCREEN: FUNCTION KEY 1.   "激活下载模板按钮
**选择屏幕
SELECTION-SCREEN BEGIN OF BLOCK MAIN WITH FRAME TITLE TEXT-001.

*    SELECT-OPTIONS:S_BUKRS FOR BKPF-BUKRS.
*    SELECT-OPTIONS:S_BUDAT FOR BKPF-BUDAT.
*    SELECT-OPTIONS:S_BELNR FOR BKPF-BELNR.

  PARAMETERS:P_FILE  LIKE RLGRAP-FILENAME ."DEFAULT 'C:\USERS\ADMINISTRATOR\DESKTOP\计划独立需求的批量导入模板.XLS'.   "上载文件路径

SELECTION-SCREEN END OF BLOCK MAIN.

INITIALIZATION.
  DATA LS_FUNCTXT TYPE SMP_DYNTXT.
  LS_FUNCTXT-ICON_TEXT = '上传文件' .
  LS_FUNCTXT-ICON_ID = ICON_IMPORT.
  SSCRFIELDS-FUNCTXT_01 = LS_FUNCTXT.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_FILE.
  PERFORM FRM_GET_FILENAME USING P_FILE.

AT SELECTION-SCREEN.

*  LOOP AT S_WERKS.
*        AUTHORITY-CHECK OBJECT 'Z_MM_003'
*                            ID 'WERKS' FIELD S_WERKS-LOW.
*        IF SY-SUBRC <> 0.
*            MESSAGE E019(ZMM001) WITH S_WERKS-LOW.
*        ENDIF.
*  ENDLOOP.

AT SELECTION-SCREEN OUTPUT.


**主程序
START-OF-SELECTION.

  PERFORM FRM_RETRIVE_DATA.
  PERFORM FRM_BAPI_CREATE_PO .
  PERFORM FRM_DOWNLOAD_DATA.


*&---------------------------------------------------------------------*
*&      FORM  FRM_BAPI_CREATE_PO
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
*  -->  P1        TEXT
*  <--  P2        TEXT
*----------------------------------------------------------------------*
FORM FRM_BAPI_CREATE_PO .
  DATA: POHEADER    LIKE TABLE OF BAPIMEPOHEADER  WITH HEADER LINE,
        POHEADERX   LIKE TABLE OF BAPIMEPOHEADERX WITH HEADER LINE,
        RETURN      LIKE TABLE OF BAPIRET2  WITH HEADER LINE,
        POITEM      LIKE TABLE OF BAPIMEPOITEM WITH HEADER LINE,
        POITEMX     LIKE TABLE OF BAPIMEPOITEMX WITH HEADER LINE,
        POSCHEDULE  LIKE TABLE OF BAPIMEPOSCHEDULE WITH HEADER LINE,
        POSCHEDULEX LIKE TABLE OF BAPIMEPOSCHEDULX WITH HEADER LINE,
        POTEXTITEM  LIKE TABLE OF BAPIMEPOTEXT WITH HEADER LINE.
  CONSTANTS: CON VALUE 'X'.
  DATA: LIFNR        TYPE LFA1-LIFNR,
        NETPR        TYPE EKPO-NETPR,
        ROW          TYPE I,
        SCHEDULELINE TYPE I.
  DATA: EPO LIKE BAPIMEPOHEADER-PO_NUMBER.
  DATA: L_RETURN LIKE RETURN.

  DATA: LV_ERROR(1).

  DATA: LV_AA   TYPE STRING,
        LV_TYPE TYPE DD01V-DATATYPE.

  REFRESH IT_PO_RESULT.
  CLEAR IT_PO_RESULT.

  REFRESH TIT_PO_RESULT.
  CLEAR TIT_PO_RESULT.

  SORT IT_PO BY BSART EKORG BUKRS EKGRP WERKS LIFNR HBBJ.

  LOOP AT IT_PO.
    CLEAR WA_PO.

    MOVE-CORRESPONDING IT_PO TO WA_PO.

    AT NEW HBBJ.
      REFRESH TIT_PO_RESULT.
      CLEAR TIT_PO_RESULT.

      CLEAR LIFNR.
      LIFNR = WA_PO-LIFNR.


      CLEAR LV_AA.
      CLEAR LV_TYPE.

      CALL FUNCTION 'NUMERIC_CHECK'
        EXPORTING
          STRING_IN  = LIFNR
        IMPORTING
          STRING_OUT = LV_AA
          HTYPE      = LV_TYPE.

      IF LV_TYPE = 'NUMC'.
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            INPUT  = LIFNR
          IMPORTING
            OUTPUT = LIFNR.
      ENDIF.
      POHEADER-DOC_TYPE     = WA_PO-BSART.
      POHEADER-VENDOR       = LIFNR.
      POHEADER-PURCH_ORG    = WA_PO-EKORG.
      POHEADER-PUR_GROUP    = WA_PO-EKGRP.
      POHEADER-COMP_CODE    = WA_PO-BUKRS.
      POHEADER-DOC_DATE     = SY-DATUM.


      POHEADERX-DOC_TYPE    = CON.
      POHEADERX-VENDOR      = CON.
      POHEADERX-PURCH_ORG   = CON.
      POHEADERX-PUR_GROUP   = CON.
      POHEADERX-COMP_CODE   = CON.
      POHEADERX-DOC_DATE    = CON.


    ENDAT.

    ROW = ROW + 10.
    POITEM-PO_ITEM = ROW.          "采购凭证的项目编号
    POITEM-QUANTITY = WA_PO-MENGE. "采购订单数量
    POITEM-MATERIAL = WA_PO-MATNR.
    POITEM-PLANT = WA_PO-WERKS.    "工厂

    IF WA_PO-PSTYP <> ''.
      SELECT SINGLE
             PSTYP
             INTO POITEM-ITEM_CAT
             FROM T163Y
             WHERE SPRAS = '1'
      AND EPSTP = WA_PO-PSTYP.
    ELSE.
      POITEM-ITEM_CAT = WA_PO-PSTYP.
    ENDIF.

    IF WA_PO-BSART = 'ZNPI'.
      POITEM-FREE_ITEM = 'X'.
    ENDIF.

    APPEND POITEM.
    CLEAR POITEM.

    POITEMX-PO_ITEM = ROW .  "采购凭证的项目编号
    POITEMX-QUANTITY = CON.  "采购订单数量
    POITEMX-MATERIAL = CON.
    POITEMX-PLANT = CON.     "工厂
    POITEMX-ITEM_CAT = CON.

    IF WA_PO-BSART = 'ZNPI'.
      POITEMX-FREE_ITEM = CON.
    ENDIF.
    APPEND POITEMX.
    CLEAR POITEMX.

    MOVE-CORRESPONDING WA_PO TO TIT_PO_RESULT.
    TIT_PO_RESULT-EBELP = ROW.
    APPEND TIT_PO_RESULT.
    CLEAR TIT_PO_RESULT.

    SCHEDULELINE = SCHEDULELINE + 1.
    POSCHEDULE-PO_ITEM = ROW . "采购凭证的项目编号
    POSCHEDULE-SCHED_LINE = SCHEDULELINE . "采购凭证的项目编号
    POSCHEDULE-DEL_DATCAT_EXT = 'D'."交货日期的类别
    POSCHEDULE-DELIVERY_DATE = WA_PO-EINDT."交货日期
    POSCHEDULE-QUANTITY  = WA_PO-MENGE."采购订单数量

    APPEND POSCHEDULE.
    CLEAR POSCHEDULE.

    POSCHEDULEX-PO_ITEM = ROW . "采购凭证的项目编号
    POSCHEDULEX-SCHED_LINE = SCHEDULELINE . "采购凭证的项目编号
    POSCHEDULEX-DEL_DATCAT_EXT = CON."交货日期的类别
    POSCHEDULEX-DELIVERY_DATE = CON."交货日期
    POSCHEDULEX-QUANTITY  = CON."采购订单数量

    APPEND POSCHEDULEX.
    CLEAR POSCHEDULEX.
    AT END OF HBBJ.

      CALL FUNCTION 'BAPI_PO_CREATE1'
        EXPORTING
          POHEADER         = POHEADER
          POHEADERX        = POHEADERX
*         POADDRVENDOR     =
*         TESTRUN          =
*         MEMORY_UNCOMPLETE      =
*         MEMORY_COMPLETE  =
*         POEXPIMPHEADER   =
*         POEXPIMPHEADERX  =
*         VERSIONS         =
*         NO_MESSAGING     =
*         NO_MESSAGE_REQ   =
*         NO_AUTHORITY     =
*         NO_PRICE_FROM_PO =
        IMPORTING
          EXPPURCHASEORDER = EPO
*         EXPHEADER        =
*         EXPPOEXPIMPHEADER      =
        TABLES
          RETURN           = RETURN
          POITEM           = POITEM
          POITEMX          = POITEMX
*         POADDRDELIVERY   =
          POSCHEDULE       = POSCHEDULE
          POSCHEDULEX      = POSCHEDULEX
*         POACCOUNT        = POACCOUNT
*         POACCOUNTPROFITSEGMENT =
*         POACCOUNTX       = POACCOUNTX
*         POCONDHEADER     =
*         POCONDHEADERX    =
*         POCOND           =
*         POCONDX          =
*         POLIMITS         =
*         POCONTRACTLIMITS =
*         POSERVICES       =
*         POSRVACCESSVALUES      =
*         POSERVICESTEXT   =
*         EXTENSIONIN      =
*         EXTENSIONOUT     =
*         POEXPIMPITEM     =
*         POEXPIMPITEMX    =
*         POTEXTHEADER     =
*         POTEXTITEM       = POTEXTITEM
*         ALLVERSIONS      =
*         POPARTNER        =
*         POCOMPONENTS     =
*         POCOMPONENTSX    =
*         POSHIPPING       =
*         POSHIPPINGX      =
*         POSHIPPINGEXP    =
        .

      CLEAR LV_ERROR.
      LOOP AT RETURN INTO L_RETURN WHERE TYPE = 'E' .
        LV_ERROR = '1'.
        EXIT.
      ENDLOOP.

      IF LV_ERROR = '1'.
        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

        CLEAR LV_MESSAGE.
        LOOP AT RETURN INTO L_RETURN WHERE TYPE = 'E' .
          CONCATENATE LV_MESSAGE L_RETURN-MESSAGE ';' INTO LV_MESSAGE.
        ENDLOOP.

        LOOP AT TIT_PO_RESULT.
          TIT_PO_RESULT-FLAG = 'E'.
          CONCATENATE '创建PO失败:' LV_MESSAGE INTO TIT_PO_RESULT-MESSAGE.
          MODIFY TIT_PO_RESULT.
          CLEAR TIT_PO_RESULT.
        ENDLOOP.
      ELSE.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            WAIT = 'X'.

        CLEAR LV_EBELN.
        LOOP AT RETURN INTO L_RETURN WHERE TYPE = 'S' AND ID = '06' AND NUMBER = '017'.
          LV_EBELN = L_RETURN-MESSAGE_V2.
        ENDLOOP.

        LOOP AT TIT_PO_RESULT.
          TIT_PO_RESULT-FLAG = 'S'.
          TIT_PO_RESULT-MESSAGE = '创建PO成功!'.
          TIT_PO_RESULT-EBELN = LV_EBELN.
          MODIFY TIT_PO_RESULT.
          CLEAR TIT_PO_RESULT.
        ENDLOOP.
      ENDIF.

      LOOP AT TIT_PO_RESULT.
        MOVE-CORRESPONDING TIT_PO_RESULT TO IT_PO_RESULT.
        APPEND IT_PO_RESULT.
        CLEAR IT_PO_RESULT.
      ENDLOOP.


      REFRESH: POITEM,POITEMX,POSCHEDULE,POSCHEDULEX,POHEADER,POHEADERX,RETURN.
      CLEAR: IT_PO, WA_PO, ROW, POITEM,POITEMX,POSCHEDULE,POSCHEDULEX,POHEADER,POHEADERX,RETURN.

    ENDAT.
  ENDLOOP.

ENDFORM. " FRM_BAPI_CREATE_PO



*&---------------------------------------------------------------------*
*&      FORM  FRM_GET_FILENAME
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
*      -->P_P_FILE  TEXT
*----------------------------------------------------------------------*
FORM FRM_GET_FILENAME  USING  P_FILE.
  DATA: L_FILETAB TYPE FILETABLE,
        L_RC      TYPE I.
  CLEAR L_FILETAB.
  REFRESH L_FILETAB.
* OPEN DIALOG
  CALL METHOD CL_GUI_FRONTEND_SERVICES=>FILE_OPEN_DIALOG
    EXPORTING
      WINDOW_TITLE         = '选择上传的文件'
*     DEFAULT_EXTENSION    =
      DEFAULT_FILENAME     = '*.XLS'
*     FILE_FILTER          = '*.XLS'
      INITIAL_DIRECTORY    = 'C:\'
      MULTISELECTION       = ''
    CHANGING
      FILE_TABLE           = L_FILETAB
      RC                   = L_RC
    EXCEPTIONS
      CNTL_ERROR           = 1
      ERROR_NO_GUI         = 2
      NOT_SUPPORTED_BY_GUI = 3
      OTHERS               = 4.
* GET FILE PATH
  CHECK L_RC EQ 1.
  READ TABLE L_FILETAB INDEX 1 INTO P_FILE.
ENDFORM.                    " FRM_GET_FILENAME



*&---------------------------------------------------------------------*
*&      FORM  FRM_RETRIVE_DATA
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
*  -->  P1        TEXT
*  <--  P2        TEXT
*----------------------------------------------------------------------*
FORM FRM_RETRIVE_DATA.

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      TEXT = '数据处理中.................'.

  TYPES: BEGIN OF KCDE_INTERN_STRUC.
           INCLUDE STRUCTURE  KCDE_CELLS.
  TYPES: END OF KCDE_INTERN_STRUC.


  DATA I_EXCEL TYPE KCDE_INTERN_STRUC OCCURS 0 WITH HEADER LINE.
  REFRESH I_EXCEL.
  CLEAR I_EXCEL.

*从已知文件名读入内表
  CALL FUNCTION 'KCD_EXCEL_OLE_TO_INT_CONVERT'
    EXPORTING
      FILENAME                = P_FILE
      I_BEGIN_COL             = 1
      I_BEGIN_ROW             = 1
      I_END_COL               = 20
      I_END_ROW               = 65535
    TABLES
      INTERN                  = I_EXCEL[]
    EXCEPTIONS
      INCONSISTENT_PARAMETERS = 1
      UPLOAD_OLE              = 2
      OTHERS                  = 3.

  IF SY-SUBRC <> 0.
    MESSAGE  '打开文件错误，请检查文件，确保关闭文件!' TYPE 'E'.
    STOP.
  ENDIF.

  REFRESH IT_PO.
  CLEAR IT_PO.

  LOOP AT I_EXCEL.
    CASE I_EXCEL-COL.
      WHEN '001'.
        WRITE I_EXCEL-VALUE TO IT_PO-BSART.
      WHEN '002'.
        WRITE I_EXCEL-VALUE TO IT_PO-EKORG.
      WHEN '003'.
        WRITE I_EXCEL-VALUE TO IT_PO-BUKRS.
      WHEN '004'.
        WRITE I_EXCEL-VALUE TO IT_PO-EKGRP.
      WHEN '005'.
        WRITE I_EXCEL-VALUE TO IT_PO-WERKS.
      WHEN '006'.
        WRITE I_EXCEL-VALUE TO IT_PO-LIFNR.
      WHEN '007'.
        WRITE I_EXCEL-VALUE TO IT_PO-PSTYP.

      WHEN '008'.
        WRITE I_EXCEL-VALUE TO IT_PO-MATNR.

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            INPUT  = IT_PO-MATNR
          IMPORTING
            OUTPUT = IT_PO-MATNR.


      WHEN '009'.
        WRITE I_EXCEL-VALUE TO IT_PO-MENGE.

      WHEN '010'.
        "  WRITE I_EXCEL-VALUE TO IT_PO-EINDT.
        IF I_EXCEL-VALUE <> '交货期'.
          CALL FUNCTION 'CONVERT_DATE_TO_INTERNAL'
            EXPORTING
              DATE_EXTERNAL = I_EXCEL-VALUE
            IMPORTING
              DATE_INTERNAL = IT_PO-EINDT.
        ENDIF.


      WHEN '011'.
        WRITE I_EXCEL-VALUE TO IT_PO-HBBJ.

    ENDCASE.
    AT END OF ROW.
      APPEND IT_PO.
      CLEAR  IT_PO.
    ENDAT.
  ENDLOOP.

* 删除表头
  DELETE IT_PO INDEX 1.

ENDFORM.                    " FRM_RETRIVE_DATA






*&---------------------------------------------------------------------*
*&      Form  FRM_DOWNLOAD_DATA
*&---------------------------------------------------------------------*
*       下载数据到xls文件
*----------------------------------------------------------------------*
FORM FRM_DOWNLOAD_DATA .

  DATA:LV_FILENAME TYPE STRING.

  DATA:BEGIN OF T_FIELDNAMES  OCCURS 0,
         NAME TYPE CHAR20,
       END OF T_FIELDNAMES.


  REFRESH T_FIELDNAMES.
  CLEAR T_FIELDNAMES.

  T_FIELDNAMES-NAME = '订单类型'.
  APPEND T_FIELDNAMES.

  T_FIELDNAMES-NAME = '采购组织'.
  APPEND T_FIELDNAMES.

  T_FIELDNAMES-NAME = '公司代码'.
  APPEND T_FIELDNAMES.

  T_FIELDNAMES-NAME = '采购组'.
  APPEND T_FIELDNAMES.

  T_FIELDNAMES-NAME = '工厂'.
  APPEND T_FIELDNAMES.

  T_FIELDNAMES-NAME = '供应商'.
  APPEND T_FIELDNAMES.

  T_FIELDNAMES-NAME = '行项目类别'.
  APPEND T_FIELDNAMES.

  T_FIELDNAMES-NAME = '物料号'.
  APPEND T_FIELDNAMES.

  T_FIELDNAMES-NAME = '数量'.
  APPEND T_FIELDNAMES.

  T_FIELDNAMES-NAME = '交货期'.
  APPEND T_FIELDNAMES.

  T_FIELDNAMES-NAME = '合并标记'.
  APPEND T_FIELDNAMES.

  T_FIELDNAMES-NAME = '采购订单号'.
  APPEND T_FIELDNAMES.

  T_FIELDNAMES-NAME = '采购订单行项目'.
  APPEND T_FIELDNAMES.

  T_FIELDNAMES-NAME = '订单创建是否成功标记'.
  APPEND T_FIELDNAMES.

  T_FIELDNAMES-NAME = '订单创建返回信息'.
  APPEND T_FIELDNAMES.

  CLEAR LV_FILENAME.

  CONCATENATE 'C:\PO批导日志' SY-DATUM SY-UZEIT '.XLS' INTO LV_FILENAME.

  CALL FUNCTION 'GUI_DOWNLOAD'
    EXPORTING
      FILENAME   = LV_FILENAME "'C:\1234.XLS'
      FILETYPE   = 'DAT'
      "这里一般用DAT，如果用ASC则1000-不会显示为-1000，而DAT会显示为-1000，如果用DBF则不会有缩进，即字符前面的空格会被除去,而且字符的前导0也会输出。
      CODEPAGE   = '8404'
    TABLES
      DATA_TAB   = IT_PO_RESULT
      FIELDNAMES = T_FIELDNAMES.

  MESSAGE '已完成创建凭证,结果保存在C盘目录下,请查看!.' TYPE 'I'.
ENDFORM.                    " FRM_DOWNLOAD_DATA
