*&---------------------------------------------------------------------*
*& Report  sample
*&---------------------------------------------------------------------*
REPORT  sample LINE-SIZE 344.


*&---------------------------------------------------------------------*
*&      TYPE DEFINITION
*&---------------------------------------------------------------------*
TYPES:
    BEGIN OF gtst_csks_cskt,
      kokrs  TYPE csks-kokrs,
      kostl  TYPE csks-kostl,
      datbi  TYPE csks-datbi,
      datab  TYPE csks-datab,
      objnr  TYPE csks-objnr,
      ltext  TYPE cskt-ltext,
    END OF gtst_csks_cskt,
    BEGIN OF gtst_cosp,
      gjahr  TYPE cosp-gjahr,
      kstar  TYPE cosp-kstar,
      beknz  TYPE cosp-beknz,
      twaer  TYPE cosp-twaer,
      wtg001 TYPE cosp-wtg001,
      wtg002 TYPE cosp-wtg002,
      wtg003 TYPE cosp-wtg003,
      wtg004 TYPE cosp-wtg004,
      wtg005 TYPE cosp-wtg005,
      wtg006 TYPE cosp-wtg006,
      wtg007 TYPE cosp-wtg007,
      wtg008 TYPE cosp-wtg008,
      wtg009 TYPE cosp-wtg009,
      wtg010 TYPE cosp-wtg010,
      wtg011 TYPE cosp-wtg011,
      wtg012 TYPE cosp-wtg012,
    END OF gtst_cosp,
    gtit_cosp  TYPE STANDARD TABLE OF gtst_cosp.


*&---------------------------------------------------------------------*
*&      DATA DEFINITION
*&---------------------------------------------------------------------*
DATA:
    gst_csks_cskt  TYPE gtst_csks_cskt,
    git_cosp       TYPE gtit_cosp,
    gv_kstar       TYPE cosp-kstar.


*&---------------------------------------------------------------------*
*&      SELECTION SCREEN DEFINITION
*&---------------------------------------------------------------------*
PARAMETERS:
    p_kokrs        TYPE csks-kokrs OBLIGATORY,                       " 管理領域
    p_kostl        TYPE csks-kostl OBLIGATORY,                       " 原価センタ
    p_datab        TYPE csks-datab OBLIGATORY.                       " 有効日
SELECT-OPTIONS
    s_kstar FOR gv_kstar.                                            " 原価要素（range table）


*----------------------------------------------------------------------*
*       AT SELECTION-SCREEN
*----------------------------------------------------------------------*
AT SELECTION-SCREEN.                                                 " 入力されたkokrsに対して存在判定
  PERFORM kokrs_checker                                                " 存在しなければerror messageが出力される
      USING
        p_kokrs.


*&---------------------------------------------------------------------*
*&      START-OF-SELECTION
*&---------------------------------------------------------------------*
START-OF-SELECTION.
  PERFORM get_csks_cskt
      USING
        p_kokrs
        p_kostl
        p_datab
      CHANGING
        gst_csks_cskt.

  PERFORM get_cosp
      USING
        gst_csks_cskt
      CHANGING
        git_cosp.

  PERFORM print_csks_and_cskt
      USING
        gst_csks_cskt.

  PERFORM print_cosp
      USING
        git_cosp.


*&---------------------------------------------------------------------*
*&      Form  kokrs_checker
*&---------------------------------------------------------------------*
*       管理領域から入力値を探索。
*       存在しない場合エラーメッセージ出力。
*----------------------------------------------------------------------*
FORM kokrs_checker
  USING
      u_kokrs TYPE csks-kokrs.

  DATA:
      lv_temp TYPE tka01-kokrs.

  SELECT SINGLE
    kokrs
  INTO
    lv_temp
  FROM
    tka01
  WHERE
    tka01~kokrs = u_kokrs
  .

  IF sy-subrc <> 0.
    MESSAGE e011(zspp01)
        WITH text-001 u_kokrs.
  ENDIF.
ENDFORM.


*&---------------------------------------------------------------------*
*&      Form  get_csks_cskt
*&---------------------------------------------------------------------*
*       csks csktから値を取得する。
*----------------------------------------------------------------------*
FORM get_csks_cskt
  USING
      u_kokrs TYPE csks-kokrs
      u_kostl TYPE csks-kostl
      u_date TYPE csks-datab
  CHANGING
      cst_csks_cskt TYPE gtst_csks_cskt.

  SELECT SINGLE
    csks~kokrs
    csks~kostl
    csks~datbi
    csks~datab
    csks~objnr
    cskt~ltext
  INTO
    CORRESPONDING FIELDS OF cst_csks_cskt
  FROM
    csks
  INNER JOIN
    cskt
    ON
      cskt~spras = sy-langu
      AND csks~kokrs = cskt~kokrs
      AND csks~kostl = cskt~kostl
      AND csks~datbi = cskt~datbi
  WHERE
    csks~kokrs =  u_kokrs
    AND csks~kostl =  u_kostl
    AND csks~datbi >= u_date
    AND csks~datab <= u_date
  .

  IF sy-subrc <> 0.
    MESSAGE
        s025(zspp01) DISPLAY LIKE 'E'.
    LEAVE LIST-PROCESSING.
  ENDIF.
ENDFORM.


*&---------------------------------------------------------------------*
*&      Form  get_cosp
*&---------------------------------------------------------------------*
*       cospから値を取得する。
*       キーとして用いるobjnr（対象番号）は、ust_csks_csktから取得する。
*----------------------------------------------------------------------*
FORM get_cosp
  USING
      ust_csks_cskt TYPE gtst_csks_cskt

  CHANGING
      cit_cosp TYPE gtit_cosp.

  CONSTANTS:
      cns_lednr TYPE cosp-lednr VALUE  '00',
      cns_wrttp TYPE cosp-wrttp VALUE  '04',
      cns_versn TYPE cosp-versn VALUE '000'.

  SELECT
    gjahr
    kstar
    beknz
    twaer
    wtg001
    wtg002
    wtg003
    wtg004
    wtg005
    wtg006
    wtg007
    wtg008
    wtg009
    wtg010
    wtg011
    wtg012
  INTO
    CORRESPONDING FIELDS OF TABLE cit_cosp
  FROM
    cosp
  WHERE
    cosp~lednr = cns_lednr
    AND cosp~objnr = ust_csks_cskt-objnr
    AND cosp~wrttp = cns_wrttp
    AND cosp~versn = cns_versn
    AND cosp~kstar IN s_kstar
  .

  SORT cit_cosp
      BY gjahr
         kstar.
ENDFORM.


*&---------------------------------------------------------------------*
*&      Form  print_csks_and_cskt
*&---------------------------------------------------------------------*
*       csks csktから得た値を、項目名を添えて枠付きで出力する
*----------------------------------------------------------------------*
FORM print_csks_and_cskt
  USING
      ust_csks_cskt TYPE gtst_csks_cskt.

  WRITE:/
      sy-uline(120),       AT  /1 sy-vline,
      text-001,            AT  12 sy-vline,
      text-002,            AT  25 sy-vline,
      text-003,            AT  38 sy-vline,
      text-004,            AT  51 sy-vline,
      text-005,            AT  75 sy-vline,
      text-006,            AT 120 sy-vline.

  WRITE:                   AT  /1 sy-vline,
      ust_csks_cskt-kokrs, AT  12 sy-vline,
      ust_csks_cskt-kostl, AT  25 sy-vline,
      ust_csks_cskt-datbi, AT  38 sy-vline,
      ust_csks_cskt-datab, AT  51 sy-vline,
      ust_csks_cskt-objnr, AT  75 sy-vline,
      ust_csks_cskt-ltext, AT 120 sy-vline,
      / sy-uline(120).
ENDFORM.


*&---------------------------------------------------------------------*
*&      Form  print_cosp
*&---------------------------------------------------------------------*
*       cospから得た値を、項目名を添えて枠付きで出力する
*----------------------------------------------------------------------*
FORM print_cosp
  USING
      uit_cosp TYPE gtit_cosp.

  DATA:
      lst_cosp TYPE gtst_cosp.

  WRITE:/
      sy-uline,      AT  /1 sy-vline,
      text-101,      AT  12 sy-vline,
      text-102,      AT  23 sy-vline,
      text-103,      AT  41 sy-vline,
      text-104,      AT  66 sy-vline,
      text-105,      AT  91 sy-vline,
      text-106,      AT 116 sy-vline,
      text-107,      AT 141 sy-vline,
      text-108,      AT 166 sy-vline,
      text-109,      AT 191 sy-vline,
      text-110,      AT 216 sy-vline,
      text-111,      AT 241 sy-vline,
      text-112,      AT 266 sy-vline,
      text-113,      AT 292 sy-vline,
      text-114,      AT 318 sy-vline,
      text-115,      AT 344 sy-vline,
      / sy-uline.

  LOOP AT uit_cosp INTO lst_cosp.
    WRITE:                                        AT  /1 sy-vline,
        lst_cosp-gjahr,                           AT  12 sy-vline,
        lst_cosp-kstar,                           AT  23 sy-vline,
        lst_cosp-beknz,                           AT  41 sy-vline,
        lst_cosp-wtg001 CURRENCY lst_cosp-twaer,  AT  66 sy-vline,
        lst_cosp-wtg002 CURRENCY lst_cosp-twaer,  AT  91 sy-vline,
        lst_cosp-wtg003 CURRENCY lst_cosp-twaer,  AT 116 sy-vline,
        lst_cosp-wtg004 CURRENCY lst_cosp-twaer,  AT 141 sy-vline,
        lst_cosp-wtg005 CURRENCY lst_cosp-twaer,  AT 166 sy-vline,
        lst_cosp-wtg006 CURRENCY lst_cosp-twaer,  AT 191 sy-vline,
        lst_cosp-wtg007 CURRENCY lst_cosp-twaer,  AT 216 sy-vline,
        lst_cosp-wtg008 CURRENCY lst_cosp-twaer,  AT 241 sy-vline,
        lst_cosp-wtg009 CURRENCY lst_cosp-twaer,  AT 266 sy-vline,
        lst_cosp-wtg010 CURRENCY lst_cosp-twaer,  AT 292 sy-vline,
        lst_cosp-wtg011 CURRENCY lst_cosp-twaer,  AT 318 sy-vline,
        lst_cosp-wtg012 CURRENCY lst_cosp-twaer,  AT 344 sy-vline.
  ENDLOOP.

  WRITE:/
      sy-uline.

ENDFORM.
