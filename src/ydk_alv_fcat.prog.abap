*&---------------------------------------------------------------------*
*& Report  YDK_ALV_FCAT
*& ALV field catalog tuning
*&---------------------------------------------------------------------*
*& Autor: Kiyanov Dmitry
*& Email: DKiyanov@mail.ru
*& site: http://saptaskbar.ru/
*&---------------------------------------------------------------------*
REPORT  ydk_alv_fcat.

TYPE-POOLS: slis.

TABLES: sscrfields.

PARAMETERS: alvstruc TYPE ydk_alv_fcat-alv_strut_key OBLIGATORY MEMORY ID ydkfcat.
PARAMETERS: spras TYPE sy-langu OBLIGATORY DEFAULT sy-langu.

DATA: BEGIN OF itd OCCURS 0.
        INCLUDE STRUCTURE ydk_alv_fcat.
DATA:
  reptext   TYPE ydk_alv_fcat_txt-reptext,
  scrtext_l TYPE ydk_alv_fcat_txt-scrtext_l,
  scrtext_m TYPE ydk_alv_fcat_txt-scrtext_m,
  scrtext_s TYPE ydk_alv_fcat_txt-scrtext_s.
DATA: END   OF itd.

FIELD-SYMBOLS <d> LIKE LINE OF itd.

DATA: alvg TYPE REF TO cl_gui_alv_grid.

CLASS lcl_event_receiver DEFINITION DEFERRED.
DATA: g_event_receiver TYPE REF TO lcl_event_receiver.

DATA: changed TYPE c.
DATA: g_order TYPE e070-trkorr.
DATA: view_only TYPE c.

CLASS lcl_event_receiver DEFINITION.
  PUBLIC SECTION.
    METHODS handle_data_changed FOR EVENT data_changed OF cl_gui_alv_grid
      IMPORTING
          er_data_changed.
ENDCLASS.

CLASS lcl_event_receiver IMPLEMENTATION.
  METHOD handle_data_changed." FOR EVENT data_changed OF cl_gui_alv_grid
*   IMPORTING
*     er_data_changed.
    changed = 'X'.
  ENDMETHOD.
ENDCLASS.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR alvstruc.
  PERFORM f4_alvstruc.

START-OF-SELECTION.
  PERFORM init.
  PERFORM get_data.
  PERFORM alv_show.

FORM init.
  AUTHORITY-CHECK OBJECT 'S_DEVELOP'
           ID 'DEVCLASS' DUMMY
           ID 'OBJTYPE'  DUMMY
           ID 'OBJNAME'  DUMMY
           ID 'P_GROUP'  DUMMY
           ID 'ACTVT' FIELD '02'.
  IF sy-subrc <> 0.
    view_only = 'X'.
  ENDIF.
ENDFORM.

FORM get_data.
  DATA: ittxt TYPE STANDARD TABLE OF ydk_alv_fcat_txt WITH HEADER LINE.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE itd
    FROM ydk_alv_fcat
   WHERE alv_strut_key = alvstruc.

  SELECT * INTO TABLE ittxt
    FROM ydk_alv_fcat_txt
   WHERE alv_strut_key = alvstruc
     AND spras = spras.

  LOOP AT ittxt.
    READ TABLE itd ASSIGNING <d> WITH KEY fieldname = ittxt-fieldname.
    IF sy-subrc <> 0.
      APPEND INITIAL LINE TO itd ASSIGNING <d>.
    ENDIF.

    MOVE-CORRESPONDING ittxt TO <d>.
  ENDLOOP.

  SORT itd BY fieldname.
ENDFORM.                    "get_data

FORM alv_show.
  DATA: fc TYPE lvc_t_fcat WITH HEADER LINE.
  DATA: repid TYPE sy-repid.
  DATA: layout TYPE lvc_s_layo.

  CALL FUNCTION 'YDK_ALV_FCAT_BUILD'
    EXPORTING
      alv_strut_key = 'YDK_ALV_FCAT'
      structures    = 'YDK_ALV_FCAT'
    TABLES
      alv_tab       = itd
      fcat          = fc.

  LOOP AT fc ASSIGNING FIELD-SYMBOL(<fc>).
    CASE <fc>-fieldname.
      WHEN 'ALV_STRUT_KEY'. <fc>-tech      = 'X'.
      WHEN 'TECH'.          <fc>-checkbox  = 'X'.
      WHEN 'CHECKBOX'.      <fc>-checkbox  = 'X'.
      WHEN 'EDIT'.          <fc>-checkbox  = 'X'.
      WHEN 'HOTSPOT'.       <fc>-checkbox  = 'X'.
      WHEN 'LOWERCASE'.     <fc>-checkbox  = 'X'.
      WHEN 'IN_FCAT'.       <fc>-checkbox  = 'X'.
      WHEN 'REPTEXT'.       <fc>-lowercase = 'X'.
      WHEN 'SCRTEXT_L'.     <fc>-lowercase = 'X'.
      WHEN 'SCRTEXT_M'.     <fc>-lowercase = 'X'.
      WHEN 'SCRTEXT_S'.     <fc>-lowercase = 'X'.
    ENDCASE.
  ENDLOOP.

  IF view_only IS INITIAL.
    layout-edit = 'X'.
  ENDIF.

  repid = sy-repid.
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY_LVC'
    EXPORTING
*     I_INTERFACE_CHECK           = ' '
*     I_BYPASSING_BUFFER          =
*     I_BUFFER_ACTIVE             =
      i_callback_program          = repid
      i_callback_pf_status_set    = 'ALV_STATUS_SET'
      i_callback_user_command     = 'ALV_USER_COMMAND'
*     I_CALLBACK_TOP_OF_PAGE      = ' '
      i_callback_html_top_of_page = 'ALV_HTML_TOP_OF_PAGE'
*     I_CALLBACK_HTML_END_OF_LIST = ' '
*     I_STRUCTURE_NAME            =
*     I_BACKGROUND_ID             = ' '
*     I_GRID_TITLE                =
*     I_GRID_SETTINGS             =
      is_layout_lvc               = layout
      it_fieldcat_lvc             = fc[]
*     IT_EXCLUDING                =
*     IT_SPECIAL_GROUPS_LVC       =
*     IT_SORT_LVC                 =
*     IT_FILTER_LVC               =
*     IT_HYPERLINK                =
*     IS_SEL_HIDE                 =
*     I_DEFAULT                   = 'X'
      i_save                      = 'A'
*     IS_VARIANT                  =
*     IT_EVENTS                   =
*     IT_EVENT_EXIT               =
*     IS_PRINT_LVC                =
*     IS_REPREP_ID_LVC            =
*     I_SCREEN_START_COLUMN       = 0
*     I_SCREEN_START_LINE         = 0
*     I_SCREEN_END_COLUMN         = 0
*     I_SCREEN_END_LINE           = 0
      i_html_height_top           = 6
*     I_HTML_HEIGHT_END           =
*     IT_ALV_GRAPHICS             =
*     IT_EXCEPT_QINFO_LVC         =
*     IR_SALV_FULLSCREEN_ADAPTER  =
    TABLES
      t_outtab                    = itd
    EXCEPTIONS
      program_error               = 1
      OTHERS                      = 2.
ENDFORM.                    "alv_show

FORM alv_html_top_of_page USING document TYPE REF TO cl_dd_document.
  DATA: str TYPE c LENGTH 255.

  str = |Structure: { alvstruc }|.
  CALL METHOD document->add_text
    EXPORTING
      text = str.

  CALL METHOD document->new_line.

  DATA: lang TYPE c LENGTH 2.
  WRITE spras TO lang.
  str = |Language: { lang }|.
  CALL METHOD document->add_text
    EXPORTING
      text = str.
ENDFORM.

FORM alv_status_set USING extab TYPE slis_t_extab.
  IF alvg IS INITIAL.
    CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
      IMPORTING
        e_grid = alvg.

    CREATE OBJECT g_event_receiver.
    SET HANDLER g_event_receiver->handle_data_changed FOR alvg.
  ENDIF.

  IF view_only = 'X'.
    APPEND 'SAVE' TO extab.
  ENDIF.

  SET PF-STATUS 'ALV' EXCLUDING extab.
ENDFORM.                    "ALV_STATUS

FORM alv_user_command USING ucomm    TYPE sy-ucomm
                            selfield TYPE slis_selfield.

  DATA: answer TYPE c.

  CALL METHOD alvg->check_changed_data.

  READ TABLE itd INDEX selfield-tabindex.

  CASE ucomm.
    WHEN 'MBACK'.
      IF changed = 'X'.
        CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
          EXPORTING
*           DEFAULTOPTION        = 'Y'
            textline1 = 'Есть изменения, сохранить?'
*           TEXTLINE2 = ' '
            titel     = 'Выход'
*           START_COLUMN         = 25
*           START_ROW = 6
*           CANCEL_DISPLAY       = 'X'
          IMPORTING
            answer    = answer.

        IF answer = 'J'.
          PERFORM save.
        ENDIF.

        IF answer = 'A'.
          EXIT.
        ENDIF.
      ENDIF.

      LEAVE TO SCREEN 0.
    WHEN 'DELF'.
      PERFORM delete_row.
      selfield-col_stable = 'X'.
      selfield-row_stable = 'X'.
      selfield-refresh    = 'X'.
    WHEN 'SAVE'.
      PERFORM save.
    WHEN '&IC1'. " Double click
  ENDCASE.
ENDFORM.                    "ALV_COMMAND

FORM save.
  DATA: ok TYPE c.
  DATA: cccoractiv TYPE t000-cccoractiv.

  CHECK view_only IS INITIAL.

  SELECT SINGLE cccoractiv INTO cccoractiv
    FROM t000
   WHERE mandt = sy-mandt.

  IF cccoractiv = '1'.
    PERFORM save_order USING ok.
    CHECK ok = 'X'.
  ENDIF.

  PERFORM save_data.
ENDFORM.

FORM save_data.
  DATA: itfcat TYPE STANDARD TABLE OF ydk_alv_fcat WITH HEADER LINE.
  DATA: ittxt  TYPE STANDARD TABLE OF ydk_alv_fcat_txt WITH HEADER LINE.

  ittxt-spras = spras.
  LOOP AT itd.
    MOVE-CORRESPONDING itd TO itfcat.
    APPEND itfcat.

    MOVE-CORRESPONDING itd TO ittxt.
    APPEND ittxt.
  ENDLOOP.

  DELETE FROM ydk_alv_fcat
   WHERE alv_strut_key = alvstruc.

  DELETE FROM ydk_alv_fcat_txt
   WHERE alv_strut_key = alvstruc
     AND spras = spras.

  INSERT ydk_alv_fcat     FROM TABLE itfcat.
  INSERT ydk_alv_fcat_txt FROM TABLE ittxt.

  COMMIT WORK.

  CLEAR changed.

  MESSAGE 'Данные сохранены' TYPE 'S'.
ENDFORM.

FORM save_order USING ok.
  DATA: tab_ko200 LIKE ko200 OCCURS 0 WITH HEADER LINE.
  DATA: tab_e071k LIKE e071k OCCURS 0 WITH HEADER LINE.

  DATA: BEGIN OF tabkey_fcat,
          alv_strut_key TYPE ydk_alv_fcat-alv_strut_key,
          asterisk      TYPE c,
        END   OF tabkey_fcat.

  DATA: BEGIN OF tabkey_txt,
          alv_strut_key TYPE ydk_alv_fcat_txt-alv_strut_key,
          spras         TYPE ydk_alv_fcat_txt-spras,
          asterisk      TYPE c,
        END   OF tabkey_txt.

  CLEAR ok.

  tabkey_fcat-alv_strut_key = alvstruc.
  tabkey_fcat-asterisk      = '*'.

  tabkey_txt-alv_strut_key = alvstruc.
  tabkey_txt-spras         = spras.
  tabkey_txt-asterisk      = '*'.

  tab_ko200-trkorr     = g_order.
  tab_ko200-pgmid      = 'R3TR'.
  tab_ko200-object     = 'TABU'.
  tab_ko200-objfunc    = 'K'.

  tab_e071k-trkorr     = tab_ko200-trkorr.
  tab_e071k-pgmid      = tab_ko200-pgmid.
  tab_e071k-object     = tab_ko200-object.
  tab_e071k-mastertype = 'TABU'.



  tab_ko200-obj_name   = 'YDK_ALV_FCAT'.
  APPEND tab_ko200.

  tab_e071k-objname    = 'YDK_ALV_FCAT'.
  tab_e071k-mastername = 'YDK_ALV_FCAT'.
  tab_e071k-tabkey     = tabkey_fcat.
  APPEND tab_e071k.

  tab_ko200-obj_name   = 'YDK_ALV_FCAT_TXT'.
  APPEND tab_ko200.

  tab_e071k-objname    = 'YDK_ALV_FCAT_TXT'.
  tab_e071k-mastername = 'YDK_ALV_FCAT_TXT'.
  tab_e071k-tabkey     = tabkey_txt.
  APPEND tab_e071k.

  CALL FUNCTION 'TR_OBJECTS_CHECK'
    TABLES
      wt_ko200                = tab_ko200
      wt_e071k                = tab_e071k
    EXCEPTIONS
      cancel_edit_other_error = 1
      show_only_other_error   = 2
      OTHERS                  = 3.
  IF sy-subrc > 1.
    MESSAGE ID sy-msgid TYPE 'I' NUMBER sy-msgno
       WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    EXIT.
  ENDIF.
  CHECK sy-subrc = 0.

  CALL FUNCTION 'TR_OBJECTS_INSERT'
    EXPORTING
      wi_order                = g_order
    IMPORTING
      we_order                = g_order
    TABLES
      wt_ko200                = tab_ko200
      wt_e071k                = tab_e071k
    EXCEPTIONS
      cancel_edit_other_error = 1
      show_only_other_error   = 2
      OTHERS                  = 3.
  IF sy-subrc > 1.
    MESSAGE ID sy-msgid TYPE 'I' NUMBER sy-msgno
       WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
  CHECK sy-subrc = 0.

  ok = 'X'.
ENDFORM.

FORM read_dynpro_field USING fieldname value.
  DATA: dynpfields TYPE STANDARD TABLE OF dynpread WITH HEADER LINE.

  dynpfields-fieldname = fieldname.
  APPEND dynpfields.

  CALL FUNCTION 'DYNP_VALUES_READ'
    EXPORTING
      dyname               = sy-repid
      dynumb               = sy-dynnr
    TABLES
      dynpfields           = dynpfields
    EXCEPTIONS
      invalid_abapworkarea = 1
      invalid_dynprofield  = 2
      invalid_dynproname   = 3
      invalid_dynpronummer = 4
      invalid_request      = 5
      no_fielddescription  = 6
      invalid_parameter    = 7
      undefind_error       = 8
      double_conversion    = 9
      stepl_not_found      = 10
      OTHERS               = 11.

  READ TABLE dynpfields INDEX 1.
  value = dynpfields-fieldvalue.
ENDFORM.                    "read_dynpro_field

FORM f4_alvstruc.
  DATA: avs TYPE ydk_alv_fcat-alv_strut_key.
  DATA: BEGIN OF itavs OCCURS 0,
          alv_strut_key TYPE ydk_alv_fcat-alv_strut_key,
        END   OF itavs.

  PERFORM read_dynpro_field USING 'ALVSTRUC' avs.
  IF avs CA '*+'.
    TRANSLATE avs USING '*%+_'.
    TRANSLATE avs TO UPPER CASE.
  ELSE.
    avs = '%'.
  ENDIF.

  SELECT DISTINCT alv_strut_key INTO TABLE itavs
    FROM ydk_alv_fcat
   WHERE alv_strut_key LIKE avs.
  CHECK sy-subrc = 0.

  IF lines( itavs ) = 1.
    READ TABLE itavs INDEX 1.
    alvstruc = itavs-alv_strut_key.
    EXIT.
  ENDIF.

  DATA: it_return TYPE STANDARD TABLE OF ddshretval WITH HEADER LINE.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
*     DDIC_STRUCTURE  = ' '
      retfield        = 'ALV_STRUT_KEY'
*     PVALKEY         = ' '
*     DYNPPROG        = ' '
*     DYNPNR          = ' '
*     DYNPROFIELD     = ' '
*     STEPL           = 0
      window_title    = 'Выбор структуры каталога ALV'
*     VALUE           = ' '
      value_org       = 'S'
*     MULTIPLE_CHOICE = ' '
*     DISPLAY         = ' '
*     CALLBACK_PROGRAM       = ' '
*     CALLBACK_FORM   = ' '
*     CALLBACK_METHOD =
*     MARK_TAB        =
*   IMPORTING
*     USER_RESET      =
    TABLES
      value_tab       = itavs
*     FIELD_TAB       =
      return_tab      = it_return
*     DYNPFLD_MAPPING =
    EXCEPTIONS
      parameter_error = 1
      no_values_found = 2
      OTHERS          = 3.
  CHECK sy-subrc = 0.

  READ TABLE it_return INDEX 1.
  alvstruc = it_return-fieldval.
ENDFORM.                                                    "f4_formnm

FORM delete_row.
  DATA: it_rows TYPE STANDARD TABLE OF lvc_s_row WITH HEADER LINE.

  CALL METHOD alvg->get_selected_rows
    IMPORTING
      et_index_rows = it_rows[].

  IF it_rows[] IS INITIAL.
    MESSAGE 'Сначала пометьте строки' TYPE 'S'.
    EXIT.
  ENDIF.

  DATA: answer TYPE c.
  CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
    EXPORTING
*     DEFAULTOPTION  = 'Y'
      textline1      = 'Удалить выделенные поля?'
*     TEXTLINE2      = ' '
      titel          = 'Удаление'
*     START_COLUMN   = 25
*     START_ROW      = 6
      cancel_display = ' '
    IMPORTING
      answer         = answer.
  CHECK answer = 'J'.

  LOOP AT it_rows.
    READ TABLE itd ASSIGNING <d> INDEX it_rows-index.
    CLEAR <d>-fieldname.
  ENDLOOP.

  DELETE itd WHERE fieldname IS INITIAL.
ENDFORM.
