FUNCTION ydk_alv_fcat_build .
*"----------------------------------------------------------------------
*"*"Локальный интерфейс:
*"  IMPORTING
*"     REFERENCE(ALV_STRUT_KEY) TYPE  YDK_ALV_STRUTURE_KEY
*"     VALUE(STRUCTURES) OPTIONAL
*"     VALUE(SAVE_EXCLUDE_FILTER) TYPE  CLIKE OPTIONAL
*"     REFERENCE(DEL_OLD_FIELD) TYPE  ABAP_BOOL DEFAULT ABAP_TRUE
*"  TABLES
*"      IN_FCAT OPTIONAL
*"      ALV_TAB
*"      FCAT
*"----------------------------------------------------------------------
*& Author: Kiyanov Dmitry
*& Email: DKiyanov@mail.ru
*& Site: https://github.com/DKiyanov
*&---------------------------------------------------------------------*
*& STRUCTURES - a list of dictionary structures / tables through "," from which a description
*& can be taken for the ALV_TAB fields, the search is performed by the field name,
*& the setting is performed in the YDK_ALV_FCAT program, transaction YDKFCAT

  DATA: ydk_alv_fcat TYPE ydk_alv_fcat.
  DATA: ydk_alv_fcat_txt TYPE ydk_alv_fcat_txt.
  DATA: it_comp TYPE if_salv_bs_t_data=>t_type_component.
  FIELD-SYMBOLS <comp>  LIKE LINE OF it_comp.

  DATA: sfc  TYPE STANDARD TABLE OF ydk_alv_fcat WITH HEADER LINE.
  DATA: sfct TYPE STANDARD TABLE OF ydk_alv_fcat_txt WITH HEADER LINE.
  FIELD-SYMBOLS <sfc_row> LIKE LINE OF sfc.

  DATA: new_field TYPE c.

  DATA: xfcat TYPE lvc_t_fcat.
  DATA: xin_fcat TYPE lvc_t_fcat.

  DATA: is_lvc TYPE c.
  DATA: in_on  TYPE c.
  DATA: in_set TYPE c.

  DATA: itstr TYPE STANDARD TABLE OF string WITH HEADER LINE.
  DATA: rtab TYPE RANGE OF dd03t-tabname WITH HEADER LINE.

  FIELD-SYMBOLS <fcat> TYPE lvc_t_fcat.
  FIELD-SYMBOLS <in_fcat> TYPE lvc_t_fcat.

  FIELD-SYMBOLS <fc> LIKE LINE OF xfcat.
  FIELD-SYMBOLS <in_fc> LIKE LINE OF xin_fcat.

  FIELD-SYMBOLS <fs>.

  rtab(3)  = 'IEQ'.
  rtab-low = alv_strut_key.
  APPEND rtab.

  IF structures IS SUPPLIED.
    CONDENSE structures NO-GAPS.
    TRANSLATE structures TO UPPER CASE.
    SPLIT structures AT ',' INTO TABLE itstr.
    LOOP AT itstr.
      rtab-low = itstr.
      APPEND rtab.
    ENDLOOP.
  ENDIF.

  DATA: rfilter TYPE RANGE OF fieldname. " Range исключаемых из сохранения полей
  IF save_exclude_filter IS SUPPLIED.
    CONDENSE save_exclude_filter NO-GAPS.
    TRANSLATE save_exclude_filter TO UPPER CASE.
    SPLIT save_exclude_filter AT ';' INTO TABLE itstr.

    LOOP AT itstr.
      APPEND VALUE #( sign = 'I' option = 'CP' low = itstr ) TO rfilter.
    ENDLOOP.
  ENDIF.
  IF rfilter IS INITIAL.
    APPEND VALUE #( sign = 'I' option = 'EQ' low = '@!@!@!' ) TO rfilter. " Это чтоб range был не пустым, иначе в него будут попадать все поля
  ENDIF.

  IF in_fcat IS SUPPLIED.
    in_on = 'X'.

    ASSIGN COMPONENT 'STYLE' OF STRUCTURE in_fcat TO <fs>.
    IF sy-subrc = 0. " значит LVC
      ASSIGN in_fcat[] TO <in_fcat>.
    ELSE.
      CALL FUNCTION 'LVC_TRANSFER_FROM_SLIS'
        EXPORTING
          it_fieldcat_alv = in_fcat[]
        IMPORTING
          et_fieldcat_lvc = xin_fcat[]
        TABLES
          it_data         = alv_tab.

      ASSIGN xin_fcat[] TO <in_fcat>.
    ENDIF.
  ENDIF.

  ASSIGN COMPONENT 'STYLE' OF STRUCTURE fcat TO <fs>.
  IF sy-subrc = 0. " значит LVC
    is_lvc = 'X'.
    ASSIGN fcat[] TO <fcat>.
  ELSE.
    ASSIGN xfcat[] TO <fcat>.
  ENDIF.

  SELECT * INTO TABLE sfc
    FROM ydk_alv_fcat
   WHERE alv_strut_key = alv_strut_key.

  SELECT * INTO TABLE sfct
    FROM ydk_alv_fcat_txt
   WHERE alv_strut_key = alv_strut_key
     AND spras = sy-langu.

  it_comp = cl_salv_bs_ddic=>get_components_by_data( alv_tab ).

* удаляем описания вложенных таблиц
  LOOP AT it_comp ASSIGNING <comp> WHERE kind = 'T' AND id IS NOT INITIAL.
    PERFORM del_rec USING it_comp CHANGING <comp>-id.
  ENDLOOP.

  LOOP AT it_comp ASSIGNING <comp> WHERE id IS INITIAL OR kind = 'S'.
    DELETE <fcat> WHERE fieldname = <comp>-name.
  ENDLOOP.

  DELETE it_comp WHERE id IS INITIAL.

* удаляем не развёрнутые структуры
  DELETE it_comp WHERE kind = 'S'.

  LOOP AT it_comp ASSIGNING <comp>.
    READ TABLE <fcat> ASSIGNING <fc> WITH KEY fieldname = <comp>-sub_path.
    IF sy-subrc <> 0.
      IF <comp>-kind <> 'E'.
        CONTINUE.
      ENDIF.

      APPEND INITIAL LINE TO <fcat> ASSIGNING <fc>.
    ENDIF.

    IF <comp>-kind <> 'E'.
      <fc>-fieldname = <comp>-sub_path.
      <fc>-tech = 'X'.
      CONTINUE.
    ENDIF.

    CLEAR in_set.
    IF in_on = 'X'.
      READ TABLE <in_fcat> ASSIGNING <in_fc> WITH KEY fieldname = <comp>-sub_path.
      IF sy-subrc = 0.
        MOVE-CORRESPONDING <in_fc> TO <fc>.
        in_set = 'X'.
      ENDIF.
    ENDIF.

    READ TABLE sfct WITH KEY fieldname = <comp>-sub_path.
    IF sy-subrc <> 0.
      CLEAR sfct.
    ENDIF.

    READ TABLE sfc WITH KEY fieldname = <comp>-sub_path.
    IF sy-subrc = 0.
      IF in_set = 'X' AND sfc-in_fcat = 'X'.
      ELSE.
        MOVE-CORRESPONDING sfct TO <fc>.
        MOVE-CORRESPONDING sfc  TO <fc>.
      ENDIF.
    ELSEIF in_set = 'X'.
      CLEAR <fc>-fieldname.

      IF <fc>-reptext IS INITIAL.
        <fc>-reptext = <fc>-coltext.
      ENDIF.

      IF  <fc>-reptext   IS INITIAL
      AND <fc>-scrtext_l IS INITIAL
      AND <fc>-scrtext_m IS INITIAL
      AND <fc>-scrtext_s IS INITIAL.
        CLEAR in_set.
      ENDIF.
    ENDIF.

    <fc>-inttype   = <comp>-s_elem-inttype.
    <fc>-intlen    = <comp>-s_elem-intlen.
    <fc>-decimals  = <comp>-s_elem-decimals.
    <fc>-rollname  = <comp>-s_elem-dataelement.
    <fc>-datatype  = <comp>-s_elem-datatype.

    IF <fc>-convexit IS INITIAL.
      <fc>-convexit  = <comp>-s_elem-conv.
    ENDIF.
    IF <fc>-outputlen IS INITIAL.
      <fc>-outputlen = <comp>-s_elem-outputlen.
    ENDIF.

    IF NOT <fc>-rollname IS INITIAL
    AND <fc>-fieldname IS INITIAL
    AND in_set IS INITIAL.
      SELECT SINGLE ddtext scrtext_l scrtext_m scrtext_s INTO
        (<fc>-reptext, <fc>-scrtext_l, <fc>-scrtext_m, <fc>-scrtext_s)
        FROM dd04t
       WHERE rollname   EQ <fc>-rollname
         AND ddlanguage EQ sy-langu
         AND as4local   EQ 'A'
         AND as4vers    EQ 0.

      SELECT SINGLE lowercase INTO <fc>-lowercase
        FROM dd04l
       WHERE rollname   EQ <fc>-rollname
         AND as4local   EQ 'A'
         AND as4vers    EQ 0.
    ENDIF.

    IF <fc>-fieldname IS INITIAL.
      <fc>-fieldname = <comp>-sub_path.

      IF <fc>-rollname IS INITIAL
      AND in_set IS INITIAL.
        SELECT SINGLE rollname INTO <fc>-rollname
          FROM dd03l
         WHERE tabname IN rtab
           AND as4local = 'A'
           AND fieldname = <fc>-fieldname.

        IF NOT <fc>-rollname IS INITIAL.
          SELECT SINGLE ddtext scrtext_l scrtext_m scrtext_s INTO
            (<fc>-reptext, <fc>-scrtext_l, <fc>-scrtext_m, <fc>-scrtext_s)
            FROM dd04t
           WHERE rollname   EQ <fc>-rollname
             AND ddlanguage EQ sy-langu
             AND as4local   EQ 'A'
             AND as4vers    EQ 0.

          SELECT SINGLE lowercase INTO <fc>-lowercase
            FROM dd04l
           WHERE rollname   EQ <fc>-rollname
             AND as4local   EQ 'A'
             AND as4vers    EQ 0.
        ELSE.
          SELECT SINGLE ddtext INTO <fc>-reptext
            FROM dd03t
           WHERE tabname IN rtab
             AND ddlanguage = sy-langu
             AND as4local = 'A'
             AND fieldname = <fc>-fieldname.
          IF sy-subrc = 0.
            <fc>-scrtext_l = <fc>-reptext.
            <fc>-scrtext_s = <fc>-reptext.
            <fc>-scrtext_m = <fc>-reptext.
          ENDIF.
        ENDIF.
      ENDIF.

      <fc>-tabname = '1'.

      IF <fc>-fieldname NOT IN rfilter.
        MOVE-CORRESPONDING <fc> TO ydk_alv_fcat.
        ydk_alv_fcat-alv_strut_key = alv_strut_key.
        INSERT ydk_alv_fcat FROM ydk_alv_fcat.

        MOVE-CORRESPONDING <fc> TO ydk_alv_fcat_txt.
        ydk_alv_fcat_txt-alv_strut_key = alv_strut_key.
        ydk_alv_fcat_txt-spras         = sy-langu.
        INSERT ydk_alv_fcat_txt FROM ydk_alv_fcat_txt.

        new_field = 'X'.
      ENDIF.
    ENDIF.
  ENDLOOP.

  IF del_old_field = abap_true.
    DATA: rdel_fname TYPE RANGE OF fieldname.

    LOOP AT sfc ASSIGNING <sfc_row>.
      IF NOT line_exists( <fcat>[ fieldname = <sfc_row>-fieldname ] )
      OR <sfc_row>-fieldname IN rfilter.
        APPEND VALUE #( sign = 'I' option = 'EQ' low = <sfc_row>-fieldname ) TO rdel_fname.
      ENDIF.
    ENDLOOP.

    IF rdel_fname[] IS NOT INITIAL.
      DELETE FROM ydk_alv_fcat
       WHERE alv_strut_key = alv_strut_key
         AND fieldname IN rdel_fname.

      DELETE FROM ydk_alv_fcat_txt
       WHERE alv_strut_key = alv_strut_key
         AND fieldname IN rdel_fname.
    ENDIF.
  ENDIF.

  LOOP AT sfc ASSIGNING <sfc_row> WHERE auto_convert = abap_true.
    IF line_exists( <fcat>[ fieldname = <sfc_row>-fieldname ] ).
      CALL FUNCTION 'YDK_CONVERSION_EXIT_REGISTER'
        EXPORTING
          fieldname = <sfc_row>-fieldname
        CHANGING
          fcat      = <fcat>.
    ENDIF.
  ENDLOOP.

  IF is_lvc IS INITIAL.
    CALL FUNCTION 'LVC_TRANSFER_TO_SLIS'
      EXPORTING
        it_fieldcat_lvc = xfcat[]
      IMPORTING
        et_fieldcat_alv = fcat[]
      TABLES
        it_data         = alv_tab.
  ENDIF.

  IF new_field = 'X'.
    AUTHORITY-CHECK OBJECT 'S_DEVELOP'
             ID 'DEVCLASS' DUMMY
             ID 'OBJTYPE'  DUMMY
             ID 'OBJNAME'  DUMMY
             ID 'P_GROUP'  DUMMY
             ID 'ACTVT' FIELD '02'.
    IF sy-subrc = 0.
      DATA: answer TYPE c.
      CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
        EXPORTING
*         DEFAULTOPTION  = 'Y'
          textline1      = 'New fields added to catalog,'(001)
          textline2      = 'go to setup'(002)
          titel          = 'ALV setting'(003)
*         START_COLUMN   = 25
*         START_ROW      = 6
          cancel_display = ' '
        IMPORTING
          answer         = answer.
      IF answer = 'J'.
        SUBMIT ydk_alv_fcat AND RETURN
          WITH alvstruc = alv_strut_key
          WITH spras    = sy-langu.
      ENDIF.
    ELSE.
      MESSAGE 'New fields have been added to the catalog, notify those responsible for setting up.'(004) TYPE 'I'.
    ENDIF.
  ENDIF.
ENDFUNCTION.

FORM del_rec USING it_comp TYPE if_salv_bs_t_data=>t_type_component CHANGING id.
  LOOP AT it_comp ASSIGNING FIELD-SYMBOL(<comp>) WHERE parent_id = id.
    PERFORM del_rec USING it_comp CHANGING <comp>-id.
  ENDLOOP.

  CLEAR id.
ENDFORM.
