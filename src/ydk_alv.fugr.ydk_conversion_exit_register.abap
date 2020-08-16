FUNCTION ydk_conversion_exit_register.
*"----------------------------------------------------------------------
*"*"Локальный интерфейс:
*"  IMPORTING
*"     REFERENCE(FIELDNAME) TYPE  CLIKE
*"     REFERENCE(CALLBACK_PROGRAM) TYPE  CLIKE OPTIONAL
*"     REFERENCE(CALLBACK_FORM) TYPE  CLIKE OPTIONAL
*"     REFERENCE(CALLBACK_INSTANCE) TYPE REF TO  OBJECT OPTIONAL
*"     REFERENCE(CALLBACK_CLASSNAME) TYPE  CLIKE OPTIONAL
*"     REFERENCE(CALLBACK_METHOD) TYPE  CLIKE OPTIONAL
*"     REFERENCE(ROLLNAME) TYPE  CLIKE OPTIONAL
*"     REFERENCE(DOMNAME) TYPE  CLIKE OPTIONAL
*"     REFERENCE(VALUES_TAB) TYPE  VRM_VALUES OPTIONAL
*"     REFERENCE(OUTPUT_LEN) TYPE  I OPTIONAL
*"  CHANGING
*"     REFERENCE(FCAT) TYPE  LVC_T_FCAT
*"----------------------------------------------------------------------

* Autor: Kiyanov Dmitry
* Email: DKiyanov@mail.ru
* Site: https://github.com/DKiyanov
*
* FM configures the ALV field catalog entry for the line specified in the fieldname parameter
* customizable fields: convexit, no_convext, outputlen
* 49 different conversions can be specified
* There are several variants for setting up the conversion, but only one of them must be selected using the transmitted parameters
* If no one parameter is filled in - an attempt is made to obtain a conversion based on the FCAT record data.
* Variants:
*   CALLBACK_FORM - CALLBACK_PROGRAM set automaticly to called FM programm
*   CALLBACK_PROGRAM   + CALLBACK_FORM
*   CALLBACK_INSTANCE  + CALLBACK_METHOD
*   CALLBACK_CLASSNAME + CALLBACK_METHOD
*   ROLLNAME - the corresponding domain must have a set of values
*   DOMNAME - domain must have a set of values
*   VALUES_TAB
*
* The incoming parameter "kind" in subroutine (form) and methods (see below) can take values:
* "I" - input conversion
* "O" - output conversion
*
* CALLBACK_PROGRAM + CALLBACK_FORM - form defenition:
*   FORM <form name> USING kind input CHANGING output.
*
* CALLBACK_INSTANCE + CALLBACK_METHOD - method defenition:
*   METHODS <method name>
*     IMPORTING
*       !kind   TYPE clike
*       !input  TYPE any
*     EXPORTING
*       !output TYPE any.
*
* CALLBACK_CLASSNAME + CALLBACK_METHOD - method defenition:
*   CLASS-METHODS conv
*     IMPORTING
*       !kind   TYPE clike
*       !input  TYPE any
*     EXPORTING
*       !output TYPE any.


  DATA: wa_cinfo LIKE LINE OF gt_cinfo.

  IF callback_form IS NOT INITIAL.
    DATA: lv_program TYPE sy-repid.
    IF callback_program IS INITIAL.
      CALL 'AB_GET_CALLER' ID 'PROGRAM' FIELD lv_program.
    ELSE.
      lv_program = callback_program.
    ENDIF.

    READ TABLE gt_cinfo INTO wa_cinfo WITH KEY program = lv_program form = callback_form.
    IF sy-subrc <> 0.
      wa_cinfo-program = lv_program.
      wa_cinfo-form    = callback_form.
    ENDIF.
  ENDIF.

  IF callback_method IS NOT INITIAL.
    IF wa_cinfo IS NOT INITIAL
    OR ( callback_instance IS NOT BOUND AND callback_classname IS INITIAL )
    OR ( callback_instance IS BOUND AND callback_classname IS NOT INITIAL ).
      MESSAGE |Invalid params call FM YDK_CONVERSION_EXIT_REGISTER| TYPE 'X'.
    ENDIF.

    READ TABLE gt_cinfo INTO wa_cinfo WITH KEY instance = callback_instance classname = callback_classname method = callback_method.
    IF sy-subrc <> 0.
      wa_cinfo-instance  = callback_instance.
      wa_cinfo-classname = callback_classname.
      wa_cinfo-method    = callback_method.
    ENDIF.
  ENDIF.

  IF values_tab IS NOT INITIAL.
    IF wa_cinfo IS NOT INITIAL.
      MESSAGE |Invalid params call FM YDK_CONVERSION_EXIT_REGISTER| TYPE 'X'.
    ENDIF.

    READ TABLE gt_cinfo INTO wa_cinfo WITH KEY itval = values_tab.
    IF sy-subrc <> 0.
      wa_cinfo-itval = values_tab.
    ENDIF.
  ENDIF.

  DATA: set_domname TYPE abap_bool.

  IF rollname IS NOT INITIAL.
    IF wa_cinfo IS NOT INITIAL.
      MESSAGE |Invalid params call FM YDK_CONVERSION_EXIT_REGISTER| TYPE 'X'.
    ENDIF.

    DATA: lv_domname TYPE domname.

    SELECT SINGLE domname INTO lv_domname
      FROM dd04l
     WHERE rollname = rollname
       AND as4local = 'A'
       AND as4vers  = '0000'.

    READ TABLE gt_cinfo INTO wa_cinfo WITH KEY domname = lv_domname.
    IF sy-subrc <> 0.
      wa_cinfo-domname = lv_domname.
      set_domname = abap_true.
    ENDIF.
  ENDIF.

  IF domname IS NOT INITIAL.
    IF wa_cinfo IS NOT INITIAL.
      MESSAGE |Invalid params call FM YDK_CONVERSION_EXIT_REGISTER| TYPE 'X'.
    ENDIF.

    READ TABLE gt_cinfo INTO wa_cinfo WITH KEY domname = domname.
    IF sy-subrc <> 0.
      wa_cinfo-domname = domname.
      set_domname = abap_true.
    ENDIF.
  ENDIF.

  IF wa_cinfo IS INITIAL.
    PERFORM get_domname_from_fcat USING fieldname fcat CHANGING wa_cinfo-domname.
    set_domname = abap_true.
  ENDIF.

  IF wa_cinfo IS INITIAL.
    MESSAGE |Invalid params call FM YDK_CONVERSION_EXIT_REGISTER| TYPE 'X'.
  ENDIF.

  IF wa_cinfo-domname IS NOT INITIAL AND set_domname = abap_true.
    PERFORM get_domain_values USING wa_cinfo-domname CHANGING wa_cinfo-itval.
  ENDIF.

  IF wa_cinfo-cvnum IS INITIAL.
    ADD 1 TO gv_last_cvnum.
    wa_cinfo-cvnum = gv_last_cvnum.

    wa_cinfo-output_len = output_len.

    LOOP AT wa_cinfo-itval ASSIGNING FIELD-SYMBOL(<val>).
      IF wa_cinfo-output_len < strlen( <val>-text ).
        wa_cinfo-output_len = strlen( <val>-text ).
      ENDIF.

      APPEND <val>-text TO wa_cinfo-itmctx ASSIGNING FIELD-SYMBOL(<mctx>).
      PERFORM prepare_mctext CHANGING <mctx>.
    ENDLOOP.

    APPEND wa_cinfo TO gt_cinfo.
  ENDIF.

  DATA: lv_fieldname TYPE string.
  lv_fieldname = fieldname.
  CONDENSE lv_fieldname NO-GAPS.
  TRANSLATE lv_fieldname TO UPPER CASE.

  DATA: itfstr TYPE STANDARD TABLE OF string.
  SPLIT lv_fieldname AT ';' INTO TABLE itfstr.

  DATA: rfname TYPE RANGE OF fieldname.

  LOOP AT itfstr ASSIGNING FIELD-SYMBOL(<fstr>).
    APPEND VALUE #( sign = 'I' option = 'CP' low = <fstr> ) TO rfname.
  ENDLOOP.

  DATA: lv_convexit TYPE c LENGTH 5.
  lv_convexit = |YDK{ wa_cinfo-cvnum }|.

  LOOP AT fcat ASSIGNING FIELD-SYMBOL(<fc>) WHERE fieldname in rfname.
    <fc>-convexit  = lv_convexit.
    <fc>-no_convext = abap_false.

    IF output_len IS NOT INITIAL.
      <fc>-outputlen = output_len.
    ELSEIF wa_cinfo-output_len IS NOT INITIAL.
      <fc>-outputlen = wa_cinfo-output_len.
    ENDIF.
  ENDLOOP.
  IF sy-subrc <> 0.
    MESSAGE |Invalid params call FM YDK_CONVERSION_EXIT_REGISTER| TYPE 'X'.
  ENDIF.
ENDFUNCTION.

FORM get_domain_values USING domname CHANGING itval TYPE vrm_values.
  DATA: itdv TYPE STANDARD TABLE OF dd07v.

  CALL FUNCTION 'DDUT_DOMVALUES_GET'
    EXPORTING
      name          = CONV ddobjname( domname )
      texts_only    = 'X'
    TABLES
      dd07v_tab     = itdv
    EXCEPTIONS
      illegal_input = 1
      OTHERS        = 2.
  IF itdv[] IS INITIAL.
    MESSAGE |Invalid params call FM YDK_CONVERSION_EXIT_REGISTER no values in domain { domname }| TYPE 'X'.
  ENDIF.

  LOOP AT itdv ASSIGNING FIELD-SYMBOL(<dv>).
    APPEND VALUE #( key = <dv>-domvalue_l text = <dv>-ddtext ) TO itval.
  ENDLOOP.
ENDFORM.

FORM get_domname_from_fcat
  USING
    fieldname
    fcat TYPE lvc_t_fcat
  CHANGING
    domname.

  READ TABLE fcat ASSIGNING FIELD-SYMBOL(<fc>) WITH KEY fieldname = fieldname.
  CHECK sy-subrc = 0.

  IF <fc>-domname IS NOT INITIAL.
    domname = <fc>-domname.
  ELSEIF <fc>-rollname IS NOT INITIAL.
    SELECT SINGLE domname INTO domname
      FROM dd04l
     WHERE rollname = <fc>-rollname
       AND as4local = 'A'
       AND as4vers  = '0000'.
  ELSEIF <fc>-ref_field IS NOT INITIAL AND <fc>-ref_table IS NOT INITIAL.
    SELECT SINGLE domname INTO domname
      FROM dd03l
     WHERE tabname   = <fc>-ref_table
       AND fieldname = <fc>-ref_field
       AND as4local  = 'A'
       AND as4vers   = '0000'.
  ENDIF.
ENDFORM.
