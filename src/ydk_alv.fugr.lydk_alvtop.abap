FUNCTION-POOL ydk_alv.                      "MESSAGE-ID ..
*&---------------------------------------------------------------------*
*& Autor: Kiyanov Dmitry
*& Email: DKiyanov@mail.ru
*& Site: https://github.com/DKiyanov
*& utilities for ALV
*&---------------------------------------------------------------------*

DATA: BEGIN OF gt_cinfo OCCURS 0,
        cvnum      TYPE n LENGTH 2, "  номер преобразования
        program    TYPE string,
        form       TYPE string,
        instance   TYPE REF TO object,
        classname  TYPE string,
        method     TYPE string,
        domname    TYPE string,
        itval      TYPE vrm_values,
        itmctx     TYPE STANDARD TABLE OF string,
        output_len TYPE i,
      END   OF gt_cinfo.

DATA: gv_last_cvnum TYPE n LENGTH 2.

FORM prepare_mctext CHANGING str.
  CONDENSE str NO-GAPS.
  TRANSLATE str TO UPPER CASE.
ENDFORM.

FORM input USING cvnum external CHANGING internal.
  CLEAR internal.

  READ TABLE gt_cinfo ASSIGNING FIELD-SYMBOL(<cinfo>) WITH KEY cvnum = cvnum.
  IF sy-subrc <> 0.
    MESSAGE |conversion setting not found. see FM YDK_CONVERSION_EXIT_REGISTER| TYPE 'X'.
  ENDIF.

  IF <cinfo>-form IS NOT INITIAL.
    PERFORM (<cinfo>-form) IN PROGRAM (<cinfo>-program) USING 'I' external CHANGING internal.
  ELSEIF <cinfo>-method IS NOT INITIAL.
    DATA: rptab TYPE abap_parmbind_tab.
    rptab = VALUE #(
      ( name = 'KIND'   value = REF #( 'I' ) )
      ( name = 'INPUT'  value = REF #( external ) )
      ( name = 'OUTPUT' value = REF #( internal ) )
    ).

    IF <cinfo>-classname IS NOT INITIAL.
      CALL METHOD (<cinfo>-classname)=>(<cinfo>-method) PARAMETER-TABLE rptab.
    ELSE.
      CALL METHOD <cinfo>-instance->(<cinfo>-method) PARAMETER-TABLE rptab.
    ENDIF.
  ELSE.
    FIELD-SYMBOLS <val> LIKE LINE OF gt_cinfo-itval.

    READ TABLE <cinfo>-itval ASSIGNING <val> WITH KEY text = external.
    IF sy-subrc <> 0.
      DATA: lv_external TYPE string.
      lv_external = external.
      PERFORM prepare_mctext CHANGING lv_external.

      READ TABLE <cinfo>-itmctx WITH KEY table_line = lv_external TRANSPORTING NO FIELDS.
      IF sy-subrc = 0.
        READ TABLE <cinfo>-itval ASSIGNING <val> INDEX sy-tabix.
      ENDIF.
    ENDIF.

    IF <val> IS ASSIGNED.
      internal = <val>-key.
    ENDIF.
  ENDIF.
ENDFORM.

FORM output USING cvnum internal CHANGING external.
  CLEAR external.

  READ TABLE gt_cinfo ASSIGNING FIELD-SYMBOL(<cinfo>) WITH KEY cvnum = cvnum.
  IF sy-subrc <> 0.
    MESSAGE |conversion setting not found. see FM YDK_CONVERSION_EXIT_REGISTER| TYPE 'X'.
  ENDIF.

  IF <cinfo>-form IS NOT INITIAL.
    PERFORM (<cinfo>-form) IN PROGRAM (<cinfo>-program) USING 'O' internal CHANGING external.
  ELSEIF <cinfo>-method IS NOT INITIAL.
    DATA: rptab TYPE abap_parmbind_tab.
    rptab = VALUE #(
      ( name = 'KIND'   value = REF #( 'O' ) )
      ( name = 'INPUT'  value = REF #( internal ) )
      ( name = 'OUTPUT' value = REF #( external ) )
    ).

    IF <cinfo>-classname IS NOT INITIAL.
      CALL METHOD (<cinfo>-classname)=>(<cinfo>-method) PARAMETER-TABLE rptab.
    ELSE.
      CALL METHOD <cinfo>-instance->(<cinfo>-method) PARAMETER-TABLE rptab.
    ENDIF.
  ELSE.
    FIELD-SYMBOLS <val> LIKE LINE OF gt_cinfo-itval.

    READ TABLE <cinfo>-itval ASSIGNING <val> WITH KEY key = internal.

    IF <val> IS ASSIGNED.
      external = <val>-text.
    ENDIF.
  ENDIF.
ENDFORM.
